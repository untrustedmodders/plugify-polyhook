#include "callback.hpp"

#include "polyhook2/MemProtector.hpp"

template<typename T>
constexpr asmjit::TypeId GetTypeIdx() noexcept {
	return static_cast<asmjit::TypeId>(asmjit::TypeUtils::TypeIdOfT<T>::kTypeId);
}

asmjit::TypeId PLH::Callback::getTypeId(DataType type) {
	switch (type) {
		case DataType::Void:
			return GetTypeIdx<void>();
		case DataType::Bool:
			return GetTypeIdx<bool>();
		case DataType::Int8:
			return GetTypeIdx<int8_t>();
		case DataType::Int16:
			return GetTypeIdx<int16_t>();
		case DataType::Int32:
			return GetTypeIdx<int32_t>();
		case DataType::Int64:
			return GetTypeIdx<int64_t>();
		case DataType::UInt8:
			return GetTypeIdx<uint8_t>();
		case DataType::UInt16:
			return GetTypeIdx<uint16_t>();
		case DataType::UInt32:
			return GetTypeIdx<uint32_t>();
		case DataType::UInt64:
			return GetTypeIdx<uint64_t>();
		case DataType::Float:
			return GetTypeIdx<float>();
		case DataType::Double:
			return GetTypeIdx<double>();
		case DataType::Pointer:
		case DataType::String:
		case DataType::WString:
		case DataType::Matrix4x4:
			return asmjit::TypeId::kUIntPtr;
		case DataType::Vector2:
#if _WIN32
			return asmjit::TypeId::kInt64;
#else
			return asmjit::TypeId::kFloat64;
#endif
		case DataType::Vector3:
		case DataType::Vector4:
#if _WIN32
			return TypeId::kUIntPtr;
#else
		return asmjit::TypeId::kFloat32x4;
#endif
	}
	return asmjit::TypeId::kVoid;
}

uint64_t PLH::Callback::getJitFunc(const asmjit::FuncSignature& sig, const asmjit::Arch arch, const PLH::Callback::CallbackEntry callback) {
	/*AsmJit is smart enough to track register allocations and will forward
	  the proper registers the right values and fixup any it dirtied earlier.
	  This can only be done if it knows the signature, and ABI, so we give it 
	  them. It also only does this mapping for calls, so we need to generate 
	  calls on our boundaries of transfers when we want argument order correct
	  (ABI stuff is managed for us when calling C code within this project via host mode).
	  It also does stack operations for us including alignment, shadow space, and
	  arguments, everything really. Manual stack push/pop is not supported using
	  the AsmJit compiler, so we must create those nodes, and insert them into
	  the Node list manually to not corrupt the compiler's tracking of things.

	  Inside the compiler, before endFunc only virtual registers may be used. Any
	  concrete physical registers will not have their liveness tracked, so will
	  be spoiled and must be manually marked dirty. After endFunc ONLY concrete
	  physical registers may be inserted as nodes.
	*/
	asmjit::CodeHolder code;        
	auto env = asmjit::Environment::host();
	env.setArch(arch);
	code.init(env);
	
	// initialize function
	asmjit::x86::Compiler cc(&code);            
	asmjit::FuncNode* func = cc.addFunc(sig);              

	/*asmjit::StringLogger log;
	auto kFormatFlags =
			asmjit::FormatFlags::kMachineCode | asmjit::FormatFlags::kExplainImms | asmjit::FormatFlags::kRegCasts
			| asmjit::FormatFlags::kHexImms | asmjit::FormatFlags::kHexOffsets  | asmjit::FormatFlags::kPositions;
	
	log.addFlags(kFormatFlags);
	code.setLogger(&log);*/
	
	// too small to really need it
	func->frame().resetPreservedFP();

	// Create labels
	asmjit::Label supercede = cc.newLabel();
	asmjit::Label override = cc.newLabel();
	asmjit::Label noPost = cc.newLabel();

	// map argument slots to registers, following abi.
	std::vector<asmjit::x86::Reg> argRegisters;
	argRegisters.reserve( sig.argCount());
	for (uint8_t argIdx = 0; argIdx < sig.argCount(); argIdx++) {
		const auto argType = sig.args()[argIdx];

		asmjit::x86::Reg arg;
		if (asmjit::TypeUtils::isInt(argType)) {
			arg = cc.newUIntPtr();
		} else if (asmjit::TypeUtils::isFloat(argType)) {
			arg = cc.newXmm();
		} else {
			//Log::log("Parameters wider than 64bits not supported", ErrorLevel::SEV);
			return 0;
		}

		func->setArg(argIdx, arg);
		argRegisters.push_back(std::move(arg));
	}

#if _WIN32
	uint32_t retSize = (uint32_t)(sizeof(uint64_t));
#else
	bool isPod = asmjit::TypeUtils::isVec128(sig.ret());
	bool isIntPod = asmjit::TypeUtils::isBetween(sig.ret(), asmjit::TypeId::kInt8x16, asmjit::TypeId::kUInt64x2);
	bool isFloatPod = asmjit::TypeUtils::isBetween(sig.ret(), asmjit::TypeId::kFloat32x4, asmjit::TypeId::kFloat64x2);
	uint32_t retSize = (uint32_t)(sizeof(uint64_t) * (isPod ? 2 : 1));
#endif

	std::vector<asmjit::x86::Reg> retRegisters;
	retRegisters.reserve(2);
	if (sig.hasRet()) {
		if (asmjit::TypeUtils::isInt(sig.ret())) {
			retRegisters.push_back(cc.newUIntPtr());
		}
#if !_WIN32
		/*else if (isIntPod) {
			retRegisters.push_back(cc.newUIntPtr());
			retRegisters.push_back(cc.newUIntPtr());
		} else if (isFloatPod) {
			retRegisters.push_back(cc.newXmm());
			retRegisters.push_back(cc.newXmm());
		}*/
#endif
		else {
			retRegisters.push_back(cc.newXmm());
		}
	}

	// setup the stack structure to hold arguments for user callback
	uint32_t stackSize = (uint32_t)(sizeof(uint64_t) * sig.argCount());
	asmjit::x86::Mem argsStack = cc.newStack(stackSize, 16);
	asmjit::x86::Mem argsStackIdx(argsStack);

	// assigns some register as index reg
	asmjit::x86::Gp i = cc.newUIntPtr();

	// stackIdx <- stack[i].
	argsStackIdx.setIndex(i);

	// r/w are sizeof(uint64_t) width now
	argsStackIdx.setSize(sizeof(uint64_t));

	// set i = 0
	cc.mov(i, 0);
	//// mov from arguments registers into the stack structure
	for (uint8_t argIdx = 0; argIdx < sig.argCount(); argIdx++) {
		const auto argType = sig.args()[argIdx];

		// have to cast back to explicit register types to gen right mov type
		if (asmjit::TypeUtils::isInt(argType)) {
			cc.mov(argsStackIdx, argRegisters.at(argIdx).as<asmjit::x86::Gp>());
		} else if(asmjit::TypeUtils::isFloat(argType)) {
			cc.movq(argsStackIdx, argRegisters.at(argIdx).as<asmjit::x86::Xmm>());
		} else {
			//Log::log("Parameters wider than 64bits not supported", ErrorLevel::SEV);
			return 0;
		}

		// next structure slot (+= sizeof(uint64_t))
		cc.add(i, sizeof(uint64_t));
	}

	// get pointer to callback and pass it to the user callback
	asmjit::x86::Gp argCallback = cc.newUIntPtr("argCallback");
	cc.mov(argCallback, this);

	// fill reg to pass struct arg type to callback
	asmjit::x86::Gp argType = cc.newUInt8();
	cc.mov(argType, CallbackType::Pre);

	// get pointer to stack structure and pass it to the user callback
	asmjit::x86::Gp argStruct = cc.newUIntPtr("argStruct");
	cc.lea(argStruct, argsStack);

	// fill reg to pass struct arg count to callback
	asmjit::x86::Gp argCountParam = cc.newUInt8();
	cc.mov(argCountParam, (uint8_t)sig.argCount());

	// create buffer for ret val
	asmjit::x86::Mem retStack = cc.newStack(retSize, 16);
	asmjit::x86::Gp retStruct = cc.newUIntPtr("retStruct");
	cc.lea(retStruct, retStack);

	// create value for function return
	asmjit::x86::Gp retValue = cc.newUInt8();

	asmjit::InvokeNode* invokePreNode;

	// Call pre callback
	cc.invoke(&invokePreNode,
			  (uint64_t)callback,
			  asmjit::FuncSignature::build<ReturnFlag, Callback*, CallbackType, Parameters*, uint8_t, ReturnValue*>()
	);

	// call to user provided function (use ABI of host compiler)
	invokePreNode->setArg(0, argCallback);
	invokePreNode->setArg(1, argType);
	invokePreNode->setArg(2, argStruct);
	invokePreNode->setArg(3, argCountParam);
	invokePreNode->setArg(4, retStruct);
	invokePreNode->setRet(0, retValue);

	cc.test(retValue, ReturnFlag::Supercede);
	cc.jnz(supercede);

	// mov from arguments stack structure into regs
	cc.mov(i, 0); // reset idx
	for (uint8_t argIdx = 0; argIdx < sig.argCount(); argIdx++) {
		const auto argType = sig.args()[argIdx];

		if (asmjit::TypeUtils::isInt(argType)) {
			cc.mov(argRegisters.at(argIdx).as<asmjit::x86::Gp>(), argsStackIdx);
		} else if (asmjit::TypeUtils::isFloat(argType)) {
			cc.movq(argRegisters.at(argIdx).as<asmjit::x86::Xmm>(), argsStackIdx);
		} else {
			//Log::log("Parameters wider than 64bits not supported", ErrorLevel::SEV);
			return 0;
		}

		// next structure slot (+= sizeof(uint64_t))
		cc.add(i, sizeof(uint64_t));
	}

	// deref the trampoline ptr (holder must live longer, must be concrete reg since push later)
	asmjit::x86::Gp origPtr = cc.zbx();
	cc.mov(origPtr, getTrampolineHolder());
	cc.mov(origPtr, asmjit::x86::ptr(origPtr));

	asmjit::InvokeNode* origInvokeNode;
	cc.invoke(&origInvokeNode, origPtr, sig);
	for (uint8_t argIdx = 0; argIdx < sig.argCount(); argIdx++) {
		origInvokeNode->setArg(argIdx, argRegisters.at(argIdx));
	}
	for (size_t retIdx = 0; retIdx < retRegisters.size(); retIdx++) {
		origInvokeNode->setRet(retIdx, retRegisters[retIdx]);
	}

	if (sig.hasRet()) {
		asmjit::x86::Mem retStackIdx(retStack);
		retStackIdx.setSize(sizeof(uint64_t));
		if (asmjit::TypeUtils::isInt(sig.ret())) {
			cc.mov(retStackIdx, retRegisters.at(0).as<asmjit::x86::Gp>());
		}
#if !_WIN32
		/*else if (isIntPod) {
			asmjit::x86::Mem retStackIdxUpper(retStack);
			retStackIdxUpper.addOffset(sizeof(uint64_t));
			retStackIdxUpper.setSize(sizeof(uint64_t));

			cc.mov(retStackIdx, retRegisters.at(0).as<asmjit::x86::Gp>());
			cc.mov(retStackIdxUpper, retRegisters.at(1).as<asmjit::x86::Gp>());
		} else if (isFloatPod) {
			asmjit::x86::Mem retStackIdxUpper(retStack);
			retStackIdxUpper.addOffset(sizeof(uint64_t));
			retStackIdxUpper.setSize(sizeof(uint64_t));

			cc.movq(retStackIdx, retRegisters.at(0).as<asmjit::x86::Xmm>());
			cc.movq(retStackIdxUpper, retRegisters.at(1).as<asmjit::x86::Xmm>());
		}*/
#endif
		else {
			cc.movq(retStackIdx, retRegisters.at(0).as<asmjit::x86::Xmm>());
		}
	}

	// this code will be executed if a callback returns Supercede
	cc.bind(supercede);

	cc.test(retValue, ReturnFlag::NoPost);
	cc.jnz(noPost);

	cc.mov(argType, CallbackType::Post);

	asmjit::InvokeNode* invokePostNode;

	cc.invoke(&invokePostNode,
			  (uint64_t)callback,
			  asmjit::FuncSignature::build<ReturnFlag, Callback*, CallbackType, Parameters*, uint8_t, ReturnValue*>()
	);

	// call to user provided function (use ABI of host compiler)
	invokePostNode->setArg(0, argCallback);
	invokePostNode->setArg(1, argType);
	invokePostNode->setArg(2, argStruct);
	invokePostNode->setArg(3, argCountParam);
	invokePostNode->setArg(4, retStruct);
	//invokePostNode->setRet(0, retValue);

	// mov from arguments stack structure into regs
	cc.mov(i, 0); // reset idx
	for (uint8_t argIdx = 0; argIdx < sig.argCount(); argIdx++) {
		const auto argType = sig.args()[argIdx];

		if (asmjit::TypeUtils::isInt(argType)) {
			cc.mov(argRegisters.at(argIdx).as<asmjit::x86::Gp>(), argsStackIdx);
		} else if (asmjit::TypeUtils::isFloat(argType)) {
			cc.movq(argRegisters.at(argIdx).as<asmjit::x86::Xmm>(), argsStackIdx);
		} else {
			//Log::log("Parameters wider than 64bits not supported", ErrorLevel::SEV);
			return 0;
		}

		// next structure slot (+= sizeof(uint64_t))
		cc.add(i, sizeof(uint64_t));
	}

	cc.bind(noPost);

	if (sig.hasRet()) {
		//cc.test(retValue, ReturnFlag::Override);
		//cc.jnz(override);
/*
#if !_WIN32
		if (isPod) {
			cc.ret(retRegisters.at(0), retRegisters.at(1));
		} else
#endif
		{
			cc.ret(retRegisters.at(0));
		}*/

		//cc.bind(override);

		asmjit::x86::Mem retStackIdx(retStack);
		retStackIdx.setSize(sizeof(uint64_t));
		if (asmjit::TypeUtils::isInt(sig.ret())) {
			asmjit::x86::Gp tmp = cc.newUIntPtr();
			cc.mov(tmp, retStackIdx);
			cc.ret(tmp);
		}
#if !_WIN32
		/*else if (isIntPod) {
			asmjit::x86::Mem retStackIdxUpper(retStack);
			retStackIdxUpper.addOffset(sizeof(uint64_t));
			retStackIdxUpper.setSize(sizeof(uint64_t));

			cc.mov(asmjit::x86::rax, retStackIdx);
			cc.mov(asmjit::x86::rdx, retStackIdxUpper);
			cc.ret();
		} else if (isFloatPod) {
			asmjit::x86::Mem retStackIdxUpper(retStack);
			retStackIdxUpper.addOffset(sizeof(uint64_t));
			retStackIdxUpper.setSize(sizeof(uint64_t));

			cc.movq(asmjit::x86::xmm0, retStackIdx);
			cc.movq(asmjit::x86::xmm1, retStackIdxUpper);
			cc.ret();
		}*/
#endif
		else {
			asmjit::x86::Xmm tmp = cc.newXmm();
			cc.movq(tmp, retStackIdx);
			cc.ret(tmp);
		}
	}

	cc.func()->frame().addDirtyRegs(origPtr);
	
	cc.endFunc();

	// write to buffer
	cc.finalize();

	// worst case, overestimates for case trampolines needed
	code.flatten();
	size_t size = code.codeSize();

	// Allocate a virtual memory (executable).
	m_callbackBuf = std::make_unique<char[]>(size);
	if (!m_callbackBuf) {
		PolyHook2DebugBreak();
		return 0;
	}

	m_callbackPtr = (uint64_t) m_callbackBuf.get();

	MemoryProtector protector(m_callbackPtr, size, R | W | X, *this, false);

	// if multiple sections, resolve linkage (1 atm)
	if (code.hasUnresolvedLinks()) {
		code.resolveUnresolvedLinks();
	}

	// Relocate to the base-address of the allocated memory.
	code.relocateToBase(m_callbackPtr);
	code.copyFlattenedData(m_callbackBuf.get(), size);

	//Log::log("JIT Stub:\n" + std::string(log.data()), ErrorLevel::INFO);
	return m_callbackPtr;
}

uint64_t PLH::Callback::getJitFunc(DataType retType, const std::vector<DataType>& paramTypes, const asmjit::Arch arch, const CallbackEntry callback) {
	asmjit::FuncSignature sig(asmjit::CallConvId::kHost, asmjit::FuncSignature::kNoVarArgs, getTypeId(retType));
	for (const DataType& type : paramTypes) {
		sig.addArg(getTypeId(type));
	}
	return getJitFunc(sig, arch, callback);
}

template<typename E>
constexpr auto to_integral(E e) -> std::underlying_type_t<E> {
	return static_cast<std::underlying_type_t<E>>(e);
}

bool PLH::Callback::addCallback(const CallbackType type, const CallbackHandler callback) {
	if (!callback)
		return false;

	std::vector<CallbackHandler>& callbacks = m_callbacks[to_integral(type)];

	for (const CallbackHandler c : callbacks) {
		if (c == callback) {
			return false;
		}
	}

	callbacks.push_back(callback);
	return true;
}

bool PLH::Callback::removeCallback(const CallbackType type, const CallbackHandler callback) {
	if (!callback)
		return false;

	std::vector<CallbackHandler>& callbacks = m_callbacks[to_integral(type)];

	for (size_t i = 0; i < callbacks.size(); i++) {
		if (callbacks[i] == callback) {
			callbacks.erase(callbacks.begin() + static_cast<ptrdiff_t>(i));
			return true;
		}
	}

	return false;
}

bool PLH::Callback::isCallbackRegistered(const CallbackType type, const CallbackHandler callback) const {
	if (!callback)
		return false;

	const std::vector<CallbackHandler>& callbacks = m_callbacks[to_integral(type)];

	for (const CallbackHandler c : callbacks) {
		if (c == callback)
			return true;
	}

	return false;
}

bool PLH::Callback::areCallbacksRegistered(const CallbackType type) const {
	return !m_callbacks[to_integral(type)].empty();
}

bool PLH::Callback::areCallbacksRegistered() const {
	return areCallbacksRegistered(CallbackType::Pre) || areCallbacksRegistered(CallbackType::Post);
}

std::vector<PLH::Callback::CallbackHandler>& PLH::Callback::getCallbacks(const CallbackType type) {
	return m_callbacks[to_integral(type)];
}

uint64_t* PLH::Callback::getTrampolineHolder() {
	return &m_trampolinePtr;
}

uint64_t* PLH::Callback::getCallbackHolder() {
	return &m_callbackPtr;
}