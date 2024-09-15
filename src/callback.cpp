#include "callback.h"

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
			return asmjit::TypeId::kUIntPtr;
	}
	return asmjit::TypeId::kVoid;
}

uint64_t PLH::Callback::getJitFunc(const asmjit::FuncSignature& sig, const asmjit::Arch arch, const PLH::Callback::tMainCallback callback) {;
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

	asmjit::StringLogger log;
	auto kFormatFlags =
			asmjit::FormatFlags::kMachineCode | asmjit::FormatFlags::kExplainImms | asmjit::FormatFlags::kRegCasts
			| asmjit::FormatFlags::kHexImms     | asmjit::FormatFlags::kHexOffsets  | asmjit::FormatFlags::kPositions;
	
	log.addFlags(kFormatFlags);
	code.setLogger(&log);
	
	// too small to really need it
	func->frame().resetPreservedFP();
	
	// map argument slots to registers, following abi.
	std::vector<asmjit::x86::Reg> argRegisters;
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
		argRegisters.push_back(arg);
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

	// get pointer to stack structure and pass it to the user callback
	asmjit::x86::Gp argStruct = cc.newUIntPtr("argStruct");
	cc.lea(argStruct, argsStack);

	// fill reg to pass struct arg count to callback
	asmjit::x86::Gp argCountParam = cc.newUInt8();
	cc.mov(argCountParam, (uint8_t)sig.argCount());

	// create buffer for ret val
	asmjit::x86::Mem retStack = cc.newStack(sizeof(uint64_t), 16);
	asmjit::x86::Gp retStruct = cc.newUIntPtr("retStruct");
	cc.lea(retStruct, retStack);

	// create value for function return
	asmjit::x86::Gp retValue = cc.newUInt8();

	// Create label for skip original function
	asmjit::Label override = cc.newNamedLabel("override");

	asmjit::InvokeNode* invokeNode;
	cc.invoke(&invokeNode,
			  (uint64_t)callback,
			  asmjit::FuncSignature::build<ReturnAction, Callback*, Parameters*, uint8_t, ReturnValue*>()
	);

	// call to user provided function (use ABI of host compiler)
	invokeNode->setArg(0, argCallback);
	invokeNode->setArg(1, argStruct);
	invokeNode->setArg(2, argCountParam);
	invokeNode->setArg(3, retStruct);
	invokeNode->setRet(0, retValue);

	cc.cmp(retValue, ReturnAction::Supercede);
	cc.je(override);

	// mov from arguments stack structure into regs
	cc.mov(i, 0); // reset idx
	for (uint8_t arg_idx = 0; arg_idx < sig.argCount(); arg_idx++) {
		const auto argType = sig.args()[arg_idx];

		if (asmjit::TypeUtils::isInt(argType)) {
			cc.mov(argRegisters.at(arg_idx).as<asmjit::x86::Gp>(), argsStackIdx);
		} else if (asmjit::TypeUtils::isFloat(argType)) {
			cc.movq(argRegisters.at(arg_idx).as<asmjit::x86::Xmm>(), argsStackIdx);
		} else {
			//Log::log("Parameters wider than 64bits not supported", ErrorLevel::SEV);
			return 0;
		}

		// next structure slot (+= sizeof(uint64_t))
		cc.add(i, sizeof(uint64_t));
	}

	// deref the trampoline ptr (holder must live longer, must be concrete reg since push later)
	asmjit::x86::Gp origPtr = cc.zbx();
	cc.mov(origPtr, (uintptr_t)getTrampolineHolder());
	cc.mov(origPtr, asmjit::x86::ptr(origPtr));

	asmjit::InvokeNode* origInvokeNode;
	cc.invoke(&origInvokeNode, origPtr, sig);
	for (uint8_t argIdx = 0; argIdx < sig.argCount(); argIdx++) {
		origInvokeNode->setArg(argIdx, argRegisters.at(argIdx));
	}

	// this code will be executed if a callback returns Supercede
	cc.bind(override);
	
	if (sig.hasRet()) {
		asmjit::x86::Mem retStackIdx(retStack);
		retStackIdx.setSize(sizeof(uint64_t));
		if (asmjit::TypeUtils::isInt(sig.ret())) {
			asmjit::x86::Gp tmp2 = cc.newUIntPtr();
			cc.mov(tmp2, retStackIdx);
			cc.ret(tmp2);
		} else {
			asmjit::x86::Xmm tmp2 = cc.newXmm();
			cc.movq(tmp2, retStackIdx);
			cc.ret(tmp2);
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

	uint64_t callbackBuf = (uint64_t) m_callbackBuf.get();

	MemoryProtector protector(callbackBuf, size, R | W | X, *this, false);

	// if multiple sections, resolve linkage (1 atm)
	if (code.hasUnresolvedLinks()) {
		code.resolveUnresolvedLinks();
	}

	// Relocate to the base-address of the allocated memory.
	code.relocateToBase(callbackBuf);
	code.copyFlattenedData(m_callbackBuf.get(), size);

	//Log::log("JIT Stub:\n" + std::string(log.data()), ErrorLevel::INFO);
	return callbackBuf;
}

uint64_t PLH::Callback::getJitFunc(DataType retType, const std::vector<DataType>& paramTypes, const asmjit::Arch arch, const tMainCallback callback) {
	asmjit::FuncSignature sig(asmjit::CallConvId::kHost, asmjit::FuncSignature::kNoVarArgs, getTypeId(retType));
	for (const DataType& type : paramTypes) {
		sig.addArg(getTypeId(type));
	}
	return getJitFunc(sig, arch, callback);
}

bool PLH::Callback::addCallback(tUserCallback callback) {
	auto it = std::find(m_callbacks.begin(), m_callbacks.end(), callback);
	if (it == m_callbacks.end()) {
		m_callbacks.push_back(callback);
		return true;
	}
	return false;
}

bool PLH::Callback::removeCallback(tUserCallback callback) {
	auto it = std::find(m_callbacks.begin(), m_callbacks.end(), callback);
	if (it != m_callbacks.end()) {
		m_callbacks.erase(it);
		return true;
	}
	return false;
}

bool PLH::Callback::isCallbackRegistered(tUserCallback callback) {
	return std::find(m_callbacks.begin(), m_callbacks.end(), callback) != m_callbacks.end();
}

bool PLH::Callback::areCallbacksRegistered() {
	return !m_callbacks.empty();
}

std::vector<PLH::Callback::tUserCallback>& PLH::Callback::getCallbacks() {
	return m_callbacks;
}

uint64_t* PLH::Callback::getTrampolineHolder() {
	return &m_trampolinePtr;
}

PLH::Callback::Callback() : m_trampolinePtr(0) {
}

PLH::Callback::~Callback() {
}
