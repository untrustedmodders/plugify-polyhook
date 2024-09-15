#include "plugin.h"

PLH::PolyHookPlugin g_polyHookPlugin;
EXPOSE_PLUGIN(PLUGIN_API, &g_polyHookPlugin)

static PLH::ReturnAction GlobalCallback(PLH::Callback* callback, const PLH::Callback::Parameters* params, const uint8_t count, const PLH::Callback::ReturnValue* ret) {
	PLH::ReturnAction returnAction = PLH::ReturnAction::Ignored;

	// TODO: Probably not thread safe

	const auto& callbacks = callback->getCallbacks();
	for (const auto& cb : callbacks) {
		PLH::ReturnAction result = cb(params, count, ret);
		if (result > returnAction)
			returnAction = result;
	}

	return returnAction;
}

PLH::Callback* PLH::PolyHookPlugin::hookDetour(void* pFunc, DataType returnType, const std::vector<DataType>& arguments) {
	if (!pFunc)
		return nullptr;

	std::lock_guard m_lock(m_mutex);

	auto it = m_detours.find(pFunc);
	if (it != m_detours.end()) {
		auto it2 = m_callbacks.find(std::pair{it->second.get(), -1});
		if (it2 != m_callbacks.end()) {
			return it2->second.get();
		}
	}

	auto callback = std::make_unique<Callback>();
	uint64_t JIT = callback->getJitFunc(returnType, arguments, asmjit::Arch::kHost, &GlobalCallback);

	auto detour = std::make_unique<NatDetour>((uintptr_t)pFunc, JIT, callback->getTrampolineHolder());
	if (!detour->hook())
		return nullptr;

	void* key = m_detours.emplace(pFunc, std::move(detour)).first->second.get();
	return m_callbacks.emplace(std::pair{key, -1}, std::move(callback)).first->second.get();
}

PLH::Callback* PLH::PolyHookPlugin::hookVirtual(void* pClass, int index, DataType returnType, const std::vector<DataType>& arguments) {
	if (!pClass || index == -1)
		return nullptr;

	std::lock_guard m_lock(m_mutex);

	auto it = m_vtables.find(pClass);
	if (it != m_vtables.end()) {
		auto it2 = m_callbacks.find(std::pair{it->second.get(), index});
		if (it2 != m_callbacks.end()) {
			return it2->second.get();
		} else {
			m_vtables.erase(it);
		}
	}

	auto callback = std::make_unique<Callback>();
	uint64_t JIT = callback->getJitFunc(returnType, arguments, asmjit::Arch::kHost, &GlobalCallback);

	auto& [redirectMap, origVFuncs] = m_tables[pClass];
	redirectMap[index] = JIT;

	auto vtable = std::make_unique<VTableSwapHook>((uint64_t)pClass, redirectMap, &origVFuncs);
	if (!vtable->hook())
		return nullptr;

	*callback->getTrampolineHolder() = origVFuncs[index];

	void* key = m_vtables.emplace(pClass, std::move(vtable)).first->second.get();
	return m_callbacks.emplace(std::pair{key, index}, std::move(callback)).first->second.get();
}

PLH::Callback* PLH::PolyHookPlugin::hookVirtual(void* pClass, void* pFunc, DataType returnType, const std::vector<DataType>& arguments) {
	return hookVirtual(pClass, getVTableIndex(pFunc), returnType, arguments);
}

bool PLH::PolyHookPlugin::unhookDetour(void* pFunc) {
	if (!pFunc)
		return false;

	std::lock_guard m_lock(m_mutex);

	auto it = m_detours.find(pFunc);
	if (it != m_detours.end()) {
		auto& detour = it->second;
		if (detour->unHook()) {
			m_callbacks.erase(std::pair{detour.get(), -1});
			m_detours.erase(it);
			return true;
		}
	}

	return false;
}

bool PLH::PolyHookPlugin::unhookVirtual(void* pClass, int index) {
	if (!pClass || index == -1)
		return false;

	std::lock_guard m_lock(m_mutex);

	auto it = m_vtables.find(pClass);
	if (it != m_vtables.end()) {
		auto& vtable = it->second;
		if (vtable->unHook()) {
			m_callbacks.erase(std::pair{vtable.get(), index});
			m_vtables.erase(it);
		}

		auto it2 = m_tables.find(pClass);
		if (it2 != m_tables.end()) {
			auto& [redirectMap, origVFuncs] = it2->second;
			redirectMap.erase(index);
			if (redirectMap.empty()) {
				m_tables.erase(it2);
				return true;
			}

			vtable = std::make_unique<VTableSwapHook>((uint64_t)pClass, redirectMap, &origVFuncs);
			if (!vtable->hook())
				return false;

			return m_vtables.emplace(pClass, std::move(vtable)).first->second.get();
		}
	}

	return false;
}

bool PLH::PolyHookPlugin::unhookVirtual(void* pClass, void* pFunc) {
	return unhookVirtual(pClass, getVTableIndex(pFunc));
}

PLH::Callback* PLH::PolyHookPlugin::findDetour(void* pFunc) const {
	auto it = m_detours.find(pFunc);
	if (it != m_detours.end()) {
		auto it2 = m_callbacks.find(std::pair{it->second.get(), -1});
		if (it2 != m_callbacks.end()) {
			return it2->second.get();
		}
	}
	return nullptr;
}

PLH::Callback* PLH::PolyHookPlugin::findVirtual(void* pClass, int index) const {
	auto it = m_vtables.find(pClass);
	if (it != m_vtables.end()) {
		auto it2 = m_callbacks.find(std::pair{it->second.get(), index});
		if (it2 != m_callbacks.end()) {
			return it2->second.get();
		}
	}
	return nullptr;
}

PLH::Callback* PLH::PolyHookPlugin::findVirtual(void* pClass, void* pFunc) const {
	return findVirtual(pClass, getVTableIndex(pFunc));
}

void PLH::PolyHookPlugin::unhookAll() {
	std::lock_guard m_lock(m_mutex);

	m_detours.clear();
	m_vtables.clear();
	m_tables.clear();
	m_callbacks.clear();
}

void PLH::PolyHookPlugin::unhookAllVirtual(void* pClass) {
	std::lock_guard m_lock(m_mutex);

	auto it = m_vtables.find(pClass);
	if (it != m_vtables.end()) {
		auto it2 = m_tables.find(pClass);
		if (it2 != m_tables.end()) {
			for (const auto& [index, func] : it2->second.first) {
				m_callbacks.erase(std::pair{it->second.get(), index});
			}
			m_tables.erase(it2);
		}
		m_vtables.erase(it);
	}
}

int PLH::PolyHookPlugin::getVTableIndex(void* pFunc) const {
	constexpr size_t size = 12;

	MemoryProtector protector((uintptr_t)pFunc, size, R, *(MemAccessor*)this);

#if defined(__GNUC__) || defined(__clang__)
	struct GCC_MemFunPtr {
		union {
			void* adrr;			// always even
			intptr_t vti_plus1; // vindex+1, always odd
		};
		intptr_t delta;
	};

	int vtindex;
	auto mfp_detail = (GCC_MemFunPtr*)&pFunc;
	if (mfp_detail->vti_plus1 & 1) {
		vtindex = (mfp_detail->vti_plus1 - 1) / sizeof(void*);
	} else {
		vtindex = -1;
	}

	return vtindex;
#elif defined(_MSC_VER)

	// https://www.unknowncheats.me/forum/c-and-c-/102577-vtable-index-pure-virtual-function.html

	// Check whether it's a virtual function call on x86

	// They look like this:a
	//		0:  8b 01                   mov    eax,DWORD PTR [ecx]
	//		2:  ff 60 04                jmp    DWORD PTR [eax+0x4]
	// ==OR==
	//		0:  8b 01                   mov    eax,DWORD PTR [ecx]
	//		2:  ff a0 18 03 00 00       jmp    DWORD PTR [eax+0x318]]

	// However, for vararg functions, they look like this:
	//		0:  8b 44 24 04             mov    eax,DWORD PTR [esp+0x4]
	//		4:  8b 00                   mov    eax,DWORD PTR [eax]
	//		6:  ff 60 08                jmp    DWORD PTR [eax+0x8]
	// ==OR==
	//		0:  8b 44 24 04             mov    eax,DWORD PTR [esp+0x4]
	//		4:  8b 00                   mov    eax,DWORD PTR [eax]
	//		6:  ff a0 18 03 00 00       jmp    DWORD PTR [eax+0x318]
	// With varargs, the this pointer is passed as if it was the first argument

	// On x64
	//		0:  48 8b 01                mov    rax,QWORD PTR [rcx]
	//		3:  ff 60 04                jmp    QWORD PTR [rax+0x4]
	// ==OR==
	//		0:  48 8b 01                mov    rax,QWORD PTR [rcx]
	//		3:  ff a0 18 03 00 00       jmp    QWORD PTR [rax+0x318]

	auto finder = [&](uint8_t* addr) {
		std::unique_ptr<MemoryProtector> protector;

		if (*addr == 0xE9) {
			// May or may not be!
			// Check where it'd jump
			addr += 5 /*size of the instruction*/ + *(uint32_t*)(addr + 1);

			protector = std::make_unique<MemoryProtector>((uintptr_t)addr, size, R, *(MemAccessor*)this);
		}

		bool ok = false;
#ifdef POLYHOOK2_ARCH_X64
		if (addr[0] == 0x48 && addr[1] == 0x8B && addr[2] == 0x01) {
			addr += 3;
			ok = true;
		} else
#endif
		if (addr[0] == 0x8B && addr[1] == 0x01) {
			addr += 2;
			ok = true;
		} else if (addr[0] == 0x8B && addr[1] == 0x44 && addr[2] == 0x24 && addr[3] == 0x04 && addr[4] == 0x8B && addr[5] == 0x00) {
			addr += 6;
			ok = true;
		}

		if (!ok)
			return -1;

		constexpr int PtrSize = static_cast<int>(sizeof(void*));

		if (*addr++ == 0xFF) {
			if (*addr == 0x60)
				return *++addr / PtrSize;
			else if (*addr == 0xA0)
				return int(*((uint32_t*)++addr)) / PtrSize;
			else if (*addr == 0x20)
				return 0;
			else
				return -1;
		}

		return -1;
	};

	return finder((uint8_t*)pFunc);
#else
#error "Compiler not support"
#endif
}

extern "C"
PLUGIN_API PLH::Callback* HookDetour(void* pFunc, PLH::DataType returnType, const std::vector<PLH::DataType>& arguments) {
	return g_polyHookPlugin.hookDetour(pFunc, returnType, arguments);
}

extern "C"
PLUGIN_API PLH::Callback* HookVirtual(void* pClass, int index, PLH::DataType returnType, const std::vector<PLH::DataType>& arguments) {
	return g_polyHookPlugin.hookVirtual(pClass, index, returnType, arguments);
}

extern "C"
PLUGIN_API PLH::Callback* HookVirtualByFunc(void* pClass, void* pFunc, PLH::DataType returnType, const std::vector<PLH::DataType>& arguments) {
	return g_polyHookPlugin.hookVirtual(pClass, pFunc, returnType, arguments);
}

extern "C"
PLUGIN_API bool UnhookDetour(void* pFunc) {
	return g_polyHookPlugin.unhookDetour(pFunc);
}

extern "C"
PLUGIN_API bool UnhookVirtual(void* pClass, int index) {
	return g_polyHookPlugin.unhookVirtual(pClass, index);
}

extern "C"
PLUGIN_API bool UnhookVirtualByFunc(void* pClass, void* pFunc) {
	return g_polyHookPlugin.unhookVirtual(pClass, pFunc);
}

extern "C"
PLUGIN_API PLH::Callback* FindDetour(void* pFunc) {
	return g_polyHookPlugin.findDetour(pFunc);
}

extern "C"
PLUGIN_API PLH::Callback* FindVirtual(void* pClass, int index) {
	return g_polyHookPlugin.findVirtual(pClass, index);
}

extern "C"
PLUGIN_API PLH::Callback* FindVirtualByFunc(void* pClass, void* pFunc) {
	return g_polyHookPlugin.findVirtual(pClass, pFunc);
}

extern "C"
PLUGIN_API void UnhookAll() {
	return g_polyHookPlugin.unhookAll();
}

extern "C"
PLUGIN_API void HookAllVirtual(void* pClass) {
	g_polyHookPlugin.unhookAllVirtual(pClass);
}

extern "C"
PLUGIN_API bool AddCallback(PLH::Callback* callback, PLH::Callback::tUserCallback handler) {
	return callback->addCallback(handler);
}

extern "C"
PLUGIN_API bool RemoveCallback(PLH::Callback* callback, PLH::Callback::tUserCallback handler) {
	return callback->removeCallback(handler);
}

extern "C"
PLUGIN_API bool IsCallbackRegistered(PLH::Callback* callback, PLH::Callback::tUserCallback handler) {
	return callback->isCallbackRegistered(handler);
}

extern "C"
PLUGIN_API bool AreCallbacksRegistered(PLH::Callback* callback) {
	return callback->areCallbacksRegistered();
}

extern "C"
PLUGIN_API bool GetArgumentBool(const PLH::Callback::Parameters* params, size_t index) { return params->getArg<bool>(index); }
extern "C"
PLUGIN_API int8_t GetArgumentInt8(const PLH::Callback::Parameters* params, size_t index) { return params->getArg<int8_t>(index); }
extern "C"
PLUGIN_API uint8_t GetArgumentUInt8(const PLH::Callback::Parameters* params, size_t index) { return params->getArg<uint8_t>(index); }
extern "C"
PLUGIN_API int16_t GetArgumentInt16(const PLH::Callback::Parameters* params, size_t index) { return params->getArg<int16_t>(index); }
extern "C"
PLUGIN_API uint16_t GetArgumentUInt16(const PLH::Callback::Parameters* params, size_t index) { return params->getArg<uint16_t>(index); }
extern "C"
PLUGIN_API int32_t GetArgumentInt32(const PLH::Callback::Parameters* params, size_t index) { return params->getArg<int32_t>(index); }
extern "C"
PLUGIN_API uint32_t GetArgumentUInt32(const PLH::Callback::Parameters* params, size_t index) { return params->getArg<uint32_t>(index); }
extern "C"
PLUGIN_API int64_t GetArgumentInt64(const PLH::Callback::Parameters* params, size_t index) { return params->getArg<int64_t>(index); }
extern "C"
PLUGIN_API uint64_t GetArgumentUInt64(const PLH::Callback::Parameters* params, size_t index) { return params->getArg<uint64_t>(index); }
extern "C"
PLUGIN_API float GetArgumentFloat(const PLH::Callback::Parameters* params, size_t index) { return params->getArg<float>(index); }
extern "C"
PLUGIN_API double GetArgumentDouble(const PLH::Callback::Parameters* params, size_t index) { return params->getArg<double>(index); }
extern "C"
PLUGIN_API void* GetArgumentPointer(const PLH::Callback::Parameters* params, size_t index) { return params->getArg<void*>(index); }
extern "C"
PLUGIN_API const char* GetArgumentString(const PLH::Callback::Parameters* params, size_t index) { return params->getArg<const char*>(index); }
extern "C"
PLUGIN_API const wchar_t* GetArgumentWString(const PLH::Callback::Parameters* params, size_t index) { return params->getArg<const wchar_t*>(index); }

extern "C"
PLUGIN_API void SetArgumentBool(const PLH::Callback::Parameters* params, size_t index, bool value) { return params->setArg(index, value); }
extern "C"
PLUGIN_API void SetArgumentInt8(const PLH::Callback::Parameters* params, size_t index, int8_t value) { return params->setArg(index, value); }
extern "C"
PLUGIN_API void SetArgumentUInt8(const PLH::Callback::Parameters* params, size_t index, uint8_t value) { return params->setArg(index, value); }
extern "C"
PLUGIN_API void SetArgumentInt16(const PLH::Callback::Parameters* params, size_t index, int16_t value) { return params->setArg(index, value); }
extern "C"
PLUGIN_API void SetArgumentUInt16(const PLH::Callback::Parameters* params, size_t index, uint16_t value) { return params->setArg(index, value); }
extern "C"
PLUGIN_API void SetArgumentInt32(const PLH::Callback::Parameters* params, size_t index, int32_t value) { return params->setArg(index, value); }
extern "C"
PLUGIN_API void SetArgumentUInt32(const PLH::Callback::Parameters* params, size_t index, uint32_t value) { return params->setArg(index, value); }
extern "C"
PLUGIN_API void SetArgumentInt64(const PLH::Callback::Parameters* params, size_t index, int64_t value) { return params->setArg(index, value); }
extern "C"
PLUGIN_API void SetArgumentUInt64(const PLH::Callback::Parameters* params, size_t index, uint64_t value) { return params->setArg(index, value); }
extern "C"
PLUGIN_API void SetArgumentFloat(const PLH::Callback::Parameters* params, size_t index, float value) { return params->setArg(index, value); }
extern "C"
PLUGIN_API void SetArgumentDouble(const PLH::Callback::Parameters* params, size_t index, double value) { return params->setArg(index, value); }
extern "C"
PLUGIN_API void SetArgumentPointer(const PLH::Callback::Parameters* params, size_t index, void* value) { return params->setArg(index, value); }
extern "C"
PLUGIN_API void SetArgumentString(const PLH::Callback::Parameters* params, size_t index, const char* value) { return params->setArg(index, value); }
extern "C"
PLUGIN_API void SetArgumentWString(const PLH::Callback::Parameters* params, size_t index, const wchar_t* value) { return params->setArg(index, value); }

extern "C"
PLUGIN_API bool GetReturnBool(const PLH::Callback::ReturnValue* ret) { return ret->getRet<bool>(); }
extern "C"
PLUGIN_API int8_t GetReturnInt8(const PLH::Callback::ReturnValue* ret) { return ret->getRet<int8_t>(); }
extern "C"
PLUGIN_API uint8_t GetReturnUInt8(const PLH::Callback::ReturnValue* ret) { return ret->getRet<uint8_t>(); }
extern "C"
PLUGIN_API int16_t GetReturnInt16(const PLH::Callback::ReturnValue* ret) { return ret->getRet<int16_t>(); }
extern "C"
PLUGIN_API uint16_t GetReturnUInt16(const PLH::Callback::ReturnValue* ret) { return ret->getRet<uint16_t>(); }
extern "C"
PLUGIN_API int32_t GetReturnInt32(const PLH::Callback::ReturnValue* ret) { return ret->getRet<int32_t>(); }
extern "C"
PLUGIN_API uint32_t GetReturnUInt32(const PLH::Callback::ReturnValue* ret) { return ret->getRet<uint32_t>(); }
extern "C"
PLUGIN_API int64_t GetReturnInt64(const PLH::Callback::ReturnValue* ret) { return ret->getRet<int64_t>(); }
extern "C"
PLUGIN_API uint64_t GetReturnUInt64(const PLH::Callback::ReturnValue* ret) { return ret->getRet<uint64_t>(); }
extern "C"
PLUGIN_API float GetReturnFloat(const PLH::Callback::ReturnValue* ret) { return ret->getRet<float>(); }
extern "C"
PLUGIN_API double GetReturnDouble(const PLH::Callback::ReturnValue* ret) { return ret->getRet<double>(); }
extern "C"
PLUGIN_API void* GetReturnPointer(const PLH::Callback::ReturnValue* ret) { return ret->getRet<void*>(); }
extern "C"
PLUGIN_API const char* GetReturnString(const PLH::Callback::ReturnValue* ret) { return ret->getRet<const char*>(); }
extern "C"
PLUGIN_API const wchar_t* GetReturnWString(const PLH::Callback::ReturnValue* ret) { return ret->getRet<const wchar_t*>(); }

extern "C"
PLUGIN_API void SetReturnBool(const PLH::Callback::ReturnValue* ret, bool value) { return ret->setRet(value); }
extern "C"
PLUGIN_API void SetReturnInt8(const PLH::Callback::ReturnValue* ret, int8_t value) { return ret->setRet(value); }
extern "C"
PLUGIN_API void SetReturnUInt8(const PLH::Callback::ReturnValue* ret, uint8_t value) { return ret->setRet(value); }
extern "C"
PLUGIN_API void SetReturnInt16(const PLH::Callback::ReturnValue* ret, int16_t value) { return ret->setRet(value); }
extern "C"
PLUGIN_API void SetReturnUInt16(const PLH::Callback::ReturnValue* ret, uint16_t value) { return ret->setRet(value); }
extern "C"
PLUGIN_API void SetReturnInt32(const PLH::Callback::ReturnValue* ret, int32_t value) { return ret->setRet(value); }
extern "C"
PLUGIN_API void SetReturnUInt32(const PLH::Callback::ReturnValue* ret, uint32_t value) { return ret->setRet(value); }
extern "C"
PLUGIN_API void SetReturnInt64(const PLH::Callback::ReturnValue* ret, int64_t value) { return ret->setRet(value); }
extern "C"
PLUGIN_API void SetReturnUInt64(const PLH::Callback::ReturnValue* ret, uint64_t value) { return ret->setRet(value); }
extern "C"
PLUGIN_API void SetReturnFloat(const PLH::Callback::ReturnValue* ret, float value) { return ret->setRet(value); }
extern "C"
PLUGIN_API void SetReturnDouble(const PLH::Callback::ReturnValue* ret, double value) { return ret->setRet(value); }
extern "C"
PLUGIN_API void SetReturnPointer(const PLH::Callback::ReturnValue* ret, void* value) { return ret->setRet(value); }
extern "C"
PLUGIN_API void SetReturnString(const PLH::Callback::ReturnValue* ret, const char* value) { return ret->setRet(value); }
extern "C"
PLUGIN_API void SetReturnWString(const PLH::Callback::ReturnValue* ret, const wchar_t* value) { return ret->setRet(value); }