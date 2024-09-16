#pragma once

#include <plugify/cpp_plugin.h>
#include <plugin_export.h>
#include "callback.hpp"

#include <polyhook2/Detour/NatDetour.hpp>
#include <polyhook2/Tests/TestEffectTracker.hpp>
#include <polyhook2/Virtuals/VTableSwapHook.hpp>
#include <polyhook2/PolyHookOsIncludes.hpp>

#include <asmjit/asmjit.h>
#include <unordered_map>

namespace PLH {
	class PolyHookPlugin final : public plg::IPluginEntry, public MemAccessor {
	public:
		PolyHookPlugin() = default;
		~PolyHookPlugin() override = default;

	private:
		void OnPluginStart() override {}
		void OnPluginEnd() override {}
		
	public:
		Callback* hookDetour(void* pFunc, DataType returnType, const std::vector<DataType>& arguments);
		Callback* hookVirtual(void* pClass, int index, DataType returnType, const std::vector<DataType>& arguments);
		Callback* hookVirtual(void* pClass, void* pFunc, DataType returnType, const std::vector<DataType>& arguments);
		bool unhookDetour(void* pFunc);
		bool unhookVirtual(void* pClass, int index);
		bool unhookVirtual(void* pClass, void* pFunc);
		Callback* findDetour(void* pFunc) const;
		Callback* findVirtual(void* pClass, int index) const;
		Callback* findVirtual(void* pClass, void* pFunc) const;

		void unhookAll();
		void unhookAllVirtual(void* pClass);

		int getVTableIndex(void* pFunc) const;

	private:
		std::map<void*, std::unique_ptr<VTableSwapHook>> m_vtables;
		std::map<void*, std::unique_ptr<NatDetour>> m_detours;
		std::map<std::pair<void*, int>, std::unique_ptr<Callback>> m_callbacks;
		std::map<void*, std::pair<VFuncMap, VFuncMap>> m_tables;
		std::mutex m_mutex;
	};
}