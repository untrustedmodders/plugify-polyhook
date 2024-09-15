#pragma once

#pragma warning(push, 0)
#include <asmjit/asmjit.h>
#pragma warning( pop )

#pragma warning( disable : 4200)
#include "polyhook2/PolyHookOs.hpp"
#include "polyhook2/ErrorLog.hpp"
#include "polyhook2/Enums.hpp"
#include "polyhook2/MemAccessor.hpp"

namespace PLH {
	enum class DataType : uint8_t {
		Void,
		Bool,
		Int8,
		UInt8,
		Int16,
		UInt16,
		Int32,
		UInt32,
		Int64,
		UInt64,
		Float,
		Double,
		Pointer,
		String, // char* (null-terminated)
		WString // wchar* (null-terminated)
	};

	enum class ReturnAction : uint8_t {
		Ignored,  // handler didn't take any action
		Handled,  // we did something, but real function should still be called
		Override, // call real function, but use my return value
		Supercede // skip real function; use my return value
	};

	class Callback : public MemAccessor {
	public:
		struct Parameters {
			template<typename T>
			void setArg(const uint8_t idx, const T val) const {
				*(T*)getArgPtr(idx) = val;
			}

			template<typename T>
			T getArg(const uint8_t idx) const {
				return *(T*)getArgPtr(idx);
			}

			// asm depends on this specific type
			// we the ILCallback allocates stack space that is set to point here
			volatile uint64_t m_arguments;

		private:
			// must be char* for aliasing rules to work when reading back out
			char* getArgPtr(const uint8_t idx) const {
				return (char*)&m_arguments + sizeof(uint64_t) * idx;
			}
		};

		struct ReturnValue {
			template<typename T>
			void setRet(const T val) const {
				*(T*)getRetPtr() = val;
			}

			template<typename T>
			T getRet() const {
				return *(T*)getRetPtr();
			}
			uint8_t* getRetPtr() const {
				return (unsigned char*)&m_retVal;
			}
			volatile uint64_t m_retVal;
		};

		typedef ReturnAction (*tMainCallback)(Callback* callback, const Parameters* params, uint8_t count, const ReturnValue* ret);
		typedef ReturnAction (*tUserCallback)(const Parameters* params, int count, const ReturnValue* ret);

		Callback();
		~Callback();

		uint64_t getJitFunc(const asmjit::FuncSignature& sig, asmjit::Arch arch, tMainCallback callback);

		uint64_t getJitFunc(DataType retType, const std::vector<DataType>& paramTypes, asmjit::Arch arch, tMainCallback callback);

		uint64_t* getTrampolineHolder();
		std::vector<tUserCallback>& getCallbacks();

		bool addCallback(tUserCallback callback);
		bool removeCallback(tUserCallback callback);
		bool isCallbackRegistered(tUserCallback callback);
		bool areCallbacksRegistered();

	private:
		asmjit::TypeId getTypeId(DataType type);

		std::unique_ptr<char[]> m_callbackBuf;
		std::vector<tUserCallback> m_callbacks;
		uint64_t m_trampolinePtr;
	};
}