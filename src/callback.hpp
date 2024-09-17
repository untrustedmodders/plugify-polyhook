#pragma once

#pragma warning(push, 0)
#include <asmjit/asmjit.h>
#pragma warning( pop )

#pragma warning( disable : 4200)
#include "polyhook2/Enums.hpp"
#include "polyhook2/ErrorLog.hpp"
#include "polyhook2/MemAccessor.hpp"
#include "polyhook2/PolyHookOs.hpp"

#include <array>
#include <vector>
#include <shared_mutex>

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
		String,
		WString
	};

	enum class ReturnAction : int32_t {
		Ignored,  ///< Handler didn't take any action
		Handled,  ///< We did something, but real function should still be called
		Override, ///< Call real function, but use my return value
		Supercede ///< Skip real function; use my return value
	};

	enum class CallbackType : bool {
		Pre,  ///< Callback will be executed before the original function
		Post  ///< Callback will be executed after the original function
	};

	enum class ReturnFlag : uint8_t {
		Default = 0, ///< Value means this gives no information about return flag.
		NoPost = 1,
		Supercede = 2,
	};

	class Callback {
	public:
		struct Parameters {
			template<typename T>
			void setArg(const uint8_t idx, const T val) const {
				*(T*) getArgPtr(idx) = val;
			}

			template<typename T>
			T getArg(const uint8_t idx) const {
				return *(T*) getArgPtr(idx);
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

		typedef ReturnFlag (*CallbackEntry)(Callback* callback, const Parameters* params, uint8_t count, const ReturnValue* ret);
		typedef ReturnAction (*CallbackHandler)(CallbackType type, const Parameters* params, int count, const ReturnValue* ret);

		using View = std::pair<std::vector<CallbackHandler>&, std::shared_lock<std::shared_mutex>>;

		Callback();
		~Callback();

		uint64_t getJitFunc(const asmjit::FuncSignature& sig, asmjit::Arch arch, CallbackEntry pre, CallbackEntry post);
		uint64_t getJitFunc(DataType retType, const std::vector<DataType>& paramTypes, CallbackEntry pre, CallbackEntry post);

		uint64_t* getTrampolineHolder();
		uint64_t* getFunctionHolder();
		View getCallbacks(CallbackType type);
		std::string_view getError() const;

		bool addCallback(CallbackType type, CallbackHandler callback);
		bool removeCallback(CallbackType type, CallbackHandler callback);
		bool isCallbackRegistered(CallbackType type, CallbackHandler callback) const;
		bool areCallbacksRegistered(CallbackType type) const;
		bool areCallbacksRegistered() const;

	private:
		static asmjit::TypeId getTypeId(DataType type);

		std::array<std::vector<CallbackHandler>, 2> m_callbacks;
		std::shared_mutex m_mutex;
		uint64_t m_functionPtr = 0;
		union {
			uint64_t m_trampolinePtr = 0;
			const char* m_errorCode;
		};
	};

	extern std::unique_ptr<asmjit::JitRuntime> g_jitRuntime;
}

inline PLH::ReturnFlag operator|(PLH::ReturnFlag lhs, PLH::ReturnFlag rhs) noexcept {
	using underlying = std::underlying_type_t<PLH::ReturnFlag>;
	return static_cast<PLH::ReturnFlag>(
			static_cast<underlying>(lhs) | static_cast<underlying>(rhs)
	);
}

inline bool operator&(PLH::ReturnFlag lhs, PLH::ReturnFlag rhs) noexcept {
	using underlying = std::underlying_type_t<PLH::ReturnFlag>;
	return static_cast<underlying>(lhs) & static_cast<underlying>(rhs);
}

inline PLH::ReturnFlag& operator|=(PLH::ReturnFlag& lhs, PLH::ReturnFlag rhs) noexcept {
	lhs = lhs | rhs;
	return lhs;
}