include(FetchContent)

message(STATUS "Pulling and configuring plugify")

FetchContent_Declare(
        plugify
        GIT_REPOSITORY https://github.com/untrustedmodders/plugify.git
        GIT_TAG e939220a6e8f3937d035fce52cd1d3459e53798d
)

set(PLUGIFY_BUILD_ASSEMBLY ON CACHE INTERNAL "Build assembly object library.")

FetchContent_MakeAvailable(plugify)