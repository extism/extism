project(extism)
cmake_minimum_required(VERSION 3.22)
include(FetchContent)

FetchContent_Declare(
    Corrosion
    GIT_REPOSITORY https://github.com/corrosion-rs/corrosion.git
    GIT_TAG v0.4.4
)
FetchContent_MakeAvailable(Corrosion)

corrosion_import_crate(MANIFEST_PATH ./Cargo.toml PROFILE release CRATES libextism FEATURES default)
target_include_directories(extism INTERFACE ../runtime)
target_include_directories(extism-static INTERFACE ../runtime)
target_include_directories(extism-shared INTERFACE ../runtime)
