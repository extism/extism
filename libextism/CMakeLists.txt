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

configure_file(extism.pc.in extism.pc @ONLY)
configure_file(extism-static.pc.in extism-static.pc @ONLY)

# corrosion doesn't supporting installing libraries yet
# https://github.com/corrosion-rs/corrosion/issues/415
# so we'll do it ourselves
include(GNUInstallDirs)
install( FILES ${CMAKE_CURRENT_BINARY_DIR}/libextism.a
    DESTINATION ${CMAKE_INSTALL_LIBDIR}
)
install( FILES ${CMAKE_CURRENT_BINARY_DIR}/libextism.so
    DESTINATION ${CMAKE_INSTALL_LIBDIR}
    OPTIONAL
)
install( FILES ../runtime/extism.h
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/extism.pc ${CMAKE_CURRENT_BINARY_DIR}/extism-static.pc DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig)
