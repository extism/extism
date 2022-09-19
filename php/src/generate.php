<?php
require_once "vendor/autoload.php";


function generate() {
    $libSearchPath = array(__DIR__, "/usr/local/lib", "/usr/lib", getenv("HOME")."/.local/lib");
    $headerSearchPath = array(__DIR__, "/usr/local/include", "/usr/include", getenv("HOME")."/.local/include");

    return (new FFIMe\FFIMe("libextism.".soext(), $headerSearchPath, $libSearchPath))
        ->include("extism.h")
        ->showWarnings(false)
        ->codeGen('ExtismLib', __DIR__.'/ExtismLib.php');
}

function soext() {
    $platform = php_uname("s");
    switch ($platform) {
        case "Darwin":
            return "dylib";
        case "Linux": 
            return "so";
        case "Windows":
            return "dll";
        default:
            throw new Exeception("Extism: unsupported platform ".$platform);
    }
}

if (!file_exists(__DIR__."/ExtismLib.php")) {
    generate();
}
    
