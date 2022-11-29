<?php
require_once "vendor/autoload.php";


function generate() {
    return (new FFIMe\FFIMe("libextism.".soext()))
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
            throw new Exception("Extism: unsupported platform ".$platform);
    }
}

if (!file_exists(__DIR__."/ExtismLib.php")) {
    generate();
}
    
