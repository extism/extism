<?php
require_once "vendor/autoload.php";

$search_path = array(__DIR__, "/usr/local/lib", "/usr/lib", getenv("HOME")."/.local");

function generate($paths) {
    for ($i = 0; $i < count($paths); $i++) {
        try {
            $ffi = (new FFIMe\FFIMe("libextism.".soext()))
                ->include("extism.h")
                ->showWarnings(false)
                ->codeGen('Extism', 'Extism.php');
        } catch (Exception $e) {
            continue;
        }
    }
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

if (!file_exists("Extism.php")) {
    generate($search_path);
}
    
