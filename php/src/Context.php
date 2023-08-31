<?php
declare(strict_types=1);
namespace Extism;

require_once "vendor/autoload.php";

function generate_extism_lib() {
    return (new \FFIMe\FFIMe("libextism.".soext()))
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
            throw new \Exception("Extism: unsupported platform ".$platform);
    }
}

if (!file_exists(__DIR__."/ExtismLib.php")) {
    generate_extism_lib();
}

require_once "ExtismLib.php";

$lib = new \ExtismLib(\ExtismLib::SOFILE);
if ($lib == null) {
    throw new \Exception("Extism: failed to create new runtime instance");
}

function set_log_file($filename, $level)
{
    global $lib;
    
    $lib->extism_log_file($filename, $level);
}

function extism_version()
{
    global $lib;

    return $lib->extism_version();
}

