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

class Context
{
    public $pointer;
    public $lib;
    
    public function __construct()
    {
        global $lib;
        
        if ($lib == null) {
            $lib = new \ExtismLib(\ExtismLib::SOFILE);
        }
        
        $this->pointer = $lib->extism_context_new();
        $this->lib = $lib;
    }
    
    public function __destruct()
    {
        global $lib;
        
        $lib->extism_context_free($this->pointer);
    }
    
    
    public function reset() 
    {
        global $lib;
    
        $lib->extism_context_reset($this->pointer);
    }
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

