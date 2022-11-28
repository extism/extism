<?php
declare(strict_types=1);
namespace Extism;

require_once "ExtismLib.php";

$lib = new \ExtismLib(\ExtismLib::SOFILE);
if ($lib == null) {
    throw new Exception("Extism: failed to create new runtime instance");
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

