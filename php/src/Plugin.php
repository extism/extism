<?php
declare(strict_types=1);
namespace Extism;

require_once "vendor/autoload.php";
require_once "generate.php";
require_once "ExtismLib.php";

$lib = new \ExtismLib(\ExtismLib::SOFILE);
if ($lib == null) {
    throw new Exception("Extism: failed to create new runtime instance");
}

function reset() 
{
    global $lib;
    
    $lib->extism_reset();
}

function set_log_file($filename, $level)
{
    global $lib;
    
    $lib->extism_log_file($filename, $level);
}

class Plugin
{
    private $lib;

    private $wasi;
    private $config;

    private $id;

    public function __construct($data, $wasi = false, $config = null) 
    {
        global $lib;

        if ($lib == null) {
            $lib = new \ExtismLib(\ExtismLib::SOFILE);
        }
        $this->lib = $lib;

        $this->wasi = $wasi;
        $this->config = $config;

        if (gettype($data) == "object" and $data->wasm != null) {
            $data = json_encode($data);
        }

        if (gettype($data) == "string") {
            $data = string_to_bytes($data);
        }

        $id = $this->lib->extism_plugin_register($data, count($data), (int)$wasi);
        if ($id < 0) {
            $err = $this->lib->extism_error(-1);
            throw new Exception("Extism: unable to load plugin: " . $err);
        }
        $this->id = $id;

        if ($config != null) {
            $cfg = string_to_bytes(json_encode(config));
            $this->lib->extism_plugin_config($this->id, $cfg, count($cfg));
        }
    }
    
    public function __destruct() {
        $this->lib->extism_plugin_destroy($this->id);    
    }

    public function getId() {
        return $this->id;
    }
    
    
    public function functionExists($name)
    {
        return $this->lib->extism_function_exists($this->id, $name);
    }

    public function call($name, $input = null)
    {        
        if (gettype($input) == "string") {
            $input = string_to_bytes($input);
        }

        $rc = $this->lib->extism_call($this->id, $name, $input, count($input));
        if ($rc != 0) {
            $msg = "code = " . $rc;
            $err = $this->lib->extism_error($this->id);
            if ($err) {
                $msg = $msg . ", error = " . $err;
            }
            throw new Execption("Extism: call to '".$name."' failed with " . $msg);
        }

        $length = $this->lib->extism_output_length($this->id);

        $buf = $this->lib->extism_output_get($this->id);

        $ouput = [];
        $data = $buf->getData();
        for ($i = 0; $i < $length; $i++) {
            $output[$i] = $data[$i];
        }

        return $output;
    }

    public function update($data, $wasi = false, $config = null) {
        if (gettype($data) == "object" and $data->wasm != null) {
            $data = json_encode($data);
        }

        if (gettype($data) == "string") {
            $data = string_to_bytes($data);
        }

        $ok = $this->lib->extism_plugin_update($this->id, $data, count($data), (int)$wasi);
        if (!$ok) {
            $err = $this->lib->extism_error(-1);
            throw new Exception("Extism: unable to update plugin: " . $err);
        }

        if ($config != null) {
            $config = json_encode($config);
            $this->lib->extism_plugin_config($this->id, $config, strlen($config));
        }
    }
}

function string_to_bytes($string) {
    $bytes = [];
    for ($i = 0; $i < strlen($string); $i++) {
        $bytes[$i] = ord($string[$i]);
    }

    return $bytes;
}