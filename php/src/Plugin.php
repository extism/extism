<?php
declare(strict_types=1);
namespace Extism;

require_once "ExtismLib.php";

class CancelHandle
{
    private $lib;
    private $handle;

    function __construct($lib, $handle)
    {
        $this->lib = $lib;
        $this->handle = $handle;
    }

    public function cancel()
    {
        return $this->lib->extism_plugin_cancel($this->handle);
    }
}

class Plugin
{
    private $lib;

    private $wasi;
    private $config;

    private $plugin;

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

        // TODO: handle error message
        $plugin = $this->lib->extism_plugin_new($data, count($data), null, 0, (int)$wasi, null);
        if ($plugin == null) {
            throw new \Exception("Extism: unable to load plugin");
        }
        $this->plugin = $plugin;

        if ($this->config != null) {
            $cfg = string_to_bytes(json_encode($config));
            $this->lib->extism_plugin_config($this->plugin, $cfg, count($cfg));
        }
    }
    
    public function __destruct() {
        $this->lib->extism_plugin_free($this->plugin);
        $this->plugin = null;
    }    
    
    public function functionExists($name)
    {
        return $this->lib->extism_plugin_function_exists($this->plugin, $name);
    }

    public function cancelHandle()
    {
        return new \CancelHandle($this->lib, $this->lib->extism_plugin_cancel_handle($this->plugin));
    }

    public function call($name, $input = null)
    {        
        if (gettype($input) == "string") {
            $input = string_to_bytes($input);
        }

        $rc = $this->lib->extism_plugin_call($this->plugin, $name, $input, count($input));
        if ($rc != 0) {
            $msg = "code = " . $rc;
            $err = $this->lib->extism_error($this->plugin);
            if ($err) {
                $msg = $msg . ", error = " . $err->toString();
            }
            throw new \Exception("Extism: call to '".$name."' failed with " . $msg);
        }

        $length = $this->lib->extism_plugin_output_length($this->plugin);

        $buf = $this->lib->extism_plugin_output_data($this->plugin);

        $output = [];
        $data = $buf->getData();
        for ($i = 0; $i < $length; $i++) {
            $output[$i] = $data[$i];
        }

        return $output;
    }
}

function string_to_bytes($string) {
    $bytes = [];
    for ($i = 0; $i < strlen($string); $i++) {
        $bytes[$i] = ord($string[$i]);
    }

    return $bytes;
}
