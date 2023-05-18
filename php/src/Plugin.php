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
    private $context;

    private $wasi;
    private $config;

    private $id;

    public function __construct($data, $wasi = false, $config = null, $ctx = null) 
    {
        if ($ctx == null) {
          $ctx = new Context();
        }

        $this->lib = $ctx->lib;

        $this->wasi = $wasi;
        $this->config = $config;

        if (gettype($data) == "object" and $data->wasm != null) {
            $data = json_encode($data);
        }

        if (gettype($data) == "string") {
            $data = string_to_bytes($data);
        }

        $id = $this->lib->extism_plugin_new($ctx->pointer, $data, count($data), null, 0, (int)$wasi);
        if ($id < 0) {
            $err = $this->lib->extism_error($ctx->pointer, -1);
            throw new \Exception("Extism: unable to load plugin: " . $err->toString());
        }
        $this->id = $id;
        $this->context = $ctx;

        if ($this->config != null) {
            $cfg = string_to_bytes(json_encode($config));
            $this->lib->extism_plugin_config($ctx->pointer, $this->id, $cfg, count($cfg));
        }
    }
    
    public function __destruct() {
        $this->lib->extism_plugin_free($this->context->pointer, $this->id);
        $this->id = -1;
    }

    public function getId() {
        return $this->id;
    }
    
    
    public function functionExists($name)
    {
        return $this->lib->extism_plugin_function_exists($this->context->pointer, $this->id, $name);
    }

    public function cancelHandle()
    {
        return new \CancelHandle($this->lib, $this->lib->extism_plugin_cancel_handle($this->context->pointer, $this->id));
    }

    public function call($name, $input = null)
    {        
        if (gettype($input) == "string") {
            $input = string_to_bytes($input);
        }

        $rc = $this->lib->extism_plugin_call($this->context->pointer, $this->id, $name, $input, count($input));
        if ($rc != 0) {
            $msg = "code = " . $rc;
            $err = $this->lib->extism_error($this->context->pointer, $this->id);
            if ($err) {
                $msg = $msg . ", error = " . $err->toString();
            }
            throw new \Exception("Extism: call to '".$name."' failed with " . $msg);
        }

        $length = $this->lib->extism_plugin_output_length($this->context->pointer, $this->id);

        $buf = $this->lib->extism_plugin_output_data($this->context->pointer, $this->id);

        $output = [];
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

        $ok = $this->lib->extism_plugin_update($this->context->pointer, $this->id, $data, count($data), null, 0, (int)$wasi);
        if (!$ok) {
            $err = $this->lib->extism_error($this->context->pointer, -1);
            throw new \Exception("Extism: unable to update plugin: " . $err->toString());
        }

        if ($config != null) {
            $config = json_encode($config);
            $this->lib->extism_plugin_config($this->context->pointer, $this->id, $config, strlen($config));
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
