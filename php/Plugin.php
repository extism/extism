<?php
declare(strict_types=1);

require_once "vendor/autoload.php";
require_once "generate.php";
require_once "Extism.php";

$lib = new Extism(Extism::SOFILE); // should `locate` shared lib again to pass here

class Plugin
{
    private $wasi;
    private $config;

    private $id;

    public function __construct($data, $wasi = false, $config = null) 
    {
        $this->wasi = $wasi;
        $this->config = $config;

        if (gettype($data) == "object" and $data->wasm != null) {
            $data = json_encode($data);
        }

        if (gettype($data) == "string") {
            $data = string_to_bytes($data);
        }

        global $lib;
        $id = $lib->extism_plugin_register($data, count($data), (int)$wasi);
        if ($id < 0) {
            throw new Exception("Extism: unable to load plugin");
        }
        $this->id = $id;

        if ($config != null) {
            $cfg = string_to_bytes(json_encode(config));
            $lib->extism_plugin_config($this->id, $cfg, count($cfg));
        }
    }

    public function id() {
        return $this->id;
    }

    public function call($name, $input = null)
    {        
        global $lib;

        if (gettype($input) == "string") {
            $input = string_to_bytes($input);
        }

        $rc = $lib->extism_call($this->id, $name, $input, count($input));
        if ($rc != 0) {
            $msg = "code = " . $rc;
            $err = $lib->extism_error($this->id);
            if ($err) {
                $msg = $msg . ", error = " . $err;
            }
            throw new Execption("Extism: call to '".$name."' failed with " . $msg);
        }

        $length = $lib->extism_output_length($this->id);
        $ty = FFI::arrayType(FFI::type("uint8_t"), [$length]);
        $buf = new uint8_t_ptr(FFI::new($ty));

        $result = $lib->extism_output_get($this->id, $buf, $length);

        $ouput = [];
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

// $wasm = file_get_contents("/Users/stevemanuel/Projects/extism/extism/wasm/code.wasm");

// $plugin = new Plugin($wasm);
// $output = $plugin->call("count_vowels", "this is a test");
// $json = json_decode(pack('C*', ...$output));
// echo "Vowels counted = " . $json->{'count'} . PHP_EOL;