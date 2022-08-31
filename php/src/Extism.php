<?php
declare(strict_types=1);

$code = "
typedef int32_t ExtismPlugin;

typedef uint64_t ExtismSize;

ExtismPlugin extism_plugin_register(const uint8_t *wasm, ExtismSize wasm_size, bool with_wasi);

bool extism_plugin_config(ExtismPlugin plugin, const uint8_t *json, ExtismSize json_size);

bool extism_function_exists(ExtismPlugin plugin, const char *func_name);

int32_t extism_call(ExtismPlugin plugin_id,
                    const char *func_name,
                    const uint8_t *data,
                    ExtismSize data_len);

const char *extism_error(ExtismPlugin plugin);

ExtismSize extism_output_length(ExtismPlugin plugin);

void extism_output_get(ExtismPlugin plugin, uint8_t *buf, ExtismSize len);

bool extism_log_file(const char *filename, const char *log_level);
";

$search_path = array(__DIR__, "/usr/local/lib", "/usr/lib", getenv("HOME")."/.local");

function locate($paths) {
    for ($i = 0; $i < count($paths); $i++) {
        try {
            global $code;
            return FFI::cdef($code, $paths[$i] . "/libextism.".soext());
        } catch (FFI\Exception $e) {
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
            throw("Extism: unsupported platform ".$patform);
    }
}

$lib = locate($search_path);
if ($lib == null) {
    throw("Extism: unable to locate library");
}

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

        $length = strlen($data);
        $ty = FFI::arrayType(FFI::type("uint8_t"), [$length]);
        $str = FFI::new($ty);
        FFI::memcpy($str, $data, $length);

        global $lib;
        $id = $lib->extism_plugin_register($str, $length, $wasi);
        if ($id < 0) {
            throw new Exception("Extism: unable to load plugin");
        }
        $this->id = $id;

        if ($config != null) {
            $cfg = json_encode(config);
            $lib.extism_plugin_config($this->id, $cfg, strlen($cfg));
        }
    }
}

$p = new Plugin("../wasm/code.wasm");