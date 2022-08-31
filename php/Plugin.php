<?php
declare(strict_types=1);

require_once "vendor/autoload.php";
require_once "Extism.php";

// $code = "
// typedef int32_t ExtismPlugin;

// typedef uint64_t ExtismSize;

// ExtismPlugin extism_plugin_register(const uint8_t *wasm, ExtismSize wasm_size, bool with_wasi);

// bool extism_plugin_config(ExtismPlugin plugin, const uint8_t *json, ExtismSize json_size);

// bool extism_function_exists(ExtismPlugin plugin, const char *func_name);

// int32_t extism_call(ExtismPlugin plugin_id,
//                     const char *func_name,
//                     const uint8_t *data,
//                     ExtismSize data_len);

// const char *extism_error(ExtismPlugin plugin);

// ExtismSize extism_output_length(ExtismPlugin plugin);

// void extism_output_get(ExtismPlugin plugin, uint8_t *buf, ExtismSize len);

// bool extism_log_file(const char *filename, const char *log_level);
// ";



// class Plugin
// {
//     private $wasi;
//     private $config;

//     private $id;

//     public function __construct($data, $wasi = false, $config = null) 
//     {
//         $this->wasi = $wasi;
//         $this->config = $config;

//         if (gettype($data) == "object" and $data->wasm != null) {
//             $data = json_encode($data);
//         }

//         $length = strlen($data);
//         // $ty = FFI::arrayType(FFI::type("uint8_t"), [$length]);
//         // $str = FFI::new($ty);
//         // FFI::memcpy($str, $data, $length);

//         global $lib;
//         $id = $lib->extism_plugin_register($data, $length, $wasi);
//         if ($id < 0) {
//             throw new Exception("Extism: unable to load plugin");
//         }
//         $this->id = $id;

//         if ($config != null) {
//             $cfg = json_encode(config);
//             $lib.extism_plugin_config($this->id, $cfg, strlen($cfg));
//         }
//     }

//     public function call($name, $input = null)
//     {
//         $c_name = FFI::new("char *");
//         FFI::memcpy($c_name, $name, strlen($name));

//         $length = strlen($input);
//         $ty = FFI::arrayType(FFI::type("uint8_t"), [$length]);
//         $c_input = FFI::new($ty);
//         FFI::memcpy($c_input, $input, $length);
        
//         global $lib;
//         $rc = $lib->extism_call($this->id, $c_name, $c_input, $length);
//         if ($rc != 0) {
//             throw new Execption("Extism: call to '".$name."' failed with code: ". $rc);
//         }
//     }
// }

$ext = new Extism(Extism::SOFILE); // should `locate` shared lib again to pass here
$wasm = unpack('C*', file_get_contents("/Users/stevemanuel/Projects/extism/extism/wasm/code.wasm"));
$id = $ext->extism_plugin_register($wasm, count($wasm), 0);
echo "plugin id => ".$id."\n";
$input = unpack('C*', "this is a test");
$ext->extism_call($id, "count_vowels", $input, count($input));