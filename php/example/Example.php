<?php

require("Plugin.php");

$wasm = file_get_contents("../wasm/code.wasm");
$plugin = new Plugin($wasm);

$output = $plugin->call("count_vowels", "this is an example");
$json = json_decode(pack('C*', ...$output));
echo "Vowels counted = " . $json->{'count'} . PHP_EOL;