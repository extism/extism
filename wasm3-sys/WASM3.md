# Vendored Wasm3

This directory contains the Wasm3 C interpreter, vendored from the upstream
source release rather than crates.io (the `wasm3` crate there is 0.3.1 from
2021 and does not track current Wasm3).

| | |
| --- | --- |
| Upstream | https://github.com/wasm3/wasm3 |
| Version | v0.9.0 |
| Commit | `0cd38327f0c721e75172f4f1eeb55854dc0517af` |
| License | MIT (`LICENSE.wasm3`) |

Only `source/` is kept (the interpreter library). The CLI, platform ports, and
docs are not vendored.

To refresh:

```sh
ver=v0.9.0
curl -fsSL "https://github.com/wasm3/wasm3/archive/refs/tags/${ver}.tar.gz" | tar -xz
rm -rf wasm3
cp -a "wasm3-${ver#v}/source" wasm3
cp "wasm3-${ver#v}/LICENSE" LICENSE.wasm3
printf '%s\n' "$ver" > VERSION
# record the annotated-tag commit from GitHub
```
