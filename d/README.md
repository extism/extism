# Extism D SDK

D bindings to the Extism host SDK.

## Development

Ensure a [D compiler](https://dlang.org/download) is installed and available in your `PATH`.

Ensure the extism shared library is installed *before* running tests or examples:

See [Installing Extism](https://extism.org/docs/install) documentation.

```sh
extism install latest
extism link
```

> Note: The `extism install` command may require `sudo`.

### Lint Sources

```sh
dub lint
```

### Running Tests

```sh
dub test
```

## Examples

### Hello World

```sh
dub run extism:hello
```
