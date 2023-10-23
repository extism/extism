# Extism D SDK

D bindings to the Extism host SDK.

## Development

1. Ensure a [D compiler](https://dlang.org/download) is installed and available in your `PATH`.
    > Note: The compiler's frontend must be at least version `2.102`.
2. Ensure the extism shared library is installed *before* running tests or examples:

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

> Note: Until [dlang/dub#2700](https://github.com/dlang/dub/issues/2700) is fixed, ensure all non-D sources (e.g. intermediate `*.d` files in `targets`) are cleaned from the repo.
>
> Run `cargo clean` to avoid seeing many invalid lint issues.

### Running Tests

```sh
dub test
```

## Examples

### Hello World

```sh
dub run extism:hello
```
