<div align="center">
    <a href="https://extism.org">
    <picture>
        <source media="(prefers-color-scheme: dark)" srcset=".github/assets/logo-horizontal-darkmode.png">
        <img alt="Extism - the WebAssembly framework" width="75%" style="max-width: 600px" src=".github/assets/logo-horizontal.png">
    </picture>
    </a>

[![Discord](https://img.shields.io/discord/1011124058408112148?color=%23404eed&label=Community%20Chat&logo=Discord&logoColor=%23404eed)](https://extism.org/discord)
![GitHub Org's stars](https://img.shields.io/github/stars/extism)
![Downloads](https://img.shields.io/crates/d/extism-manifest)
![GitHub License](https://img.shields.io/github/license/extism/extism)
![GitHub release (with filter)](https://img.shields.io/github/v/release/extism/extism)

</div>

# Overview

Extism is a lightweight framework for building with WebAssembly (Wasm). It
supports running Wasm code on servers, the edge, CLIs, IoT, browsers and
everything in between. Extism is designed to be "universal" in that it supports
a common interface, no matter where it runs.

> **Note:** One of the primary use cases for Extism is **building extensible
> software & plugins**. You want to be able to execute arbitrary, untrusted code
> from your users? Extism makes this safe and practical to do.

Additionally, Extism adds some extra utilities on top of standard Wasm runtimes.
For example, we support persistent memory/module-scope variables, secure &
host-controlled HTTP without WASI, runtime limiters & timers, simpler host
function linking, and more. Extism users build:

- plug-in systems
- FaaS platforms
- code generators
- web applications
- & much more...

# Run WebAssembly In Your App

Pick a SDK to import into your program, and refer to the documentation to get
started:

| Type        | Language                                                                                       | Source Code                                                             | Package                                                                 |
| ----------- | ---------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------- | ----------------------------------------------------------------------- |
| Rust SDK    | <img alt="Rust SDK" src="https://extism.org/img/sdk-languages/rust.svg" width="50px"/>         | https://github.com/extism/extism/tree/main/runtime                      | [Crates.io](https://crates.io/crates/extism)                            |
| JS SDK      | <img alt="JS SDK" src="https://extism.org/img/sdk-languages/js.svg" width="50px"/>             | https://github.com/extism/js-sdk <br/>(supports Web, Node, Deno & Bun!) | [NPM](https://www.npmjs.com/package/@extism/extism)                     |
| Elixir SDK  | <img alt="Elixir SDK" src="https://extism.org/img/sdk-languages/elixir.svg" width="50px"/>     | https://github.com/extism/elixir-sdk                                    | [Hex](https://hex.pm/packages/extism)                                   |
| Go SDK      | <img alt="Go SDK" src="https://extism.org/img/sdk-languages/go.svg" width="50px"/>             | https://github.com/extism/go-sdk                                        | [Go mod](https://pkg.go.dev/github.com/extism/go-sdk)                   |
| Haskell SDK | <img alt="Haskell SDK" src="https://extism.org/img/sdk-languages/haskell.svg" width="50px"/>   | https://github.com/extism/haskell-sdk                                   | [Hackage](https://hackage.haskell.org/package/extism)                   |
| Java SDK    | <img alt="Java SDK" src="https://extism.org/img/sdk-languages/java-android.svg" width="50px"/> | https://github.com/extism/java-sdk                                      | [Sonatype](https://central.sonatype.com/artifact/org.extism.sdk/extism) |
| .NET SDK    | <img alt=".NET SDK" src="https://extism.org/img/sdk-languages/dotnet.svg" width="50px"/>       | https://github.com/extism/dotnet-sdk <br/>(supports C# & F#!)           | [Nuget](https://www.nuget.org/packages/Extism.Sdk)                      |
| OCaml SDK   | <img alt="OCaml SDK" src="https://extism.org/img/sdk-languages/ocaml.svg" width="50px"/>       | https://github.com/extism/ocaml-sdk                                     | [opam](https://opam.ocaml.org/packages/extism/)                         |
| Perl SDK    | <img alt="Perl SDK" src="https://extism.org/img/sdk-languages/perl.svg" width="50px"/>         | https://github.com/extism/perl-sdk                                      | [CPAN](https://metacpan.org/pod/Extism)                                 |
| PHP SDK     | <img alt="PHP SDK" src="https://extism.org/img/sdk-languages/php.svg" width="50px"/>           | https://github.com/extism/php-sdk                                       | [Packagist](https://packagist.org/packages/extism/extism)               |
| Python SDK  | <img alt="Python SDK" src="https://extism.org/img/sdk-languages/python.svg" width="50px"/>     | https://github.com/extism/python-sdk                                    | [PyPi](https://pypi.org/project/extism/)                                |
| Ruby SDK    | <img alt="Ruby SDK" src="https://extism.org/img/sdk-languages/ruby.svg" width="50px"/>         | https://github.com/extism/ruby-sdk                                      | [RubyGems](https://rubygems.org/gems/extism)                            |
| Zig SDK     | <img alt="Zig SDK" src="https://extism.org/img/sdk-languages/zig.svg" width="50px"/>           | https://github.com/extism/zig-sdk                                       | N/A                                                                     |
| C SDK       | <img alt="C SDK" src="https://extism.org/img/sdk-languages/c.svg" width="50px"/>               | https://github.com/extism/extism/tree/main/libextism                    | N/A                                                                     |
| C++ SDK     | <img alt="C++ SDK" src="https://extism.org/img/sdk-languages/cpp.svg" width="50px"/>           | https://github.com/extism/cpp-sdk                                       | N/A                                                                     |

# Compile WebAssembly to run in Extism Hosts

Extism Hosts (running the SDK) must execute WebAssembly code that has a [PDK, or Plug-in Development Kit](https://extism.org/docs/concepts/pdk), 
library compiled in to the `.wasm` binary. PDKs make it easy for plug-in /
extension code authors to read input from the host and return data back, read
provided configuration, set/get variables, make outbound HTTP calls if allowed,
and more.

Pick a PDK to import into your Wasm program, and refer to the documentation to
get started:

| Type               | Language                                                                                                   | Source Code                                                   | Package                                                   |
| ------------------ | ---------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------- | --------------------------------------------------------- |
| Rust PDK           | <img alt="Rust PDK" src="https://extism.org/img/sdk-languages/rust.svg" width="50px"/>                     | https://github.com/extism/rust-pdk                            | [Crates.io](https://crates.io/crates/extism-pdk)          |
| JS PDK             | <img alt="JS PDK" src="https://extism.org/img/sdk-languages/js.svg" width="50px"/>                         | https://github.com/extism/js-pdk                              | N/A                                                       |
| Go PDK             | <img alt="Go PDK" src="https://extism.org/img/sdk-languages/go.svg" width="50px"/>                         | https://github.com/extism/go-pdk                              | [Go mod](https://pkg.go.dev/github.com/extism/go-pdk)     |
| Haskell PDK        | <img alt="Haskell PDK" src="https://extism.org/img/sdk-languages/haskell.svg" width="50px"/>               | https://github.com/extism/haskell-pdk                         | [Hackage](https://hackage.haskell.org/package/extism-pdk) |
| AssemblyScript PDK | <img alt="AssemblyScript PDK" src="https://extism.org/img/sdk-languages/assemblyscript.svg" width="50px"/> | https://github.com/extism/assemblyscript-pdk                  | [NPM](https://www.npmjs.com/package/@extism/as-pdk)       |
| .NET PDK           | <img alt=".NET PDK" src="https://extism.org/img/sdk-languages/dotnet.svg" width="50px"/>                   | https://github.com/extism/dotnet-pdk <br/>(supports C# & F#!) | https://www.nuget.org/packages/Extism.Pdk                 |
| C PDK              | <img alt="C PDK" src="https://extism.org/img/sdk-languages/c.svg" width="50px"/>                           | https://github.com/extism/c-pdk                               | N/A                                                       |
| Zig PDK            | <img alt="Zig PDK" src="https://extism.org/img/sdk-languages/zig.svg" width="50px"/>                       | https://github.com/extism/zig-pdk                             | N/A                                                       |

# Support

## Discord

If you experience any problems or have any questions, please join our
[Discord](https://extism.org/discord) and let us know. Our community is very
responsive and happy to help get you started.

## Usage

Head to the [project website](https://extism.org) for more information and docs.
Also, consider reading an [overview](https://extism.org/docs/overview) of Extism
and its goals & approach.

## Contribution

Thank you for considering a contribution to Extism, we are happy to help you
make a PR or find something to work on!

The easiest way to start would be to join the
[Discord](https://extism.org/discord) or open an issue on the
[`extism/proposals`](https://github.com/extism/proposals) issue tracker, which
can eventually become an Extism Improvement Proposal (EIP).

For more information, please read the
[Contributing](https://extism.org/docs/concepts/contributing) guide.

---

## Who's behind this?

Extism is an open-source product from the team at:

<p align="left">
  <a href="https://dylibso.com" _target="blanks"><img width="200px" src="https://user-images.githubusercontent.com/7517515/198204119-5afdebb9-a5d8-4322-bd2a-46179c8d7b24.svg"/></a>
</p>

_Reach out and tell us what you're building! We'd love to help:_
<a href="mailto:hello@dylibso.com">hello@dylibso.com</a>
