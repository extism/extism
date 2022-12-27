### _Welcome!_

**Please note:** this project still under active development. It's usable, but expect some rough edges while work is underway. If you're interested in working on or building with Extism, please join our [Discord](https://discord.gg/cx3usBCWnc) and let us know - we are happy to help get you started.

[![Discord](https://img.shields.io/discord/1011124058408112148?color=%23404eed&label=Community%20Chat&logo=Discord&logoColor=%23404eed)](https://discord.gg/cx3usBCWnc)

# [Extism](https://extism.org)

The universal plug-in system. Run WebAssembly extensions inside your app. Use idiomatic Host SDKs for [<img src="https://cdn.rawgit.com/simple-icons/simple-icons/develop/icons/go.svg" width="18" height="18" />  Go](https://extism.org/docs/integrate-into-your-codebase/go-host-sdk), 
[<img src="https://cdn.rawgit.com/simple-icons/simple-icons/develop/icons/ruby.svg" width="18" height="18" />  Ruby](https://extism.org/docs/integrate-into-your-codebase/ruby-host-sdk), [<img src="https://cdn.rawgit.com/simple-icons/simple-icons/develop/icons/python.svg" width="18" height="18" />  Python](https://extism.org/docs/integrate-into-your-codebase/python-host-sdk), 
[<img src="https://cdn.rawgit.com/simple-icons/simple-icons/develop/icons/nodedotjs.svg" width="18" height="18" />  Node](https://extism.org/docs/integrate-into-your-codebase/node-host-sdk), [<img src="https://cdn.rawgit.com/simple-icons/simple-icons/develop/icons/rust.svg" width="18" height="18" /> Rust](https://extism.org/docs/integrate-into-your-codebase/rust-host-sdk), 
[<img src="https://cdn.rawgit.com/simple-icons/simple-icons/develop/icons/c.svg" width="18" height="18" /> C](https://extism.org/docs/integrate-into-your-codebase/c-host-sdk), [<img src="https://cdn.rawgit.com/simple-icons/simple-icons/develop/icons/cplusplus.svg" width="18" height="18" /> C++](https://extism.org/docs/integrate-into-your-codebase/cpp-host-sdk), 
[<img src="https://cdn.rawgit.com/simple-icons/simple-icons/develop/icons/ocaml.svg" width="18" height="18" /> OCaml](https://extism.org/docs/integrate-into-your-codebase/ocaml-host-sdk), 
[<img src="https://cdn.rawgit.com/simple-icons/simple-icons/develop/icons/haskell.svg" width="18" height="18" /> Haskell](https://extism.org/docs/integrate-into-your-codebase/haskell-host-sdk), 
[<img src="https://cdn.rawgit.com/simple-icons/simple-icons/develop/icons/php.svg" width="18" height="18" /> PHP](https://extism.org/docs/integrate-into-your-codebase/php-host-sdk), 
[<img src="https://cdn.rawgit.com/simple-icons/simple-icons/develop/icons/elixir.svg" width="18" height="18" /> Elixir / <img src="https://cdn.rawgit.com/simple-icons/simple-icons/develop/icons/erlang.svg" width="18" height="18" /> Erlang](https://extism.org/docs/integrate-into-your-codebase/elixir-or-erlang-host-sdk), 
[<img src="https://cdn.rawgit.com/simple-icons/simple-icons/develop/icons/dotnet.svg" width="18" height="18" /> .NET](https://extism.org/docs/integrate-into-your-codebase/dotnet-host-sdk),
[<img src="https://cdn.rawgit.com/simple-icons/simple-icons/develop/icons/java.svg" width="18" height="18" /> Java](https://extism.org/docs/integrate-into-your-codebase/java-host-sdk) &amp; more (others coming soon). 

Plug-in development kits (PDK) for plug-in authors supported in [<img src="https://cdn.rawgit.com/simple-icons/simple-icons/develop/icons/rust.svg" width="18" height="18" /> Rust](https://github.com/extism/rust-pdk), [<img src="https://cdn.rawgit.com/simple-icons/simple-icons/develop/icons/assemblyscript.svg" width="18" height="18" /> AssemblyScript](https://github.com/extism/assemblyscript-pdk), [<img src="https://cdn.rawgit.com/simple-icons/simple-icons/develop/icons/go.svg" width="18" height="18" /> Go](https://github.com/extism/go-pdk), [<img src="https://cdn.rawgit.com/simple-icons/simple-icons/develop/icons/c.svg" width="18" height="18" /> C / <img src="https://cdn.rawgit.com/simple-icons/simple-icons/develop/icons/cplusplus.svg" width="18" height="18" /> C++](https://github.com/extism/c-pdk), and [<img src="https://cdn.rawgit.com/simple-icons/simple-icons/develop/icons/haskell.svg" width="18" height="18" /> Haskell](https://github.com/extism/haskell-pdk).

<p align="center">
  <img style="width: 70%;" src="https://user-images.githubusercontent.com/7517515/208771403-940431e4-68df-449d-ae11-5cf6620174c1.png" alt="Extism embedded SDK language support"/>
</p>


Add a flexible, secure, and _bLaZiNg FaSt_ plug-in system to your project. Server, desktop, mobile, web, database -- you name it. Enable users to write and execute safe extensions to your software in **3 easy steps:**

### 1. Import

Import an Extism Host SDK into your code as a library dependency.

### 2. Integrate 

Identify the place(s) in your code where some arbitrary logic should run (the plug-in!), returning your code some results.

### 3. Execute

Load WebAssembly modules at any time in your app's lifetime and Extism will execute them in a secure sandbox, fully isolated from your program's memory.

--- 

## Usage

Head to the [project website](https://extism.org) for more information and docs. Also, consider reading an [overview](https://extism.org/docs/overview) of Extism and its goals & approach.

## Contribution

Thank you for considering a contribution to Extism, we are happy to help you make a PR or find something to work on! 

The easiest way to start would be to join the [Discord](https://discord.gg/cx3usBCWnc) or open an issue on the [`extism/proposals`](https://github.com/extism/proposals) issue tracker, which can eventually become an Extism Improvement Proposal (EIP).

---

## Who's behind this?

Extism is an open-source product from the team at:

<p align="left">
  <a href="https://dylib.so" _target="blanks"><img width="200px" src="https://user-images.githubusercontent.com/7517515/198204119-5afdebb9-a5d8-4322-bd2a-46179c8d7b24.svg"/></a>
</p>



_Reach out and tell us what you're building! We'd love to help._
