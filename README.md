# Extism

The universal plug-in system. Run WebAssembly extensions inside your app. Use idiomatic Host SDKs for [Go](https://extism.org/docs/integrate-into-your-codebase/go-host-sdk), 
[Ruby](https://extism.org/docs/integrate-into-your-codebase/ruby-host-sdk), [Python](https://extism.org/docs/integrate-into-your-codebase/python-host-sdk), 
[Node](https://extism.org/docs/integrate-into-your-codebase/node-host-sdk), [Rust](https://extism.org/docs/integrate-into-your-codebase/rust-host-sdk), 
[C](https://extism.org/docs/integrate-into-your-codebase/c-host-sdk), [C++](https://extism.org/docs/integrate-into-your-codebase/cpp-host-sdk), 
[OCaml](https://extism.org/docs/integrate-into-your-codebase/ocaml-host-sdk) &amp; more (others coming soon). 

Plug-in development kits (PDK) for plug-in authors supported in Rust, AssemblyScript, Go, C/C++.

<p align="center">
  <img src="https://user-images.githubusercontent.com/7517515/184472910-36d42d73-bd1e-49e2-9b4d-9b020959603d.png"/>
</p>

Add a flexible, secure, and _bLaZiNg FaSt_ plug-in system to your project. Server, desktop, mobile, web, database -- you name it. Enable users to write and execute safe extensions to your software in **3 easy steps:**

### 1. Import

Import Extism host SDK into your code as a library dependency.

### 2. Integrate 

Identify the place(s) in your code where some arbitrary logic should run (the plug-in!), returning your code some results.


### 3. Execute

Load WebAssembly modules at any time in your app's lifetime and Extism will execute them in a secure sandbox, fully isolated from your program's memory.

--- 

## Usage

Head to the [project website](https://extism.org) for more information and docs. Also, consider reading an [overview](overview) of Extism and its goals & approach.