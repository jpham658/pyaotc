# Pyaotc User Guide

## Dependencies
`cargo` - 1.75.0

`rustc` - 1.75.0

LLVM 14 (`llvm`)

Clang (`clang`)

Boehm Garbage Collector (`libgc-dev`)

To automate installation of LLVM 14, a setup script `./setup.sh` is provided. This downloads LLVM 14 from [https://apt.llvm.org/](https://apt.llvm.org/) as well as the `llvm` package.

## Building Pyaotc
To automate building, the script `./build.sh` can be used. This essentially downloads Polly (a dependency needed to set up `llvm-sys`) and builds the project with `cargo build`.

Users should __ALWAYS__ run `./setup.sh` first to resolve dependency issues, then run `./build.sh` to build the project.

### Note about Polly and Clang 14
Polly and Clang have an unfortunate breaking dependency whereby installing one will remove the other. This means the user should take extra care when building the project. Choosing to build the project with `cargo build` will result in unexpected behaviour.

To work around this, if you would like to use `cargo build`, preface it first with `sudo apt install libpolly-14-dev`. __This will remove Clang 14!__ Make sure to install it back before running the compiler by using `sudo apt install clang`.

## Usage
To compile a Python file, you can use `cargo run {filename}.py` after running the `./build.sh` script or you can use `./pyaotc.sh {filename}.py`.

Using `./pyaotc.sh` will check if a release version of the project binary has been built yet. If it has, it will target that binary, otherwise, it will build it. This might be a bit safer than running `cargo run {filename}.py`, as it may require running `/build.sh` more than once due to the fragility of the Polly-Clang dependency.

Both will generate an executable file `./filename` that you can run in the terminal.