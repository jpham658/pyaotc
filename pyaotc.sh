#!/bin/bash

# Entry point for the compiler.
# If the project hasn't been built yet, builds the project
# with the Polly dependency. Then installs Clang 14 for the compiler's execution.

set -e 

COMPILER="./target/release/pyaotc"

is_installed() {
    dpkg-query -W -f='${Status}' "$1" 2>/dev/null | grep -q "install ok installed"
}

if [ -z "$1" ]; then
    echo "Usage: $0 <source-file>"
    exit 1
fi

if [ ! -f "$COMPILER" ]; then
    echo "Binary for compiler not found. Building project..."

    # Must download Polly to first build llvm-sys
    echo "Downloading Polly... (required to build llvm-sys)"
    sudo apt install -y libpolly-14-dev
    cargo build
    cargo build --release
fi

# Then download clang-14 for compiler functionality 
if ! is_installed "clang-14"; then
    echo "Installing Clang 14..."
    sudo apt install -y clang
    # Set up alternatives to point to Clang 14 
    sudo update-alternatives --install /usr/bin/clang clang /usr/bin/clang-14 100
fi

exec $COMPILER $1
