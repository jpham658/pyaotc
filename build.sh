#!/bin/bash

# Use this script to build the project.

set -e 

is_installed() {
    dpkg-query -W -f='${Status}' "$1" 2>/dev/null | grep -q "install ok installed"
}

echo "Building Pyaotc..."
echo "Downloading Polly... (required to build llvm-sys)"
sudo apt install -y libpolly-14-dev
cargo build
cargo build --release

echo "Verifying LLVM 14"
llvm-config --version

# Then download clang-14 for compiler functionality 
if ! is_installed "clang-14"; then
    echo "Installing Clang 14..."
    sudo apt install -y clang
    # Set up alternatives to point to Clang 14 
    sudo update-alternatives --install /usr/bin/clang clang /usr/bin/clang-14 100
else
    echo "Clang 14 already installed."
fi

echo "Build complete!"