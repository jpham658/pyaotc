#!/bin/bash

# Use this file to set up your dependencies for the project!
# This assumes you already have Cargo and Rust installed.

set -e 

is_installed() {
    dpkg-query -W -f='${Status}' "$1" 2>/dev/null | grep -q "install ok installed"
}

echo "Updating package list and installing software-properties-common repo..."
sudo apt update && sudo apt upgrade && sudo apt install lsb-release wget software-properties-common gnupg

if ! is_installed "llvm-14"; then
    wget https://apt.llvm.org/llvm.sh
    chmod +x llvm.sh
    sudo ./llvm.sh 14 all
fi

# Set up alternatives to point to LLVM-14 
echo "Setting up llvm-config alternatives"
sudo update-alternatives --install /usr/bin/llvm-config llvm-config /usr/bin/llvm-config-14 100

echo "Setting LLVM_SYS_140_PREFIX (llvm-sys requirement)"
export LLVM_SYS_140_PREFIX=$(llvm-config --prefix)

if ! is_installed "llvm"; then
    echo "Installing LLVM Link..."
    sudo apt install -y llvm
fi

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
fi

echo "Setup complete!"