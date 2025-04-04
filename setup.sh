#!/bin/bash

# Use this script to set up your dependencies for the project!
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
export PATH=$(llvm-config-14 --prefix)/bin:$PATH
export LIBRARY_PATH=$(llvm-config-14 --prefix)/lib:$LIBRARY_PATH
export LD_LIBRARY_PATH=$(llvm-config-14 --prefix)/lib:$LD_LIBRARY_PATH
export C_INCLUDE_PATH=$(llvm-config-14 --prefix)/include:$C_INCLUDE_PATH
export CPLUS_INCLUDE_PATH=$(llvm-config-14 --prefix)/include:$CPLUS_INCLUDE_PATH

if ! is_installed "llvm"; then
    echo "Installing LLVM Link..."
    sudo apt install -y llvm
fi

if ! is_installed "libgc-dev"; then
    echo "Installing LLVM Link..."
    sudo apt install -y llvm
fi

echo "Setup complete!"