#!/bin/bash

# Function to check if a package is installed
is_installed() {
    dpkg -l | grep -q "$1"
}

echo "Updating package list..."
sudo apt update

if ! is_installed "llvm-14"; then
    echo "LLVM 14 not found, installing..."
    sudo apt install -y llvm-14 llvm-14-dev llvm-14-tools
else
    echo "LLVM 14 is already installed."
fi

if ! is_installed "clang-14"; then
    echo "Clang 14 not found, installing..."
    sudo apt install -y clang-14
else
    echo "Clang 14 is already installed."
fi

if ! is_installed "libpolly-14-dev"; then
    echo "libpolly-14-dev not found, installing..."
    sudo apt install -y libpolly-14-dev
else
    echo "libpolly-14-dev is already installed."
fi

# Set up alternatives to point to LLVM-14 and Clang-14 if needed
echo "Setting up llvm-config and clang alternatives..."
sudo update-alternatives --install /usr/bin/llvm-config llvm-config /usr/bin/llvm-config-14 100
sudo update-alternatives --install /usr/bin/clang clang /usr/bin/clang-14 100

echo "Verifying LLVM 14, Clang 14, and libpolly-14-dev installation..."
llvm-config --version
clang --version
dpkg -l | grep libpolly-14-dev

echo "Setup complete!"
