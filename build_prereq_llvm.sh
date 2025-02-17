#!/bin/bash

# Set output names
OUTPUT_LL="outputs/prereq_llvm.ll"

# Find all .c and .h files in the current directory
C_FILES=$(find object_impl -maxdepth 1 -name "*.c")
H_FILES=$(find object_impl -maxdepth 1 -name "*.h")

echo "Found C files: $C_FILES"
echo "Found Header files: $H_FILES"

# Create empty array for LLVM files
LL_FILES=()

# Compile each .c file to .ll file
for FILE in $C_FILES; do
    BASENAME=$(basename "$FILE" .c)
    clang -S -emit-llvm "$FILE" -o "${BASENAME}.ll"
    LL_FILES+=("${BASENAME}.ll")
done

# Link all generated LLVM IR files into one
llvm-link "${LL_FILES[@]}" -S -o "$OUTPUT_LL"

echo "LLVM IR generated: $OUTPUT_LL"
