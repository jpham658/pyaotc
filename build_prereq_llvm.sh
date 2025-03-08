#!/bin/bash

OUTPUT_DIR="outputs/$1/prereq_llvm"
OUTPUT_LL="outputs/$1/prereq_llvm.ll"

mkdir -p "$OUTPUT_DIR"

mapfile -t C_FILES < <(find object collections -maxdepth 1 -name "*.c" -print)

echo "Found C files: ${C_FILES[*]}"

LL_FILES=()

# Compile each .c file to .ll and move it to the output directory
for FILE in "${C_FILES[@]}"; do
    BASENAME=$(basename "$FILE" .c)
    OUTPUT_LL_FILE="${OUTPUT_DIR}/${BASENAME}.ll"
    
    clang -S -emit-llvm "$FILE" -o "$OUTPUT_LL_FILE"
    
    if [[ $? -eq 0 ]]; then
        LL_FILES+=("$OUTPUT_LL_FILE")
    else
        echo "Error compiling $FILE"
        exit 1
    fi
done

touch "$OUTPUT_LL"

# Link all generated LLVM IR files into one
if [[ ${#LL_FILES[@]} -gt 0 ]]; then
    echo "Linking LLVM IR files: ${LL_FILES[*]}"
    llvm-link "${LL_FILES[@]}" -S -o "$OUTPUT_LL" || { echo "llvm-link failed"; exit 1; }
    echo "LLVM IR generated: $OUTPUT_LL"
else
    echo "No LLVM IR files to link."
    exit 1
fi
