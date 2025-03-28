#!/bin/bash

SRC_DIRS=("collections" "object")
TEST_DIRS=("collections/tests" "object/tests")

CC=clang
CFLAGS="-lgc" 

echo "🔨 Compiling source files..."
OBJ_FILES=()
for dir in "${SRC_DIRS[@]}"; do
    for src_file in "$dir"/*.c; do
        [ -e "$src_file" ] || continue  
        obj_file="${src_file%.c}.o"
        $CC $CFLAGS -c "$src_file" -o "$obj_file"
        if [ $? -ne 0 ]; then
            echo "❌ Compilation failed: $src_file"
            exit 1
        fi
        OBJ_FILES+=("$obj_file")
    done
done

echo "🚀 Running tests..."
for dir in "${TEST_DIRS[@]}"; do
    for test_file in "$dir"/*.c; do
        [ -e "$test_file" ] || continue  
        test_executable="${test_file%.c}.out"

        $CC $CFLAGS "$test_file" "${OBJ_FILES[@]}" -o "$test_executable"
        if [ $? -ne 0 ]; then
            echo "❌ Test compilation failed: $test_file"
            exit 1
        fi

        ./"$test_executable"
        if [ $? -eq 0 ]; then
            echo "✅ Test passed: $test_file"
        else
            echo "❌ Test failed: $test_file"
            exit 1
        fi

        rm "$test_executable"
    done
done

rm "${OBJ_FILES[@]}"
echo "🎉 All tests completed!"