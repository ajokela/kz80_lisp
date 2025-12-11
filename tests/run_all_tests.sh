#!/bin/bash
# Run all kz80_lisp tests

set -e
cd "$(dirname "$0")/.."

echo "========================================"
echo "Running Rust unit tests..."
echo "========================================"
cargo test

echo ""
echo "========================================"
echo "Running Interpreter LISP tests..."
echo "========================================"
cat tests/test_interpreter.lisp | ./target/release/kz80_lisp

echo ""
echo "========================================"
echo "Running Float Library tests..."
echo "========================================"
cat float.lisp tests/test_float_library.lisp | ./target/release/kz80_lisp

echo ""
echo "========================================"
echo "Running Compiler tests..."
echo "========================================"
cat compiler.lisp tests/test_compiler.lisp | ./target/release/kz80_lisp

echo ""
echo "========================================"
echo "ALL TEST SUITES COMPLETE"
echo "========================================"
