#!/usr/bin/env bash
set -e

case_num=0

# project root directory
root="$(cd "$(dirname "$0")/.." && pwd)"

# Path to the compiled COBOL executable
exe="$root/incollege"

# Test folders
cases="$root/test/test1/cases"        # Input test cases
expected="$root/test/test1/expected"  # Expected outputs
actual="$root/test/test1/actual"      # Outputs we generate

# Files used by the COBOL program
input="$root/test/InCollege-Input.txt"
output="$root/out/InCollege-Output.txt"
accounts="$root/data/accounts.dat"

# Compile the COBOL program once before running tests
cobc -x -free -o "$exe" "$root/src/incollege.cob"

fail=0

# Loop through every test case file
for f in "$cases"/Test*.txt; do
  name="$(basename "$f" .txt)"
  echo "$name"

  # Clear output and accounts file before each test
  : > "$output"
  : > "$accounts"

  # Copy current test case into the program input file
  cp "$f" "$input"

  "$exe" || true

  # Save the program output for comparison
  cp "$output" "$actual/$name.out"

  # Compare actual output with expected output
  if [ -f "$expected/$name.out" ] &&
     diff -u "$expected/$name.out" "$actual/$name.out" >/dev/null; then
    case_num=$((case_num + 1))
    echo "================================ [CASE $case_num: PASSED] ================================"
  else
    echo "================================ [CASE $case_num: FAILED] ================================"
    case_num=$((case_num + 1))
    fail=1
  fi

  echo
done
exit $fail
