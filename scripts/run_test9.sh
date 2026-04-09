#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "$0")/.." && pwd)"
suite_root="$root/test/Test9"
input_file="$root/test/InCollege-Input.txt"
output_file="$root/out/InCollege-Output.txt"
exe="$root/incollege"

data_files=(
  accounts.dat
  profiles.dat
  connections.dat
  requests.dat
  jobs.dat
  applications.dat
  messages.dat
)

backup_dir="$(mktemp -d)"

restore_workspace() {
  for name in "${data_files[@]}"; do
    if [ -f "$backup_dir/$name" ]; then
      cp "$backup_dir/$name" "$root/data/$name"
    fi
  done

  if [ -f "$backup_dir/InCollege-Input.txt" ]; then
    cp "$backup_dir/InCollege-Input.txt" "$input_file"
  fi

  if [ -f "$backup_dir/InCollege-Output.txt" ]; then
    cp "$backup_dir/InCollege-Output.txt" "$output_file"
  fi

  rm -rf "$backup_dir"
}

trap restore_workspace EXIT

mkdir -p "$suite_root/Actual"

for name in "${data_files[@]}"; do
  if [ -f "$root/data/$name" ]; then
    cp "$root/data/$name" "$backup_dir/$name"
  else
    : > "$backup_dir/$name"
  fi
done

if [ -f "$input_file" ]; then
  cp "$input_file" "$backup_dir/InCollege-Input.txt"
else
  : > "$backup_dir/InCollege-Input.txt"
fi

if [ -f "$output_file" ]; then
  cp "$output_file" "$backup_dir/InCollege-Output.txt"
else
  : > "$backup_dir/InCollege-Output.txt"
fi

compile_app() {
  if command -v docker >/dev/null 2>&1; then
    if ! docker image inspect incollege-cobol >/dev/null 2>&1; then
      docker build -t incollege-cobol "$root/docker" >/dev/null
    fi

    docker run --rm -v "$root:/app" incollege-cobol \
      cobc -x -free -I /app/src -o /app/incollege /app/src/incollege.cob
    return
  fi

  if command -v cobc >/dev/null 2>&1; then
    cobc -x -free -I "$root/src" -o "$exe" "$root/src/incollege.cob"
    return
  fi

  echo "Neither docker nor cobc is available. Cannot execute Test9." >&2
  exit 1
}

run_program() {
  local case_input="$1"
  local stdout_path="$2"

  cp "$case_input" "$input_file"
  : > "$output_file"

  if command -v docker >/dev/null 2>&1; then
    docker run --rm -i -v "$root:/app" incollege-cobol \
      /app/incollege < "$input_file" > "$stdout_path"
  else
    "$exe" < "$input_file" > "$stdout_path"
  fi
}

seed_case() {
  local case_name="$1"
  local seed_dir="$suite_root/Seed/$case_name"

  for name in "${data_files[@]}"; do
    : > "$root/data/$name"
  done

  if [ -d "$seed_dir" ]; then
    for seed_file in "$seed_dir"/*; do
      [ -f "$seed_file" ] || continue
      cp "$seed_file" "$root/data/$(basename "$seed_file")"
    done
  fi
}

assert_file() {
  local haystack="$1"
  local rules="$2"
  local failures=0

  while IFS= read -r rule || [ -n "$rule" ]; do
    [ -n "$rule" ] || continue
    case "$rule" in
      \#*)
        continue
        ;;
      REQUIRE:*)
        local expected="${rule#REQUIRE: }"
        if ! grep -F -- "$expected" "$haystack" >/dev/null; then
          echo "    missing: $expected"
          failures=1
        fi
        ;;
      ABSENT:*)
        local unexpected="${rule#ABSENT: }"
        if grep -F -- "$unexpected" "$haystack" >/dev/null; then
          echo "    unexpected: $unexpected"
          failures=1
        fi
        ;;
      ORDER:*)
        local ordered="${rule#ORDER: }"
        local first="${ordered%% => *}"
        local second="${ordered#* => }"
        local first_line second_line
        first_line="$(grep -nF -- "$first" "$haystack" | head -n1 | cut -d: -f1 || true)"
        second_line="$(grep -nF -- "$second" "$haystack" | head -n1 | cut -d: -f1 || true)"

        if [ -z "$first_line" ] || [ -z "$second_line" ] || [ "$first_line" -ge "$second_line" ]; then
          echo "    order violation: $first => $second"
          failures=1
        fi
        ;;
      *)
        echo "    unsupported assertion: $rule"
        failures=1
        ;;
    esac
  done < "$rules"

  return "$failures"
}

run_single_case() {
  local case_name="$1"
  local input_name="$2"
  local assert_name="$3"
  local stdout_name="$4"
  local outfile_name="$5"

  local input_path="$suite_root/Input/$input_name"
  local assert_path="$suite_root/Assertions/$assert_name"
  local stdout_path="$suite_root/Actual/$stdout_name"
  local outfile_path="$suite_root/Actual/$outfile_name"

  run_program "$input_path" "$stdout_path"
  cp "$output_file" "$outfile_path"

  if ! diff -u "$stdout_path" "$outfile_path" > "$suite_root/Actual/${case_name}.stdout-vs-outfile.diff"; then
    echo "  stdout/output mismatch"
    return 1
  fi
  rm -f "$suite_root/Actual/${case_name}.stdout-vs-outfile.diff"

  if ! assert_file "$stdout_path" "$assert_path"; then
    echo "  assertion failure"
    return 1
  fi

  return 0
}

compile_app

failures=0

echo "TC01_No_Messages"
seed_case "TC01_No_Messages"
if run_single_case \
  "TC01_No_Messages" \
  "TC01_No_Messages.txt" \
  "TC01_No_Messages.assert.txt" \
  "TC01_No_Messages.stdout.txt" \
  "TC01_No_Messages.outfile.txt"; then
  echo "  PASS"
else
  echo "  FAIL"
  failures=1
fi
echo

echo "TC02_View_One_Message"
seed_case "TC02_View_One_Message"
if run_single_case \
  "TC02_View_One_Message" \
  "TC02_View_One_Message.txt" \
  "TC02_View_One_Message.assert.txt" \
  "TC02_View_One_Message.stdout.txt" \
  "TC02_View_One_Message.outfile.txt"; then
  echo "  PASS"
else
  echo "  FAIL"
  failures=1
fi
echo

echo "TC03_View_Multiple_Messages"
seed_case "TC03_View_Multiple_Messages"
if run_single_case \
  "TC03_View_Multiple_Messages" \
  "TC03_View_Multiple_Messages.txt" \
  "TC03_View_Multiple_Messages.assert.txt" \
  "TC03_View_Multiple_Messages.stdout.txt" \
  "TC03_View_Multiple_Messages.outfile.txt"; then
  echo "  PASS"
else
  echo "  FAIL"
  failures=1
fi
echo

echo "TC04_Persistence_Run1"
seed_case "TC04_Persistence"
if run_single_case \
  "TC04_Persistence_Run1" \
  "TC04_Persistence_Run1.txt" \
  "TC04_Persistence_Run1.assert.txt" \
  "TC04_Persistence_Run1.stdout.txt" \
  "TC04_Persistence_Run1.outfile.txt"; then
  echo "  PASS"
else
  echo "  FAIL"
  failures=1
fi
echo

echo "TC04_Persistence_Run2"
if run_single_case \
  "TC04_Persistence_Run2" \
  "TC04_Persistence_Run2.txt" \
  "TC04_Persistence_Run2.assert.txt" \
  "TC04_Persistence_Run2.stdout.txt" \
  "TC04_Persistence_Run2.outfile.txt"; then
  echo "  PASS"
else
  echo "  FAIL"
  failures=1
fi
echo

exit "$failures"
