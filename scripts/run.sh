#!/usr/bin/env bash
set -e

# Compile
docker run --rm -v "$PWD:/app" incollege-cobol \
  cobc -x -free -o /app/incollege /app/src/incollege.cob

# Run (reads input from test/InCollege-Input.txt)
docker run --rm -v "$PWD:/app" incollege-cobol \
  /app/incollege < /app/test/InCollege-Input.txt