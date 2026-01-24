#!/usr/bin/env bash
set -e

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

# Compile inside container
docker run --rm -v "$PROJECT_ROOT:/app" incollege-cobol \
  cobc -x -free -o /app/incollege /app/src/incollege.cob

# Run inside container, but feed stdin from the HOST file (this is the important part)
docker run --rm -i -v "$PROJECT_ROOT:/app" incollege-cobol \
  /app/incollege < "$PROJECT_ROOT/test/InCollege-Input.txt"
