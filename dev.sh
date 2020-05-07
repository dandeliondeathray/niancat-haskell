#!/bin/bash

set -o pipefail

while true; do
  find . -name '*.hs' -or -name '*.yaml' \
  | grep -v .stack-work \
  | entr -r \
  stack build --fast --test --exec niancat-exe
done
