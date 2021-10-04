#!/usr/bin/env bash

set -euo pipefail

cabal build
parse_recipe="$(cabal list-bin parse-recipe)"
output="test-batch-run-output.datalog"

rm -f "$output"
for recipe in $(find "$1" -name '*.md'); do
  "$parse_recipe" "$recipe" >> "$output"
  echo '' >> "$output"
done
