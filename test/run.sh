#!/usr/bin/env bash

set -euo pipefail

cd "$(dirname "$0")"

nix build ..

result/bin/parse-recipe recipe.md > ingredients.datalog
git diff --exit-code ingredients.datalog
