#!/usr/bin/env bash

set -euo pipefail

cd "$(dirname "$0")/.."

cabal build
parse_recipe="$(cabal list-bin parse-recipe)"

"$parse_recipe" test/recipe.md > test/ingredients.datalog
git diff --exit-code test/ingredients.datalog
