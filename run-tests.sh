#!/usr/bin/env bash

set -euxo pipefail

python -m doctest -v ./*.py
shelltest --hide-successes .
