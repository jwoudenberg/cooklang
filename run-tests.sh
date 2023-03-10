#!/usr/bin/env bash

set -euxo pipefail

python -m doctest -v ./src/*.py
shelltest --hide-successes .
