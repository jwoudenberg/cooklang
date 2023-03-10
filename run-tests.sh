#!/usr/bin/env bash

set -euxo pipefail

python -m doctest -v ./src/*.py
shelltest --hide-successes .
flake8 src/*
black --check src/*
