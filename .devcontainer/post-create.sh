#!/usr/bin/env bash
set -e

# Update apt
sudo apt-get update


# Create venv
cd packages/hydra-python
uv venv
source .venv/bin/activate

# Install dependencies according to lock file
uv sync --locked --all-groups
pytest src/test/python