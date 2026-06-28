#!/usr/bin/env bash
# Session-local wrapper: sbt compile in packages/hydra-scala (#509 work).
# NOT for sync use — temporary scaffolding to bypass per-invocation Bash prompts.
set -euo pipefail
export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64
cd "$(dirname "$0")/../packages/hydra-scala"
exec sbt --no-colors compile
