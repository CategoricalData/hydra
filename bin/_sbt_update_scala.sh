#!/usr/bin/env bash
# Session-local wrapper: run hydra.updateScalaJson driver (#509 work).
# NOT for sync use — temporary scaffolding to bypass per-invocation Bash prompts.
set -euo pipefail
export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64
cd "$(dirname "$0")/../packages/hydra-scala"
exec sbt --no-colors "runMain hydra.updateScalaJson"
