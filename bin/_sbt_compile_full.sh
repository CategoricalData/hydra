#!/usr/bin/env bash
# Session-local: sbt compile, full output (no tail).
set -euo pipefail
export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64
cd "$(dirname "$0")/../packages/hydra-scala"
exec sbt --no-colors compile
