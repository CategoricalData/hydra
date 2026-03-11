#!/bin/bash
# Test baseline (pre-existing) and new modules, timing each
for mod in ExistingPythonCoder AvroCoder CppNames; do
  echo "=== $mod ==="
  start=$(date +%s)
  gtimeout 600 stack exec test-one-module -- "$mod" 2>&1
  rc=$?
  end=$(date +%s)
  elapsed=$((end - start))
  if [ $rc -eq 0 ]; then
    echo "OK (${elapsed}s)"
  elif [ $rc -eq 124 ]; then
    echo "TIMEOUT after ${elapsed}s"
  else
    echo "FAILED (exit $rc, ${elapsed}s)"
  fi
  echo ""
done
