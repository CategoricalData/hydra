#!/usr/bin/env python3
"""Byte-compare Python-generated dist/json/hydra-python against the Haskell-
generated canonical, file by file. Reports BYTE-EQ / DIFFER per module with
per-file size deltas.

Usage:
  compare-self-host.py [--ours DIR] [--canon DIR]

Defaults:
  --ours  /tmp/hp-from-python
  --canon dist/json/hydra-python/src/main/json
"""
from __future__ import annotations

import argparse
import os
import subprocess
import sys
from pathlib import Path

MODULES = ["coder", "environment", "language", "names",
           "serde", "syntax", "testing", "utils"]


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    here = Path(__file__).resolve().parent.parent
    ap.add_argument("--ours", default="/tmp/hp-from-python")
    ap.add_argument("--canon", default=str(here / "dist/json/hydra-python/src/main/json"))
    args = ap.parse_args()

    print(f"Comparing:")
    print(f"  ours  = {args.ours}")
    print(f"  canon = {args.canon}")
    print()
    print(f"{'module':>12}  {'status':<10}  {'ours':>10}  {'canon':>10}  {'delta':>8}  {'diff lines':>10}")
    print("-" * 70)

    eq_count = 0
    for m in MODULES:
        ours_p = Path(args.ours) / "hydra" / "python" / f"{m}.json"
        canon_p = Path(args.canon) / "hydra" / "python" / f"{m}.json"
        if not ours_p.exists():
            print(f"{m:>12}  MISSING")
            continue
        if not canon_p.exists():
            print(f"{m:>12}  NO CANON")
            continue
        s1 = ours_p.stat().st_size
        s2 = canon_p.stat().st_size
        r = subprocess.run(["diff", str(ours_p), str(canon_p)],
                           capture_output=True, text=True)
        if r.returncode == 0:
            print(f"{m:>12}  {'BYTE-EQ':<10}  {s1:>10}  {s2:>10}  {s1-s2:>+8}  {'0':>10}")
            eq_count += 1
        else:
            diff_lines = len(r.stdout.splitlines())
            print(f"{m:>12}  {'DIFFER':<10}  {s1:>10}  {s2:>10}  {s1-s2:>+8}  {diff_lines:>10}")

    print("-" * 70)
    print(f"Summary: {eq_count} / {len(MODULES)} byte-identical")
    return 0 if eq_count == len(MODULES) else 1


if __name__ == "__main__":
    sys.exit(main())
