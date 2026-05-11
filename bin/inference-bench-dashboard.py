#!/usr/bin/env python3
"""Cross-host inference-benchmark dashboard.

Aggregates per-host inference-bench JSON outputs into side-by-side tables,
one per series. Mirrors the styling and color conventions of
``bin/benchmark-dashboard.py`` (the kernel-tests benchmark) so the two
dashboards feel like one family.

Each input file is one of:
  - New (run-directory) shape:   {"_metadata": {...}, "results": [<record>, ...]}
  - Legacy (ad-hoc) shape:       [<record>, ...]

A record is ``{host, namespace, n, elapsed_seconds, ok, error}``.
Records are grouped by ``namespace`` (the bench series); for each series,
this script prints:

  1. Raw wallclock table   (n × host)
  2. Adjusted table        (raw minus the n=0 baseline per host)
  3. Relative-to-fastest   (on adjusted numbers when baseline is present)
  4. Power-law fit         T(n) ≈ c · n^k  with R²

If the runners produced an n=0 baseline (timed inference on an empty target
module — measures per-call inference setup with no actual work), this
script subtracts it from each n>0 measurement before fitting.

Usage:
    inference-bench-dashboard.py FILE1.json [FILE2.json ...]   # explicit files
    inference-bench-dashboard.py --dir benchmark/inference-runs # scan a runs dir
    inference-bench-dashboard.py --dir DIR --run RUN            # one specific run
"""
from __future__ import annotations

import argparse
import json
import math
import sys
from collections.abc import Sequence
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent / "lib"))
from dashboard_common import (  # noqa: E402
    GRAY,
    GREEN,
    RED,
    RESET,
    run_sort_key as _run_sort_key,
)


# Canonical series order; unknown namespaces sort to the end.
_SERIES_ORDER = ["hydra.bench.linearChain",
                 "hydra.bench.polymorphicChain",
                 "hydra.bench.fanOut"]


def _load_file(path: str | Path) -> list[dict]:
    """Load a single inference-bench JSON, handling both shapes."""
    with open(path) as f:
        data = json.load(f)
    if isinstance(data, list):
        return data
    if isinstance(data, dict) and "results" in data:
        return data["results"]
    raise ValueError(f"{path}: expected array or {{_metadata, results}}")


def _load_files(paths: Sequence[str]) -> list[dict]:
    """Concatenate records from multiple files."""
    out: list[dict] = []
    for p in paths:
        try:
            out.extend(_load_file(p))
        except (OSError, ValueError) as e:
            print(f"WARNING: could not load {p}: {e}", file=sys.stderr)
    return out


def _load_run_dir(run_dir: Path) -> list[dict]:
    """Load every *.json under a run_<TS>/ directory."""
    records: list[dict] = []
    for f in sorted(run_dir.glob("*.json")):
        try:
            records.extend(_load_file(f))
        except (OSError, ValueError) as e:
            print(f"WARNING: could not load {f}: {e}", file=sys.stderr)
    return records


def _find_runs(runs_root: Path) -> list[Path]:
    """List run_<TS>[_<tag>]/ directories under ``runs_root``, oldest first."""
    if not runs_root.exists():
        return []
    return sorted(
        (d for d in runs_root.iterdir()
         if d.is_dir() and d.name.startswith("run_")),
        key=lambda p: _run_sort_key(p.name))


def _resolve_run(runs_root: Path, run_spec: str | None) -> Path | None:
    """Pick a single run directory by name (full or substring), or the latest."""
    runs = _find_runs(runs_root)
    if not runs:
        return None
    if run_spec is None:
        return runs[-1]
    for r in runs:
        if r.name == run_spec or r.name == f"run_{run_spec}":
            return r
    matches = [r for r in runs if run_spec in r.name]
    if not matches:
        return None
    if len(matches) > 1:
        print(f"Warning: '{run_spec}' matches {len(matches)} runs; using "
              f"most recent: {matches[-1].name}", file=sys.stderr)
    return matches[-1]


def _fit_power(points: list[tuple[int, float]]) -> tuple[float, float, float] | None:
    """Fit log T = log c + k log n on positive (n, t) points.

    Returns (k, c, R²) or None if there aren't enough usable points.
    """
    pts = [(n, t) for (n, t) in points if n > 0 and t > 0]
    if len(pts) < 2:
        return None
    xs = [math.log(n) for n, _ in pts]
    ys = [math.log(t) for _, t in pts]
    nn = len(xs)
    mx = sum(xs) / nn
    my = sum(ys) / nn
    num = sum((x - mx) * (y - my) for x, y in zip(xs, ys))
    den = sum((x - mx) ** 2 for x in xs)
    if den == 0:
        return None
    k = num / den
    a = my - k * mx
    yhat = [a + k * x for x in xs]
    ss_res = sum((y - yh) ** 2 for y, yh in zip(ys, yhat))
    ss_tot = sum((y - my) ** 2 for y in ys)
    r2 = 1 - ss_res / ss_tot if ss_tot > 0 else 1.0
    return (k, math.exp(a), r2)


def _fmt_time(t: float, width: int) -> str:
    """Right-align a wallclock time with 's' suffix in a fixed width."""
    return f"{t:>{width-1}.2f}s"


def _ns_key(ns: str) -> tuple[int, str]:
    try:
        return (_SERIES_ORDER.index(ns), ns)
    except ValueError:
        return (len(_SERIES_ORDER), ns)


def _host_sort_key(host: str, records: dict[int, dict]) -> tuple[float, str]:
    """Stable host order: fastest first (by largest successful n)."""
    ok_records = [r for r in records.values() if r.get("ok") and r.get("n", 0) > 0]
    if ok_records:
        biggest = max(ok_records, key=lambda r: r["n"])
        return (biggest.get("elapsed_seconds", float("inf")), host)
    return (float("inf"), host)


def _print_series(namespace: str,
                  by_host: dict[str, dict[int, dict]],
                  slowdown_threshold: float = 10.0) -> None:
    """Print all tables for one series."""
    if not by_host:
        return
    print()
    print(f"=== {namespace} ===")

    hosts = sorted(by_host.keys(),
                   key=lambda h: _host_sort_key(h, by_host[h]))
    sizes = sorted({n for h in by_host.values() for n in h.keys()})

    col_w = 14
    header = f"{'n':>5}  " + "  ".join(f"{h:>{col_w}}" for h in hosts)
    sep = "-" * len(header)

    # 1. Raw wallclock.
    print()
    print("Raw wallclock:")
    print(header)
    print(sep)
    for n in sizes:
        cells = []
        for h in hosts:
            r = by_host[h].get(n)
            if r is None:
                cells.append(f"{'—':>{col_w}}")
            elif r.get("ok"):
                cells.append(_fmt_time(r["elapsed_seconds"], col_w))
            else:
                cells.append(f"{RED}{'FAIL':>{col_w}}{RESET}")
        label = "0 (base)" if n == 0 else str(n)
        print(f"{label:>5}  " + "  ".join(cells))

    has_baseline = any(0 in by_host[h] and by_host[h][0].get("ok") for h in hosts)

    # 2. Adjusted table (subtract baseline). Color: slow hosts in red.
    if has_baseline:
        print()
        print("Adjusted (n>0 minus n=0 baseline):")
        print(header)
        print(sep)
        for n in sizes:
            if n == 0:
                continue
            adj_by_host = {}
            for h in hosts:
                r = by_host[h].get(n)
                base = by_host[h].get(0)
                if r is None or not r.get("ok") or base is None or not base.get("ok"):
                    continue
                adj = r["elapsed_seconds"] - base["elapsed_seconds"]
                if adj > 0:
                    adj_by_host[h] = adj
            fastest = min(adj_by_host.values()) if adj_by_host else None
            cells = []
            for h in hosts:
                r = by_host[h].get(n)
                base = by_host[h].get(0)
                if r is None or not r.get("ok"):
                    cells.append(f"{'—':>{col_w}}")
                elif base is None or not base.get("ok"):
                    cells.append(f"{'(no base)':>{col_w}}")
                else:
                    adj = r["elapsed_seconds"] - base["elapsed_seconds"]
                    if adj <= 0:
                        cells.append(f"{'~0':>{col_w}}")
                    else:
                        cell = _fmt_time(adj, col_w)
                        if fastest is not None and fastest > 0:
                            ratio = adj / fastest
                            if ratio >= slowdown_threshold:
                                cell = f"{RED}{cell}{RESET}"
                        cells.append(cell)
            print(f"{n:>5}  " + "  ".join(cells))

    # 3. Relative-to-fastest at each n. Hosts whose adjusted time is below
    # the noise floor (< 0.05s) are shown as '~noise' rather than producing
    # nonsensical ratios against a near-zero fastest.
    noise_floor = 0.05
    print()
    print("Relative (vs fastest at each n):")
    print(header)
    print(sep)
    for n in sizes:
        if n == 0:
            continue
        rows: list[tuple[str, float | None, bool]] = []  # (host, t, in_noise)
        for h in hosts:
            r = by_host[h].get(n)
            base = by_host[h].get(0) if has_baseline else None
            if r is None or not r.get("ok"):
                rows.append((h, None, False))
                continue
            t = r["elapsed_seconds"]
            if base and base.get("ok"):
                t = t - base["elapsed_seconds"]
            in_noise = t < noise_floor
            rows.append((h, t, in_noise))
        usable = [t for _, t, in_noise in rows if t is not None and not in_noise]
        if not usable:
            continue
        fastest = min(usable)
        cells = []
        for h, t, in_noise in rows:
            if t is None:
                cells.append(f"{'—':>{col_w}}")
            elif in_noise:
                cells.append(f"{GRAY}{'~noise':>{col_w}}{RESET}")
            else:
                ratio = t / fastest
                if ratio < 1.005:
                    cells.append(f"{GREEN}{'1.00x':>{col_w}}{RESET}")
                elif ratio >= slowdown_threshold:
                    cells.append(f"{RED}{ratio:>{col_w-1}.2f}x{RESET}")
                else:
                    cells.append(f"{ratio:>{col_w-1}.2f}x")
        print(f"{n:>5}  " + "  ".join(cells))

    # 4. Power-law fit on baseline-adjusted data.
    print()
    print("Power-law fit  T(n) ≈ c · n^k  on baseline-adjusted data:")
    print(f"{'host':>{col_w}}  {'k':>8}  {'c':>10}  {'R²':>8}")
    print("-" * (col_w + 32))
    for h in hosts:
        records = by_host[h]
        base = records.get(0) if has_baseline else None
        baseline_t = (base["elapsed_seconds"]
                      if base and base.get("ok") else 0.0)
        points = []
        for n, r in records.items():
            if n == 0 or not r.get("ok"):
                continue
            t = r["elapsed_seconds"] - baseline_t
            if t > 0:
                points.append((n, t))
        fit = _fit_power(points)
        if fit is None:
            print(f"{h:>{col_w}}  {GRAY}{'(insufficient)':>32}{RESET}")
            continue
        k, c, r2 = fit
        # Color the exponent: green for ~linear, red for >=1.3.
        if k >= 1.3:
            k_str = f"{RED}{k:>8.3f}{RESET}"
        elif k <= 1.1:
            k_str = f"{GREEN}{k:>8.3f}{RESET}"
        else:
            k_str = f"{k:>8.3f}"
        print(f"{h:>{col_w}}  {k_str}  {c:>10.4f}  {r2:>8.4f}")


def _index_by_series(records: list[dict]) -> dict[str, dict[str, dict[int, dict]]]:
    """Nest records as: namespace → host → n → record."""
    by_ns: dict[str, dict[str, dict[int, dict]]] = {}
    for r in records:
        ns = r.get("namespace", "(unknown)")
        host = r.get("host", "<unknown>")
        n = r.get("n")
        if n is None:
            continue
        by_ns.setdefault(ns, {}).setdefault(host, {})[n] = r
    return by_ns


def main(argv: Sequence[str]) -> int:
    ap = argparse.ArgumentParser(
        prog="inference-bench-dashboard.py",
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    ap.add_argument("files", nargs="*",
                    help="Explicit JSON files to aggregate.")
    ap.add_argument("--dir", default=None,
                    help="Scan a runs directory (benchmark/inference-runs).")
    ap.add_argument("--run", default=None,
                    help="With --dir: pick a specific run (name or substring); "
                         "default is the most recent run.")
    ap.add_argument("--slowdown", type=float, default=10.0,
                    help="Threshold for highlighting slowness in red (default: 10x).")
    args = ap.parse_args(argv)

    if not args.files and not args.dir:
        ap.error("no input: pass FILE(s) positionally or use --dir")

    records: list[dict] = []
    if args.files:
        records.extend(_load_files(args.files))
    if args.dir:
        runs_root = Path(args.dir)
        run = _resolve_run(runs_root, args.run)
        if run is None:
            print(f"No run found under {runs_root}"
                  + (f" matching '{args.run}'" if args.run else ""),
                  file=sys.stderr)
            return 1
        print(f"Run: {run.name}", file=sys.stderr)
        records.extend(_load_run_dir(run))

    if not records:
        print("No results to display.", file=sys.stderr)
        return 1

    by_ns = _index_by_series(records)
    all_ok = all(r.get("ok") for r in records)
    for ns in sorted(by_ns.keys(), key=_ns_key):
        _print_series(ns, by_ns[ns], slowdown_threshold=args.slowdown)

    return 0 if all_ok else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
