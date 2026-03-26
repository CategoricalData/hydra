#!/usr/bin/env python3
"""
Bootstrapping demo dashboard for Hydra.

Reads JSON result files produced by bootstrap-all.sh and displays a summary
of the bootstrapping runs.

Usage:
    python3 bin/bootstrapping-dashboard.py [latest|diff] [options]

Subcommands:
    latest                  Show the most recent run (default)
    diff                    Compare two runs

Options:
    --dir DIR               Runs directory (default: bootstrap/runs)
    --run RUN               Show a specific run (accepts exact name, tag, or substring)
    --depth N               How many levels deep to show test groups (default: 0, off)
    --group GROUP           Filter to a specific test group
    --old RUN               Run directory for 'previous' in diff mode
    --new RUN               Run directory for 'current' in diff mode
    --threshold N           Highlight deltas exceeding N percent in diff mode (default: 10)

Run directories are named run_YYYY-MM-DD_HHMMSS_mmm[_tag] under bootstrap/runs/.
Each contains metadata.json, per-path files like haskell-to-java.json, and
benchmark files like haskell-to-java.benchmark.json (or _N.benchmark.json for repeats).
"""

import argparse
import glob
import json
import re
import statistics
from pathlib import Path

# ANSI colour helpers
_RED = "\033[91m"
_GREEN = "\033[92m"
_GRAY = "\033[90m"
_BOLD = "\033[1m"
_RESET = "\033[0m"

_RUN_DIR_RE = re.compile(r"^run_(\d{4}-\d{2}-\d{2}_\d{6}_\d{3})(_.+)?$")
_ANSI_RE = re.compile(r"\033\[[0-9;]*m")

HOSTS = ["haskell", "java", "scala", "python"]
TARGETS = ["haskell", "java", "scala", "python", "clojure", "common-lisp", "emacs-lisp", "scheme"]


def visible_len(s):
    return len(_ANSI_RE.sub("", s))


def _run_sort_key(dirname):
    m = _RUN_DIR_RE.match(dirname)
    return m.group(1) if m else dirname


def find_run_dirs(runs_dir):
    """Return sorted list of run directory Paths."""
    p = Path(runs_dir)
    if not p.exists():
        return []
    dirs = [d for d in p.iterdir() if d.is_dir() and d.name.startswith("run_")]
    return sorted(dirs, key=lambda d: _run_sort_key(d.name))


def resolve_run(runs_dir, spec):
    """Resolve a run spec (exact name, tag, or substring) to a directory Path."""
    dirs = find_run_dirs(runs_dir)
    if not dirs:
        return None
    for d in dirs:
        if d.name == spec:
            return d
    for d in dirs:
        if d.name == f"run_{spec}":
            return d
    matches = [d for d in dirs if spec in d.name]
    return matches[-1] if matches else None


def load_run(run_dir):
    """Load a run: metadata + per-path JSONs + benchmark JSONs."""
    run_dir = Path(run_dir)
    meta_path = run_dir / "metadata.json"
    metadata = json.loads(meta_path.read_text()) if meta_path.exists() else {}

    paths = {}
    for f in sorted(run_dir.glob("*.json")):
        if f.name == "metadata.json" or ".benchmark." in f.name:
            continue
        try:
            data = json.loads(f.read_text())
            key = f.stem  # e.g. "haskell-to-java"
            paths[key] = data
        except (json.JSONDecodeError, KeyError):
            continue

    return {"dir": run_dir, "metadata": metadata, "paths": paths}


def load_benchmark_timing(run_dir, path_key):
    """Load benchmark JSONs for a path, return (median_ms, stddev_ms) or (None, None)."""
    run_dir = Path(run_dir)

    # Try single file first
    single = run_dir / f"{path_key}.benchmark.json"
    if single.exists():
        try:
            d = json.loads(single.read_text())
            tms = (d.get("summary") or {}).get("totalTimeMs")
            return (tms, 0.0) if tms is not None else (None, None)
        except (json.JSONDecodeError, KeyError):
            return (None, None)

    # Try numbered files
    pattern = str(run_dir / f"{path_key}_*.benchmark.json")
    files = sorted(glob.glob(pattern))
    if not files:
        # Fall back to embedded testResults in per-path JSON
        path_json = run_dir / f"{path_key}.json"
        if path_json.exists():
            try:
                d = json.loads(path_json.read_text())
                tr = d.get("testResults")
                if tr and tr.get("summary"):
                    tms = tr["summary"].get("totalTimeMs")
                    return (tms, 0.0) if tms is not None else (None, None)
            except (json.JSONDecodeError, KeyError):
                pass
        return (None, None)

    times = []
    for fp in files:
        try:
            d = json.loads(Path(fp).read_text())
            tms = (d.get("summary") or {}).get("totalTimeMs")
            if tms is not None:
                times.append(tms)
        except (json.JSONDecodeError, KeyError):
            continue
    if not times:
        return (None, None)
    med = statistics.median(times)
    sd = statistics.stdev(times) if len(times) > 1 else 0.0
    return (med, sd)


def load_benchmark_data(run_dir, path_key):
    """Load all benchmark JSONs for a path, returning list of parsed data dicts.

    Used for detailed test group views.
    """
    run_dir = Path(run_dir)

    single = run_dir / f"{path_key}.benchmark.json"
    if single.exists():
        try:
            return [json.loads(single.read_text())]
        except (json.JSONDecodeError, KeyError):
            return []

    pattern = str(run_dir / f"{path_key}_*.benchmark.json")
    files = sorted(glob.glob(pattern))
    if not files:
        # Fall back to embedded testResults
        path_json = run_dir / f"{path_key}.json"
        if path_json.exists():
            try:
                d = json.loads(path_json.read_text())
                tr = d.get("testResults")
                if tr:
                    return [tr]
            except (json.JSONDecodeError, KeyError):
                pass
        return []

    results = []
    for fp in files:
        try:
            results.append(json.loads(Path(fp).read_text()))
        except (json.JSONDecodeError, KeyError):
            continue
    return results


def count_repeats(run_dir, path_key):
    """Count how many benchmark repeat files exist for a path."""
    run_dir = Path(run_dir)
    single = run_dir / f"{path_key}.benchmark.json"
    if single.exists():
        return 1
    pattern = str(run_dir / f"{path_key}_*.benchmark.json")
    files = glob.glob(pattern)
    return len(files) if files else 0


def fmt_time(seconds):
    """Format seconds as a human-readable string."""
    if seconds is None:
        return "?"
    if seconds < 60:
        return f"{seconds:.1f}s"
    mins = int(seconds) // 60
    secs = seconds - mins * 60
    return f"{mins}m {secs:.0f}s"


def fmt_time_short(seconds):
    """Format seconds compactly for matrix cells."""
    if seconds is None:
        return "?"
    if seconds < 0.1:
        return f"{seconds * 1000:.0f}ms"
    return f"{seconds:.1f}s"


def capitalize(s):
    return "-".join(w[0].upper() + w[1:] for w in s.split("-"))


def print_run_header(run):
    meta = run["metadata"]

    parts = [f"  Run: {run['dir'].name}"]
    if meta.get("commit"):
        branch = meta.get("branch", "")
        msg = meta.get("commitMessage", "")
        parts.append(f"  Commit: {meta['commit']} ({branch}) \"{msg}\"")
    if meta.get("startTime"):
        parts.append(f"  Time: {meta['startTime']}")
    if meta.get("totalTimeSeconds") is not None:
        parts.append(f"  Duration: {fmt_time(meta['totalTimeSeconds'])}")

    for p in parts:
        print(p)
    print()


def draw_matrix(header_row, host_rows, lines_per_host):
    """Draw a box-drawing matrix. host_rows is a list of lists-of-rows per host."""
    num_cols = len(header_row)
    all_rows = [header_row]
    for host_block in host_rows:
        all_rows.extend(host_block)

    # Compute column widths
    col_widths = []
    for c in range(num_cols):
        max_len = 0
        for row in all_rows:
            if c < len(row):
                vl = visible_len(row[c])
                if vl > max_len:
                    max_len = vl
        col_widths.append(max_len + 2)

    def draw_rule(left, mid, right):
        parts = [f"  {left}"]
        for c in range(num_cols):
            if c > 0:
                parts.append(mid)
            parts.append("\u2500" * col_widths[c])
        parts.append(right)
        print("".join(parts))

    def draw_row(row):
        parts = ["  \u2502"]
        for c in range(num_cols):
            val = row[c] if c < len(row) else ""
            pad = col_widths[c] - 1 - visible_len(val)
            parts.append(f" {val}{' ' * pad}\u2502")
        print("".join(parts))

    draw_rule("\u250c", "\u252c", "\u2510")
    draw_row(all_rows[0])
    draw_rule("\u251c", "\u253c", "\u2524")

    for hi, host_block in enumerate(host_rows):
        if hi > 0:
            draw_rule("\u251c", "\u253c", "\u2524")
        for row in host_block:
            draw_row(row)

    draw_rule("\u2514", "\u2534", "\u2518")
    print()


def cmd_latest(args):
    runs_dir = args.dir
    dirs = find_run_dirs(runs_dir)

    if not dirs:
        print(f"No run directories found in {runs_dir}")
        return

    if args.run:
        run_dir = resolve_run(runs_dir, args.run)
        if not run_dir:
            print(f"No run directory found matching '{args.run}'.")
            return
    else:
        run_dir = dirs[-1]

    run = load_run(run_dir)
    if not run["paths"]:
        print(f"Run '{run_dir.name}' contains no path data.")
        return

    print_run_header(run)

    # Determine which hosts/targets are present
    hosts_present = []
    targets_present = []
    for key in run["paths"]:
        parts = key.split("-to-")
        if len(parts) == 2:
            if parts[0] not in hosts_present:
                hosts_present.append(parts[0])
            if parts[1] not in targets_present:
                targets_present.append(parts[1])

    hosts = [h for h in HOSTS if h in hosts_present]
    targets = [t for t in TARGETS if t in targets_present]

    if not hosts or not targets:
        print("No valid path data found.")
        return

    # Check if any path has repeats > 1
    max_repeats = 0
    for host in hosts:
        for target in targets:
            r = count_repeats(run_dir, f"{host}-to-{target}")
            if r > max_repeats:
                max_repeats = r

    header_row = ["Host \\ Target"] + [capitalize(t) for t in targets]
    host_rows = []

    for host in hosts:
        line1 = [capitalize(host)]
        line2 = [""]
        line3 = [""]
        for target in targets:
            key = f"{host}-to-{target}"
            data = run["paths"].get(key)
            if data is None:
                line1.append("(not run)")
                line2.append("")
                line3.append("")
                continue

            status = data.get("status", "?")
            if status == "fail":
                line1.append(f"{_RED}FAILED{_RESET}")
                line2.append("")
                line3.append("")
                continue

            gen = data.get("generation") or {}
            main_gen = gen.get("main") or {}
            test_gen = gen.get("test") or {}

            mf = main_gen.get("fileCount", "?")
            mt = fmt_time_short(main_gen.get("timeSeconds"))
            tf = test_gen.get("fileCount", "?")
            tt = fmt_time_short(test_gen.get("timeSeconds"))

            line1.append(f"{mf} main @ {mt}")
            line2.append(f"{tf} test @ {tt}")

            # Test run time from benchmark JSONs
            med_ms, sd_ms = load_benchmark_timing(run_dir, key)
            if med_ms is not None:
                ttime = fmt_time_short(med_ms / 1000.0)
                if max_repeats > 1 and sd_ms > 0:
                    ttime += f" +/-{fmt_time_short(sd_ms / 1000.0)}"
                line3.append(f"tests: {ttime}")
            else:
                line3.append(f"{_GRAY}tests: \u2014{_RESET}")

        host_rows.append([line1, line2, line3])

    print("  Each cell: main files @ gen time / test files @ gen time / test run time")
    print()
    draw_matrix(header_row, host_rows, 3)

    # Show test group details if --depth > 0
    if args.depth and args.depth > 0:
        print_test_details(run, hosts, targets, args.depth, args.group)


def aggregate_groups(all_runs_data):
    """Aggregate multiple benchmark runs into one with median timing + stddev.

    Takes a list of benchmark data dicts. Returns (groups, summary) where
    groups have median totalTimeMs and _stddev fields.
    """
    if not all_runs_data:
        return [], {}

    if len(all_runs_data) == 1:
        d = all_runs_data[0]
        return d.get("groups", []), d.get("summary", {})

    # Collect timing samples per group path
    samples = {}  # path -> [totalTimeMs, ...]
    for d in all_runs_data:
        _collect_group_samples(d.get("groups", []), samples)

    # Use first run as structural template, apply median
    base_groups = json.loads(json.dumps(all_runs_data[0].get("groups", [])))
    _apply_median_to_groups(base_groups, samples)

    # Aggregate summary
    summary_times = [d.get("summary", {}).get("totalTimeMs", 0) for d in all_runs_data]
    summary = dict(all_runs_data[0].get("summary", {}))
    summary["totalTimeMs"] = round(statistics.median(summary_times), 1)
    if len(summary_times) > 1:
        summary["_stddev"] = round(statistics.stdev(summary_times), 1)

    return base_groups, summary


def _collect_group_samples(groups, samples):
    for g in groups:
        path = g.get("path", "")
        tms = g.get("totalTimeMs", 0)
        samples.setdefault(path, []).append(tms)
        _collect_group_samples(g.get("subgroups", []), samples)


def _apply_median_to_groups(groups, samples):
    for g in groups:
        path = g.get("path", "")
        if path in samples and samples[path]:
            times = samples[path]
            g["totalTimeMs"] = round(statistics.median(times), 1)
            if len(times) > 1:
                g["_stddev"] = round(statistics.stdev(times), 1)
        _apply_median_to_groups(g.get("subgroups", []), samples)


def print_test_details(run, hosts, targets, depth, group_filter):
    """Print detailed test group timing from benchmark JSONs."""
    print(f"  {_BOLD}Test Group Details{_RESET} (depth={depth})")
    print()

    for host in hosts:
        for target in targets:
            key = f"{host}-to-{target}"
            bench_data = load_benchmark_data(run["dir"], key)
            if not bench_data:
                continue

            groups, _summary = aggregate_groups(bench_data)
            if not groups:
                continue

            repeats = len(bench_data)
            repeat_str = f" ({repeats} repeats)" if repeats > 1 else ""
            print(f"  {_BOLD}{host} \u2192 {target}{_RESET}{repeat_str}")

            def print_group(g, indent, current_depth):
                if current_depth > depth:
                    return
                name = g.get("path", "?").split("/")[-1]
                if group_filter and group_filter.lower() not in name.lower():
                    for sg in g.get("subgroups", []):
                        print_group(sg, indent, current_depth)
                    return

                passed = g.get("passed", 0)
                failed = g.get("failed", 0)
                time_ms = g.get("totalTimeMs", 0)
                stddev = g.get("_stddev")
                prefix = "  " * indent
                fail_str = f" {_RED}{failed}F{_RESET}" if failed > 0 else ""
                time_str = f"{time_ms:8.1f}ms"
                if stddev is not None and stddev > 0:
                    time_str += f" +/-{stddev:.0f}"
                print(f"    {prefix}{name:40s} {passed:4d}P{fail_str}  {time_str}")

                for sg in g.get("subgroups", []):
                    print_group(sg, indent + 1, current_depth + 1)

            for g in groups:
                print_group(g, 0, 1)
            print()


def cmd_diff(args):
    runs_dir = args.dir
    dirs = find_run_dirs(runs_dir)

    if len(dirs) < 2 and not (args.old and args.new):
        print("Need at least 2 runs for diff (or use --old and --new).")
        return

    if args.new:
        new_dir = resolve_run(runs_dir, args.new)
        if not new_dir:
            print(f"No run found matching '{args.new}'.")
            return
    else:
        new_dir = dirs[-1]

    if args.old:
        old_dir = resolve_run(runs_dir, args.old)
        if not old_dir:
            print(f"No run found matching '{args.old}'.")
            return
    else:
        idx = next((i for i, d in enumerate(dirs) if d == new_dir), -1)
        if idx <= 0:
            print("No previous run found for comparison.")
            return
        old_dir = dirs[idx - 1]

    old_run = load_run(old_dir)
    new_run = load_run(new_dir)

    print(f"  Old: {old_run['dir'].name}")
    print(f"  New: {new_run['dir'].name}")
    print()

    threshold = args.threshold

    all_keys = sorted(set(list(old_run["paths"].keys()) + list(new_run["paths"].keys())))
    for key in all_keys:
        old_data = old_run["paths"].get(key)
        new_data = new_run["paths"].get(key)

        if not old_data and not new_data:
            continue

        print(f"  {_BOLD}{key}{_RESET}")

        # Compare generation times
        for phase in ["main", "test"]:
            old_t = None
            new_t = None
            if old_data and old_data.get("generation"):
                p = old_data["generation"].get(phase) or {}
                old_t = p.get("timeSeconds")
            if new_data and new_data.get("generation"):
                p = new_data["generation"].get(phase) or {}
                new_t = p.get("timeSeconds")

            old_str = fmt_time_short(old_t) if old_t is not None else "\u2014"
            new_str = fmt_time_short(new_t) if new_t is not None else "\u2014"

            delta_str = ""
            if old_t and new_t and old_t > 0:
                pct = (new_t - old_t) / old_t * 100
                if abs(pct) >= threshold:
                    color = _RED if pct > 0 else _GREEN
                    delta_str = f"  {color}{pct:+.0f}%{_RESET} <-"
                else:
                    delta_str = f"  {_GRAY}{pct:+.0f}%{_RESET}"

            print(f"    {phase:6s} gen:  {old_str:>8s} \u2192 {new_str:>8s}{delta_str}")

        # Compare test run times from benchmark JSONs
        old_med, _ = load_benchmark_timing(old_run["dir"], key) if old_data else (None, None)
        new_med, _ = load_benchmark_timing(new_run["dir"], key) if new_data else (None, None)
        if old_med is not None or new_med is not None:
            old_str = fmt_time_short(old_med / 1000.0) if old_med is not None else "\u2014"
            new_str = fmt_time_short(new_med / 1000.0) if new_med is not None else "\u2014"
            delta_str = ""
            if old_med and new_med and old_med > 0:
                pct = (new_med - old_med) / old_med * 100
                if abs(pct) >= threshold:
                    color = _RED if pct > 0 else _GREEN
                    delta_str = f"  {color}{pct:+.0f}%{_RESET} <-"
                else:
                    delta_str = f"  {_GRAY}{pct:+.0f}%{_RESET}"
            print(f"    {'tests':6s}:     {old_str:>8s} \u2192 {new_str:>8s}{delta_str}")

        # Compare path total time
        old_pt = old_data.get("pathTimeSeconds") if old_data else None
        new_pt = new_data.get("pathTimeSeconds") if new_data else None
        if old_pt is not None or new_pt is not None:
            old_str = fmt_time(old_pt) if old_pt is not None else "\u2014"
            new_str = fmt_time(new_pt) if new_pt is not None else "\u2014"
            delta_str = ""
            if old_pt and new_pt and old_pt > 0:
                pct = (new_pt - old_pt) / old_pt * 100
                if abs(pct) >= threshold:
                    color = _RED if pct > 0 else _GREEN
                    delta_str = f"  {color}{pct:+.0f}%{_RESET} <-"
                else:
                    delta_str = f"  {_GRAY}{pct:+.0f}%{_RESET}"
            print(f"    {'total':6s}:     {old_str:>8s} \u2192 {new_str:>8s}{delta_str}")

        print()


def main():
    parser = argparse.ArgumentParser(
        description="Bootstrapping demo dashboard for Hydra")
    parser.add_argument("command", nargs="?", default="latest",
                        choices=["latest", "diff"],
                        help="Subcommand (default: latest)")
    parser.add_argument("--dir", default="bootstrap/runs",
                        help="Runs directory (default: bootstrap/runs)")
    parser.add_argument("--run",
                        help="Show a specific run (latest mode)")
    parser.add_argument("--depth", type=int, default=0,
                        help="Test group detail depth (default: 0, off)")
    parser.add_argument("--group",
                        help="Filter to a specific test group")
    parser.add_argument("--threshold", type=int, default=10,
                        help="Highlight deltas exceeding N%% in diff mode (default: 10)")
    parser.add_argument("--old",
                        help="Run directory for 'previous' in diff mode")
    parser.add_argument("--new",
                        help="Run directory for 'current' in diff mode")

    args = parser.parse_args()

    if args.command == "diff":
        cmd_diff(args)
    else:
        cmd_latest(args)


if __name__ == "__main__":
    main()
