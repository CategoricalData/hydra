#!/usr/bin/env python3
"""
Cross-implementation benchmark dashboard for Hydra.

Reads JSON benchmark files produced by the instrumented test runners
(controlled by HYDRA_BENCHMARK_OUTPUT) and displays a comparison table.

Usage:
    python3 bin/benchmark-dashboard.py [latest|compare|history] [options]

Subcommands:
    latest                      Show most recent run per language (default)
    diff                        Compare two most recent runs per language
    compare <base> <feature>    Compare two branches
    history --group <group>     Show timing over commits

Options:
    --dir DIR           Benchmark runs directory (default: benchmark/runs)
    --lang LANG         Show only one language (haskell, java, python)
    --group GROUP       Show only a specific group and its subgroups
    --threshold N       Highlight deltas exceeding N percent (default: 10)
    --last N            Show only the N most recent runs (history mode)
    --depth N           How many levels deep to display (default: 2)
    --slowdown N        Highlight times exceeding Nx Haskell's in red (default: 10)
"""

import argparse
import json
import os
import sys
from pathlib import Path


def load_runs(runs_dir: str) -> list[dict]:
    """Load all JSON run files from the directory.

    Supports two layouts:
      - New: runs_dir/run_<timestamp>/<language>.json
      - Legacy: runs_dir/<timestamp>_<language>_<commit>.json
    """
    runs = []
    runs_path = Path(runs_dir)
    if not runs_path.exists():
        return runs

    for d in sorted(runs_path.iterdir()):
        if d.is_dir() and d.name.startswith("run_"):
            for f in sorted(d.glob("*.json")):
                try:
                    with open(f) as fh:
                        data = json.load(fh)
                        data["_file"] = f"{d.name}/{f.name}"
                        runs.append(data)
                except (json.JSONDecodeError, KeyError):
                    pass

    return runs


def index_groups(groups: list[dict]) -> dict[str, dict]:
    """Build a flat index from group path to group data."""
    result = {}
    for g in groups:
        result[g["path"]] = g
        if "subgroups" in g:
            result.update(index_groups(g["subgroups"]))
    return result


def fmt_time(ms: float) -> str:
    """Format milliseconds consistently (always ms, no unit mixing)."""
    if ms >= 100:
        return f"{ms:.0f}ms"
    if ms >= 10:
        return f"{ms:.1f}ms"
    return f"{ms:.2f}ms"


# ANSI color codes
RED = "\033[31m"
GREEN = "\033[32m"
GRAY = "\033[90m"
RESET = "\033[0m"

# Width of each language column (visible characters, excluding ANSI codes)
LANG_COL_WIDTH = 24


def fmt_counts(passed: int, failed: int, skipped: int) -> str:
    """Format test counts: passed in default, failed in red, skipped in gray.
    Returns a string with ANSI codes; visible width is always 12 chars."""
    passed_str = f"{passed:>4}" if passed > 0 else "    "
    if failed > 0:
        failed_str = f"{RED}{failed:>4}{RESET}"
    else:
        failed_str = "    "
    if skipped > 0:
        skipped_str = f"{GRAY}{skipped:>4}{RESET}"
    else:
        skipped_str = "    "
    return passed_str + failed_str + skipped_str


def fmt_counts_header(lang: str) -> str:
    """Format the header for a language column. Visible width = 2 + 23 = 25."""
    return f"  {lang.capitalize():>23}"


def fmt_cell(passed: int, failed: int, skipped: int, time_ms: float,
             ref_time_ms=None, slowdown_threshold: float = 10.0) -> str:
    """Format a full cell: counts + time. Visible width = 2 + 12 + 1 + 10 = 25.
    If ref_time_ms is set: red if >= slowdown_threshold times slower, green if strictly faster."""
    time_str = f"{fmt_time(time_ms):>10}"
    if ref_time_ms is not None and ref_time_ms > 0:
        if time_ms / ref_time_ms >= slowdown_threshold:
            time_str = f"{RED}{time_str}{RESET}"
        elif time_ms < ref_time_ms:
            time_str = f"{GREEN}{time_str}{RESET}"
    return f"  {fmt_counts(passed, failed, skipped)} {time_str}"


def fmt_cell_missing() -> str:
    """Format a cell for a missing group. Visible width = 25."""
    return f"  {'--':>12} {'--':>10}"


def latest_run_per_language(runs: list[dict]) -> dict[str, dict]:
    """Find the most recent run for each language."""
    latest = {}
    for run in runs:
        lang = run.get("metadata", {}).get("language", "")
        if lang:
            latest[lang] = run  # runs are sorted by filename, so last wins
    return latest


def latest_run_per_language_branch(runs: list[dict], branch: str) -> dict[str, dict]:
    """Find the most recent run for each language on a specific branch."""
    latest = {}
    for run in runs:
        meta = run.get("metadata", {})
        if meta.get("branch", "") == branch:
            lang = meta.get("language", "")
            if lang:
                latest[lang] = run
    return latest


def print_run_header(lang: str, run: dict):
    """Print metadata for a run."""
    meta = run.get("metadata", {})
    commit = meta.get("commit", "?")
    branch = meta.get("branch", "?")
    msg = meta.get("commitMessage", "")
    ts = meta.get("timestamp", "?")
    print(f"  {lang.capitalize():<10} {ts[:16]} ({commit}, {branch}) \"{msg}\"")


def cmd_latest(args, runs: list[dict]):
    """Show the most recent run for each language, side by side."""
    latest = latest_run_per_language(runs)
    if not latest:
        print("No benchmark runs found.")
        return

    if args.lang:
        latest = {k: v for k, v in latest.items() if k == args.lang}

    langs = sorted(latest.keys(), key=lambda l: (0 if l == "haskell" else 1, l))
    print("Latest runs:")
    for lang in langs:
        print_run_header(lang, latest[lang])
    print()

    # Build group indexes
    indexes = {lang: index_groups(latest[lang].get("groups", [])) for lang in langs}

    # Collect top-level group paths from first language
    first_lang = langs[0]
    top_groups = latest[first_lang].get("groups", [])

    # Filter by --group if specified
    if args.group:
        top_groups = [g for g in top_groups if args.group in g["path"]]

    # Header
    lang_cols = "".join(fmt_counts_header(lang) for lang in langs)
    header = f"{'Group':<40}{lang_cols}"
    print(header)
    sep = "-" * len(header)
    print(sep)

    totals = {lang: {"passed": 0, "failed": 0, "skipped": 0, "time": 0.0} for lang in langs}

    def print_group(tg, indent, remaining_depth):
        path = tg["path"]
        label = path.rsplit("/", 1)[-1] if "/" in path else path

        max_label = 38 - 2 * indent
        if len(label) > max_label:
            label = label[:max_label - 1] + "\u2026"
        short = ("  " * indent) + label

        # Get Haskell time as reference for slowdown highlighting
        haskell_g = indexes.get("haskell", {}).get(path)
        ref_time = haskell_g.get("totalTimeMs", 0) if haskell_g else None

        cols = ""
        for lang in langs:
            g = indexes[lang].get(path)
            if g is not None:
                p = g.get("passed", 0)
                f = g.get("failed", 0)
                s = g.get("skipped", 0)
                t = g.get("totalTimeMs", 0)
                if indent == 0:
                    totals[lang]["passed"] += p
                    totals[lang]["failed"] += f
                    totals[lang]["skipped"] += s
                    totals[lang]["time"] += t
                rt = ref_time if lang != "haskell" else None
                cols += fmt_cell(p, f, s, t, rt, args.slowdown)
            else:
                cols += fmt_cell_missing()
        print(f"{short:<40}{cols}")

        if remaining_depth > 1 and "subgroups" in tg:
            for sg in tg.get("subgroups", []):
                print_group(sg, indent + 1, remaining_depth - 1)

    for tg in top_groups:
        print_group(tg, 0, args.depth)

    print(sep)
    haskell_total_time = totals.get("haskell", {}).get("time")
    total_cols = ""
    for lang in langs:
        t = totals[lang]
        rt = haskell_total_time if lang != "haskell" else None
        total_cols += fmt_cell(t['passed'], t['failed'], t['skipped'], t['time'], rt, args.slowdown)
    print(f"{'TOTAL':<40}{total_cols}")


def cmd_compare(args, runs: list[dict]):
    """Compare the latest run on two branches for each language."""
    base_runs = latest_run_per_language_branch(runs, args.base_branch)
    feat_runs = latest_run_per_language_branch(runs, args.feature_branch)

    if args.lang:
        base_runs = {k: v for k, v in base_runs.items() if k == args.lang}
        feat_runs = {k: v for k, v in feat_runs.items() if k == args.lang}

    langs = sorted(set(base_runs.keys()) & set(feat_runs.keys()), key=lambda l: (0 if l == "haskell" else 1, l))
    if not langs:
        print(f"No matching runs found for branches '{args.base_branch}' and '{args.feature_branch}'.")
        return

    threshold = args.threshold

    print(f"Comparing branches: {args.base_branch} vs {args.feature_branch}")
    print()

    for lang in langs:
        base = base_runs[lang]
        feat = feat_runs[lang]
        base_idx = index_groups(base.get("groups", []))
        feat_idx = index_groups(feat.get("groups", []))

        print(f"{lang.capitalize()}:")
        print(f"  base:    ", end="")
        print_run_header("", base)
        print(f"  feature: ", end="")
        print_run_header("", feat)
        print()

        header = f"{'Group':<40}  {'base':>10}  {'feature':>10}  {'Delta':>8}"
        print(header)
        print("-" * len(header))

        for tg in base.get("groups", []):
            path = tg["path"]
            short = path.split("/", 1)[-1] if "/" in path else path
            bt = tg.get("totalTimeMs", 0)
            ft_g = feat_idx.get(path, {})
            ft = ft_g.get("totalTimeMs", 0)

            if bt > 0:
                delta = (ft - bt) / bt * 100
                delta_str = f"{delta:+.1f}%"
                marker = "  <-" if abs(delta) >= threshold else ""
            else:
                delta_str = "-"
                marker = ""

            print(f"{short:<40}  {fmt_time(bt):>10}  {fmt_time(ft):>10}  {delta_str:>8}{marker}")

        print()


def cmd_history(args, runs: list[dict]):
    """Show timing history for a specific group across commits."""
    if not args.group:
        print("--group is required for history mode.")
        return

    if args.lang:
        filtered = [r for r in runs if r.get("metadata", {}).get("language") == args.lang]
    else:
        filtered = runs

    if args.last:
        filtered = filtered[-args.last:]

    if not filtered:
        print("No matching runs found.")
        return

    # Group by language
    by_lang = {}
    for r in filtered:
        lang = r.get("metadata", {}).get("language", "?")
        by_lang.setdefault(lang, []).append(r)

    langs = sorted(by_lang.keys(), key=lambda l: (0 if l == "haskell" else 1, l))

    print(f"History: {args.group}")
    print()

    header = f"{'Commit':<10} {'Date':<8}"
    for lang in langs:
        header += f"  {lang.capitalize():>10} {'Delta':>7}"
    print(header)
    print("-" * len(header))

    prev_times = {lang: None for lang in langs}

    # Collect unique commits in order
    seen = set()
    commits = []
    for r in filtered:
        c = r.get("metadata", {}).get("commit", "?")
        if c not in seen:
            seen.add(c)
            commits.append(c)

    for commit in commits:
        # Find runs for this commit
        commit_runs = {
            r.get("metadata", {}).get("language", "?"): r
            for r in filtered
            if r.get("metadata", {}).get("commit") == commit
        }

        if not commit_runs:
            continue

        # Get date from first run
        any_run = next(iter(commit_runs.values()))
        date = any_run.get("metadata", {}).get("timestamp", "")[:10]
        msg = any_run.get("metadata", {}).get("commitMessage", "")

        line = f"{commit:<10} {date[5:]:<8}"
        show_msg = False

        for lang in langs:
            run = commit_runs.get(lang)
            if run:
                idx = index_groups(run.get("groups", []))
                # Find the group - try exact match and prefix match
                g = None
                for path, data in idx.items():
                    if path.endswith("/" + args.group) or path == args.group or args.group in path:
                        g = data
                        break
                if g:
                    t = g.get("totalTimeMs", 0)
                    if prev_times[lang] is not None and prev_times[lang] > 0:
                        delta = (t - prev_times[lang]) / prev_times[lang] * 100
                        delta_str = f"{delta:+.1f}%"
                        if abs(delta) >= args.threshold:
                            show_msg = True
                    else:
                        delta_str = ""
                    prev_times[lang] = t
                    line += f"  {fmt_time(t):>10} {delta_str:>7}"
                else:
                    line += f"  {'':>10} {'':>7}"
            else:
                line += f"  {'':>10} {'':>7}"

        if show_msg and msg:
            line += f'  "{msg}"'

        print(line)


def cmd_diff(args, runs: list[dict]):
    """Compare the two most recent runs for each language."""
    # Group runs by language
    by_lang: dict[str, list[dict]] = {}
    for r in runs:
        lang = r.get("metadata", {}).get("language", "")
        if lang:
            by_lang.setdefault(lang, []).append(r)

    if args.lang:
        by_lang = {k: v for k, v in by_lang.items() if k == args.lang}

    threshold = args.threshold

    for lang, lang_runs in sorted(by_lang.items()):
        if len(lang_runs) < 2:
            print(f"{lang.capitalize()}: only one run available, nothing to diff.")
            print()
            continue

        prev_run = lang_runs[-2]
        curr_run = lang_runs[-1]

        print(f"{lang.capitalize()} diff:")
        print(f"  previous: ", end="")
        print_run_header("", prev_run)
        print(f"  current:  ", end="")
        print_run_header("", curr_run)
        print()

        prev_idx = index_groups(prev_run.get("groups", []))
        curr_idx = index_groups(curr_run.get("groups", []))

        header = f"{'Group':<40}  {'previous':>10}  {'current':>10}  {'Delta':>8}"
        print(header)
        sep = "-" * len(header)
        print(sep)

        prev_total = 0.0
        curr_total = 0.0

        top_groups = curr_run.get("groups", [])
        if args.group:
            top_groups = [g for g in top_groups if args.group in g["path"]]

        def print_diff_group(tg, indent, remaining_depth):
            path = tg["path"]
            label = path.rsplit("/", 1)[-1] if "/" in path else path
            max_label = 38 - 2 * indent
            if len(label) > max_label:
                label = label[:max_label - 1] + "\u2026"
            short = ("  " * indent) + label
            ct = tg.get("totalTimeMs", 0)
            pg = prev_idx.get(path)
            pt = pg.get("totalTimeMs", 0) if pg is not None else 0
            if indent == 0:
                nonlocal prev_total, curr_total
                prev_total += pt
                curr_total += ct

            if pg is None:
                pt_str = f"{'--':>10}"
            else:
                pt_str = fmt_time(pt)
            if pt > 0:
                delta = (ct - pt) / pt * 100
                delta_str = f"{delta:+.1f}%"
                marker = "  <-" if abs(delta) >= threshold else ""
            else:
                delta_str = "-"
                marker = ""

            print(f"{short:<40}  {pt_str:>10}  {fmt_time(ct):>10}  {delta_str:>8}{marker}")

            if remaining_depth > 1 and "subgroups" in tg:
                for sg in tg.get("subgroups", []):
                    print_diff_group(sg, indent + 1, remaining_depth - 1)

        for tg in top_groups:
            print_diff_group(tg, 0, args.depth)

        print(sep)
        if prev_total > 0:
            total_delta = (curr_total - prev_total) / prev_total * 100
            total_delta_str = f"{total_delta:+.1f}%"
        else:
            total_delta_str = "-"
        print(f"{'TOTAL':<40}  {fmt_time(prev_total):>10}  {fmt_time(curr_total):>10}  {total_delta_str:>8}")
        print()


def main():
    parser = argparse.ArgumentParser(
        description="Hydra cross-implementation benchmark dashboard")
    parser.add_argument("command", nargs="?", default="latest",
                        choices=["latest", "compare", "diff", "history"],
                        help="Dashboard view (default: latest)")
    parser.add_argument("base_branch", nargs="?", help="Base branch (compare mode)")
    parser.add_argument("feature_branch", nargs="?", help="Feature branch (compare mode)")
    parser.add_argument("--dir", default="benchmark/runs",
                        help="Benchmark runs directory")
    parser.add_argument("--lang", help="Show only one language")
    parser.add_argument("--group", help="Filter to a specific group")
    parser.add_argument("--threshold", type=float, default=10.0,
                        help="Highlight threshold in percent (default: 10)")
    parser.add_argument("--last", type=int, help="Show only N most recent runs")
    parser.add_argument("--depth", type=int, default=2,
                        help="How many levels deep to display (default: 2)")
    parser.add_argument("--slowdown", type=float, default=10.0,
                        help="Highlight times exceeding N times Haskell's (default: 10)")

    args = parser.parse_args()

    runs = load_runs(args.dir)
    if not runs:
        print(f"No benchmark files found in {args.dir}/")
        print()
        print("To generate benchmark data, run tests with HYDRA_BENCHMARK_OUTPUT:")
        print()
        print("  # Haskell")
        print("  HYDRA_BENCHMARK_OUTPUT=benchmark/runs/haskell.json \\")
        print("    stack test --test-arguments='+RTS -N -RTS'")
        print()
        print("  # Python")
        print("  HYDRA_BENCHMARK_OUTPUT=benchmark/runs/python.json \\")
        print("    python3 -m pytest hydra-python/src/test/python/test_suite_runner.py")
        sys.exit(1)

    if args.command == "latest":
        cmd_latest(args, runs)
    elif args.command == "compare":
        if not args.base_branch or not args.feature_branch:
            print("Usage: benchmark-dashboard.py compare <base-branch> <feature-branch>")
            sys.exit(1)
        cmd_compare(args, runs)
    elif args.command == "diff":
        cmd_diff(args, runs)
    elif args.command == "history":
        cmd_history(args, runs)


if __name__ == "__main__":
    main()
