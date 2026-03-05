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
    --run RUN           Show a specific run directory in latest mode (e.g. 'baseline')
    --old RUN           Run directory for 'previous' in diff mode (default: run before --new)
    --new RUN           Run directory for 'current' in diff mode (default: latest)

Run directories can be renamed with a tag suffix for easier tracking, e.g.
run_2026-02-26_063120_353_baseline.  Chronological ordering uses only the
timestamp portion.  The --old and --new flags accept exact names, names
without the run_ prefix, or substring matches (e.g. just 'baseline').
"""

import argparse
import json
import math
import re
import statistics
import sys
from pathlib import Path


# Pattern for run directory names: run_YYYY-MM-DD_HHMMSS_mmm[_tag]
# The timestamp portion (YYYY-MM-DD_HHMMSS_mmm) is used for chronological sorting.
_RUN_DIR_RE = re.compile(r"^run_(\d{4}-\d{2}-\d{2}_\d{6}_\d{3})(_.+)?$")


def _run_sort_key(dirname):
    """Extract the timestamp portion of a run directory name for sorting.

    Supports optional human-readable tags after the timestamp, e.g.
    run_2026-02-26_063120_353_baseline.  Falls back to the full name
    so that unrecognised directories still sort deterministically.
    """
    m = _RUN_DIR_RE.match(dirname)
    if m:
        return m.group(1)
    return dirname


def load_runs(runs_dir):
    """Load all benchmark run directories.

    Each run directory (run_<timestamp>) may contain:
      - Single run: haskell.json, java.json, python.json
      - Multi-run:  haskell_1.json, haskell_2.json, ..., java_1.json, ...

    Returns a list of "aggregated run" dicts, one per (run_dir, language).
    Each aggregated run has the same schema as a single run, but with
    median timing and an additional 'stddev' field on each group.
    """
    runs = []
    runs_path = Path(runs_dir)
    if not runs_path.exists():
        return runs

    for d in sorted(runs_path.iterdir(), key=lambda p: _run_sort_key(p.name)):
        if not (d.is_dir() and d.name.startswith("run_")):
            continue

        # Group JSON files by language
        lang_files = {}
        for f in sorted(d.glob("*.json")):
            name = f.stem  # e.g. "haskell", "haskell_1", "java_3"
            parts = name.rsplit("_", 1)
            if len(parts) == 2 and parts[1].isdigit():
                lang = parts[0]
            else:
                lang = name
            lang_files.setdefault(lang, []).append(f)

        for lang, files in lang_files.items():
            raw_runs = []
            for f in files:
                try:
                    with open(f) as fh:
                        data = json.load(fh)
                        raw_runs.append(data)
                except (json.JSONDecodeError, KeyError):
                    pass

            if not raw_runs:
                continue

            aggregated = aggregate_runs(raw_runs, d.name)
            filter_excluded_groups(aggregated)
            runs.append(aggregated)

    return runs


# Groups that are excluded from all dashboard views and totals.
# _initialization measures test framework startup, not Hydra kernel performance.
_EXCLUDED_GROUPS = {"_initialization"}


def filter_excluded_groups(run):
    """Remove excluded groups (e.g. _initialization) from a run."""
    groups = run.get("groups", [])
    run["groups"] = [g for g in groups
                     if g["path"].rsplit("/", 1)[-1] not in _EXCLUDED_GROUPS]


def aggregate_runs(raw_runs, dir_name):
    """Aggregate multiple runs of the same language into one with median + stddev."""
    if len(raw_runs) == 1:
        run = raw_runs[0]
        run["_file"] = dir_name
        run["_repeat"] = 1
        add_stddev(run.get("groups", []), {})
        return run

    # Use first run as the structural template
    base = json.loads(json.dumps(raw_runs[0]))  # deep copy
    base["_file"] = dir_name
    base["_repeat"] = len(raw_runs)

    # Build timing samples: path -> [time1, time2, ...]
    all_samples = {}
    for run in raw_runs:
        collect_samples(run.get("groups", []), all_samples)

    # Also collect summary-level samples
    summary_times = [r.get("summary", {}).get("totalTimeMs", 0) for r in raw_runs]

    # Apply median timing and stddev to base structure
    apply_median(base.get("groups", []), all_samples)
    add_stddev(base.get("groups", []), all_samples)

    # Aggregate summary
    if summary_times:
        base["summary"]["totalTimeMs"] = round1(statistics.median(summary_times))
        base["summary"]["_stddev"] = round1(statistics.stdev(summary_times)) if len(summary_times) > 1 else 0.0

    # Aggregate pass/fail/skip counts (use max of passed, sum of failed — but really
    # these should be identical across runs; use first run's counts)

    return base


def collect_samples(groups, samples):
    """Recursively collect timing samples from a group tree."""
    for g in groups:
        path = g["path"]
        samples.setdefault(path, []).append(g.get("totalTimeMs", 0))
        if "subgroups" in g:
            collect_samples(g["subgroups"], samples)


def apply_median(groups, samples):
    """Replace totalTimeMs with the median across runs."""
    for g in groups:
        path = g["path"]
        times = samples.get(path, [])
        if times:
            g["totalTimeMs"] = round1(statistics.median(times))
        if "subgroups" in g:
            apply_median(g["subgroups"], samples)


def add_stddev(groups, samples):
    """Add _stddev field to each group."""
    for g in groups:
        path = g["path"]
        times = samples.get(path, [])
        if len(times) > 1:
            g["_stddev"] = round1(statistics.stdev(times))
        else:
            g["_stddev"] = 0.0
        if "subgroups" in g:
            add_stddev(g["subgroups"], samples)


def round1(x):
    """Round to 1 decimal place."""
    return round(x * 10) / 10


def index_groups(groups):
    """Build a flat index from group path to group data."""
    result = {}
    for g in groups:
        result[g["path"]] = g
        if "subgroups" in g:
            result.update(index_groups(g["subgroups"]))
    return result


def fmt_val(x):
    """Format a numeric value with 2 decimal places."""
    return f"{x:.2f}"


# ANSI color codes
RED = "\033[31m"
GREEN = "\033[32m"
GRAY = "\033[90m"
RESET = "\033[0m"


def fmt_counts(passed, failed, skipped):
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


def fmt_cell(passed, failed, skipped, time_ms, stddev, time_w, sd_w,
             ref_time_ms=None, slowdown_threshold=10.0):
    """Format a full cell with dynamic column widths."""
    time_str = f"{fmt_val(time_ms):>{time_w}}"
    if sd_w > 0:
        sd_raw = f"\u00b1{fmt_val(stddev)}"
        sd_str = f" {GRAY}{sd_raw:<{sd_w}}{RESET}"
    else:
        sd_str = ""
    if ref_time_ms is not None and ref_time_ms > 0:
        if time_ms / ref_time_ms >= slowdown_threshold:
            time_str = f"{RED}{time_str}{RESET}"
        elif time_ms < ref_time_ms:
            time_str = f"{GREEN}{time_str}{RESET}"
    return f" | {fmt_counts(passed, failed, skipped)} {time_str}{sd_str}"


def fmt_cell_missing(time_w, sd_w):
    """Format a cell for a missing group."""
    sd_pad = f" {'':>{sd_w}}" if sd_w > 0 else ""
    return f" | {'--':>12} {'--':>{time_w}}{sd_pad}"


def fmt_counts_header(lang, col_w):
    """Format the header for a language column with dynamic width."""
    return f" | {lang.capitalize():<{col_w}}"


def compute_col_widths(all_time_vals, all_sd_vals, show_stddev=True):
    """Compute the minimum column widths for time and stddev fields.
    Returns (time_width, sd_width, total_col_width).
    When show_stddev is False, sd_width is 0 and the column is omitted."""
    time_w = max((len(fmt_val(v)) for v in all_time_vals), default=4)
    if show_stddev:
        sd_w = max((len(f"\u00b1{fmt_val(v)}") for v in all_sd_vals), default=5)
        col_w = 12 + 1 + time_w + 1 + sd_w
    else:
        sd_w = 0
        col_w = 12 + 1 + time_w
    return time_w, sd_w, col_w


def latest_run_per_language(runs):
    """Find the most recent run for each language."""
    latest = {}
    for run in runs:
        lang = run.get("metadata", {}).get("language", "")
        if lang:
            latest[lang] = run  # runs are sorted by filename, so last wins
    return latest


def latest_run_per_language_branch(runs, branch):
    """Find the most recent run for each language on a specific branch."""
    latest = {}
    for run in runs:
        meta = run.get("metadata", {})
        if meta.get("branch", "") == branch:
            lang = meta.get("language", "")
            if lang:
                latest[lang] = run
    return latest


def print_run_header(lang, run):
    """Print metadata for a run."""
    meta = run.get("metadata", {})
    commit = meta.get("commit", "?")
    branch = meta.get("branch", "?")
    msg = meta.get("commitMessage", "")
    ts = meta.get("timestamp", "?")
    repeat = run.get("_repeat", 1)
    repeat_str = f" x{repeat}" if repeat > 1 else ""
    print(f"  {lang.capitalize():<10} {ts[:16]} ({commit}, {branch}) \"{msg}\"{repeat_str}")


def cmd_latest(args, runs):
    """Show the most recent run for each language, side by side."""
    if args.run:
        # Filter to runs matching the specified directory
        matching = [r for r in runs if _match_run_dir(r.get("_file", ""), args.run)]
        if not matching:
            # Distinguish between missing directory and empty/incomplete run
            runs_path = Path(args.dir)
            matching_dirs = [d for d in runs_path.iterdir()
                             if d.is_dir() and _match_run_dir(d.name, args.run)] if runs_path.exists() else []
            if matching_dirs:
                dir_name = matching_dirs[0].name
                json_count = len(list(matching_dirs[0].glob("*.json")))
                if json_count == 0:
                    print(f"Run '{dir_name}' exists but contains no benchmark data (incomplete run?).")
                else:
                    print(f"Run '{dir_name}' exists with {json_count} JSON file(s) but none could be loaded.")
            else:
                print(f"No run directory found matching '{args.run}'.")
            return
        latest = latest_run_per_language(matching)
    else:
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

    # Pass 1: collect all time/stddev values per language to compute column widths
    lang_vals = {lang: {"times": [], "sds": []} for lang in langs}
    totals = {lang: {"passed": 0, "failed": 0, "skipped": 0, "time": 0.0, "time_sq": []} for lang in langs}

    def collect_values(tg, indent, remaining_depth):
        path = tg["path"]
        for lang in langs:
            g = indexes[lang].get(path)
            if g is not None:
                t = g.get("totalTimeMs", 0)
                sd = g.get("_stddev", 0.0)
                lang_vals[lang]["times"].append(t)
                lang_vals[lang]["sds"].append(sd)
                if indent == 0:
                    totals[lang]["passed"] += g.get("passed", 0)
                    totals[lang]["failed"] += g.get("failed", 0)
                    totals[lang]["skipped"] += g.get("skipped", 0)
                    totals[lang]["time"] += t
                    totals[lang]["time_sq"].append((t, sd))
        if remaining_depth > 1 and "subgroups" in tg:
            for sg in tg.get("subgroups", []):
                collect_values(sg, indent + 1, remaining_depth - 1)

    for tg in top_groups:
        collect_values(tg, 0, args.depth)

    # Add total row values
    for lang in langs:
        t = totals[lang]
        total_sd = math.sqrt(sum(sd * sd for _, sd in t["time_sq"])) if t["time_sq"] else 0.0
        t["total_sd"] = round1(total_sd)
        lang_vals[lang]["times"].append(t["time"])
        lang_vals[lang]["sds"].append(total_sd)

    # Compute per-language column widths (hide stddev for single-run languages)
    col_widths = {}
    for lang in langs:
        show_sd = latest[lang].get("_repeat", 1) > 1
        col_widths[lang] = compute_col_widths(lang_vals[lang]["times"], lang_vals[lang]["sds"], show_sd)

    # Pass 2: render
    lang_cols = "".join(fmt_counts_header(lang, col_widths[lang][2]) for lang in langs)
    header = f"{'Group':<40}{lang_cols}"
    print(header)
    sep = "-" * len(header)
    print(sep)

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
            tw, sw, _ = col_widths[lang]
            g = indexes[lang].get(path)
            if g is not None:
                p = g.get("passed", 0)
                f = g.get("failed", 0)
                s = g.get("skipped", 0)
                t = g.get("totalTimeMs", 0)
                sd = g.get("_stddev", 0.0)
                rt = ref_time if lang != "haskell" else None
                cols += fmt_cell(p, f, s, t, sd, tw, sw, rt, args.slowdown)
            else:
                cols += fmt_cell_missing(tw, sw)
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
        tw, sw, _ = col_widths[lang]
        t = totals[lang]
        rt = haskell_total_time if lang != "haskell" else None
        total_cols += fmt_cell(t['passed'], t['failed'], t['skipped'], t['time'],
                               t['total_sd'], tw, sw, rt, args.slowdown)
    print(f"{'TOTAL':<40}{total_cols}")


def cmd_compare(args, runs):
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

            print(f"{short:<40}  {fmt_val(bt):>10}  {fmt_val(ft):>10}  {delta_str:>8}{marker}")

        print()


def cmd_history(args, runs):
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
                    prev_t = prev_times[lang]
                    if prev_t is not None and prev_t > 0:
                        delta = (t - prev_t) / prev_t * 100
                        delta_str = f"{delta:+.1f}%"
                        if abs(delta) >= args.threshold:
                            show_msg = True
                    else:
                        delta_str = ""
                    prev_times[lang] = t
                    line += f"  {fmt_val(t):>10} {delta_str:>7}"
                else:
                    line += f"  {'':>10} {'':>7}"
            else:
                line += f"  {'':>10} {'':>7}"

        if show_msg and msg:
            line += f'  "{msg}"'

        print(line)


def _match_run_dir(file_field, query):
    """Check if a run's _file field matches a query string.

    Accepts exact match, with/without 'run_' prefix, or a substring match
    (e.g. a tag like 'baseline' matches 'run_2026-02-26_063120_353_baseline').
    """
    if not file_field or not query:
        return False
    normalized = query if query.startswith("run_") else "run_" + query
    if file_field == normalized or file_field == query:
        return True
    # Substring match (e.g. tag name or partial timestamp)
    return query in file_field


def find_run_by_dir(runs, dir_name):
    """Find runs matching a directory name (with or without 'run_' prefix)."""
    return [r for r in runs if _match_run_dir(r.get("_file", ""), dir_name)]


def cmd_diff(args, runs):
    """Compare two runs for each language.

    By default, compares the two most recent runs. Use --old and --new to
    select specific run directories by name.
    """
    # Group runs by language
    by_lang = {}
    for r in runs:
        lang = r.get("metadata", {}).get("language", "")
        if lang:
            by_lang.setdefault(lang, []).append(r)

    if args.lang:
        by_lang = {k: v for k, v in by_lang.items() if k == args.lang}

    threshold = args.threshold

    for lang, lang_runs in sorted(by_lang.items()):
        # Select previous and current runs
        # Resolve --new first (default: latest)
        if args.new:
            new_matches = [r for r in lang_runs if _match_run_dir(r.get("_file", ""), args.new)]
            if not new_matches:
                print(f"{lang.capitalize()}: no run found matching --new '{args.new}'")
                print()
                continue
            curr_run = new_matches[-1]
        else:
            curr_run = lang_runs[-1]

        # Resolve --old (default: the run immediately preceding --new)
        if args.old:
            old_matches = [r for r in lang_runs if _match_run_dir(r.get("_file", ""), args.old)]
            if not old_matches:
                print(f"{lang.capitalize()}: no run found matching --old '{args.old}'")
                print()
                continue
            prev_run = old_matches[-1]
        else:
            # Find the run immediately before curr_run in chronological order
            curr_pos = next((i for i, r in enumerate(lang_runs) if r is curr_run), None)
            if curr_pos is None or curr_pos < 1:
                print(f"{lang.capitalize()}: no earlier run available to diff against.")
                print()
                continue
            prev_run = lang_runs[curr_pos - 1]

        print(f"{lang.capitalize()} diff:")
        print(f"  previous: ", end="")
        print_run_header("", prev_run)
        print(f"  current:  ", end="")
        print_run_header("", curr_run)
        print()

        prev_idx = index_groups(prev_run.get("groups", []))

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
                pt_str = fmt_val(pt)
            if pt > 0:
                delta = (ct - pt) / pt * 100
                delta_str = f"{delta:+.1f}%"
                marker = "  <-" if abs(delta) >= threshold else ""
            else:
                delta_str = "-"
                marker = ""

            print(f"{short:<40}  {pt_str:>10}  {fmt_val(ct):>10}  {delta_str:>8}{marker}")

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
        print(f"{'TOTAL':<40}  {fmt_val(prev_total):>10}  {fmt_val(curr_total):>10}  {total_delta_str:>8}")
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
    parser.add_argument("--run",
                        help="Show a specific run directory in latest mode (e.g. run_2026-02-26_063120_353 or just 'baseline')")
    parser.add_argument("--old",
                        help="Run directory name for 'previous' in diff mode (e.g. run_2026-02-26_063120_353)")
    parser.add_argument("--new",
                        help="Run directory name for 'current' in diff mode (default: latest)")

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
