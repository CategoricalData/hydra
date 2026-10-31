#!/usr/bin/env python3
"""
Persist the Hydra N×N cross-generation bootstrap matrix as a Markdown user doc.

Issue #769: `bin/run-bootstrapping-demo.sh` already computes everything a matrix cell needs
(codegen time, test-gen time, test-run time, pass/fail, commit, date) and writes it to a
per-run directory under `bootstrap/runs/` — which is gitignored, so the data is discarded after
each invocation. This tool captures that output permanently and incrementally: it reads a run
directory, merges the cells that run actually exercised into the tracked matrix doc, and leaves
every other cell at its last-known state with an honest, visible "last verified" date.

No single event has to run the whole N×N matrix; the page's staleness is itself informative.

Usage:
    bin/bootstrap-matrix.py update --run <run_dir> [--doc <path>]
        Merge the cells exercised in <run_dir> into the matrix doc (creating it if absent).
    bin/bootstrap-matrix.py render [--doc <path>]
        Re-render the human-readable table from the embedded data block (no run needed).
    bin/bootstrap-matrix.py annotate --cell <host>:<target> (--issue <N> | --clear) [--doc <path>]
        Link a red or never-verified cell to the GitHub issue tracking it (or remove the link).

Options:
    --run DIR    A bootstrap run directory (bootstrap/runs/run_*). Contains metadata.json,
                 <host>-to-<target>.json, and <host>-to-<target>.benchmark.json files.
    --doc PATH   The tracked matrix doc (default: docs/bootstrap-matrix.md).

Storage model: the doc carries a machine-readable JSON block inside an HTML comment
(<!-- BOOTSTRAP-MATRIX-DATA ... -->) that is the source of truth for every cell, plus a
human-readable Markdown table rendered from it. Partial runs overwrite only the cells they
touch; the rest are preserved verbatim, so the doc is a durable, incrementally-updated record.
"""

import argparse
import glob
import json
import statistics
import sys
from pathlib import Path

# Canonical host/target ordering (matches the issue's ~9-language listing and the README
# implementations table). Kept explicit so the rendered table order is stable regardless of
# which cells a given run exercised.
LANGUAGES = [
    "haskell", "java", "python", "scala", "typescript",
    "clojure", "common-lisp", "scheme", "emacs-lisp",
]

DISPLAY_NAME = {
    "haskell": "Haskell", "java": "Java", "python": "Python", "scala": "Scala",
    "typescript": "TypeScript", "clojure": "Clojure", "common-lisp": "Common Lisp",
    "scheme": "Scheme", "emacs-lisp": "Emacs Lisp",
}

DATA_BEGIN = "<!-- BOOTSTRAP-MATRIX-DATA"
DATA_END = "BOOTSTRAP-MATRIX-DATA-END -->"

DEFAULT_DOC = "docs/bootstrap-matrix.md"


def repo_root():
    """Resolve the repo root from this script's location (bin/ is at the root)."""
    return Path(__file__).resolve().parent.parent


# --------------------------------------------------------------------------- run parsing


def load_benchmark_summary(run_dir, path_key):
    """Return the test-run summary dict (totalTimeMs, totalPassed, totalSkipped) for a cell,
    merging single / numbered benchmark files. Returns {} if none found."""
    run_dir = Path(run_dir)

    files = []
    single = run_dir / f"{path_key}.benchmark.json"
    if single.exists():
        files = [single]
    else:
        files = [Path(p) for p in sorted(glob.glob(str(run_dir / f"{path_key}_*.benchmark.json")))]

    summaries = []
    for f in files:
        try:
            summaries.append((json.loads(f.read_text()).get("summary") or {}))
        except (json.JSONDecodeError, OSError):
            continue

    if not summaries:
        # Fall back to a testResults block embedded in the per-cell JSON.
        pj = run_dir / f"{path_key}.json"
        if pj.exists():
            try:
                tr = (json.loads(pj.read_text()).get("testResults") or {})
                if tr.get("summary"):
                    summaries = [tr["summary"]]
            except (json.JSONDecodeError, OSError):
                pass

    if not summaries:
        return {}

    times = [s["totalTimeMs"] for s in summaries if s.get("totalTimeMs") is not None]
    out = {}
    if times:
        out["totalTimeMs"] = statistics.median(times)
    # Pass/skip: take the first summary's counts (they should be identical across repeats).
    for k in ("totalPassed", "totalSkipped"):
        for s in summaries:
            if s.get(k) is not None:
                out[k] = s[k]
                break
    return out


def cell_from_run(run_dir, meta, host, target):
    """Build a cell record from a run's per-path JSON + benchmark, or None if not exercised."""
    run_dir = Path(run_dir)
    key = f"{host}-to-{target}"
    pj = run_dir / f"{key}.json"
    if not pj.exists():
        return None
    try:
        data = json.loads(pj.read_text())
    except (json.JSONDecodeError, OSError):
        return None

    status = data.get("status", "?")
    gen = data.get("generation") or {}
    main_gen = gen.get("main") or {}
    test_gen = gen.get("test") or {}
    bench = load_benchmark_summary(run_dir, key)

    passed = bench.get("totalPassed")
    # A cell is "green" iff codegen succeeded AND at least one test passed with no failures.
    # The demo marks status=fail on any codegen/test error, so status is the primary signal;
    # a green requires status ok AND a positive pass count (a 0-pass "success" is not green).
    green = status not in ("fail", "prep") and passed is not None and passed > 0

    cell = {
        "status": "green" if green else "red",
        "rawStatus": status,
        "mainFiles": main_gen.get("fileCount"),
        "mainGenSeconds": main_gen.get("timeSeconds"),
        "testFiles": test_gen.get("fileCount"),
        "testGenSeconds": test_gen.get("timeSeconds"),
        "testRunMs": bench.get("totalTimeMs"),
        "passed": passed,
        "skipped": bench.get("totalSkipped"),
        "verifiedDate": (meta.get("startTime") or "")[:10],
        "verifiedCommit": meta.get("commit", "unknown"),
    }
    return cell


def cells_from_run(run_dir):
    """Return {(host,target): cell} for every cell the run exercised, plus the run metadata."""
    run_dir = Path(run_dir)
    meta_path = run_dir / "metadata.json"
    meta = json.loads(meta_path.read_text()) if meta_path.exists() else {}

    cells = {}
    for host in LANGUAGES:
        for target in LANGUAGES:
            cell = cell_from_run(run_dir, meta, host, target)
            if cell is not None:
                cells[(host, target)] = cell
    return cells, meta


# --------------------------------------------------------------------------- doc I/O


def read_doc_data(doc_path):
    """Parse the embedded JSON data block from the doc. Returns {} if the doc/block is absent."""
    p = Path(doc_path)
    if not p.exists():
        return {}
    text = p.read_text()
    start = text.find(DATA_BEGIN)
    end = text.find(DATA_END)
    if start == -1 or end == -1:
        return {}
    block = text[start + len(DATA_BEGIN):end].strip()
    try:
        return json.loads(block)
    except json.JSONDecodeError:
        return {}


def merge_cells(existing, new_cells):
    """Overwrite exercised cells; preserve the rest. `existing` is the parsed data dict with a
    'cells' map keyed 'host-to-target'. Returns the merged data dict."""
    data = dict(existing) if existing else {}
    cells = dict(data.get("cells") or {})
    for (host, target), cell in new_cells.items():
        cells[f"{host}-to-{target}"] = cell
    data["cells"] = cells
    return data


# --------------------------------------------------------------------------- rendering


def fmt_secs(v):
    if v is None:
        return "—"
    try:
        v = float(v)
    except (TypeError, ValueError):
        return "—"
    if v < 60:
        return f"{v:.1f}s"
    m = int(v // 60)
    s = v - m * 60
    return f"{m}m{s:.0f}s"


def fmt_ms(v):
    if v is None:
        return "—"
    return fmt_secs(float(v) / 1000.0)


def cell_glyph(cell):
    if cell is None:
        return "⚪"  # never verified
    return "🟢" if cell.get("status") == "green" else "🔴"


def _fc(v):
    """File count for display. The underlying bootstrap JSON may not capture a count (value is
    None, not merely absent — so a plain .get(k, '?') default won't fire); render those as '?'."""
    return "?" if v is None else v


ISSUE_URL = "https://github.com/CategoricalData/hydra/issues/{}"


def cell_issue(data, host, target, cell):
    """The tracking issue for a cell, shown only while the cell is red or never verified: once a cell turns
    green the link is stale, so it is hidden (the annotation itself stays in the data block)."""
    issue = (data.get("issues") or {}).get(f"{host}-to-{target}")
    if issue is None or (cell is not None and cell.get("status") == "green"):
        return None
    return issue


def render_cell_detail(cell):
    """One compact multi-line cell body for the details table."""
    if cell is None:
        return "never verified"
    passed = cell.get("passed")
    pass_str = f"{passed} pass" if passed is not None else "—"
    skipped = cell.get("skipped")
    if skipped:
        pass_str += f", {skipped} skip"
    parts = [
        f"gen {_fc(cell.get('mainFiles'))}f/{fmt_secs(cell.get('mainGenSeconds'))}",
        f"test-gen {_fc(cell.get('testFiles'))}f/{fmt_secs(cell.get('testGenSeconds'))}",
        f"run {fmt_ms(cell.get('testRunMs'))}",
        pass_str,
    ]
    return "<br>".join(parts)


def _wrap(text, width=110):
    """Wrap a paragraph to <width chars per line without splitting Markdown link/code spans.
    Keeps the documentation-style-guide's <120-char, one-sentence-ish-per-line convention while
    leaving inline `code`, [links](...), and em-dashes intact."""
    words = text.split(" ")
    out, cur = [], ""
    for w in words:
        if cur and len(cur) + 1 + len(w) > width:
            out.append(cur)
            cur = w
        else:
            cur = f"{cur} {w}" if cur else w
    if cur:
        out.append(cur)
    return out


def render_doc(data):
    """Render the full Markdown doc from the data dict (glyph grid + details + data block)."""
    cells = data.get("cells") or {}

    def get(host, target):
        return cells.get(f"{host}-to-{target}")

    lines = []
    lines.append("# Hydra cross-generation bootstrap matrix")
    lines.append("")
    lines.extend(_wrap(
        "This page records, for every ordered pair of Hydra host languages, whether the **host** "
        "(left) can generate the hydra-core kernel *into* the **target** (top) and pass the full "
        "hydra-core test suite there."))
    lines.append("")
    lines.extend(_wrap(
        "It is the durable, versioned answer to "
        "[#409](https://github.com/CategoricalData/hydra/issues/409) "
        "(\"is Hydra truly mutually self-hosting across every host pair?\"): rather than a one-off "
        "full-matrix run, each cell carries the date and git commit it was **last actually verified "
        "against**. Cells refresh opportunistically — whenever a release's bootstrap-coverage pass "
        "happens to exercise a cell (the triad, plus whatever else that release checks) — so a stale "
        "date is honest information, not a gap to hide. The data is produced by "
        "`bin/run-bootstrapping-demo.sh` and merged in by `bin/bootstrap-matrix.py`; see "
        "[docs/demos.md](demos.md) and "
        "[docs/release-workflow.md § Bootstrap coverage](release-workflow.md#bootstrap-coverage)."))
    lines.append("")
    lines.append("**Legend:** 🟢 generates + full test suite passes · 🔴 anything less · "
                 "⚪ never verified (no data yet).")
    lines.append("")

    # --- Glyph grid (host × target) ---
    lines.append("## Status grid")
    lines.append("")
    header = "| Host \\ Target | " + " | ".join(DISPLAY_NAME[t] for t in LANGUAGES) + " |"
    sep = "|" + "---|" * (len(LANGUAGES) + 1)
    lines.append(header)
    lines.append(sep)
    for host in LANGUAGES:
        row = [f"**{DISPLAY_NAME[host]}**"]
        for target in LANGUAGES:
            row.append(cell_glyph(get(host, target)))
        lines.append("| " + " | ".join(row) + " |")
    lines.append("")

    # --- Per-cell details (only cells that have data) ---
    lines.append("## Cell details")
    lines.append("")
    lines.extend(_wrap(
        "Each entry: codegen (files/time) · test-gen (files/time) · test-run time · pass count · "
        "last verified date @ commit. Red or unverified cells link the issue tracking them. Cells with no "
        "data and no tracking issue are omitted."))
    lines.append("")
    lines.append("| Cell | Status | Detail | Last verified |")
    lines.append("|---|---|---|---|")
    any_cell = False
    for host in LANGUAGES:
        for target in LANGUAGES:
            cell = get(host, target)
            issue = cell_issue(data, host, target, cell)
            if cell is None and issue is None:
                continue
            any_cell = True
            verified = ("—" if cell is None
                        else f"{cell.get('verifiedDate', '?')} @ `{cell.get('verifiedCommit', '?')}`")
            status = cell_glyph(cell)
            if issue is not None:
                status += f" [#{issue}]({ISSUE_URL.format(issue)})"
            lines.append(
                f"| {DISPLAY_NAME[host]} → {DISPLAY_NAME[target]} "
                f"| {status} "
                f"| {render_cell_detail(cell)} "
                f"| {verified} |")
    if not any_cell:
        lines.append("| _(no cells verified yet)_ | | | |")
    lines.append("")

    lines.append("---")
    lines.append("")
    lines.extend(_wrap(
        "_This page is generated. To refresh a cell, run a bootstrap cell exercising it "
        "(`bin/run-bootstrapping-demo.sh --hosts H --targets T`) and then "
        "`bin/bootstrap-matrix.py update --run <run_dir>`. Do not hand-edit the data block below._"))
    lines.append("")

    # --- Machine-readable data block (source of truth) ---
    lines.append(DATA_BEGIN)
    lines.append(json.dumps(data, indent=2, sort_keys=True))
    lines.append(DATA_END)
    lines.append("")

    return "\n".join(lines)


# --------------------------------------------------------------------------- commands


def cmd_update(args):
    doc_path = Path(args.doc)
    if not doc_path.is_absolute():
        doc_path = repo_root() / doc_path

    new_cells, meta = cells_from_run(args.run)
    if not new_cells:
        print(f"No exercised cells found in {args.run} (no <host>-to-<target>.json files).",
              file=sys.stderr)
        return 1

    existing = read_doc_data(doc_path)
    merged = merge_cells(existing, new_cells)
    doc_path.parent.mkdir(parents=True, exist_ok=True)
    doc_path.write_text(render_doc(merged))

    print(f"Updated {len(new_cells)} cell(s) in {doc_path}:")
    for (host, target), cell in sorted(new_cells.items()):
        print(f"  {host} → {target}: {cell['status']} "
              f"({cell.get('passed', '—')} pass) @ {cell.get('verifiedCommit')}")
    return 0


def cmd_render(args):
    doc_path = Path(args.doc)
    if not doc_path.is_absolute():
        doc_path = repo_root() / doc_path
    data = read_doc_data(doc_path)
    doc_path.parent.mkdir(parents=True, exist_ok=True)
    doc_path.write_text(render_doc(data))
    print(f"Re-rendered {doc_path} from embedded data ({len(data.get('cells') or {})} cell(s)).")
    return 0


def cmd_annotate(args):
    doc_path = Path(args.doc)
    if not doc_path.is_absolute():
        doc_path = repo_root() / doc_path
    host, sep, target = args.cell.partition(":")
    if not sep or host not in LANGUAGES or target not in LANGUAGES:
        print(f"--cell must be <host>:<target> with each one of: {', '.join(LANGUAGES)}", file=sys.stderr)
        return 1
    data = read_doc_data(doc_path)
    issues = dict(data.get("issues") or {})
    key = f"{host}-to-{target}"
    if args.clear:
        issues.pop(key, None)
    else:
        issues[key] = args.issue
    data["issues"] = issues
    doc_path.write_text(render_doc(data))
    print(f"{host} → {target}: " + ("issue link cleared" if args.clear else f"linked to #{args.issue}"))
    return 0


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = parser.add_subparsers(dest="command", required=True)

    p_update = sub.add_parser("update", help="Merge a run's exercised cells into the matrix doc.")
    p_update.add_argument("--run", required=True, help="Bootstrap run directory.")
    p_update.add_argument("--doc", default=DEFAULT_DOC, help=f"Matrix doc (default: {DEFAULT_DOC}).")
    p_update.set_defaults(func=cmd_update)

    p_render = sub.add_parser("render", help="Re-render the table from the embedded data block.")
    p_render.add_argument("--doc", default=DEFAULT_DOC, help=f"Matrix doc (default: {DEFAULT_DOC}).")
    p_render.set_defaults(func=cmd_render)

    p_annotate = sub.add_parser("annotate", help="Link a red/unverified cell to its tracking issue.")
    p_annotate.add_argument("--cell", required=True, help="<host>:<target>, e.g. clojure:java.")
    group = p_annotate.add_mutually_exclusive_group(required=True)
    group.add_argument("--issue", type=int, help="GitHub issue number tracking the cell.")
    group.add_argument("--clear", action="store_true", help="Remove the cell's issue link.")
    p_annotate.add_argument("--doc", default=DEFAULT_DOC, help=f"Matrix doc (default: {DEFAULT_DOC}).")
    p_annotate.set_defaults(func=cmd_annotate)

    args = parser.parse_args()
    sys.exit(args.func(args))


if __name__ == "__main__":
    main()
