"""Shared helpers for Hydra dashboard scripts (benchmark-dashboard.py,
bootstrapping-dashboard.py).

Run directories are named run_YYYY-MM-DD_HHMMSS_mmm[_tag]. The timestamp
portion is used for chronological sorting; tags are decorative.
"""

import re


RUN_DIR_RE = re.compile(r"^run_(\d{4}-\d{2}-\d{2}_\d{6}_\d{3})(_.+)?$")
ANSI_RE = re.compile(r"\033\[[0-9;]*m")


def run_sort_key(dirname):
    """Sort key for a run directory name: timestamp portion, or the full
    name as a fallback so unrecognised directories still sort deterministically."""
    m = RUN_DIR_RE.match(dirname)
    return m.group(1) if m else dirname


def visible_len(s):
    """Length of a string ignoring ANSI escape sequences."""
    return len(ANSI_RE.sub("", s))


# ANSI color codes. Both dashboards use these; the benchmark dashboard uses
# the 30-series (RED=31 etc.) and bootstrap uses the bright 90-series
# (RED=91 etc.). We export both so each dashboard keeps its current look.
RED = "\033[31m"
GREEN = "\033[32m"
GRAY = "\033[90m"
RESET = "\033[0m"

BRIGHT_RED = "\033[91m"
BRIGHT_GREEN = "\033[92m"
BOLD = "\033[1m"
