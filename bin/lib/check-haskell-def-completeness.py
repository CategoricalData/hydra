#!/usr/bin/env python3
"""Registration-completeness check for Haskell kernel-authoring source (#554).

Haskell has no runtime reflection, so this is a source-text scanner rather than
the reflection-based check used for Java (Defs.checkComplete) and Python
(check_complete): it cannot be a faithful port of either. It looks, per file,
for the same authoring mistake those two catch: a definition is written as a
top-level binding, but is never added to that module's own `definitions = [...]`
assembly list, so it silently never reaches the generated Module value.

Kernel-authoring modules under packages/hydra-kernel/.../Hydra/Sources/Kernel/
follow one of three dialects (Terms/, Types/, Lib/), each pairing a distinct
top-level type signature with how the assembly list wraps it:

  Terms/*.hs  ::  TypedTermDefinition _     wrapped:  toDefinition <name>
  Types/*.hs  ::  TypeDefinition            bare:     <name>
  Lib/*.hs    ::  PrimitiveDefinition       bare:     <name>

This check only considers bindings with an EXPLICIT top-level type signature
matching one of the three definition types above. It does not attempt to
resolve untyped, factory-produced bindings (e.g. Constants.hs's
`keyClasses = defineAnnotationKey "classes" $ ...`, which has no local type
signature) — that would require type inference a text scanner can't do. This
is a deliberate under-approximation: false negatives (a real orphan hidden
behind an untyped factory binding) are possible; false positives are not, by
construction, since only explicitly-typed bindings are checked at all.

Some definitions are deliberately excluded from a module's assembly list (e.g.
Types/Util.hs's `either_`/`pair`, which shadow built-in type constructors and
would conflict with target-language natives if emitted). Mark these with an
`-- unregistered: <reason>` comment on the line directly above the binding's
type signature; the scanner treats that as an intentional exclusion, not an
orphan.

This check is also, by construction, per-module only — same as Java's/Python's
scope. It does not check whether a file's own `module_` is itself referenced
from Terms/All.hs, Types/All.hs, or Manifest.hs (a coarser, module-level
instance of the same defect class — see docs/recipes/extending-hydra-core.md).

Usage: check-haskell-def-completeness.py <hydra-root>

Exit 0 if every scanned file's typed definitions are all present in that
file's own assembly list. Exit 1 and print every orphan (file, binding name)
otherwise.
"""
import re
import sys
from pathlib import Path

KERNEL_SOURCES_REL = "packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel"

# Definition types as they appear in a `:: Foo` signature. Terms/*.hs elements are
# referenced wrapped (`toDefinition name`); Types/*.hs and Lib/*.hs elements are
# referenced bare (`name`) — find_assembly_references strips the wrapper
# unconditionally, so both conventions are handled without distinguishing them here.
DIALECTS = ("TypedTermDefinition", "TypeDefinition", "PrimitiveDefinition")

# Matches a top-level type signature: `name :: Sometype ...` at column 0.
# Captures the bound name and the right-hand side of `::` (checked for a dialect match).
SIGNATURE_RE = re.compile(r"^([a-zA-Z_][a-zA-Z0-9_']*)\s*::\s*(.+)$", re.MULTILINE)

# An `-- unregistered: <reason>` comment on its own line, directly above a type
# signature, marks that binding as a deliberate exclusion from its module's
# assembly list (see module docstring).
UNREGISTERED_MARKER_RE = re.compile(r"^\s*--\s*unregistered:")

# Matches the opening of an assembly list: either the local `definitions = [`
# binding (the common case) or an inline `moduleDefinitions = [` (single-def
# modules like Languages.hs, which skip the `where definitions = ...` indirection).
ASSEMBLY_LIST_OPEN_RE = re.compile(r"(?:moduleDefinitions|definitions)\s*=\s*\[")

IDENTIFIER_RE = re.compile(r"[a-zA-Z_][a-zA-Z0-9_']*")


def find_typed_bindings(original_text: str) -> dict:
    """Return {name: dialect_type} for every top-level binding whose signature's
    right-hand side starts with one of the three kernel definition types, excluding
    any binding marked `-- unregistered: <reason>` on the preceding line.

    Operates on the ORIGINAL (not comment-stripped) text, since the marker itself
    is a comment and must still be visible to detect."""
    lines = original_text.split("\n")
    result = {}
    for m in SIGNATURE_RE.finditer(original_text):
        name, rhs = m.group(1), m.group(2).strip()
        for dialect_type in DIALECTS:
            if rhs == dialect_type or rhs.startswith(dialect_type + " "):
                line_no = original_text.count("\n", 0, m.start())
                prev_line = lines[line_no - 1] if line_no > 0 else ""
                if not UNREGISTERED_MARKER_RE.match(prev_line):
                    result[name] = dialect_type
                break
    return result


def _extract_bracketed_body(text: str, open_bracket_index: int) -> str:
    """Given the index of a `[` character, return the text strictly between it
    and its matching `]`, respecting nested `[`/`]` (kernel-source elements can
    contain nested lists/parens, e.g. `toDefinition (foo :: TypedTermDefinition
    ([(Int, [b])] -> ...))`) — a non-greedy regex up to the first `]` is wrong
    whenever an element's own type annotation contains a list type."""
    depth = 0
    for i in range(open_bracket_index, len(text)):
        if text[i] == "[":
            depth += 1
        elif text[i] == "]":
            depth -= 1
            if depth == 0:
                return text[open_bracket_index + 1:i]
    return text[open_bracket_index + 1:]  # unterminated; best effort


def _top_level_comma_split(body: str) -> list:
    """Split a bracketed list body on commas, but only at paren/bracket depth 0,
    so an element's own `(foo :: TypedTermDefinition (A -> B))` isn't split apart."""
    parts = []
    depth = 0
    current = []
    for ch in body:
        if ch in "([":
            depth += 1
        elif ch in ")]":
            depth -= 1
        if ch == "," and depth == 0:
            parts.append("".join(current))
            current = []
        else:
            current.append(ch)
    parts.append("".join(current))
    return parts


def find_assembly_references(text: str) -> set:
    """Return the set of bare names referenced in every assembly list in this file
    (`definitions = [...]` and/or an inline `moduleDefinitions = [...]`), stripping
    a `toDefinition` wrapper and any `:: Type` annotation. Kernel-source elements are
    one of: `name`, `toDefinition name`, or `toDefinition (name :: SomeType)` — in
    every case the referenced binding is the identifier immediately after an
    optional `toDefinition` and optional opening paren."""
    names = set()
    for open_match in ASSEMBLY_LIST_OPEN_RE.finditer(text):
        open_bracket_index = open_match.end() - 1  # index of the `[` itself
        body = _extract_bracketed_body(text, open_bracket_index)
        for raw in _top_level_comma_split(body):
            token = raw.strip()
            if not token:
                continue
            token = re.sub(r"^toDefinition\s+", "", token)
            token = token.lstrip("(").strip()
            id_match = IDENTIFIER_RE.match(token)
            if id_match:
                names.add(id_match.group(0))
    return names


def strip_comments(text: str) -> str:
    """Strip Haskell line comments (`-- ...`) conservatively. Kernel sources do not
    use `--` inside string literals in definitions lists or signatures, so a per-line
    strip is safe here (this is not a general Haskell comment stripper)."""
    lines = []
    for line in text.split("\n"):
        idx = line.find("--")
        lines.append(line[:idx] if idx != -1 else line)
    return "\n".join(lines)


def check_file(path: Path) -> list:
    """Return a list of orphaned binding names for this file (empty if clean)."""
    original_text = path.read_text()
    typed = find_typed_bindings(original_text)
    if not typed:
        return []
    referenced = find_assembly_references(strip_comments(original_text))
    return sorted(name for name in typed if name not in referenced)


def main() -> int:
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <hydra-root>", file=sys.stderr)
        return 2
    hydra_root = Path(sys.argv[1]).resolve()
    kernel_sources = hydra_root / KERNEL_SOURCES_REL
    if not kernel_sources.is_dir():
        print(f"  check-haskell-def-completeness: {kernel_sources} not found; skipping",
              file=sys.stderr)
        return 0

    orphans_by_file = {}
    for hs_path in sorted(kernel_sources.rglob("*.hs")):
        orphans = check_file(hs_path)
        if orphans:
            orphans_by_file[hs_path.relative_to(hydra_root)] = orphans

    if not orphans_by_file:
        print("  check-haskell-def-completeness: every scanned definition is registered",
              file=sys.stderr)
        return 0

    print("  check-haskell-def-completeness: found unregistered definition(s):", file=sys.stderr)
    for rel_path, orphans in sorted(orphans_by_file.items()):
        print(f"    {rel_path}: {', '.join(orphans)}", file=sys.stderr)
    print("  Add each to its module's `definitions = [...]` list, or remove the unused binding.",
          file=sys.stderr)
    return 1


if __name__ == "__main__":
    sys.exit(main())
