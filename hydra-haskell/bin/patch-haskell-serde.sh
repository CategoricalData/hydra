#!/bin/bash
# Bootstrap patch for src/gen-main/haskell/Hydra/Ext/Haskell/Serde.hs
#
# The regenerated literalToExpr function emits Haskell source code for literals.
# For Double/Float values it calls Literals.showFloat64/32, which matches Haskell's
# Show and produces "NaN", "Infinity", "-Infinity" for IEEE 754 special values.
# Those strings are not valid Haskell literals and break compilation of any
# generated test-suite code that embeds NaN/Inf.
#
# This patch wraps showFloat64/32 with guards that emit (0/0), (1/0), (-1/0)
# for NaN, +Inf, and -Inf respectively.
#
# Run this script after any sync step that regenerates gen-main/haskell.
# See CLAUDE.md and feature_312_float_tests for context.

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_HASKELL_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
SERDE_FILE="$HYDRA_HASKELL_DIR/src/gen-main/haskell/Hydra/Ext/Haskell/Serde.hs"

if [ ! -f "$SERDE_FILE" ]; then
    echo "ERROR: $SERDE_FILE not found"
    exit 1
fi

if grep -q "showDoubleForHaskell" "$SERDE_FILE"; then
    echo "Patch already applied."
    exit 0
fi

# Apply the patch using python for robust multiline substitution
python3 - "$SERDE_FILE" <<'PYEOF'
import sys
path = sys.argv[1]
with open(path, 'r') as f:
    content = f.read()

old_block = '''      let parensIfNeg =
              \\b -> \\e -> Logic.ifElse b (Strings.cat [
                "(",
                e,
                ")"]) e
      in (Serialization.cst (case lit of
        Syntax.LiteralChar v0 -> Literals.showString (Literals.showUint16 v0)
        Syntax.LiteralDouble v0 -> parensIfNeg (Equality.lt v0 0.0) (Literals.showFloat64 v0)
        Syntax.LiteralFloat v0 -> parensIfNeg (Equality.lt v0 0.0) (Literals.showFloat32 v0)'''

new_block = '''      let parensIfNeg =
              \\b -> \\e -> Logic.ifElse b (Strings.cat [
                "(",
                e,
                ")"]) e
          -- BOOTSTRAP PATCH: render NaN/Inf as valid Haskell expressions.
          showDoubleForHaskell :: Double -> String
          showDoubleForHaskell v
            | Prelude.isNaN v = "(0/0)"
            | Prelude.isInfinite v = if v Prelude.> 0 then "(1/0)" else "(-1/0)"
            | otherwise = parensIfNeg (v Prelude.< 0) (Literals.showFloat64 v)
          showFloatForHaskell :: Float -> String
          showFloatForHaskell v
            | Prelude.isNaN v = "(0/0)"
            | Prelude.isInfinite v = if v Prelude.> 0 then "(1/0)" else "(-1/0)"
            | otherwise = parensIfNeg (v Prelude.< 0) (Literals.showFloat32 v)
      in (Serialization.cst (case lit of
        Syntax.LiteralChar v0 -> Literals.showString (Literals.showUint16 v0)
        Syntax.LiteralDouble v0 -> showDoubleForHaskell v0
        Syntax.LiteralFloat v0 -> showFloatForHaskell v0'''

if old_block not in content:
    print("ERROR: expected block not found in Serde.hs. Patch not applied.")
    sys.exit(1)

content = content.replace(old_block, new_block)
with open(path, 'w') as f:
    f.write(content)
print("Patched successfully.")
PYEOF

echo "Patched: $SERDE_FILE"
