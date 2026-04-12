#!/usr/bin/env python3
"""Break long lines in generated Scala files at natural break points.

This post-processes generated Scala files to prevent the Scala compiler
from running out of memory on extremely long single-line expressions.
"""

import sys
import os
import re
import glob

MAX_LINE = 160  # Maximum line length before breaking

def break_line(line, indent=""):
    """Break a long line at natural Scala break points."""
    if len(line) <= MAX_LINE:
        return line

    result = []
    current = ""
    paren_depth = 0
    in_string = False
    i = 0

    while i < len(line):
        c = line[i]
        current += c

        # Track string literals to avoid breaking inside them
        if c == '"' and (i == 0 or line[i-1] != '\\'):
            in_string = not in_string

        if not in_string:
            if c == '(' or c == '[':
                paren_depth += 1
            elif c == ')' or c == ']':
                paren_depth -= 1
            elif c == '=' and i + 1 < len(line) and line[i + 1] == '>':
                # Break after => only in lambda context (preceded by ')'), not in type context
                if len(current) > 60 and i > 0 and line[i-1] == ')':
                    current += '>'
                    i += 1
                    result.append(current)
                    current = indent + "    "
            elif c == ',' and len(current) > 100:
                # Break after comma when segment is long
                result.append(current)
                current = indent + "  "

        i += 1

    if current:
        result.append(current)

    if len(result) <= 1:
        # If still too long, force break every MAX_LINE chars at commas
        if len(line) > MAX_LINE * 2:
            parts = []
            seg = ""
            for i, c in enumerate(line):
                seg += c
                if c == ',' and len(seg) > MAX_LINE:
                    parts.append(seg)
                    seg = indent + "  "
            if seg:
                parts.append(seg)
            if len(parts) > 1:
                return "\n".join(parts)
        return line

    return "\n".join(result)


def fix_haskell_escapes(line):
    """Convert Haskell-style escape sequences to Scala-compatible ones."""
    # Convert named escapes like \NUL, \SOH, \STX, etc. to \uXXXX
    haskell_named_escapes = {
        'NUL': '\u0000', 'SOH': '\u0001', 'STX': '\u0002', 'ETX': '\u0003',
        'EOT': '\u0004', 'ENQ': '\u0005', 'ACK': '\u0006', 'BEL': '\u0007',
        'BS': '\u0008', 'HT': '\u0009', 'LF': '\u000a', 'VT': '\u000b',
        'FF': '\u000c', 'CR': '\u000d', 'SO': '\u000e', 'SI': '\u000f',
        'DLE': '\u0010', 'DC1': '\u0011', 'DC2': '\u0012', 'DC3': '\u0013',
        'DC4': '\u0014', 'NAK': '\u0015', 'SYN': '\u0016', 'ETB': '\u0017',
        'CAN': '\u0018', 'EM': '\u0019', 'SUB': '\u001a', 'ESC': '\u001b',
        'FS': '\u001c', 'GS': '\u001d', 'RS': '\u001e', 'US': '\u001f',
        'DEL': '\u007f',
    }
    for name, replacement in haskell_named_escapes.items():
        escaped_replacement = '\\u' + format(ord(replacement), '04x')
        line = line.replace('\\' + name, escaped_replacement)
    # Convert decimal escapes like \955 to actual UTF-8 characters
    def replace_escape(m):
        code_point = int(m.group(1))
        try:
            return chr(code_point)
        except (ValueError, OverflowError):
            return m.group(0)  # Keep original if invalid
    return re.sub(r'\\(\d{3,5})', replace_escape, line)


def process_file(filepath):
    """Process a single Scala file, breaking long lines and fixing escapes."""
    with open(filepath, 'r') as f:
        lines = f.readlines()

    modified = False
    new_lines = []
    for line in lines:
        # Fix Haskell-style decimal escapes in string literals
        fixed = fix_haskell_escapes(line)
        if fixed != line:
            modified = True
            line = fixed
        stripped = line.rstrip('\n')
        if len(stripped) > MAX_LINE:
            indent = " " * (len(stripped) - len(stripped.lstrip()))
            broken = break_line(stripped, indent)
            if broken != stripped:
                modified = True
                new_lines.append(broken + '\n')
            else:
                new_lines.append(line)
        else:
            new_lines.append(line)

    if modified:
        with open(filepath, 'w') as f:
            f.writelines(new_lines)
        return True
    return False


def main():
    scala_dir = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))),
                              "dist", "scala", "hydra-kernel", "src", "main", "scala")

    count = 0
    for filepath in glob.glob(os.path.join(scala_dir, "**", "*.scala"), recursive=True):
        if process_file(filepath):
            count += 1
            print(f"  Broke lines in: {os.path.relpath(filepath, scala_dir)}")

    print(f"\nProcessed {count} files")


if __name__ == "__main__":
    main()
