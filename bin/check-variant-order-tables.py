#!/usr/bin/env python3
"""Check the hand-authored declared-variant-order tables in the host overlays against the kernel.

TypeScript, Python and the four Lisp dialects keep a table of declared variant order for the
hydra.core.model unions (Term, Type, Literal, IntegerType, FloatType), used by their ordering
primitives (#718, #827). This compares each table with dist/json/hydra-kernel's hydra/core/model.json.
Exit 1 on any mismatch. Usage: bin/check-variant-order-tables.py
"""
import json, re, sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
MODEL = ROOT / "dist/json/hydra-kernel/src/main/json/hydra/core/model.json"
FAMILIES = {"Term": "term", "Type": "type", "Literal": "literal", "IntegerType": "integer", "FloatType": "float"}


def unwrap(t):
    while True:
        if "annotated" in t: t = t["annotated"]["body"]
        elif "forall" in t: t = t["forall"]["body"]
        elif "variables" in t and "body" in t: t = t["body"]
        else: return t


def declared():
    out = {}
    for d in json.load(open(MODEL))["definitions"]:
        short = d["type"]["name"].split(".")[-1]
        if short in FAMILIES:
            out[FAMILIES[short]] = [f["name"] for f in unwrap(d["type"]["body"])["union"]]
    return out


def snake(s): return re.sub(r"([A-Z])", lambda m: "_" + m.group(1).lower(), s)


def ts(p):
    s = p.read_text()
    body = s[s.index("const VARIANT_ORDER"):]
    body = body[:body.index("};")]
    return {m.group(1): re.findall(r'"(\w+)"', m.group(2))
            for m in re.finditer(r"(\w+): \[(.*?)\]", body, re.S)}


def py(p):
    s = p.read_text()
    body = s[s.index("_VARIANT_ORDER"):]
    body = body[:body.index("\n}\n")]
    out = {}
    for m in re.finditer(r'"(\w+)": \((.*?)\)', body, re.S):
        fam = {v: k for k, v in FAMILIES.items()}.get(m.group(1), m.group(1))
        pre = m.group(1)
        out[FAMILIES.get(fam, pre.lower())] = [
            v[0].lower() + v[1:] for v in
            (re.sub("^" + pre.replace("IntegerType", "Integer").replace("FloatType", "Float"), "", x)
             for x in re.findall(r'"(\w+)"', m.group(2)))]
    return out


def lisp(p):
    s = p.read_text()
    out = {}
    for m in re.finditer(r'(?:cons |\()"(\w+)"\s*(?:\.\s*)?\'?\((.*?)\)\)', s, re.S):
        out[m.group(1)] = re.findall(r"[:']?([a-z_0-9]+)", m.group(2))
    for m in re.finditer(r'"(\w+)"\s+\[(.*?)\]', s, re.S):  # clojure
        out[m.group(1)] = re.findall(r":([a-z_0-9]+)", m.group(2))
    for m in re.finditer(r"cons '(\w+) '\((.*?)\)\)", s, re.S):  # scheme
        out[m.group(1)] = m.group(2).split()
    return out


def camel(v): return re.sub(r"_(\w)", lambda m: m.group(1).upper(), v)


HOSTS = {
    "typescript": ("overlay/typescript/hydra-kernel/src/main/typescript/hydra/core/overlay/typescript/lib/ordering.ts", ts),
    "python": ("overlay/python/hydra-kernel/src/main/python/hydra/core/overlay/python/util/_compare.py", py),
    "clojure": ("overlay/clojure/hydra-kernel/src/main/clojure/hydra/core/overlay/clojure/lib/ordering.clj", lisp),
    "common-lisp": ("overlay/common-lisp/hydra-kernel/src/main/common-lisp/hydra/core/overlay/common_lisp/lib/ordering.lisp", lisp),
    "emacs-lisp": ("overlay/emacs-lisp/hydra-kernel/src/main/emacs-lisp/hydra/core/overlay/emacs_lisp/lib/ordering.el", lisp),
    "scheme": ("overlay/scheme/hydra-kernel/src/main/scheme/hydra/overlay/scheme/lib/ordering.scm", lisp),
}

want = declared()
bad = 0
for host, (rel, parse) in HOSTS.items():
    got = parse(ROOT / rel)
    for fam, order in want.items():
        if host == "python" and fam in ("integer", "float"):
            continue  # Python compares ints/floats natively; no variant table
        have = [camel(v) for v in got.get(fam, [])]
        if have != order:
            bad += 1
            print(f"MISMATCH {host}/{fam}:\n  declared {order}\n  table    {have}")
print("variant-order tables: " + ("FAIL" if bad else "OK"))
sys.exit(1 if bad else 0)
