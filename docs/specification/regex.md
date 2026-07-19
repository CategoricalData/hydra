# Hydra regex syntax (draft specification)

**Status:** Draft — Stage 0 (host-engine investigation). Tracks issue
[#567](https://github.com/CategoricalData/hydra/issues/567).

> **Dependency note.** The `docs/specification/` tree is nominally owed by
> [#417](https://github.com/CategoricalData/hydra/issues/417) (primitives finalization). This file is
> created early under #567 so the regex grammar has a home; #417 may absorb or relocate it. The
> grammar/BNF formalism must also be reconciled with
> [#497](https://github.com/CategoricalData/hydra/issues/497) (formally specified Hydra textual
> syntax), of which the regex sub-syntax is a concrete increment.
>
> **Naming.** This work uses the **`parse`/`print`** axis, superseding the earlier `read`/`show`
> naming (project-wide adoption, per the coordinator): `hydra.parse.regex` (text → AST),
> `hydra.print.regex` (AST → canonical text), and `hydra.print.<dialect>.regex` (per-dialect
> rendering, e.g. `hydra.print.jvm.regex`).

## Purpose

Hydra's `hydra.lib.regex` primitives (`find`, `findAll`, `matches`, `replace`, `replaceAll`, `split`)
currently delegate to each host's native regex engine, so a pattern's meaning is **host-defined**. For
a finalized, translingual primitive set this is unacceptable: a pattern must mean the same thing on
every host. This document defines a **Hydra-specific regex syntax** and records the per-host engine
survey that establishes the **mappability invariant** — every Hydra regex AST node must be emittable in
every supported target dialect.

## Stage 0 — host regex-engine survey

Each Hydra host delegates the `hydra.lib.regex` primitives to a native engine. The engines and their
dialect families:

| Host | Engine / library | Dialect family | Notes |
|------|------------------|----------------|-------|
| Haskell | `Text.Regex.TDFA` | **POSIX ERE** | Leftmost-longest (POSIX) semantics; no Perl `\d`/`\w`/`\s`, no lazy quantifiers, no lookaround/backrefs. `matches` anchors via `^(...)$`. |
| Java | `java.util.regex` | PCRE-like | Perl-family. `matches` (via `Pattern.matches`) anchors whole string. |
| Scala | `java.util.regex` (direct) | PCRE-like | Same engine as Java. `Pattern.matches` anchors; replacements quoted via `Matcher.quoteReplacement`. |
| Clojure | `clojure.core` regex → `java.util.regex` | PCRE-like | `re-matches` anchors whole string; `replaceAll` quotes replacement. |
| Python | `re` | PCRE-like | `matches` uses `re.fullmatch` (anchored). |
| TypeScript | JS built-in `RegExp` | **ECMA-262** | `matches` anchors via `m[0] === s`. Only `matches`/`replace`/`findAll` implemented; `replace` uses the `g` flag (acts as replace-all); replacement `$1` etc. are interpreted (not quoted). |
| Common Lisp | `cl-ppcre` | PCRE-like (Perl 5) | Strongest Lisp engine; full Perl syntax, no shim needed. `matches` uses `^(?:...)$`. |
| Scheme | Guile `(ice-9 regex)` → POSIX C lib | **POSIX ERE** | Leftmost-longest; no Perl extensions. `matches` naively concatenates `^...$` (mishandles top-level `\|`). |
| Emacs Lisp | Emacs built-in regexp | Emacs (POSIX-ERE-shimmed) | **Weak link.** A `hydra--posix-to-emacs-regex` pre-pass swaps `\|`/`\(`/`\)` escaping. See gap below. |
| Go | (none — no regex overlay) | RE2 (when built) | Go is a "head bud"; no regex overlay exists yet. Go's stdlib is `regexp` (RE2): linear-time, **no backreferences, no lookaround**. |

### Key observations

1. **Two engine families already coexist:** POSIX-ERE (Haskell, Scheme) and PCRE/ECMA (Java, Scala,
   Clojure, Python, TypeScript, Common Lisp). Any Hydra core that must map to *both* is constrained to
   their common intersection.
2. **POSIX ERE is the de-facto notional canonical syntax today.** The Emacs shim is explicitly written
   as "POSIX ERE → Emacs regexp," and the Haskell host of record uses POSIX ERE (TDFA). This strongly
   favors a **POSIX-ERE-flavored** Hydra core (with the deliberate, familiar intersection the issue
   calls for), rather than a PCRE-flavored one.
3. **Leftmost-longest vs. leftmost-first match semantics differ** between POSIX (Haskell, Scheme) and
   Perl/ECMA (everyone else). For the minimal core (no ambiguity-sensitive constructs like `a|ab` where
   it matters, or with them carefully specified) this must be pinned in the spec, not left implicit.
4. **`matches` is whole-string-anchored on every host** — this is already a settled Hydra convention and
   the core spec should keep it (a pattern anchors implicitly for `matches`, explicitly with `^`/`$`
   elsewhere).

## Mappability matrix — minimal core

Minimal-core features from the issue: literals, `[...]` classes, `.`, quantifiers `* + ? {n,m}`,
alternation `|`, anchors `^ $`, grouping `()`. Whether each host's engine expresses each **natively**
(N), **via the existing shim** (S), or is **broken/unsupported** (✗):

| Feature | Haskell (TDFA) | Java/Scala/Clojure | Python | TypeScript | Common Lisp | Scheme (POSIX) | Emacs | Go (RE2) |
|---------|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| literals | N | N | N | N | N | N | N | N |
| `[...]` classes | N | N | N | N | N | N | N | N |
| `.` | N | N | N | N | N | N | N | N |
| `*` `+` `?` | N | N | N | N | N | N | N | N |
| `{n,m}` | N | N | N | N | N | N | **✗** | N |
| alternation `|` | N | N | N | N | N | N | S (`\|`) | N |
| grouping `()` | N | N | N | N | N | N | S (`\(\)`) | N |
| anchors `^ $` | N | N | N | N | N | N | S (`\``/`\'` for whole) | N |

### The one real gap: Emacs `{n,m}`

Every minimal-core feature maps to every host **except** `{n,m}` on Emacs Lisp. The
`hydra--posix-to-emacs-regex` shim translates only `| ( )` (swapping backslash escaping); it does
**not** touch braces. In Emacs regexp, bounded quantifiers are `\{n,m\}` (braces are literal when
unescaped), so an incoming `{n,m}` passes through as literal braces — **the quantifier silently
breaks**.

**Resolution options** (to decide in Stage 1 / with #497 coordination):

- **(A) Extend the Emacs shim** to also translate `{n,m}` → `\{n,m\}` (mirroring the `| ( )` swap,
  with the same bracket-class tracking to avoid rewriting `{` inside `[...]`). This keeps `{n,m}` in
  the minimal core and preserves the mappability invariant. **Recommended** — the fix is small and
  local, and the shim already establishes the pattern.
- **(B) Drop `{n,m}` from the minimal core** and defer it to the advanced-feature roadmap. Cleaner
  invariant now, but `{n,m}` is squarely "familiar intersection" and its omission would be surprising.

The recommendation is **(A)**: keep `{n,m}` in the minimal core and extend the Emacs shim during
implementation, so the mappability invariant holds across all ten (nine + Go bud) hosts.

## Stage 1 — grammar + AST (draft)

Design goals: (1) a **POSIX-ERE-flavored familiar intersection**, per Stage 0; (2) every production
maps to every host dialect (mappability invariant); (3) a canonical printed form so that
`print.regex ∘ parse.regex` is idempotent on well-formed input.

### Concrete syntax (BNF)

Minimal core only. `|` is alternation; `.` is any-char; the metacharacters are
`. ^ $ * + ? ( ) [ ] { } | \`.

```bnf
regex        ::= alternation
alternation  ::= sequence ( "|" sequence )*     -- each branch (sequence) must be non-empty; see below
sequence     ::= quantified+                     -- non-empty, EXCEPT the whole regex may be empty ("")
quantified   ::= atom quantifier?
quantifier   ::= "*" | "+" | "?" | "{" bound "}"
bound        ::= INT | INT "," | INT "," INT     -- {n}  {n,}  {n,m}   (no {,m} form)
atom         ::= group | class | "." | anchor | literal
group        ::= "(" alternation? ")"            -- () (empty group) is legal; matches the empty string
anchor       ::= "^" | "$"
class        ::= "[" "^"? classitem+ "]"         -- non-empty; [] and [^] are NOT legal
classitem    ::= classrange | classchar
classrange   ::= classchar "-" classchar
literal      ::= CHAR | "\" METACHAR             -- an ordinary char, or an escaped metacharacter
classchar    ::= CLASSCHAR | "\" CLASSMETA        -- inside [...] the metaset is { \ ] ^ - }
INT          ::= DIGIT+
METACHAR     ::= "." | "^" | "$" | "*" | "+" | "?" | "(" | ")" | "[" | "]" | "{" | "}" | "|" | "\"
```

Deliberately **excluded** from the minimal core (deferred to the advanced roadmap, each with its own
per-dialect portability decision): Perl shorthands `\d \w \s`, non-greedy `*?`, lookaround,
backreferences, named groups, Unicode property classes, POSIX class names `[[:alpha:]]`, inline flags,
the `{,m}` (up-to-m) quantifier, and the empty/negated-empty classes `[]` / `[^]`. Excluding
`\d`/`\w`/`\s` is what keeps the invariant clean — POSIX ERE (Haskell TDFA, Scheme) does not support
them (confirmed by probe: TDFA does not recognize `\s`).

#### Grammar decisions (final)

Each of these was decided against the host-engine evidence (see the TDFA probe results below):

- **No `{,m}`.** POSIX ERE has no empty-lower-bound interval; ECMA only accepts it under the ES2018
  web-compat annex (and rejects it in Unicode mode). It is also redundant with the fully portable
  `{0,m}`. Dropped. The three portable interval forms remain: `{n}`, `{n,}`, `{n,m}`.
- **No empty alternation branch.** `a|`, `|b`, `a||b`, `a|||` are **rejected** by `hydra.parse.regex`.
  POSIX ERE (TDFA) rejects them natively; ECMA/PCRE accept them but they are redundant — "`foo` or the
  empty string" is written `(foo)?` — and a stray `|` silently turns a pattern into match-anywhere.
  Every branch must contain at least one `quantified`.
- **Empty group `()` is legal.** It matches the empty string and is semantically inert (Hydra groups
  are **grouping-only, non-capturing** — there are no capture indices or backreferences in the core).
  TDFA accepts `()` natively (probe: match). Far less error-prone than an empty branch.
- **Empty class `[]` / negated-empty `[^]` are illegal.** They are an ECMA-only construct — TDFA
  (POSIX) errors on both, and they are outside the "familiar intersection." A class must list at least
  one item. "Any character" is `.`; there is no need for `[^]`.
- **Escaping, not positional rules, everywhere.** A literal metacharacter is always written with a
  backslash; Hydra never relies on POSIX positional conventions (e.g. "`]` first in a class is
  literal"). Inside a class, a literal `]`, `^`, `-`, or `\` is written `\]`, `\^`, `\-`, `\\`.
- **The whole regex may be empty.** `""` parses to a single empty `RegexSequence` at top level (no
  `|`), and matches the empty string. This is the *one* place an empty sequence is allowed; it is not
  an empty *branch*.

### Abstract syntax (Hydra regex AST)

Proposed new kernel type module `hydra.regex` (namespace `hydra.regex`), a sibling of `hydra.ast`.
Sketch (final DSL in `packages/hydra-kernel/.../Kernel/Types/Regex.hs`):

```
Regex        = Alternation      -- top-level alias
Alternation  = [RegexSequence]                  -- non-empty; joined by |
RegexSequence = [Quantified]                    -- possibly empty (matches empty string)
Quantified   = { atom: Atom, quantifier: Quantifier }
Quantifier    = one | zeroOrOne | zeroOrMore | oneOrMore
              | exactly Int | atLeast Int | range QuantifierRange    -- {n} {n,} {n,m}
QuantifierRange = { min: Int, max: Int }
Atom          = literal CodePoint | any | anchorStart | anchorEnd
              | group Alternation | class CharacterClass
CharacterClass  = { negated: Bool, items: [ClassItem] }          -- items non-empty
ClassItem       = character CodePoint | range CharacterRange
CharacterRange  = { from: CodePoint, to: CodePoint }
```

Characters are **full Unicode scalar values** (range `[U+0000, U+10FFFF]`, surrogates
`U+D800–U+DFFF` excluded) — **not** BMP-only. They are represented as **`int32`** code points: the
scalar-value range `[0, 0x10FFFF]` fits comfortably in signed 32-bit, and `int32` matches the code-point
representation used throughout Hydra's string and parser primitives (`hydra.lib.strings.toList`/`fromList`
and the `hydra.parsers` combinators all operate on `int32` code points), so no width conversion is needed
at the parser/printer boundaries. `hydra.parse.regex` rejects out-of-range or lone-surrogate code points,
so the AST only ever holds valid scalar values. Each host printer is responsible for emitting a code point
correctly in its target encoding (a literal char, a surrogate pair on UTF-16 hosts, `\u{...}`, etc.) — the
AST carries the abstract code point only.

> Note: `QuantifierRange` and `CharacterRange` are **named** record types, not inline records nested
> in the union variants. Hydra's coders cannot name an anonymous record nested inside a union variant
> (the Haskell coder emits an unbound `placeholder` type variable), so every record used as a variant
> payload must be a top-level definition — the same convention `hydra.query` follows with its `Range`
> type.

Reuse note: `hydra.query.RegexQuantifier` / `Range` (in `Kernel/Types/Query.hs`) already model exactly
these quantifiers — but specialized to quantify a graph **path**, not a character atom. The new
`hydra.regex.Quantifier` should mirror its shape; a future refactor could hoist a shared
`Quantifier`, but that is out of scope for #567 (noted as a Finding).

### Semantics to pin

- **`.` matches ANY character, including newline.** This is a deliberate departure from every host's
  native `.` (which excludes `\n` — confirmed by probe for TDFA, and standard for ECMA/PCRE). Hydra's
  "any" means *any*; a user who wants the line-oriented behavior writes `[^\n]` explicitly. The
  consequence is that **no** printer emits a bare `.` — every dialect renders Hydra `.` as an explicit
  newline-inclusive form (see "Per-dialect rendering" below).
- **Anchors `^` / `$` are string-boundary, not line-boundary.** They match the empty string at the
  start / end of the *whole input*, not at internal line breaks. (TDFA's native `^`/`$` lean
  line-oriented — probe: `^.*$` matched a single line of a multi-line input — so the TDFA printer must
  take care here. Line anchoring / multiline mode is on the excluded-advanced list.)
- **Match preference:** **leftmost-longest (POSIX)** is the canonical Hydra semantics, since the
  minimal core has no greedy/lazy distinction and POSIX is the host-of-record (TDFA) behavior. Where a
  host is leftmost-first (Perl/ECMA), the minimal core's constructs are chosen so observable results
  agree on non-pathological inputs; the conformance suite pins the divergent cases (e.g. `a|ab` on
  `"ab"`).
- **`matches` is whole-string-anchored** (settled convention).
- **Ill-formed = does not parse under `hydra.parse.regex`.** Failure is Hydra-defined and portable
  (same pattern is `left`/`nothing` on every host), per the issue's either/optional codomain rule.

### Escaping and canonical printing

Escaping is purely a **concrete-syntax / serialization** concern — the AST holds bare code points, with
no notion of escaping. `hydra.parse.regex` consumes escapes; the printers decide when to emit them.

- **`hydra.print.regex` (canonical) escapes minimally.** A character is escaped only where it is
  special *in the context it appears*. `hydra.parse.regex` accepts liberally (`\x` for any `x`), so
  `print ∘ parse` is idempotent on well-formed input.
- **Top-level metaset** (outside `[...]`): `. ^ $ * + ? ( ) [ ] { } | \`. A `-` is **not** special at
  top level and prints bare (`a-b` at top level is three literals, not a range).
- **Class metaset** (inside `[...]`): a literal `\`, `]`, `^`, or `-` is **always** written escaped
  (`\\`, `\]`, `\^`, `\-`) — uniformly, with no positional special-casing. Examples:
  `[\-a]`, `[a\-]`, `[a-z\-]`, `[a\-b]`, `[\-]`, `[^\-a]`.

### Per-dialect rendering (Stage 3 preview)

Two constructs require a non-identity rewrite on every printer, both driven by the "`.` = any incl.
newline" decision. The **explicit code-point range** form works on *all* engines (they accept
literal-char ranges) and is the safe universal rewrite; `\s\S` does **not** (TDFA rejects `\s` — probe):

- Hydra `.` (any incl. newline) → an explicit full-range class, e.g. `[\x00-\x{10FFFF}]` (POSIX/TDFA),
  or `[\s\S]` where the dialect supports it (ECMA/PCRE). **Never** a bare `.`.
- A user-written `[^\n]` (any except newline) → renders natively; this is the line-oriented `.` that
  most hosts default to.

The **Emacs `{n,m}` shim gap** (Stage 0) still applies: extend `hydra--posix-to-emacs-regex` to
translate `{n,m}` → `\{n,m\}` (option A), tracked for Stage 3/4.

### Host-engine probe results (Haskell TDFA 1.3.2)

Run against the Haskell host of record to ground the decisions above (not assumed from the standard):

| Probe | TDFA result | Decision it settles |
|-------|-------------|---------------------|
| `()` empty group | **match** (accepted) | `()` legal |
| `[]`, `[^]` empty/neg-empty class | **error** | `[]`/`[^]` excluded |
| `a\|` empty branch | **error** | empty branches rejected |
| `.` vs `"\n"` (`a.b` on `"a\nb"`) | **no match** | `.` excludes newline natively → universal rewrite |
| `[\s\S]` for "any incl. newline" | **no match** | `\s` unsupported on TDFA → use explicit range |
| `[\x00-\x{10FFFF}]` for "any incl. newline" | **match** | explicit range is the portable rewrite |
| `a{,2}` (up-to form) | **no match** (literal) | `{,m}` excluded |
| `a{2,}`, `a{n,m}` | **match** | `{n,}`, `{n,m}` portable |

## Next stages (see the branch plan)

- **Stage 1** — grammar/BNF + Hydra regex AST (verify the invariant against this matrix; resolve the
  Emacs `{n,m}` gap via option A).
- **Stage 2** — `hydra.parse.regex` (text → AST) and `hydra.print.regex` (AST → canonical text).
- **Stage 3** — per-dialect `hydra.print.<dialect>.regex` translators (one per distinct dialect family:
  a POSIX-ERE dialect, a PCRE/`java.util.regex` dialect, an ECMA-262 dialect, an Emacs dialect, an RE2
  dialect).
- **Stage 4** — rewire `hydra.lib.regex` to Hydra-defined semantics; either-returning codomains for
  fail-on-malformed ops.
- **Stage 5** — host-independent conformance suite.
