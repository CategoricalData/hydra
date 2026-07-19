# Hydra textual syntax

**Status: Draft** · Part of the [Hydra specification](https://github.com/CategoricalData/hydra/issues/579) ·
Specification half of [#497](https://github.com/CategoricalData/hydra/issues/497)
(the parser and round-trip validation remain in #497, which implements this document)

This document specifies the Hydra textual notation for terms, types, literals, and type schemes:
the presentation syntax used throughout the specification,
produced by the `hydra.print.*` kernel functions and read by the `hydra.parse.*` functions.
The grammar is unambiguous, with stated precedence, and is normative for both directions:
a conforming printer MUST emit text matching the canonical form (§4),
and a conforming parser MUST accept every production of the grammar (§3).

The key words MUST, MUST NOT, SHOULD, and MAY are to be interpreted as described in RFC 2119.

## 1. The round-trip contract

Every expression MUST round-trip:
parsing printed text recovers the original term or type exactly,
with everything semantically significant intact — including annotations.
Only non-significant surface may change on a round trip: whitespace, parenthesization, and comments.

Consequences:

- The printer is always faithful.
  If a term has annotations, the printed text shows them, and parsing recovers them.
  Display-oriented simplification (for example, stripping annotations for readability)
  is performed by transforming the term *before* printing — never by a lossy printer mode.
- Stacked annotations are structure: they print as nested and parse back as nested.
  The "outermost wins" aggregate view of annotation maps is a reader-side convention
  with no syntactic footprint.
- Binary literals carry their full payload (base64; see §2.6).

## 2. Lexical structure

### 2.1 Whitespace

Whitespace between tokens is insignificant.
Terminal symbols never include leading or trailing whitespace.
Canonical spacing is specified in §4.

### 2.2 Comments

`#` begins a line comment, which extends to the end of the line.
`#` is only special outside string literals and backticked names.
Comments are whitespace-class:
the printer never emits them, and they are lost on a round trip,
exactly like non-canonical whitespace.

### 2.3 Operators and tokens

The grammar is Unicode-only: there are no ASCII synonyms for the Unicode operators.

| Token | Role |
|---|---|
| `λ` | term-level lambda binder |
| `Λ` | term-level type-lambda (System F) binder |
| `forall` | universal quantification in types (`Type.forall`) |
| `∀` | type-scheme binder (distinct from `forall`; see §3) |
| `→` | function types; also kind arrows (reserved) |
| `⇒` | type-scheme constraint arrow |
| `⟨` `⟩` | term-level type application |
| `@` | annotation marker |
| `:=` | let bindings |
| `=` | fields and map entries |
| `:` | type ascription (lambda domains, literal suffixes, type fields) |
| `` ` `` | name escaping |
| `#` | comment start |

### 2.4 Names

A *bare* name is a nonempty sequence of dot-separated segments of alphanumeric characters.
Outside binder heads, bare dotted names are read greedily as qualified names
(`hydra.core.Term` is one name).

A name may also be written in *backticked* form — `` ` `` … `` ` `` — using the string escape
set of §2.5 for its content.
The backticked form admits any name, including names that cannot be written bare.

**The general escaping rule: the natural structural reading always wins,
and a name is backtick-escaped exactly when its bare spelling would interfere with that reading.**
`λv:t0.body` binds `v` of type `t0` in `body`, with no marks.
The interference cases, and therefore the situations in which the canonical printer escapes a name, are:

1. the name contains characters outside the bare alphabet;
2. the name is a reserved word (§2.7);
3. the name contains a dot in a position where the dot is structural.

Case 3 concerns *binder heads*: the region between a binder (`λ`, `Λ`, `forall`, `∀`)
and the dot that terminates it.
Within a binder head, at bracket depth zero, dots are structural
and bare names are single dot-free segments.
A dotted name there must be escaped: ``λx:`hydra.core.Term`.body``.
Inside nested delimiters — `<…>`, `(…)`, or backticks — dotted names read greedily as usual:
`λx:list<hydra.core.Term>.body` needs no escaping.

### 2.5 Strings

String literals use the JSON string syntax and escape rules,
exactly as specified in the [JSON wire format](../json-format.md#string-escapes):
`\"` and `\\` always escaped; the five shortcut escapes `\b` `\f` `\n` `\r` `\t`;
`\u00xx` (lowercase hex) for remaining control characters;
all other characters emitted as literal UTF-8, with no `\/` and no surrogate pairs.
The same kernel code SHOULD serve JSON and textual printing and parsing of strings.

### 2.6 Literals

- **Booleans**: `true`, `false`.
- **Integers**: decimal digits with a mandatory width suffix — `42:int32`, `-7:int64`.
  Widths: `bigint`, `int8`, `int16`, `int32`, `int64`, `uint8`, `uint16`, `uint32`, `uint64`.
- **Floats**: digits with a mandatory precision suffix — `0.42:float32`, `2.5e10:float64`.
  Digit strings follow the JSON wire format's float rule:
  the shortest decimal representation that parses back at the value's precision
  to the exact IEEE 754 bit pattern; the printer never tidies its input.
  The non-finite sentinels are bare suffixed tokens: `NaN:float64`, `Infinity:float32`,
  `-Infinity:float64`; negative zero is written directly as `-0.0`.
- **Decimals** (arbitrary precision): a bare numeric literal with no suffix — `3.14`.
  The lexical form is exactly JSON's number grammar:
  an optional sign, integer digits, an optional fraction part, and an optional exponent part
  (`123`, `3.14`, `-0.5`, `1.2e9`, `6.02e-23`).
  An unsuffixed numeric literal is always a decimal;
  integer and float literals always carry a `:width` suffix.
  The canonical digit string is defined by the `printDecimal` primitive
  (positional form up to a pinned magnitude threshold, exponent form beyond it;
  specified with the wire format's decimal entry so the primitive, the JSON encoding,
  and this syntax agree by construction).
  Exponent parts bind to the numeric token by maximal munch:
  `1.2e9` is one decimal literal, while `1.2 e9` is the application of `1.2` to the name `e9`.
- **Binary**: a base64 string with the `binary` suffix — `"aGVsbG8=":binary`.
  Standard base64 alphabet with padding, matching `binaryToString` and the JSON wire format.
- The character `-` occurs only inside numeric literals (there are no infix operators),
  so a form such as `f -1:int32` lexes unambiguously.

### 2.7 Reserved words

The following bare names are reserved in all positions.
To use one as a name, write it backticked.

```
_ bigint binary boolean case decimal effect either false float32 float64
forall given in inject int8 int16 int32 int64 left let list map none optional
project record right set string true uint8 uint16 uint32 uint64 union unit
unwrap void wrap NaN Infinity
```

Validation SHOULD reject reserved names in positions that quoting cannot rescue,
in particular `_` as a user field or variant name (it would collide with the case-default wildcard).

## 3. Grammar

Precedence, tightest to loosest:

1. postfix: `@` annotation, `⟨T⟩` type application
2. application: adjacency, left-associative
3. `→`: right-associative
4. binder bodies (`λ`, `Λ`, `let`, `forall`, `∀`): extend maximally to the right

`(` … `)` overrides precedence anywhere.
A parenthesized single term or type is grouping; with a comma it is a pair.

```
-- Terms (21 variants of hydra.core.Term)
term          ::= annotatedTerm | applicationTerm | caseTerm | eitherTerm
                | injectTerm | lambdaTerm | letTerm | listTerm | literalTerm
                | mapTerm | optionalTerm | pairTerm | projectTerm | recordTerm
                | setTerm | typeApplicationTerm | typeLambdaTerm | unitTerm
                | unwrapTerm | variableTerm | wrapTerm
                | '(' term ')'

annotatedTerm ::= term annot
annot         ::= '@' term        -- postfix; payload parsed at tightest level,
                                  -- parenthesize larger payloads: t @(f x);
                                  -- common case: t @{description = "..."}.
                                  -- Chains left: x @m1 @m2 =
                                  -- annotated(annotated(x, m1), m2)
applicationTerm ::= term term     -- adjacency; left-associative: add 1 2
caseTerm      ::= 'case' '(' name ')' termFields   -- default handler, if present,
                                                   -- appears LAST as the wildcard
                                                   -- field '_'
eitherTerm    ::= 'left' '(' term ')' | 'right' '(' term ')'
injectTerm    ::= 'inject' '(' name ')' termFields   -- exactly one field
lambdaTerm    ::= 'λ' binderName [':' type] '.' term   -- domain type optional
letTerm       ::= 'let' binding (',' binding)* 'in' term
listTerm      ::= '[' [term (',' term)*] ']'
literalTerm   ::= literal
mapTerm       ::= '{' '=' '}' | '{' mapEntry (',' mapEntry)* '}'
                                               -- '{=}' is the empty map
mapEntry      ::= term '=' term
optionalTerm  ::= 'given' '(' term ')' | 'none'
pairTerm      ::= '(' term ',' term ')'
projectTerm   ::= 'project' '(' name ')' '{' name '}'
recordTerm    ::= 'record' '(' name ')' termFields
setTerm       ::= '{' [term (',' term)*] '}'   -- '{}' is the empty set; nonempty
                                               -- braces: '=' in entries means map
typeApplicationTerm ::= term '⟨' type '⟩'      -- postfix
typeLambdaTerm ::= 'Λ' binderName '.' term
unitTerm      ::= 'unit'
unwrapTerm    ::= 'unwrap' '(' name ')'
variableTerm  ::= name                         -- also primitive references
wrapTerm      ::= 'wrap' '(' name ')' '{' term '}'

name          ::= bareName | '`' nameChars '`' -- backtick form per §2.4
binderName    ::= segment | '`' nameChars '`'  -- bare names in binder heads are
                                               -- single dot-free segments (§2.4)

binding       ::= name [':' '(' typeScheme ')'] ':=' term
termFields    ::= '{' [termField (',' termField)*] '}'
termField     ::= name '=' term

-- Literals (lexical forms in §2.6)
literal       ::= booleanLit | integerLit | floatLit | stringLit | binaryLit
                | decimalLit
booleanLit    ::= 'true' | 'false'
integerLit    ::= intDigits ':' intWidth
intWidth      ::= 'bigint' | 'int8' | 'int16' | 'int32' | 'int64'
                | 'uint8' | 'uint16' | 'uint32' | 'uint64'
floatLit      ::= (floatDigits | 'NaN' | 'Infinity' | '-Infinity') ':'
                  ('float32' | 'float64')
stringLit     ::= JSON string
decimalLit    ::= decimalDigits
binaryLit     ::= stringLit ':' 'binary'

-- Types (18 variants of hydra.core.Type)
type          ::= annotatedType | applicationType | effectType | eitherType
                | forallType | functionType | listType | literalType | mapType
                | optionalType | pairType | recordType | setType | unionType
                | unitType | variableType | voidType | wrapType
                | '(' type ')'

annotatedType ::= type annot
applicationType ::= type type                  -- adjacency; left-associative:
                                               -- hydra.coders.Adapter t1 t2 v1 v2 e
effectType    ::= 'effect' '<' type '>'
eitherType    ::= 'either' '<' type ',' type '>'
forallType    ::= 'forall' binder '.' type
binder        ::= binderName
                | '(' binderName ':' kind ')'  -- kinded form RESERVED (#226)
kind          ::= '*' | kind '→' kind          -- RESERVED, not yet emitted
functionType  ::= type '→' type                -- right-associative
listType      ::= 'list' '<' type '>'
literalType   ::= 'binary' | 'boolean' | 'decimal' | 'string' | intWidth
                | 'float32' | 'float64'
mapType       ::= 'map' '<' type ',' type '>'
optionalType  ::= 'optional' '<' type '>'
pairType      ::= '(' type ',' type ')'
recordType    ::= 'record' typeFields          -- row only; no nominal name
setType       ::= 'set' '<' type '>'
unionType     ::= 'union' typeFields
unitType      ::= 'unit'
variableType  ::= name
voidType      ::= 'void'
wrapType      ::= 'wrap' '(' type ')'          -- body type; no name at type level

typeFields    ::= '{' [typeField (',' typeField)*] '}'
typeField     ::= name ':' type

-- Type schemes (distinct notation from forallType)
typeScheme    ::= '∀' [binderName (',' binderName)*] '.'
                  [schemeConstraints '⇒'] type
                                               -- '∀.t' is a monomorphic scheme
schemeConstraints ::= '(' constraint (',' constraint)* ')'
constraint    ::= className typeVariable       -- e.g. ordering a
```

Notes:

- `forall` belongs to `Type.forall` (the type has "for all" in its name);
  `∀` belongs to `TypeScheme`.
  The two notations are deliberately distinct.
- Sets and maps share brace syntax.
  Nonempty braces are a map exactly when entries contain a top-level `=`;
  `{}` is the empty set and `{=}` is the empty map.
- Separators: bindings bind with `:=`; fields and map entries use `=`; type fields use `:`.
- The kinded binder form `forall (f : * → *). τ` is reserved for the kind system (#226)
  and is not yet produced by any printer.

## 4. Canonical form

The exact output of `hydra.print.core` is the canonical rendering:
spacing, parenthesization, separators, escaping, and element order.
Canonical rendering MUST be byte-identical across implementations;
this is guaranteed by translingual generation (one printer, generated into every host)
and confirmed by the cross-host formatting tests.

- Canonical output is minimally parenthesized under the precedence table of §3.
  Parenthesization beyond the minimum is grammatically valid input but is not canonical.
- Canonical spacing: a single space around `:=`, `=`, `→`, `⇒`, and between adjacent
  applications (`f x`); a space after `,`; no space around `:`, `.`, `@`, `⟨⟩`,
  or inside brackets.
- The canonical printer backtick-escapes a name if and only if
  one of the three interference cases of §2.4 holds.
- Sets and maps print their elements and entries in **canonical order**:
  ascending by the canonical total order on terms (the `ordering` class's order).
- Record and union fields print in declaration order (field order is meaningful).

## 5. Sibling notations (non-normative)

The kernel also serializes adjacent type families
(`hydra.print.{graph,typing,errors,variants,docs,paths}`).
Those notations are out of scope here and may be specified as their sections of the
specification land.

## Appendix: migration deltas (temporary)

This appendix is the implementation checklist for bringing `hydra.show.core` /
`hydra.print.core` into conformance with this specification; delete it once the
migration lands.
The current implementation differs as follows:

- The implementation namespace is still `hydra.show.*`;
  it is renamed to `hydra.print.*` (with `hydra.parse.*` for the parser side).
- Term and type application print with an `@` separator inside full parentheses;
  this specification uses adjacency.
- `@` becomes the annotation marker; annotations are currently not printed at all.
- `Type.forall` currently prints `∀`; this specification gives it the `forall` keyword.
- `TypeScheme` currently prints `forall` and `=>`; this specification gives it `∀` and `⇒`.
- Bindings currently print with `=`; this specification uses `:=`.
- Binary literals currently print as a lossy `[binary]` placeholder;
  this specification requires the base64 form.
- The case-statement default handler currently prints as a field named `[default]`;
  this specification uses the `_` wildcard.
- Canonical output is currently fully parenthesized;
  this specification requires minimal parenthesization.

Verified blast radius: one formatting-test expectation
(`packages/hydra-kernel/src/main/haskell/Hydra/Sources/Test/Generation.hs`),
the lexicon's type-application signatures,
all formatting-test expectations that pin annotation-erased output,
and the primitives-catalog signature renderer (#417).
One residual specification task lives in the JSON wire format document:
pinning the shortest-decimal tie and exponent-notation rule explicitly
(it is implicit today in the shared literal-printing primitives).
