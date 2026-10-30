# Hydra textual syntax

**Status: Draft** · Part of the [Hydra specification](https://github.com/CategoricalData/hydra/issues/579) ·
Specification half of [#497](https://github.com/CategoricalData/hydra/issues/497)
(the parser and round-trip validation remain in #497, which implements this document)

This document specifies the Hydra textual notation for terms, types, literals, and type schemes:
the presentation syntax used throughout the specification,
produced by the `hydra.core.print.*` kernel functions.
A general parser for this notation is planned ([#497](https://github.com/CategoricalData/hydra/issues/497)) but not yet part of the kernel;
`hydra.core.parse.*` currently covers only docs and regular expressions.
The grammar is unambiguous, with stated precedence, and is normative for both directions:
a conforming printer MUST emit text matching the canonical form (§4),
and a conforming parser MUST accept every production of the grammar (§3).

The key words MUST, MUST NOT, SHOULD, and MAY are to be interpreted as described in RFC 2119.
Named **provisions** (`HYDRA-SYN-…`, shown in bold at the head of a claim) follow the provisions
convention in [index.md](index.md#provisions).

## 1. The round-trip contract

**[HYDRA-SYN-ROUND-TRIP]** Every expression round-trips: parsing printed text recovers the original
term or type exactly, with everything semantically significant intact — including annotations. Only
non-significant surface may change on a round trip: whitespace, parenthesization, and comments.

Consequences:

- **[HYDRA-SYN-PRINTER-FAITHFUL]** The printer is always faithful: there is no lossy printer mode. If a
  term has annotations, the printed text shows them, and parsing recovers them.
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

**Typing the Unicode operators.**
Because the grammar is Unicode-only, every program needs `λ`, `Λ`, `∀`, `→`, `⇒`, `⟨`, and `⟩`,
none of which are on a standard keyboard.
The Hydra VS Code extension ([#806](https://github.com/CategoricalData/hydra/issues/806)) supplies
them through a Lean/Agda-style abbreviation: type a backslash followed by a name and accept the
completion — `\lambda` or `\l` inserts `λ`, `\Lambda` / `\L` inserts `Λ`, `\forall` / `\all` inserts
`∀`, `\to` / `\r` inserts `→`, `\Rightarrow` / `\R` inserts `⇒`, and `\langle` / `\rangle`
(or `\<` / `\>`) insert `⟨` / `⟩`.
The same prefixes are also available as plain snippets.
The extension's TextMate grammar (`source.hydra`) is the single source of truth for highlighting,
and is intended to be reused by the IntelliJ plugin, the documentation site, and a future GitHub
Linguist entry; editors without the extension can still enter these characters by any system input
method, since the operators are ordinary Unicode code points.

### 2.4 Names

A *bare* name is a nonempty sequence of dot-separated segments of alphanumeric characters.
Outside binder heads, bare dotted names are read greedily as qualified names
(`hydra.core.model.Term` is one name).

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
A dotted name there must be escaped: ``λx:`hydra.core.model.Term`.body``.
Inside nested delimiters — `<…>`, `(…)`, or backticks — dotted names read greedily as usual:
`λx:list<hydra.core.model.Term>.body` needs no escaping.

### 2.5 Strings

String literals use the JSON string syntax and escape rules,
exactly as specified in the [JSON wire format](json-format.md#string-escapes):
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
  The canonical digit string is defined by the `printDecimal` primitive, which is
  **representation-faithful**: decimal values are scale-distinct (an integer coefficient
  plus a scale, so `1.10` and `1.1` are distinct values), coefficient digits are
  preserved exactly — trailing zeros included — and zero prints per its scale
  (`0`, `0.0`, `0.00`).
  Layout follows the ECMAScript `Number::toString` (§6.1.6.1.20) / RFC 8785 (JCS)
  thresholds and spellings:
  with `a` the adjusted exponent (decimal position of the leading significant digit),
  positional form when `−6 ≤ a < 21`,
  and exponent form otherwise (one digit before the point, lowercase `e`, always-signed
  exponent, coefficient digits preserved: `1.10e+30`, `6.02e-23`).
  `parseDecimal` is scale-preserving (`"1.10"` and `"1.1"` parse to distinct values) and
  accepts the full number grammar above.
  The JSON wire format's decimal encoding is this same form:
  the JSON printer and parser call `printDecimal`/`parseDecimal` directly.
  Exponent parts bind to the numeric token by maximal munch:
  `1.2e9` is one decimal literal, while `1.2 e9` is the application of `1.2` to the name `e9`.
- **Binary**: a base64 string with the `binary` suffix — `"aGVsbG8=":binary`.
  Standard base64 alphabet with padding, exactly as specified by the
  [JSON wire format](json-format.md)'s binary encoding.
- The character `-` occurs only inside numeric literals (there are no infix operators),
  so a form such as `f -1:int32` lexes unambiguously.

### 2.7 Reserved words

The following bare names are reserved in all positions.
To use one as a name, write it backticked.

```
_ as bigint binary boolean cases decimal effect either false float32 float64
forall from given impure in import inject int8 int16 int32 int64 lazy left let
list map module none optional partial primitive project record right set
string term true type uint8 uint16 uint32 uint64 union unit unwrap void wrap
NaN Infinity
```

This includes the module-grammar keywords of §6 (`module`, `import`, `as`, `from`, `type`, `term`,
`primitive`, `impure`, `partial`) alongside the term/type grammar's own keywords (§3) — reserving
both sets globally, rather than only at each keyword's own syntactic position, keeps the grammar's
adjacency-based type/term application (§3) unambiguous at every definition boundary: a definition's
trailing type or term would otherwise be indistinguishable from an application that consumes the
next definition's keyword as an argument.

Validation SHOULD reject reserved names in positions that quoting cannot rescue,
in particular `_` as a user field or variant name (it would collide with the case-default wildcard).

## 3. Grammar

Precedence, tightest to loosest:

1. postfix: `@` annotation, `⟨T⟩` type application
2. application: adjacency, left-associative
3. infix operator sugar (RESERVED, #811): `!`/`¬` prefix, then (tightest to loosest)
   `*` `/`, `+` `-`, `++` (concat, exact token TBD), comparison (`<` `<=` `==` `>=` `>`),
   `&&`, `||`, `|>` — all looser than application and tighter than `→`, so
   `f x + g y` parses as `(f x) + (g y)`, and `a + b → c` parses as `(a + b) → c`.
   The relative order among these operator tiers follows conventional precedence
   (arithmetic tighter than comparison, comparison tighter than boolean, pipeline
   loosest) and is not otherwise stated in #811; it is not yet exercised by any
   printer or parser.
4. `→`: right-associative
5. binder bodies (`λ`, `Λ`, `let`, `forall`, `∀`): extend maximally to the right

`(` … `)` overrides precedence anywhere.
A parenthesized single term or type is grouping; with a comma it is a pair.

```
-- Terms (21 variants of hydra.core.model.Term)
term          ::= annotatedTerm | applicationTerm | caseTerm | eitherTerm
                | injectTerm | lambdaTerm | letTerm | listTerm | literalTerm
                | mapTerm | optionalTerm | pairTerm | projectTerm | recordTerm
                | setTerm | typeApplicationTerm | typeLambdaTerm | unitTerm
                | unwrapTerm | variableTerm | wrapTerm
                | '(' term ')'
                | operatorTerm | doTerm   -- SUGAR, RESERVED (#811), not yet implemented

annotatedTerm ::= term annot
annot         ::= '@' term        -- postfix; payload parsed at tightest level,
                                  -- parenthesize larger payloads: t @(f x);
                                  -- common case: t @{description = "..."}.
                                  -- Chains left: x @m1 @m2 =
                                  -- annotated(annotated(x, m1), m2)
applicationTerm ::= term term     -- adjacency; left-associative: add 1 2
caseTerm      ::= 'cases' '(' name ')' termFields   -- default handler, if present,
                                                    -- appears LAST as the wildcard
                                                    -- field '_'
eitherTerm    ::= 'left' '(' term ')' | 'right' '(' term ')'
injectTerm    ::= 'inject' '(' name ')' termFields   -- exactly one field
                | name sugarFields                   -- SUGAR (#809): one field;
                                                     -- disambiguated from a record
                                                     -- by the schema (union type)
lambdaTerm    ::= 'λ' binderName [':' type] '.' term   -- domain type optional
                | 'λ' pattern '.' term          -- SUGAR, RESERVED (#811): destructuring
                                                -- lambda; λ(x,y).body = λp.let (x,y):=p in body
letTerm       ::= 'let' binding (',' binding)* 'in' term
listTerm      ::= '[' [term (',' term)*] ']'
literalTerm   ::= literal
mapTerm       ::= '{' '=' '}' | '{' mapEntry (',' mapEntry)* '}'
                                               -- '{=}' is the empty map
mapEntry      ::= term '=' term
optionalTerm  ::= 'given' '(' term ')' | 'none'
pairTerm      ::= '(' term ',' term ')'
projectTerm   ::= 'project' '(' name ')' '{' name '}'
                | name '[' name ']'            -- SUGAR (#809): Person[firstName]
recordTerm    ::= 'record' '(' name ')' termFields
                | name sugarFields             -- SUGAR (#809): Person{a = 1, b}
setTerm       ::= '{' [term (',' term)*] '}'   -- '{}' is the empty set; nonempty
                                               -- braces: '=' in entries means map
typeApplicationTerm ::= term '⟨' type '⟩'      -- postfix
typeLambdaTerm ::= 'Λ' binderName '.' term
unitTerm      ::= 'unit'
unwrapTerm    ::= 'unwrap' '(' name ')'
                | name '[' ']'                 -- SUGAR (#809): Meters[]
variableTerm  ::= name                         -- also primitive references
wrapTerm      ::= 'wrap' '(' name ')' '{' term '}'
                | name '{' term '}'            -- SUGAR (#809): Meters{100:int32};
                                               -- the term must NOT be field-shaped
                                               -- (a lone name is a one-field record,
                                               -- not a wrapped variable)

name          ::= bareName | '`' nameChars '`' -- backtick form per §2.4
binderName    ::= segment | '`' nameChars '`'  -- bare names in binder heads are
                                               -- single dot-free segments (§2.4)

binding       ::= name [':' '(' typeScheme ')'] ':=' term
                | pattern ':=' term             -- SUGAR, RESERVED (#811): destructuring
                                                -- let; no type ascription on a pattern
termFields    ::= '{' [termField (',' termField)*] '}'
termField     ::= name '=' term
sugarFields   ::= '{' [sugarField (',' sugarField)*] '}'   -- #809; no whitespace
                                                           -- between name and '{'
sugarField    ::= name '=' term | name        -- a bare 'name' ⇒ 'name = unit'

-- Destructuring patterns (SUGAR, RESERVED (#811), not yet implemented): a pattern
-- stands for a single fresh parameter immediately taken apart by a 'let', e.g.
-- 'λ(x,y).body' desugars to 'λp.let (x,y):=p in body'; there is no new AST node,
-- so the printer does not reconstruct a pattern from the desugared let/lambda.
pattern       ::= '(' name ',' name ')'         -- pair destructuring
                | '{' name (',' name)* '}'      -- record destructuring (field names
                                               -- double as the bound variable names)

-- Infix operator sugar (SUGAR, RESERVED (#811), not yet implemented): parse-only;
-- the parser desugars each operator immediately to the primitive-application form
-- shown, and the printer continues to emit only that prefix form (§1 round-trip
-- carve-out: see the Notes below). Precedence is given in this section's table above.
operatorTerm  ::= term arithOp term             -- 'x + y' ⇒ 'math.add x y', etc.
                | term compareOp term           -- 'x < y' ⇒ 'ordering.lt x y', etc.
                | term '&&' term                -- ⇒ 'logic.and x y'
                | term '||' term                -- ⇒ 'logic.or x y'
                | '!' term                      -- prefix; ⇒ 'logic.not x'
                                                -- ('¬' also admissible, per §2.3's
                                                -- Unicode-only convention; token TBD
                | term '++' term                -- concat; exact token TBD, see #811
                | term '|>' term (' |>' term)*  -- pipeline; 'x |> f |> g' ⇒ 'g (f x)'
arithOp       ::= '+' | '-' | '*' | '/'         -- '-' as binary minus: exact
                                               -- primitive TBD, see #811
compareOp     ::= '<' | '<=' | '==' | '>=' | '>'

-- Monadic 'do' block (SUGAR, RESERVED (#811), not yet implemented): parse-only,
-- desugaring immediately (driven by the monad tag, not by inference) to nested
-- calls on the tagged monad's 'bind' primitive. 'map' is excluded (no lawful
-- uniform 'bind' over a two-parameter map). An untagged 'do', resolved by a
-- Monad typeclass at inference time, is a separate, not-yet-tracked follow-up.
doTerm        ::= 'do' doTag '{' doClause (',' doClause)* '}'
doTag         ::= 'either' | 'list' | 'optional' | 'effect'
doClause      ::= name ':=' term                -- a bind; desugars to <tag>.bind
                | term                          -- the final result (last clause only)

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

-- Types (18 variants of hydra.core.model.Type)
type          ::= annotatedType | applicationType | effectType | eitherType
                | forallType | functionType | listType | literalType | mapType
                | optionalType | pairType | recordType | setType | unionType
                | unitType | variableType | voidType | wrapType
                | '(' type ')'

annotatedType ::= type annot
applicationType ::= type type                  -- adjacency; left-associative:
                                               -- hydra.core.coders.Adapter t1 t2 v1 v2 e
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

-- Term signatures (hydra.core.typing.TermSignature; distinct from typeScheme):
-- the notation for a term or primitive definition's ': <signature>' ascription.
-- 'lazy' marks an individual parameter's Parameter.isLazy; it is NOT part of the
-- general 'type' production above and has no meaning outside this position
-- (#800) -- a bare type (a let-binding's ascription, a type alias, etc.) never
-- carries 'lazy'.
termSignature ::= '∀' [binderName (',' binderName)*] '.'
                  [schemeConstraints '⇒'] signatureArrow
signatureArrow ::= [lazy] type '→' signatureArrow | type
                                               -- right-associative; 'lazy' may
                                               -- prefix any parameter position
                                               -- (all but the last, which is
                                               -- the signature's result type)
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
- `termSignature`'s header (`∀` ... `⇒`) is identical to `typeScheme`'s; only the body differs,
  since a `TermSignature`'s parameters (unlike a bare `type`'s function-arrow chain) can each
  carry a laziness flag. Example: `primitive ifElse : ∀t. boolean → lazy t → lazy t → t`.
- **Nominal sugar (#809).** A type name written immediately before `{...}` or `[...]` (no
  intervening whitespace) is abbreviated nominal construction/elimination, dropping the
  `record(…)`/`inject(…)`/`wrap(…)`/`project(…)`/`unwrap(…)` keyword wrapper. The keyword forms
  remain valid — this is additive sugar.
  - `Name sugarFields` (e.g. `Person{firstName = "Alice", age = 30:int32}`) is **record**
    construction; a bare field `contactInfo` abbreviates `contactInfo = unit`.
  - The same `Name sugarFields` with a single field over a **union** type is **injection**
    (`Color{red}`, `Length{meters = 1.0:float64}`). It is syntactically identical to a one-field
    record; the two are told apart by the **schema**: the reader parses the form as a record and,
    against the type environment (the module's own type definitions, plus imported schema), rewrites
    a one-field record over a union type into an injection. Without a schema (e.g. parsing a bare
    term with no module context), the form stays a record. A multi-field construct over a union type
    is malformed and left for type checking.
  - `Name '{' term '}'` with a non-field-shaped term is **wrap** (`Meters{100:int32}`). A lone name
    inside the braces is a one-field record, not a wrapped variable, so wrapping a *variable* keeps
    the keyword form `wrap(Name){x}`.
  - `Name '[' name ']'` is **projection** (`Person[firstName]`); `Name '[' ']'` is **unwrap**
    (`Meters[]`). Both are unambiguous from syntax alone (no schema needed): `[` cannot occur inside
    a name, so there is no collision with qualified variable names.
  - The whitespace rule is load-bearing: `Name{…}` / `Name[…]` (abutting) are sugar; `Name {…}` /
    `Name […]` (with a space) are ordinary application of `Name` to a set/map/list.
- **Operator, destructuring, and `do`-block sugar (RESERVED, #811).** `operatorTerm`, `pattern`
  (destructuring in `lambdaTerm`/`binding`), and `doTerm` are reserved for #811 and are not yet
  produced or accepted by any printer or parser. Several spellings are explicitly unsettled in
  #811 itself rather than decided here: binary `-`'s underlying primitive, the concat operator's
  token (candidate `++`), and whether `!` or `¬` is the boolean-negation spelling. `case`
  extended to match nested shapes (not just the top-level variant tag) is noted in #811 as
  in-scope but not yet designed, and is intentionally omitted from this grammar.
  This sugar is **parse-only when it lands**: #811 states the parser will desugar it immediately
  to the existing primitive-application/`let`/`λ`/`cases` forms shown above, with no new `Term`
  AST node, and the printer will not re-emit the sugared spelling. This is a scoped carve-out of
  §1's round-trip contract, parallel to the existing `ModuleDependency` alias carve-out (§6.4):
  the grammar describes what the parser will accept, not a new printed canonical form.

## 4. Canonical form

The exact output of `hydra.core.print.model` is the canonical rendering:
spacing, parenthesization, separators, escaping, and element order.
**[HYDRA-SYN-CANONICAL-BYTE-IDENTICAL]** Canonical rendering is byte-identical across implementations;
this is guaranteed by translingual generation (one printer, generated into every host)
and confirmed by the cross-host formatting tests.

- Canonical output is minimally parenthesized under the precedence table of §3.
  Parenthesization beyond the minimum is grammatically valid input but is not canonical.
- Canonical spacing: a single space around `:=`, `=`, `→`, `⇒`, and between adjacent
  applications (`f x`); a space after `,`; no space around `:`, `.`, `@`, `⟨⟩`,
  or inside brackets.
- The canonical printer backtick-escapes a name if and only if
  one of the three interference cases of §2.4 holds.
- **[HYDRA-SYN-CANONICAL-NOMINAL-SUGAR]** The canonical printer emits the nominal sugar (#809), not
  the keyword wrapper, for record (`Name{…}`), injection (`Name{…}`), projection (`Name[field]`),
  and unwrap (`Name[]`). Record/injection fields print `name = term`, with a unit-valued field
  abbreviated bareword `name`. Wrap prints `Name{term}` except when the wrapped body is a bare
  variable, which would reparse as a one-field record; that single case prints the keyword form
  `wrap(Name){x}`. Because injection is distinguished from a one-field record only by the schema,
  round-tripping an injection through `print → parse` reproduces the injection only when the reader
  has the same type environment; parsed without a schema, the printed `Name{field}` reads back as a
  one-field record (the canonical string is identical either way).
- **[HYDRA-SYN-CANONICAL-COLLECTION-ORDER]** Sets and maps print their elements and entries in
  canonical order: ascending by the canonical total order on terms — the `ordering` class's order,
  specified in [ordering-and-equality.md](ordering-and-equality.md)
  (one structural rule over records/unions/wrappers, per-type rules for literals and
  built-in constructors; note the decimal tiebreak: numerically equal decimals order
  by scale, smaller first, `1.1 < 1.10`).
- **[HYDRA-SYN-FIELD-DECLARATION-ORDER]** Record and union fields print in declaration order (field
  order is meaningful).

## 5. Sibling notations (non-normative)

The kernel also serializes adjacent type families
(`hydra.core.print.{graph,typing,errors,variants,docs,paths}`).
Those notations are out of scope here and may be specified as their sections of the
specification land.

## 6. Module-level grammar

A module is a namespace, a list of dependencies on other modules, and a list of definitions
(§6.1), each optionally preceded by a `##` doc-comment block (§6.2). Dependencies may be
imported bare or under a local alias (§6.3), chosen by a module-wide collision-avoidance rule
(§6.4) that this specification does not need to restate in full (it governs *which* imports are
aliased, not the syntax of an import line). Produced and consumed by the
`hydra.core.print.packaging` and `hydra.core.parse.packaging` kernel functions.

### 6.1 Grammar

```
module        ::= moduleDoc? moduleHeader importDecl* definition*

moduleHeader  ::= 'module' moduleName
moduleName    ::= name                          -- dotted; read greedily per §2.4

importDecl    ::= 'import' moduleName [importAlias] [importPackage]
importAlias   ::= 'as' name                      -- bare or dotted; see §6.3
importPackage ::= 'from' name                     -- disambiguates when the same module
                                                  -- name is published by more than one
                                                  -- package

definition    ::= typeDef | termDef | primitiveDef
typeDef       ::= defDoc? 'type' localName '=' typeScheme
termDef       ::= defDoc? 'term' localName [':' termSignature] '=' term
primitiveDef  ::= defDoc? ['impure'] ['partial'] 'primitive' localName ':' termSignature
                  ['=' term]                      -- optional default implementation

localName     ::= segment | '`' nameChars '`'     -- unqualified; qualified under
                                                  -- moduleName on definition, per §6.1's
                                                  -- own module namespace
```

### 6.2 Doc comments

```
moduleDoc     ::= docSection ('\n##\n' docSection)*
defDoc        ::= docSection ('\n##\n' docSection)*
docSection    ::= docLine ('\n' docLine)*
docLine       ::= '## ' docText | '##'            -- '##' alone is a blank separator line
```

A `##` line (distinct from the whitespace-class `#` line comment of §2.2) immediately above a
module header or a `type`/`term`/`primitive` definition is a doc comment: unlike an ordinary
comment, it is structured `EntityMetadata`, not discarded commentary (see the Notes below for
the current printer/parser asymmetry). Printed sections, separated by a blank `##` line, appear
in a fixed order: the description, then each free-form comment, then a single block of `@key`
lines (`@seeAlso`, `@availableSince`, `@deprecatedSince`, `@requirement`, `@recommendation`), in
that order. A `@seeAlso` line's payload is one of `type <Name>`, `term <name>`,
`primitive <name>`, `module <moduleName>`, `package <packageName>`, `term-expr <text>`, or
`type-expr <text>`, naming the kind of thing referenced. Order among the `@key` lines themselves
is not semantically significant, but the printer's fixed order keeps output deterministic.

Record and union fields (§3) carry only a plain description line (one `## ...` line, directly
above the field) — the richer `@key` forms above are module- and definition-level only.

### 6.3 Import aliasing

An import is written bare (`import <module>`) unless a local-name collision requires an alias
(`import <module> as <alias>`); the choice is the printer's to make, not the author's, each time
a module is printed (§6.4). An alias may itself be multi-segment (e.g. `as error.file`), when a
shorter candidate alias collides. The `from <package>` clause disambiguates a module name that
more than one package publishes; it is independent of aliasing and may appear on either a bare
or an aliased import.

### 6.4 Alias selection (informative)

The alias (or bareness) of each import is chosen by the printer, not carried in the AST: a
module dependency has no alias field of its own. Printing a module computes, for every imported
module, whether it can stay bare — leaving it unqualified introduces no local-name collision,
either against another import or against the printed module's own declared names — and resolves
the remaining collisions with the shortest-unique-dotted-suffix available, preferring to leave
the more heavily referenced side of a collision bare. This specification states the *syntax* two
import lines may take (§6.3); the collision-resolution algorithm itself is an implementation
detail of the printer, not part of the grammar.

### Notes

- Reserved words: `module`, `import`, `as`, `from`, `type`, `term`, `primitive`, `impure`,
  `partial` are reserved globally (§2.7), not only at their own syntactic position, so that a
  definition's trailing type or term cannot swallow the next definition's keyword as an argument.
- `impure`/`partial` default to absent, meaning pure and total respectively; both prefix keywords
  are printed only when `false`, and `impure` precedes `partial` when both are present.
- A `term`/`primitive` definition's `: termSignature` ascription is the term-signature notation
  of §3 (`∀` header, `lazy` parameters), not a plain `type`; a `type` definition's own `=`
  right-hand side is always a `typeScheme`, never a `termSignature`.
- **Bare-import round-trip gap (#805).** The printer may leave two colliding imports split bare
  vs. aliased (the one with fewer references gets the alias; see §6.4). A single-file parser,
  with no access to a bare-imported module's own definitions, cannot always re-qualify a bare
  name back to the bare-imported module in that collision case — `print → parse → print` is
  textually stable when no collision exists, but not guaranteed stable across a reparse in the
  collision case. This is a tracked, open gap, not a design decision to leave unfixed.
- The parser does not yet reconstruct `EntityMetadata` from `##` comments (§6.2): a parsed
  module's and every parsed definition's metadata is always `Nothing`, matching the printer's
  intentional omission of doc-comment output when metadata is absent. Doc-comment printing
  (§6.2) is implemented; doc-comment parsing is not yet.

## Appendix: migration deltas (temporary)

This appendix is the implementation checklist for bringing `hydra.core.print.model`
into conformance with this specification; delete it once the migration lands.
The current implementation differs as follows:

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
