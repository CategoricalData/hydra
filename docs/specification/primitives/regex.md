<!-- NOTE: this page will be automatically generated from the primitive definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Regex.hs (generator not yet
     built). Hand-authored draft under #417; the specifications here are the normative TARGET
     and may deliberately diverge from the currently-frozen definitions.
     Conventions (notation, laziness, badges, floating point) are defined in index.md. -->

# hydra.lib.regex

Regular-expression matching over strings.
Pattern syntax and semantics are Hydra-defined and translingual: they are specified once, in
Hydra's regular-expression specification ([regex.md](../regex.md)), and the same pattern means
the same thing on every host.
Matching is leftmost-longest: a match begins at the earliest possible position in the string,
and among the matches beginning at that position the longest is chosen.
`findAll`, `replaceAll`, and `split` operate on non-overlapping matches, found left to right.

#### find — **Draft**

`string → string → optional<string>`

Usage: `find pat s`

Find the first match of a pattern within a string.
Returns `given t`, where `t` is the leftmost-longest substring of `s` matching `pat`, or `none`
if `pat` does not match anywhere in `s`.
Pattern syntax and semantics are defined by Hydra's regular-expression specification
([regex.md](../regex.md)).
Behavior on a pattern that does not parse under the Hydra grammar is currently unspecified,
pending #567: the primitives will move to either-returning signatures.
<!-- [PENDING #567]: either-returning rewiring (Stage 4) -->

Since: 0.15

#### findAll — **Draft**

`string → string → list<string>`

Usage: `findAll pat s`

Find all matches of a pattern within a string.
Returns the list of all leftmost-longest, non-overlapping matches of `pat` in `s`, in the order
they appear; the list is empty if `pat` does not match anywhere.
Pattern syntax and semantics are defined by Hydra's regular-expression specification
([regex.md](../regex.md)).
Behavior on a pattern that does not parse under the Hydra grammar is currently unspecified,
pending #567: the primitives will move to either-returning signatures.
<!-- [PENDING #567]: either-returning rewiring (Stage 4) -->

Since: 0.15

#### matches — **Draft**

`string → string → boolean`

Usage: `matches pat s`

Test whether a pattern matches an entire string.
Returns `true` exactly when `pat` matches the whole of `s`; matching is anchored at both ends,
so a match of a proper substring of `s` does not suffice.
To test whether `pat` matches anywhere within `s`, use `find`: a match exists somewhere in `s`
exactly when `find pat s` is not `none`.
Pattern syntax and semantics are defined by Hydra's regular-expression specification
([regex.md](../regex.md)).
Behavior on a pattern that does not parse under the Hydra grammar is currently unspecified,
pending #567: the primitives will move to either-returning signatures.
<!-- [PENDING #567]: either-returning rewiring (Stage 4) -->

Since: 0.15

#### replace — **Draft**

`string → string → string → string`

Usage: `replace pat repl s`

Replace the first match of a pattern within a string.
Returns `s` with the leftmost-longest match of `pat` replaced by `repl`; if `pat` does not
match anywhere, `s` is returned unchanged.
Replacement-string syntax (capture-group references, literal escapes) is defined by Hydra's
regular-expression specification ([regex.md](../regex.md)); aspects not yet specified there are
pending #567.
Pattern syntax and semantics are likewise defined by [regex.md](../regex.md).
Behavior on a pattern that does not parse under the Hydra grammar is currently unspecified,
pending #567: the primitives will move to either-returning signatures.
<!-- [PENDING #567]: either-returning rewiring (Stage 4) -->

Since: 0.15

#### replaceAll — **Draft**

`string → string → string → string`

Usage: `replaceAll pat repl s`

Replace all matches of a pattern within a string.
Returns `s` with every leftmost-longest, non-overlapping match of `pat` replaced by `repl`,
proceeding left to right; if `pat` does not match anywhere, `s` is returned unchanged.
Replacement-string syntax (capture-group references, literal escapes) is defined by Hydra's
regular-expression specification ([regex.md](../regex.md)); aspects not yet specified there are
pending #567.
Pattern syntax and semantics are likewise defined by [regex.md](../regex.md).
Behavior on a pattern that does not parse under the Hydra grammar is currently unspecified,
pending #567: the primitives will move to either-returning signatures.
<!-- [PENDING #567]: either-returning rewiring (Stage 4) -->

Since: 0.15

#### split — **Draft**

`string → string → list<string>`

Usage: `split pat s`

Split a string by matches of a pattern.
Returns the list of substrings of `s` obtained by splitting at every leftmost-longest,
non-overlapping match of `pat`; the matched separators themselves are not included in the
result.
If `pat` does not match anywhere, the result is the single-element list containing `s`.
Trailing empty substrings are retained: a match at the end of the input delimits a final
empty element, so splitting `"a,b,"` on the pattern `,` yields `["a", "b", ""]`.
This matches `hydra.lib.strings.splitOn`, and distinguishes inputs that differ only by a
trailing separator.
Pattern syntax and semantics are defined by Hydra's regular-expression specification
([regex.md](../regex.md)).
Behavior on a pattern that does not parse under the Hydra grammar is currently unspecified,
pending #567: the primitives will move to either-returning signatures.
<!-- [PENDING #567]: either-returning rewiring (Stage 4) -->

Since: 0.15
