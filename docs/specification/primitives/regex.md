<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.lib.regex

<!-- Note: this is an automatically generated file. Do not edit. -->

Pattern syntax and semantics are Hydra-defined and translingual: they are specified once, in Hydra's regular-expression specification, and the same pattern means the same thing on every host.

Matching is leftmost-longest: a match begins at the earliest possible position in the string, and among the matches beginning at that position the longest is chosen.

findAll, replaceAll, and split operate on non-overlapping matches, found left to right.

## hydra.core.lib.regex.find

Find the first regex match within a string, returning the matched substring if any.

`∀.string → string → optional<string>`

*Since: 0.15.*

find(pat, s) returns Just(t) where t is the leftmost-longest substring of s matching pat, or Nothing if pat does not match anywhere in s.

Pattern syntax and semantics are Hydra-defined and translingual; see docs/specification/regex.md.

Total; a pattern that does not parse under the Hydra regex grammar is treated as not matching anywhere, the same as a well-formed pattern with no match.

**[HYDRA-CORE-LIB-REGEX-FIND-RETURNS-LEFTMOST-LONGEST-MATCH] (requirement) **find returns the leftmost-longest substring matching the pattern, wrapped, when the pattern matches anywhere in the string, including alternation where the longer branch wins and greedy quantifiers that pick the longest run.

**[HYDRA-CORE-LIB-REGEX-FIND-NONE-WHEN-NO-MATCH] (requirement) **find returns none when the pattern does not match anywhere in the string, including on the empty string, and when the pattern is ill-formed under the Hydra regex grammar (treated the same as a well-formed pattern with no match, never a host-specific parse error).

## hydra.core.lib.regex.findAll

Find all non-overlapping regex matches within a string.

`∀.string → string → list<string>`

*Since: 0.15.*

findAll(pat, s) returns the list of all leftmost-longest, non-overlapping matches of pat in s, in the order they appear. Returns the empty list if pat does not match anywhere.

Pattern syntax and semantics are Hydra-defined and translingual; see docs/specification/regex.md.

Total; an unparseable pattern yields the empty list, the same as a well-formed pattern with no matches.

**[HYDRA-CORE-LIB-REGEX-FIND-ALL-RETURNS-ALL-NON-OVERLAPPING-MATCHES-IN-ORDER] (requirement) **findAll returns every leftmost-longest, non-overlapping match of the pattern in the string, in the order they appear.

**[HYDRA-CORE-LIB-REGEX-FIND-ALL-EMPTY-LIST-WHEN-NO-MATCH] (requirement) **findAll returns the empty list when the pattern does not match anywhere, including when the pattern is ill-formed under the Hydra regex grammar (treated the same as a well-formed pattern with no matches).

## hydra.core.lib.regex.matches

Test whether a regex matches an entire string.

`∀.string → string → boolean`

*Since: 0.15.*

matches(pat, s) returns true iff pat matches the whole of s (anchored at both ends; a match of a proper substring of s does not suffice). To test whether pat matches anywhere within s, use find.

Pattern syntax and semantics are Hydra-defined and translingual; see docs/specification/regex.md.

Total; an unparseable pattern yields false, the same as a well-formed pattern with no match.

**[HYDRA-CORE-LIB-REGEX-MATCHES-IS-WHOLE-STRING-ANCHORED] (requirement) **matches is always anchored to the whole string at both ends, regardless of explicit ^/$ in the pattern: a match of a proper substring does not suffice, and ^/$ never match at an internal line break (string-boundary anchors, not line-boundary).

**[HYDRA-CORE-LIB-REGEX-MATCHES-TESTS-MINIMAL-CORE-GRAMMAR] (requirement) **matches correctly evaluates every construct in the minimal-core regex grammar: literals, character classes (including negation and ranges), the wildcard (matching any character including newline), quantifiers (?, *, +, {n}, {n,}, {n,m}), alternation (leftmost-longest preference, not leftmost-first), and non-capturing grouping (including nested and quantified groups).

**[HYDRA-CORE-LIB-REGEX-MATCHES-TREATS-ESCAPED-METACHARACTERS-AS-LITERALS] (requirement) **matches treats a backslash-escaped metacharacter as the literal character, both at top level and inside a character class, and treats an unescaped dash as a literal at top level (never forming a range outside a class) while forming a range only inside a class.

**[HYDRA-CORE-LIB-REGEX-MATCHES-FALSE-WHEN-NO-MATCH-OR-ILL-FORMED] (requirement) **matches returns false when the pattern does not match the whole string, including when the pattern is ill-formed under the Hydra regex grammar (treated the same as a well-formed pattern with no match, never a host-specific parse error) and for constructs outside the minimal core (such as the {,m} quantifier form).

## hydra.core.lib.regex.replace

Replace the first regex match in a string with a replacement string.

`∀.string → string → string → string`

*Since: 0.15.*

replace(pat, repl, s) returns s with the first leftmost-longest match of pat replaced by repl. If pat does not match, s is returned unchanged.

Replacement-string syntax (capture-group references such as $1 or \\1, literal escapes) is host-defined.

Pattern syntax and semantics are Hydra-defined and translingual; see docs/specification/regex.md.

Total; an unparseable pattern leaves s unchanged, the same as a well-formed pattern with no match.

**[HYDRA-CORE-LIB-REGEX-REPLACE-REPLACES-LEFTMOST-LONGEST-MATCH] (requirement) **replace replaces the first (leftmost-longest) match of the pattern with the replacement string, including an anchored match at the start of the string.

**[HYDRA-CORE-LIB-REGEX-REPLACE-UNCHANGED-WHEN-NO-MATCH] (requirement) **replace returns the string unchanged when the pattern does not match anywhere, including when the pattern is ill-formed under the Hydra regex grammar (treated the same as a well-formed pattern with no match).

## hydra.core.lib.regex.replaceAll

Replace all non-overlapping regex matches in a string with a replacement string.

`∀.string → string → string → string`

*Since: 0.15.*

replaceAll(pat, repl, s) returns s with every leftmost-longest, non-overlapping match of pat replaced by repl. If pat does not match anywhere, s is returned unchanged.

Replacement-string syntax is host-defined; see replace for the caveat.

Pattern syntax and semantics are Hydra-defined and translingual; see docs/specification/regex.md.

Total; an unparseable pattern leaves s unchanged, the same as a well-formed pattern with no match.

**[HYDRA-CORE-LIB-REGEX-REPLACE-ALL-REPLACES-EVERY-NON-OVERLAPPING-MATCH] (requirement) **replaceAll replaces every leftmost-longest, non-overlapping match of the pattern with the replacement string, including when the replacement string is empty.

**[HYDRA-CORE-LIB-REGEX-REPLACE-ALL-UNCHANGED-WHEN-NO-MATCH] (requirement) **replaceAll returns the string unchanged when the pattern does not match anywhere, including when the pattern is ill-formed under the Hydra regex grammar (treated the same as a well-formed pattern with no match).

## hydra.core.lib.regex.split

Split a string by occurrences of a regex pattern.

`∀.string → string → list<string>`

*Since: 0.15.*

split(pat, s) returns the list of substrings of s obtained by splitting on every leftmost-longest, non-overlapping match of pat.

Trailing empty splits are host-defined (some engines retain them, some discard them; for portable code, do not rely on the trailing-empty behavior).

Pattern syntax and semantics are Hydra-defined and translingual; see docs/specification/regex.md.

Total; an unparseable pattern yields the single-element list containing s, the same as a well-formed pattern with no match.

**[HYDRA-CORE-LIB-REGEX-SPLIT-SPLITS-ON-EVERY-NON-OVERLAPPING-MATCH] (requirement) **split returns the substrings of the string obtained by splitting on every leftmost-longest, non-overlapping match of the pattern, including splitting on a fixed delimiter, on a character class, and splitting into a trailing empty substring when the pattern matches at the end of the string.

**[HYDRA-CORE-LIB-REGEX-SPLIT-SINGLE-ELEMENT-LIST-WHEN-NO-MATCH-OR-ILL-FORMED] (requirement) **split returns the single-element list containing the original string unchanged when the pattern does not match anywhere, including when the pattern is ill-formed under the Hydra regex grammar (treated the same as a well-formed pattern with no match).
