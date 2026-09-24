// Package regex implements the hydra.core.lib.regex primitives.
//
// Regex syntax is host-defined; Go's regexp package is RE2 (no backreferences
// or lookaround). The kernel's own patterns (namespace/identifier validation)
// are RE2-compatible. If a cross-host regex test requires a PCRE-only feature,
// that is a conformance question to escalate, not to work around here.
//
// Patterns are compiled per call. Invalid patterns are a program error and
// panic rather than being silently ignored.
package regex

import (
	"regexp"

	"hydra.dev/hydra/overlay/go/util"
)

func compile(pattern any) *regexp.Regexp {
	re, err := regexp.Compile(pattern.(string))
	if err != nil {
		panic("hydra.core.lib.regex: invalid pattern " + pattern.(string) + ": " + err.Error())
	}
	return re
}

// Find : string -> string -> optional<string>. Returns the first match, or
// none when the pattern does not match.
func Find(pattern any, input any) any {
	re := compile(pattern)
	loc := re.FindStringIndex(input.(string))
	if loc == nil {
		return util.None()
	}
	return util.Some(input.(string)[loc[0]:loc[1]])
}

// FindAll : string -> string -> list<string> (all non-overlapping matches)
func FindAll(pattern any, input any) any {
	ms := compile(pattern).FindAllString(input.(string), -1)
	out := make([]any, len(ms))
	for i, m := range ms {
		out[i] = m
	}
	return out
}

// Matches : string -> string -> boolean (whole/partial match per RE2 semantics)
func Matches(pattern any, input any) any {
	return compile(pattern).MatchString(input.(string))
}

// Replace : string -> string -> string -> string (replace the first match)
func Replace(pattern any, replacement any, input any) any {
	re := compile(pattern)
	s := input.(string)
	loc := re.FindStringIndex(s)
	if loc == nil {
		return s
	}
	return s[:loc[0]] + replacement.(string) + s[loc[1]:]
}

// ReplaceAll : string -> string -> string -> string (replace all matches)
func ReplaceAll(pattern any, replacement any, input any) any {
	return compile(pattern).ReplaceAllLiteralString(input.(string), replacement.(string))
}

// Split : string -> string -> list<string> (split input on the pattern)
func Split(pattern any, input any) any {
	parts := compile(pattern).Split(input.(string), -1)
	out := make([]any, len(parts))
	for i, p := range parts {
		out[i] = p
	}
	return out
}
