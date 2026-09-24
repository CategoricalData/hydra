// Package strings implements the hydra.core.lib.strings primitives.
//
// Strings are Go string values, but every character-oriented operation works on
// Unicode code points (Go runes / int32), not bytes: length counts code points,
// toList/fromList bridge string <-> []any of int32, and maybeCharAt indexes by
// code point. Lists are []any; optional results use the overlay util.Optional.
package strings

import (
	gostrings "strings"
	"unicode"
	"unicode/utf8"

	"hydra.dev/hydra/overlay/go/util"
)

// Cat : list<string> -> string
func Concat(lst any) any {
	var b gostrings.Builder
	for _, s := range lst.([]any) {
		b.WriteString(s.(string))
	}
	return b.String()
}

// Cat2 : string -> string -> string
func Concat2(a any, b any) any { return a.(string) + b.(string) }

// FromList : list<int32> -> string. Each element is a Unicode code point.
func FromList(lst any) any {
	xs := lst.([]any)
	rs := make([]rune, len(xs))
	for i, x := range xs {
		rs[i] = x.(int32)
	}
	return string(rs)
}

// Intercalate : string -> list<string> -> string
func Join(sep any, lst any) any {
	xs := lst.([]any)
	parts := make([]string, len(xs))
	for i, x := range xs {
		parts[i] = x.(string)
	}
	return gostrings.Join(parts, sep.(string))
}

// Length : string -> int32. Counts Unicode code points, not bytes.
func Length(s any) any { return int32(utf8.RuneCountInString(s.(string))) }

// Lines : string -> list<string>. Splits on newline like Haskell's lines: a
// trailing newline does not produce a final empty element.
func Lines(s any) any {
	str := s.(string)
	out := []any{}
	if str == "" {
		return out
	}
	start := 0
	for i := 0; i < len(str); i++ {
		if str[i] == '\n' {
			out = append(out, str[start:i])
			start = i + 1
		}
	}
	if start < len(str) {
		out = append(out, str[start:])
	}
	return out
}

// MaybeCharAt : int32 -> string -> optional<int32>. Indexes by code point,
// returning None if the index is out of bounds.
func CharAt(i any, s any) any {
	idx := int(i.(int32))
	rs := []rune(s.(string))
	if idx < 0 || idx >= len(rs) {
		return util.None()
	}
	return util.Some(int32(rs[idx]))
}

// IsEmpty : string -> boolean
func IsEmpty(s any) any { return s.(string) == "" }

// SplitOn : string -> string -> list<string>. Splits the second string on
// occurrences of the first (delimiter), matching Data.List.Split.splitOn.
func SplitOn(delim any, s any) any {
	parts := gostrings.Split(s.(string), delim.(string))
	out := make([]any, len(parts))
	for i, p := range parts {
		out[i] = p
	}
	return out
}

// ToList : string -> list<int32>. Yields the Unicode code points.
func ToList(s any) any {
	rs := []rune(s.(string))
	out := make([]any, len(rs))
	for i, r := range rs {
		out[i] = int32(r)
	}
	return out
}

// ToLower : string -> string
func ToLower(s any) any { return gostrings.Map(unicode.ToLower, s.(string)) }

// ToUpper : string -> string
func ToUpper(s any) any { return gostrings.Map(unicode.ToUpper, s.(string)) }

// TrimSpace : string -> string
func TrimSpace(s any) any { return gostrings.TrimSpace(s.(string)) }

// Unlines : list<string> -> string. Joins with newlines and appends a trailing
// newline after each element, matching Haskell's unlines.
func Unlines(lst any) any {
	var b gostrings.Builder
	for _, s := range lst.([]any) {
		b.WriteString(s.(string))
		b.WriteByte('\n')
	}
	return b.String()
}
