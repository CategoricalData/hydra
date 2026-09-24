// Package chars implements the hydra.core.lib.chars primitives.
//
// All functions operate on Unicode code points represented as int32 (Go runes),
// matching the Haskell reference which works over Data.Char applied to the code
// point. Predicates return a Go bool; classifiers preserve non-cased code points.
package chars

import "unicode"

// IsAlpha : int32 -> boolean
func IsAlpha(c any) any { return unicode.IsLetter(rune(c.(int32))) }

// IsAlphaNum : int32 -> boolean
func IsAlphaNum(c any) any {
	r := rune(c.(int32))
	return unicode.IsLetter(r) || unicode.IsNumber(r)
}

// IsDigit : int32 -> boolean. ASCII decimal digit only, matching Data.Char.isDigit.
func IsDigit(c any) any {
	r := rune(c.(int32))
	return r >= '0' && r <= '9'
}

// IsLower : int32 -> boolean
func IsLower(c any) any { return unicode.IsLower(rune(c.(int32))) }

// IsSpace : int32 -> boolean
func IsSpace(c any) any { return unicode.IsSpace(rune(c.(int32))) }

// IsUpper : int32 -> boolean
func IsUpper(c any) any { return unicode.IsUpper(rune(c.(int32))) }

// ToLower : int32 -> int32
func ToLower(c any) any { return int32(unicode.ToLower(rune(c.(int32)))) }

// ToUpper : int32 -> int32
func ToUpper(c any) any { return int32(unicode.ToUpper(rune(c.(int32)))) }
