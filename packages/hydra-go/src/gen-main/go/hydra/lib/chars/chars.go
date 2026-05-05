// Note: this is an automatically generated file. Do not edit.

package chars

import "unicode"

// hydra.lib.chars.isAlphaNum : Int32 -> Boolean
func IsAlphaNum(c any) any {
	r := rune(c.(int32))
	return unicode.IsLetter(r) || unicode.IsDigit(r)
}

// hydra.lib.chars.isLower : Int32 -> Boolean
func IsLower(c any) any {
	return unicode.IsLower(rune(c.(int32)))
}

// hydra.lib.chars.isSpace : Int32 -> Boolean
func IsSpace(c any) any {
	return unicode.IsSpace(rune(c.(int32)))
}

// hydra.lib.chars.isUpper : Int32 -> Boolean
func IsUpper(c any) any {
	return unicode.IsUpper(rune(c.(int32)))
}

// hydra.lib.chars.toLower : Int32 -> Int32
func ToLower(c any) any {
	return int32(unicode.ToLower(rune(c.(int32))))
}

// hydra.lib.chars.toUpper : Int32 -> Int32
func ToUpper(c any) any {
	return int32(unicode.ToUpper(rune(c.(int32))))
}
