// Note: this is an automatically generated file. Do not edit.

package strings

import (
	gostrings "strings"
	"unicode/utf8"
)

// hydra.lib.strings.cat : [String] -> String
func Cat(lst any) any {
	xs := lst.([]any)
	parts := make([]string, len(xs))
	for i, x := range xs {
		parts[i] = x.(string)
	}
	return gostrings.Join(parts, "")
}

// hydra.lib.strings.cat2 : String -> String -> String
func Cat2(a any) any {
	return func(b any) any {
		return a.(string) + b.(string)
	}
}

// hydra.lib.strings.splitOn : String -> String -> [String]
func SplitOn(sep any) any {
	return func(s any) any {
		parts := gostrings.Split(s.(string), sep.(string))
		result := make([]any, len(parts))
		for i, p := range parts {
			result[i] = p
		}
		return result
	}
}

// hydra.lib.strings.intercalate : String -> [String] -> String
func Intercalate(sep any) any {
	return func(lst any) any {
		xs := lst.([]any)
		parts := make([]string, len(xs))
		for i, x := range xs {
			parts[i] = x.(string)
		}
		return gostrings.Join(parts, sep.(string))
	}
}

// hydra.lib.strings.toUpper : String -> String
func ToUpper(s any) any {
	return gostrings.ToUpper(s.(string))
}

// hydra.lib.strings.toLower : String -> String
func ToLower(s any) any {
	return gostrings.ToLower(s.(string))
}

// hydra.lib.strings.length : String -> Int
func Length(s any) any {
	return utf8.RuneCountInString(s.(string))
}

// hydra.lib.strings.toList : String -> [Int32]
func ToList(s any) any {
	runes := []rune(s.(string))
	result := make([]any, len(runes))
	for i, r := range runes {
		result[i] = int32(r)
	}
	return result
}

// hydra.lib.strings.fromList : [Int32] -> String
func FromList(lst any) any {
	xs := lst.([]any)
	runes := make([]rune, len(xs))
	for i, x := range xs {
		runes[i] = rune(x.(int32))
	}
	return string(runes)
}

// hydra.lib.strings.null : String -> Boolean
func Null(s any) any {
	return len(s.(string)) == 0
}

// hydra.lib.strings.lines : String -> [String]
func Lines(s any) any {
	parts := gostrings.Split(s.(string), "\n")
	result := make([]any, len(parts))
	for i, p := range parts {
		result[i] = p
	}
	return result
}

// hydra.lib.strings.unlines : [String] -> String
func Unlines(lst any) any {
	xs := lst.([]any)
	parts := make([]string, len(xs))
	for i, x := range xs {
		parts[i] = x.(string)
	}
	return gostrings.Join(parts, "\n")
}

// hydra.lib.strings.charAt : Int -> String -> Int32
func CharAt(n any) any {
	return func(s any) any {
		runes := []rune(s.(string))
		return int32(runes[n.(int)])
	}
}
