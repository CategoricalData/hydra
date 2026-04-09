// Note: this is an automatically generated file. Do not edit.

package lists

import (
	"fmt"
	"sort"
)

func cmp(a, b any) int {
	switch av := a.(type) {
	case int:
		bv := b.(int)
		if av < bv {
			return -1
		} else if av > bv {
			return 1
		}
		return 0
	case int32:
		bv := b.(int32)
		if av < bv {
			return -1
		} else if av > bv {
			return 1
		}
		return 0
	case int64:
		bv := b.(int64)
		if av < bv {
			return -1
		} else if av > bv {
			return 1
		}
		return 0
	case float32:
		bv := b.(float32)
		if av < bv {
			return -1
		} else if av > bv {
			return 1
		}
		return 0
	case float64:
		bv := b.(float64)
		if av < bv {
			return -1
		} else if av > bv {
			return 1
		}
		return 0
	case string:
		bv := b.(string)
		if av < bv {
			return -1
		} else if av > bv {
			return 1
		}
		return 0
	case bool:
		bv := b.(bool)
		if av == bv {
			return 0
		} else if !av {
			return -1
		}
		return 1
	default:
		sa := fmt.Sprintf("%v", a)
		sb := fmt.Sprintf("%v", b)
		if sa < sb {
			return -1
		} else if sa > sb {
			return 1
		}
		return 0
	}
}

// hydra.lib.lists.map : (a -> b) -> [a] -> [b]
func Map(f any) any {
	return func(lst any) any {
		fn := f.(func(any) any)
		xs := lst.([]any)
		result := make([]any, len(xs))
		for i, x := range xs {
			result[i] = fn(x)
		}
		return result
	}
}

// hydra.lib.lists.foldl : (b -> a -> b) -> b -> [a] -> b
func Foldl(f any) any {
	return func(z any) any {
		return func(lst any) any {
			fn := f.(func(any) any)
			acc := z
			for _, x := range lst.([]any) {
				acc = fn(acc).(func(any) any)(x)
			}
			return acc
		}
	}
}

// hydra.lib.lists.concat : [[a]] -> [a]
func Concat(lst any) any {
	xs := lst.([]any)
	var result []any
	for _, sub := range xs {
		result = append(result, sub.([]any)...)
	}
	if result == nil {
		return []any{}
	}
	return result
}

// hydra.lib.lists.concat2 : [a] -> [a] -> [a]
func Concat2(a any) any {
	return func(b any) any {
		xs := a.([]any)
		ys := b.([]any)
		result := make([]any, 0, len(xs)+len(ys))
		result = append(result, xs...)
		result = append(result, ys...)
		return result
	}
}

// hydra.lib.lists.cons : a -> [a] -> [a]
func Cons(x any) any {
	return func(lst any) any {
		xs := lst.([]any)
		result := make([]any, 0, len(xs)+1)
		result = append(result, x)
		result = append(result, xs...)
		return result
	}
}

// hydra.lib.lists.head : [a] -> a
func Head(lst any) any {
	return lst.([]any)[0]
}

// hydra.lib.lists.tail : [a] -> [a]
func Tail(lst any) any {
	xs := lst.([]any)
	return xs[1:]
}

// hydra.lib.lists.init : [a] -> [a]
func Init(lst any) any {
	xs := lst.([]any)
	return xs[:len(xs)-1]
}

// hydra.lib.lists.last : [a] -> a
func Last(lst any) any {
	xs := lst.([]any)
	return xs[len(xs)-1]
}

// hydra.lib.lists.filter : (a -> Boolean) -> [a] -> [a]
func Filter(f any) any {
	return func(lst any) any {
		fn := f.(func(any) any)
		xs := lst.([]any)
		result := make([]any, 0)
		for _, x := range xs {
			if fn(x).(bool) {
				result = append(result, x)
			}
		}
		return result
	}
}

// hydra.lib.lists.null : [a] -> Boolean
func Null(lst any) any {
	return len(lst.([]any)) == 0
}

// hydra.lib.lists.length : [a] -> Int
func Length(lst any) any {
	return len(lst.([]any))
}

// hydra.lib.lists.reverse : [a] -> [a]
func Reverse(lst any) any {
	xs := lst.([]any)
	n := len(xs)
	result := make([]any, n)
	for i, x := range xs {
		result[n-1-i] = x
	}
	return result
}

// hydra.lib.lists.zip : [a] -> [b] -> [(a, b)]
func Zip(a any) any {
	return func(b any) any {
		xs := a.([]any)
		ys := b.([]any)
		n := len(xs)
		if len(ys) < n {
			n = len(ys)
		}
		result := make([]any, n)
		for i := 0; i < n; i++ {
			result[i] = [2]any{xs[i], ys[i]}
		}
		return result
	}
}

// hydra.lib.lists.sort : [a] -> [a]
func Sort(lst any) any {
	xs := lst.([]any)
	result := make([]any, len(xs))
	copy(result, xs)
	sort.Slice(result, func(i, j int) bool {
		return cmp(result[i], result[j]) < 0
	})
	return result
}

// hydra.lib.lists.elem : a -> [a] -> Boolean
func Elem(x any) any {
	return func(lst any) any {
		for _, v := range lst.([]any) {
			if cmp(x, v) == 0 {
				return true
			}
		}
		return false
	}
}

// hydra.lib.lists.pure : a -> [a]
func Pure(x any) any {
	return []any{x}
}

// hydra.lib.lists.nub : [a] -> [a]
func Nub(lst any) any {
	xs := lst.([]any)
	result := make([]any, 0)
	for _, x := range xs {
		found := false
		for _, y := range result {
			if cmp(x, y) == 0 {
				found = true
				break
			}
		}
		if !found {
			result = append(result, x)
		}
	}
	return result
}

// hydra.lib.lists.take : Int -> [a] -> [a]
func Take(n any) any {
	return func(lst any) any {
		xs := lst.([]any)
		count := n.(int)
		if count > len(xs) {
			count = len(xs)
		}
		if count < 0 {
			count = 0
		}
		return xs[:count]
	}
}

// hydra.lib.lists.drop : Int -> [a] -> [a]
func Drop(n any) any {
	return func(lst any) any {
		xs := lst.([]any)
		count := n.(int)
		if count > len(xs) {
			count = len(xs)
		}
		if count < 0 {
			count = 0
		}
		return xs[count:]
	}
}

// hydra.lib.lists.replicate : Int -> a -> [a]
func Replicate(n any) any {
	return func(x any) any {
		count := n.(int)
		result := make([]any, count)
		for i := 0; i < count; i++ {
			result[i] = x
		}
		return result
	}
}

// hydra.lib.lists.intersperse : a -> [a] -> [a]
func Intersperse(sep any) any {
	return func(lst any) any {
		xs := lst.([]any)
		if len(xs) <= 1 {
			result := make([]any, len(xs))
			copy(result, xs)
			return result
		}
		result := make([]any, 0, len(xs)*2-1)
		for i, x := range xs {
			if i > 0 {
				result = append(result, sep)
			}
			result = append(result, x)
		}
		return result
	}
}

// hydra.lib.lists.bind : [a] -> (a -> [b]) -> [b]
func Bind(lst any) any {
	return func(f any) any {
		fn := f.(func(any) any)
		xs := lst.([]any)
		var result []any
		for _, x := range xs {
			result = append(result, fn(x).([]any)...)
		}
		if result == nil {
			return []any{}
		}
		return result
	}
}

// hydra.lib.lists.apply : [a -> b] -> [a] -> [b]
func Apply(fs any) any {
	return func(lst any) any {
		fns := fs.([]any)
		xs := lst.([]any)
		var result []any
		for _, f := range fns {
			fn := f.(func(any) any)
			for _, x := range xs {
				result = append(result, fn(x))
			}
		}
		if result == nil {
			return []any{}
		}
		return result
	}
}

// hydra.lib.lists.at : Int -> [a] -> a
func At(n any) any {
	return func(lst any) any {
		return lst.([]any)[n.(int)]
	}
}

// hydra.lib.lists.intercalate : [a] -> [[a]] -> [a]
func Intercalate(sep any) any {
	return func(lst any) any {
		xs := lst.([]any)
		if len(xs) == 0 {
			return []any{}
		}
		s := sep.([]any)
		var result []any
		for i, sub := range xs {
			if i > 0 {
				result = append(result, s...)
			}
			result = append(result, sub.([]any)...)
		}
		if result == nil {
			return []any{}
		}
		return result
	}
}

// hydra.lib.lists.singleton : a -> [a]
var Singleton = Pure

// hydra.lib.lists.safeHead : [a] -> Maybe a
func SafeHead(lst any) any {
	xs := lst.([]any)
	if len(xs) == 0 {
		return nil
	}
	v := any(xs[0])
	return &v
}

// hydra.lib.lists.sortOn : (a -> b) -> [a] -> [a]
func SortOn(f any) any {
	return func(lst any) any {
		fn := f.(func(any) any)
		xs := lst.([]any)
		r := make([]any, len(xs))
		copy(r, xs)
		sort.Slice(r, func(i, j int) bool {
			return cmp(fn(r[i]), fn(r[j])) < 0
		})
		return any(r)
	}
}

// hydra.lib.lists.zipWith : (a -> b -> c) -> [a] -> [b] -> [c]
func ZipWith(f any) any {
	return func(xs any) any {
		return func(ys any) any {
			fn := f.(func(any) any)
			as := xs.([]any)
			bs := ys.([]any)
			n := len(as)
			if len(bs) < n {
				n = len(bs)
			}
			r := make([]any, n)
			for i := 0; i < n; i++ {
				r[i] = fn(as[i]).(func(any) any)(bs[i])
			}
			return any(r)
		}
	}
}

// hydra.lib.lists.find : (a -> Bool) -> [a] -> Maybe a
func Find(pred any) any {
	return func(lst any) any {
		fn := pred.(func(any) any)
		for _, x := range lst.([]any) {
			if fn(x).(bool) {
				v := x
				return &v
			}
		}
		return nil
	}
}

// hydra.lib.lists.dropWhile : (a -> Bool) -> [a] -> [a]
func DropWhile(pred any) any {
	return func(lst any) any {
		fn := pred.(func(any) any)
		xs := lst.([]any)
		for i, x := range xs {
			if !fn(x).(bool) {
				return any(xs[i:])
			}
		}
		return any([]any{})
	}
}

// hydra.lib.lists.span : (a -> Bool) -> [a] -> ([a], [a])
func Span(pred any) any {
	return func(lst any) any {
		fn := pred.(func(any) any)
		xs := lst.([]any)
		for i, x := range xs {
			if !fn(x).(bool) {
				return [2]any{any(xs[:i]), any(xs[i:])}
			}
		}
		return [2]any{lst, any([]any{})}
	}
}

// hydra.lib.lists.group : [a] -> [[a]]
func Group(lst any) any {
	xs := lst.([]any)
	if len(xs) == 0 {
		return any([]any{})
	}
	var result []any
	current := []any{xs[0]}
	for _, x := range xs[1:] {
		if x == current[0] {
			current = append(current, x)
		} else {
			result = append(result, any(current))
			current = []any{x}
		}
	}
	result = append(result, any(current))
	return any(result)
}

// hydra.lib.lists.partition : (a -> Bool) -> [a] -> ([a], [a])
func Partition(pred any) any {
	return func(lst any) any {
		fn := pred.(func(any) any)
		var yes, no []any
		for _, x := range lst.([]any) {
			if fn(x).(bool) {
				yes = append(yes, x)
			} else {
				no = append(no, x)
			}
		}
		if yes == nil { yes = []any{} }
		if no == nil { no = []any{} }
		return [2]any{any(yes), any(no)}
	}
}

// hydra.lib.lists.transpose : [[a]] -> [[a]]
func Transpose(lst any) any {
	xss := lst.([]any)
	if len(xss) == 0 {
		return any([]any{})
	}
	maxLen := 0
	for _, xs := range xss {
		if l := len(xs.([]any)); l > maxLen {
			maxLen = l
		}
	}
	result := make([]any, maxLen)
	for i := 0; i < maxLen; i++ {
		col := make([]any, 0)
		for _, xs := range xss {
			row := xs.([]any)
			if i < len(row) {
				col = append(col, row[i])
			}
		}
		result[i] = any(col)
	}
	return any(result)
}
