// Note: this is an automatically generated file. Do not edit.

package languages

import (
  "hydra.dev/hydra/coders"
  "hydra.dev/hydra/core"
  libsets "hydra.dev/hydra/lib/sets"
  "hydra.dev/hydra/reflect"
)

var HydraLanguage = func () any {
  var eliminationVariants any = libsets.FromList(reflect.EliminationVariants)
  var literalVariants any = libsets.FromList(reflect.LiteralVariants)
  var floatTypes any = libsets.FromList(reflect.FloatTypes)
  var functionVariants any = libsets.FromList(reflect.FunctionVariants)
  var integerTypes any = libsets.FromList(reflect.IntegerTypes)
  var termVariants any = libsets.FromList(reflect.TermVariants)
  var typeVariants any = libsets.FromList(reflect.TypeVariants)
  types := func (t core.Type) any {
    return true
  }
  return coders.Language{Name: coders.LanguageName("hydra.core"), Constraints: coders.LanguageConstraints{EliminationVariants: eliminationVariants.([]any), LiteralVariants: literalVariants.([]any), FloatTypes: floatTypes.([]any), FunctionVariants: functionVariants.([]any), IntegerTypes: integerTypes.([]any), TermVariants: termVariants.([]any), TypeVariants: typeVariants.([]any), Types: func (_p core.Type) bool {
    return types(_p).(bool)
  }}}
}().(coders.Language)
