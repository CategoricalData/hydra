// Note: this is an automatically generated file. Do not edit.

package showerror

import (
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/error"
  "hydra.dev/hydra/formatting"
  liblists "hydra.dev/hydra/lib/lists"
  libliterals "hydra.dev/hydra/lib/literals"
  libsets "hydra.dev/hydra/lib/sets"
  libstrings "hydra.dev/hydra/lib/strings"
  showcore "hydra.dev/hydra/show/core"
  showmeta "hydra.dev/hydra/show/meta"
  showtyping "hydra.dev/hydra/show/typing"
  "hydra.dev/hydra/typing"
  "hydra.dev/hydra/variants"
)

func CheckingError (ce error.CheckingError) string {
  return func (x any) any {
    switch v := x.(type) {
      case error.CheckingErrorIncorrectUnification:
      return func (v1 error.IncorrectUnificationError) any {
        return IncorrectUnificationError(v1)
      }(v.Value)
      case error.CheckingErrorNotAForallType:
      return func (v1 error.NotAForallTypeError) any {
        return NotAForallTypeError(v1)
      }(v.Value)
      case error.CheckingErrorNotAFunctionType:
      return func (v1 error.NotAFunctionTypeError) any {
        return NotAFunctionTypeError(v1)
      }(v.Value)
      case error.CheckingErrorTypeArityMismatch:
      return func (v1 error.TypeArityMismatchError) any {
        return TypeArityMismatchError(v1)
      }(v.Value)
      case error.CheckingErrorTypeMismatch:
      return func (v1 error.TypeMismatchError) any {
        return TypeMismatchError(v1)
      }(v.Value)
      case error.CheckingErrorUnboundTypeVariables:
      return func (v1 error.UnboundTypeVariablesError) any {
        return UnboundTypeVariablesError(v1)
      }(v.Value)
      case error.CheckingErrorUnequalTypes:
      return func (v1 error.UnequalTypesError) any {
        return UnequalTypesError(v1)
      }(v.Value)
      case error.CheckingErrorUnsupportedTermVariant:
      return func (v1 error.UnsupportedTermVariantError) any {
        return UnsupportedTermVariantError(v1)
      }(v.Value)
      case error.CheckingErrorUntypedLambda:
      return func (v1 error.UntypedLambdaError) any {
        return UntypedLambdaError[error.UntypedLambdaError](v1)
      }(v.Value)
      case error.CheckingErrorUntypedLetBinding:
      return func (v1 error.UntypedLetBindingError) any {
        return UntypedLetBindingError(v1)
      }(v.Value)
    }
    return nil
  }(ce).(string)
}

func DecodingError (de error.DecodingError) string {
  return libstrings.Cat2("decoding error: ").(func(any) any)(func (v any) any {
    return v
  }(de)).(string)
}

func DuplicateBindingError (e error.DuplicateBindingError) string {
  return libstrings.Cat2("duplicate binding: ").(func(any) any)(func (v any) any {
    return v.(error.DuplicateBindingError).Name
  }(e)).(string)
}

func DuplicateFieldError (e error.DuplicateFieldError) string {
  return libstrings.Cat2("duplicate field: ").(func(any) any)(func (v any) any {
    return v.(error.DuplicateFieldError).Name
  }(e)).(string)
}

func Error_ (e error.Error) string {
  return func (x any) any {
    switch v := x.(type) {
      case error.ErrorChecking:
      return func (v1 error.CheckingError) any {
        return CheckingError(v1)
      }(v.Value)
      case error.ErrorDecoding:
      return func (v1 error.DecodingError) any {
        return DecodingError(v1)
      }(v.Value)
      case error.ErrorDuplicateBinding:
      return func (v1 error.DuplicateBindingError) any {
        return DuplicateBindingError(v1)
      }(v.Value)
      case error.ErrorDuplicateField:
      return func (v1 error.DuplicateFieldError) any {
        return DuplicateFieldError(v1)
      }(v.Value)
      case error.ErrorOther:
      return func (v1 error.OtherError) any {
        return OtherError(v1)
      }(v.Value)
      case error.ErrorUndefinedField:
      return func (v1 error.UndefinedFieldError) any {
        return UndefinedFieldError(v1)
      }(v.Value)
      case error.ErrorUndefinedTerm:
      return func (v1 error.UndefinedTermError) any {
        return UndefinedTermError(v1)
      }(v.Value)
      case error.ErrorUndefinedType:
      return func (v1 error.UndefinedTypeError) any {
        return UndefinedTypeError(v1)
      }(v.Value)
      case error.ErrorUnexpectedTermVariant:
      return func (v1 error.UnexpectedTermVariantError) any {
        return UnexpectedTermVariantError(v1)
      }(v.Value)
      case error.ErrorUnexpectedTypeVariant:
      return func (v1 error.UnexpectedTypeVariantError) any {
        return UnexpectedTypeVariantError(v1)
      }(v.Value)
      case error.ErrorUnification:
      return func (v1 error.UnificationError) any {
        return UnificationError(v1)
      }(v.Value)
    }
    return nil
  }(e).(string)
}

func IncorrectUnificationError (e error.IncorrectUnificationError) string {
  return func () any {
    var subst any = func (v any) any {
      return v.(error.IncorrectUnificationError).Substitution
    }(e)
    return libstrings.Cat2("incorrect unification: ").(func(any) any)(showtyping.TypeSubst(subst.(typing.TypeSubst)))
  }().(string)
}

func NotAForallTypeError (e error.NotAForallTypeError) string {
  return func () any {
    var typ any = func (v any) any {
      return v.(error.NotAForallTypeError).Type_
    }(e)
    return func () any {
      var args any = func (v any) any {
        return v.(error.NotAForallTypeError).TypeArguments
      }(e)
      return libstrings.Cat([]any{"not a forall type: ", showcore.Type_(typ.(core.Type)), ". Trying to apply ", libliterals.ShowInt32(liblists.Length(args)), " type argument(s): ", formatting.ShowList[core.Type](showcore.Type_, args.([]any))})
    }()
  }().(string)
}

func NotAFunctionTypeError (e error.NotAFunctionTypeError) string {
  return func () any {
    var typ any = func (v any) any {
      return v.(error.NotAFunctionTypeError).Type_
    }(e)
    return libstrings.Cat2("not a function type: ").(func(any) any)(showcore.Type_(typ.(core.Type)))
  }().(string)
}

func OtherError (oe error.OtherError) string {
  return func (v any) any {
    return v
  }(oe).(string)
}

func TypeArityMismatchError (e error.TypeArityMismatchError) string {
  return func () any {
    var typ any = func (v any) any {
      return v.(error.TypeArityMismatchError).Type_
    }(e)
    return func () any {
      var expected any = func (v any) any {
        return v.(error.TypeArityMismatchError).ExpectedArity
      }(e)
      return func () any {
        var actual any = func (v any) any {
          return v.(error.TypeArityMismatchError).ActualArity
        }(e)
        return func () any {
          var args any = func (v any) any {
            return v.(error.TypeArityMismatchError).TypeArguments
          }(e)
          return libstrings.Cat([]any{"type ", showcore.Type_(typ.(core.Type)), " applied to the wrong number of type arguments (expected ", libliterals.ShowInt32(expected), ", got ", libliterals.ShowInt32(actual), "): ", formatting.ShowList[core.Type](showcore.Type_, args.([]any))})
        }()
      }()
    }()
  }().(string)
}

func TypeMismatchError (e error.TypeMismatchError) string {
  return func () any {
    var expected any = func (v any) any {
      return v.(error.TypeMismatchError).ExpectedType
    }(e)
    return func () any {
      var actual any = func (v any) any {
        return v.(error.TypeMismatchError).ActualType
      }(e)
      return libstrings.Cat([]any{"type mismatch: expected ", showcore.Type_(expected.(core.Type)), " but found ", showcore.Type_(actual.(core.Type))})
    }()
  }().(string)
}

func UnboundTypeVariablesError (e error.UnboundTypeVariablesError) string {
  return func () any {
    var vars any = func (v any) any {
      return v.(error.UnboundTypeVariablesError).Variables
    }(e)
    return func () any {
      var typ any = func (v any) any {
        return v.(error.UnboundTypeVariablesError).Type_
      }(e)
      return libstrings.Cat([]any{"unbound type variables: {", libstrings.Intercalate(", ").(func(any) any)(liblists.Map(func (v any) any {
        return v
      }).(func(any) any)(libsets.ToList(vars))), "} in type ", showcore.Type_(typ.(core.Type))})
    }()
  }().(string)
}

func UndefinedFieldError (e error.UndefinedFieldError) string {
  return func () any {
    var fname any = func (v any) any {
      return v.(error.UndefinedFieldError).FieldName
    }(e)
    return func () any {
      var tname any = func (v any) any {
        return v.(error.UndefinedFieldError).TypeName
      }(e)
      return libstrings.Cat([]any{"no such field \"", fname, "\" in type \"", tname, "\""})
    }()
  }().(string)
}

func UndefinedTermError (e error.UndefinedTermError) string {
  return libstrings.Cat2("undefined term: ").(func(any) any)(func (v any) any {
    return v.(error.UndefinedTermError).Name
  }(e)).(string)
}

func UndefinedTypeError (e error.UndefinedTypeError) string {
  return libstrings.Cat2("undefined type: ").(func(any) any)(func (v any) any {
    return v.(error.UndefinedTypeError).Name
  }(e)).(string)
}

func UnequalTypesError (e error.UnequalTypesError) string {
  return func () any {
    var types any = func (v any) any {
      return v.(error.UnequalTypesError).Types
    }(e)
    return func () any {
      var desc any = func (v any) any {
        return v.(error.UnequalTypesError).Description
      }(e)
      return libstrings.Cat([]any{"unequal types ", formatting.ShowList[core.Type](showcore.Type_, types.([]any)), " in ", desc})
    }()
  }().(string)
}

func UnexpectedTermVariantError (e error.UnexpectedTermVariantError) string {
  return func () any {
    var expected any = func (v any) any {
      return v.(error.UnexpectedTermVariantError).ExpectedVariant
    }(e)
    return func () any {
      var actual any = func (v any) any {
        return v.(error.UnexpectedTermVariantError).ActualTerm
      }(e)
      return libstrings.Cat([]any{"expected ", showmeta.TermVariant(expected.(variants.TermVariant)), " term but found ", showcore.Term(actual.(core.Term))})
    }()
  }().(string)
}

func UnexpectedTypeVariantError (e error.UnexpectedTypeVariantError) string {
  return func () any {
    var expected any = func (v any) any {
      return v.(error.UnexpectedTypeVariantError).ExpectedVariant
    }(e)
    return func () any {
      var actual any = func (v any) any {
        return v.(error.UnexpectedTypeVariantError).ActualType
      }(e)
      return libstrings.Cat([]any{"expected ", showmeta.TypeVariant(expected.(variants.TypeVariant)), " type but found ", showcore.Type_(actual.(core.Type))})
    }()
  }().(string)
}

func UnificationError (e error.UnificationError) string {
  return func () any {
    var lt any = func (v any) any {
      return v.(error.UnificationError).LeftType
    }(e)
    return func () any {
      var rt any = func (v any) any {
        return v.(error.UnificationError).RightType
      }(e)
      return func () any {
        var msg any = func (v any) any {
          return v.(error.UnificationError).Message
        }(e)
        return libstrings.Cat([]any{"unification error: cannot unify ", showcore.Type_(lt.(core.Type)), " with ", showcore.Type_(rt.(core.Type)), ": ", msg})
      }()
    }()
  }().(string)
}

func UnsupportedTermVariantError (e error.UnsupportedTermVariantError) string {
  return libstrings.Cat2("unsupported term variant: ").(func(any) any)(showmeta.TermVariant(func (v any) any {
    return v.(error.UnsupportedTermVariantError).TermVariant
  }(e).(variants.TermVariant))).(string)
}

func UntypedLambdaError[T0 any] (_ T0) string {
  return "untyped lambda"
}

func UntypedLetBindingError (e error.UntypedLetBindingError) string {
  return func () any {
    var b any = func (v any) any {
      return v.(error.UntypedLetBindingError).Binding
    }(e)
    return libstrings.Cat2("untyped let binding: ").(func(any) any)(showcore.Binding(b.(core.Binding)))
  }().(string)
}
