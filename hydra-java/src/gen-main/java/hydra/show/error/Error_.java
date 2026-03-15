// Note: this is an automatically generated file. Do not edit.

package hydra.show.error;

/**
 * String representations of hydra.error types
 */
public interface Error_ {
  static String checkingError(hydra.error.CheckingError ce) {
    return (ce).accept(new hydra.error.CheckingError.PartialVisitor<>() {
      @Override
      public String visit(hydra.error.CheckingError.IncorrectUnification v1) {
        return hydra.show.error.Error_.incorrectUnificationError((v1).value);
      }
      
      @Override
      public String visit(hydra.error.CheckingError.NotAForallType v1) {
        return hydra.show.error.Error_.notAForallTypeError((v1).value);
      }
      
      @Override
      public String visit(hydra.error.CheckingError.NotAFunctionType v1) {
        return hydra.show.error.Error_.notAFunctionTypeError((v1).value);
      }
      
      @Override
      public String visit(hydra.error.CheckingError.TypeArityMismatch v1) {
        return hydra.show.error.Error_.typeArityMismatchError((v1).value);
      }
      
      @Override
      public String visit(hydra.error.CheckingError.TypeMismatch v1) {
        return hydra.show.error.Error_.typeMismatchError((v1).value);
      }
      
      @Override
      public String visit(hydra.error.CheckingError.UnboundTypeVariables v1) {
        return hydra.show.error.Error_.unboundTypeVariablesError((v1).value);
      }
      
      @Override
      public String visit(hydra.error.CheckingError.UnequalTypes v1) {
        return hydra.show.error.Error_.unequalTypesError((v1).value);
      }
      
      @Override
      public String visit(hydra.error.CheckingError.UnsupportedTermVariant v1) {
        return hydra.show.error.Error_.unsupportedTermVariantError((v1).value);
      }
      
      @Override
      public String visit(hydra.error.CheckingError.UntypedLambda v1) {
        return hydra.show.error.Error_.untypedLambdaError((v1).value);
      }
      
      @Override
      public String visit(hydra.error.CheckingError.UntypedLetBinding v1) {
        return hydra.show.error.Error_.untypedLetBindingError((v1).value);
      }
    });
  }
  
  static String decodingError(hydra.error.DecodingError de) {
    return hydra.lib.strings.Cat2.apply(
      "decoding error: ",
      (de).value);
  }
  
  static String duplicateBindingError(hydra.error.DuplicateBindingError e) {
    return hydra.lib.strings.Cat2.apply(
      "duplicate binding: ",
      ((e).name).value);
  }
  
  static String duplicateFieldError(hydra.error.DuplicateFieldError e) {
    return hydra.lib.strings.Cat2.apply(
      "duplicate field: ",
      ((e).name).value);
  }
  
  static String error(hydra.error.Error_ e) {
    return (e).accept(new hydra.error.Error_.PartialVisitor<>() {
      @Override
      public String visit(hydra.error.Error_.Checking v1) {
        return hydra.show.error.Error_.checkingError((v1).value);
      }
      
      @Override
      public String visit(hydra.error.Error_.Decoding v1) {
        return hydra.show.error.Error_.decodingError((v1).value);
      }
      
      @Override
      public String visit(hydra.error.Error_.DuplicateBinding v1) {
        return hydra.show.error.Error_.duplicateBindingError((v1).value);
      }
      
      @Override
      public String visit(hydra.error.Error_.DuplicateField v1) {
        return hydra.show.error.Error_.duplicateFieldError((v1).value);
      }
      
      @Override
      public String visit(hydra.error.Error_.Other v1) {
        return hydra.show.error.Error_.otherError((v1).value);
      }
      
      @Override
      public String visit(hydra.error.Error_.UndefinedField v1) {
        return hydra.show.error.Error_.undefinedFieldError((v1).value);
      }
      
      @Override
      public String visit(hydra.error.Error_.UndefinedTerm v1) {
        return hydra.show.error.Error_.undefinedTermError((v1).value);
      }
      
      @Override
      public String visit(hydra.error.Error_.UndefinedType v1) {
        return hydra.show.error.Error_.undefinedTypeError((v1).value);
      }
      
      @Override
      public String visit(hydra.error.Error_.UnexpectedTermVariant v1) {
        return hydra.show.error.Error_.unexpectedTermVariantError((v1).value);
      }
      
      @Override
      public String visit(hydra.error.Error_.UnexpectedTypeVariant v1) {
        return hydra.show.error.Error_.unexpectedTypeVariantError((v1).value);
      }
      
      @Override
      public String visit(hydra.error.Error_.Unification v1) {
        return hydra.show.error.Error_.unificationError((v1).value);
      }
    });
  }
  
  static String incorrectUnificationError(hydra.error.IncorrectUnificationError e) {
    hydra.typing.TypeSubst subst = (e).substitution;
    return hydra.lib.strings.Cat2.apply(
      "incorrect unification: ",
      hydra.show.typing.Typing.typeSubst(subst));
  }
  
  static String notAForallTypeError(hydra.error.NotAForallTypeError e) {
    hydra.util.ConsList<hydra.core.Type> args = (e).typeArguments;
    hydra.core.Type typ = (e).type;
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "not a forall type: ",
      hydra.show.core.Core.type(typ),
      ". Trying to apply ",
      hydra.lib.literals.ShowInt32.apply(hydra.lib.lists.Length.apply(args)),
      " type argument(s): ",
      hydra.formatting.Formatting.showList(
        hydra.show.core.Core::type,
        args)));
  }
  
  static String notAFunctionTypeError(hydra.error.NotAFunctionTypeError e) {
    hydra.core.Type typ = (e).type;
    return hydra.lib.strings.Cat2.apply(
      "not a function type: ",
      hydra.show.core.Core.type(typ));
  }
  
  static String otherError(hydra.error.OtherError oe) {
    return (oe).value;
  }
  
  static String typeArityMismatchError(hydra.error.TypeArityMismatchError e) {
    Integer actual = (e).actualArity;
    hydra.util.ConsList<hydra.core.Type> args = (e).typeArguments;
    Integer expected = (e).expectedArity;
    hydra.core.Type typ = (e).type;
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "type ",
      hydra.show.core.Core.type(typ),
      " applied to the wrong number of type arguments (expected ",
      hydra.lib.literals.ShowInt32.apply(expected),
      ", got ",
      hydra.lib.literals.ShowInt32.apply(actual),
      "): ",
      hydra.formatting.Formatting.showList(
        hydra.show.core.Core::type,
        args)));
  }
  
  static String typeMismatchError(hydra.error.TypeMismatchError e) {
    hydra.core.Type actual = (e).actualType;
    hydra.core.Type expected = (e).expectedType;
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "type mismatch: expected ",
      hydra.show.core.Core.type(expected),
      " but found ",
      hydra.show.core.Core.type(actual)));
  }
  
  static String unboundTypeVariablesError(hydra.error.UnboundTypeVariablesError e) {
    hydra.core.Type typ = (e).type;
    hydra.util.PersistentSet<hydra.core.Name> vars = (e).variables;
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "unbound type variables: {",
      hydra.lib.strings.Intercalate.apply(
        ", ",
        hydra.lib.lists.Map.apply(
          wrapped -> (wrapped).value,
          hydra.lib.sets.ToList.apply(vars))),
      "} in type ",
      hydra.show.core.Core.type(typ)));
  }
  
  static String undefinedFieldError(hydra.error.UndefinedFieldError e) {
    hydra.core.Name fname = (e).fieldName;
    hydra.core.Name tname = (e).typeName;
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "no such field \"",
      (fname).value,
      "\" in type \"",
      (tname).value,
      "\""));
  }
  
  static String undefinedTermError(hydra.error.UndefinedTermError e) {
    return hydra.lib.strings.Cat2.apply(
      "undefined term: ",
      ((e).name).value);
  }
  
  static String undefinedTypeError(hydra.error.UndefinedTypeError e) {
    return hydra.lib.strings.Cat2.apply(
      "undefined type: ",
      ((e).name).value);
  }
  
  static String unequalTypesError(hydra.error.UnequalTypesError e) {
    String desc = (e).description;
    hydra.util.ConsList<hydra.core.Type> types = (e).types;
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "unequal types ",
      hydra.formatting.Formatting.showList(
        hydra.show.core.Core::type,
        types),
      " in ",
      desc));
  }
  
  static String unexpectedTermVariantError(hydra.error.UnexpectedTermVariantError e) {
    hydra.core.Term actual = (e).actualTerm;
    hydra.variants.TermVariant expected = (e).expectedVariant;
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "expected ",
      hydra.show.meta.Meta.termVariant(expected),
      " term but found ",
      hydra.show.core.Core.term(actual)));
  }
  
  static String unexpectedTypeVariantError(hydra.error.UnexpectedTypeVariantError e) {
    hydra.core.Type actual = (e).actualType;
    hydra.variants.TypeVariant expected = (e).expectedVariant;
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "expected ",
      hydra.show.meta.Meta.typeVariant(expected),
      " type but found ",
      hydra.show.core.Core.type(actual)));
  }
  
  static String unificationError(hydra.error.UnificationError e) {
    hydra.core.Type lt = (e).leftType;
    String msg = (e).message;
    hydra.core.Type rt = (e).rightType;
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "unification error: cannot unify ",
      hydra.show.core.Core.type(lt),
      " with ",
      hydra.show.core.Core.type(rt),
      ": ",
      msg));
  }
  
  static String unsupportedTermVariantError(hydra.error.UnsupportedTermVariantError e) {
    return hydra.lib.strings.Cat2.apply(
      "unsupported term variant: ",
      hydra.show.meta.Meta.termVariant((e).termVariant));
  }
  
  static <T0> String untypedLambdaError(T0 ignored) {
    return "untyped lambda";
  }
  
  static String untypedLetBindingError(hydra.error.UntypedLetBindingError e) {
    hydra.core.Binding b = (e).binding;
    return hydra.lib.strings.Cat2.apply(
      "untyped let binding: ",
      hydra.show.core.Core.binding(b));
  }
}
