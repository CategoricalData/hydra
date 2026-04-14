// Note: this is an automatically generated file. Do not edit.

package hydra.show;

/**
 * String representations of hydra.error types
 */
public interface Errors {
  static String checkingError(hydra.error.checking.CheckingError ce) {
    return (ce).accept(new hydra.error.checking.CheckingError.PartialVisitor<>() {
      @Override
      public String visit(hydra.error.checking.CheckingError.IncorrectUnification v1) {
        return hydra.show.Errors.incorrectUnificationError((v1).value);
      }

      @Override
      public String visit(hydra.error.checking.CheckingError.NotAForallType v1) {
        return hydra.show.Errors.notAForallTypeError((v1).value);
      }

      @Override
      public String visit(hydra.error.checking.CheckingError.NotAFunctionType v1) {
        return hydra.show.Errors.notAFunctionTypeError((v1).value);
      }

      @Override
      public String visit(hydra.error.checking.CheckingError.TypeArityMismatch v1) {
        return hydra.show.Errors.typeArityMismatchError((v1).value);
      }

      @Override
      public String visit(hydra.error.checking.CheckingError.TypeMismatch v1) {
        return hydra.show.Errors.typeMismatchError((v1).value);
      }

      @Override
      public String visit(hydra.error.checking.CheckingError.UnboundTypeVariables v1) {
        return hydra.show.Errors.unboundTypeVariablesError((v1).value);
      }

      @Override
      public String visit(hydra.error.checking.CheckingError.UnequalTypes v1) {
        return hydra.show.Errors.unequalTypesError((v1).value);
      }

      @Override
      public String visit(hydra.error.checking.CheckingError.UnsupportedTermVariant v1) {
        return hydra.show.Errors.unsupportedTermVariantError((v1).value);
      }

      @Override
      public String visit(hydra.error.checking.CheckingError.UntypedLambda v1) {
        return hydra.show.Errors.untypedLambdaError((v1).value);
      }

      @Override
      public String visit(hydra.error.checking.CheckingError.UntypedLetBinding v1) {
        return hydra.show.Errors.untypedLetBindingError((v1).value);
      }
    });
  }

  static String decodingError(hydra.errors.DecodingError de) {
    return hydra.lib.strings.Cat2.apply(
      "decoding error: ",
      (de).value);
  }

  static String error(hydra.errors.Error_ e) {
    return (e).accept(new hydra.errors.Error_.PartialVisitor<>() {
      @Override
      public String visit(hydra.errors.Error_.Checking v1) {
        return hydra.show.Errors.checkingError((v1).value);
      }

      @Override
      public String visit(hydra.errors.Error_.Decoding v1) {
        return hydra.show.Errors.decodingError((v1).value);
      }

      @Override
      public String visit(hydra.errors.Error_.DuplicateBinding v1) {
        return hydra.show.error.Core.duplicateBindingError((v1).value);
      }

      @Override
      public String visit(hydra.errors.Error_.DuplicateField v1) {
        return hydra.show.error.Core.duplicateFieldError((v1).value);
      }

      @Override
      public String visit(hydra.errors.Error_.Extraction ignored) {
        return "extraction error";
      }

      @Override
      public String visit(hydra.errors.Error_.Inference ignored) {
        return "inference error";
      }

      @Override
      public String visit(hydra.errors.Error_.Other v1) {
        return hydra.show.Errors.otherError((v1).value);
      }

      @Override
      public String visit(hydra.errors.Error_.Resolution ignored) {
        return "resolution error";
      }

      @Override
      public String visit(hydra.errors.Error_.UndefinedField v1) {
        return hydra.show.error.Core.undefinedFieldError((v1).value);
      }

      @Override
      public String visit(hydra.errors.Error_.UndefinedTermVariable v1) {
        return hydra.show.error.Core.undefinedTermVariableError((v1).value);
      }

      @Override
      public String visit(hydra.errors.Error_.UntypedTermVariable v1) {
        return hydra.show.error.Core.untypedTermVariableError((v1).value);
      }

      @Override
      public String visit(hydra.errors.Error_.UnexpectedTermVariant v1) {
        return hydra.show.error.Core.unexpectedTermVariantError((v1).value);
      }

      @Override
      public String visit(hydra.errors.Error_.UnexpectedTypeVariant v1) {
        return hydra.show.error.Core.unexpectedTypeVariantError((v1).value);
      }

      @Override
      public String visit(hydra.errors.Error_.Unification v1) {
        return hydra.show.Errors.unificationError((v1).value);
      }
    });
  }

  static String incorrectUnificationError(hydra.error.checking.IncorrectUnificationError e) {
    hydra.typing.TypeSubst subst = (e).substitution;
    return hydra.lib.strings.Cat2.apply(
      "incorrect unification: ",
      hydra.show.Typing.typeSubst(subst));
  }

  static String notAForallTypeError(hydra.error.checking.NotAForallTypeError e) {
    java.util.List<hydra.core.Type> args = (e).typeArguments;
    hydra.core.Type typ = (e).type;
    return hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      "not a forall type: ",
      hydra.show.Core.type(typ),
      ". Trying to apply ",
      hydra.lib.literals.ShowInt32.apply(hydra.lib.lists.Length.apply(args)),
      " type argument(s): ",
      hydra.Formatting.showList(
        hydra.show.Core::type,
        args)));
  }

  static String notAFunctionTypeError(hydra.error.checking.NotAFunctionTypeError e) {
    hydra.core.Type typ = (e).type;
    return hydra.lib.strings.Cat2.apply(
      "not a function type: ",
      hydra.show.Core.type(typ));
  }

  static String otherError(hydra.errors.OtherError oe) {
    return (oe).value;
  }

  static String typeArityMismatchError(hydra.error.checking.TypeArityMismatchError e) {
    Integer actual = (e).actualArity;
    java.util.List<hydra.core.Type> args = (e).typeArguments;
    Integer expected = (e).expectedArity;
    hydra.core.Type typ = (e).type;
    return hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      "type ",
      hydra.show.Core.type(typ),
      " applied to the wrong number of type arguments (expected ",
      hydra.lib.literals.ShowInt32.apply(expected),
      ", got ",
      hydra.lib.literals.ShowInt32.apply(actual),
      "): ",
      hydra.Formatting.showList(
        hydra.show.Core::type,
        args)));
  }

  static String typeMismatchError(hydra.error.checking.TypeMismatchError e) {
    hydra.core.Type actual = (e).actualType;
    hydra.core.Type expected = (e).expectedType;
    return hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      "type mismatch: expected ",
      hydra.show.Core.type(expected),
      " but found ",
      hydra.show.Core.type(actual)));
  }

  static String unboundTypeVariablesError(hydra.error.checking.UnboundTypeVariablesError e) {
    hydra.core.Type typ = (e).type;
    java.util.Set<hydra.core.Name> vars = (e).variables;
    return hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      "unbound type variables: {",
      hydra.lib.strings.Intercalate.apply(
        ", ",
        hydra.lib.lists.Map.apply(
          wrapped -> (wrapped).value,
          hydra.lib.sets.ToList.apply(vars))),
      "} in type ",
      hydra.show.Core.type(typ)));
  }

  static String unequalTypesError(hydra.error.checking.UnequalTypesError e) {
    String desc = (e).description;
    java.util.List<hydra.core.Type> types = (e).types;
    return hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      "unequal types ",
      hydra.Formatting.showList(
        hydra.show.Core::type,
        types),
      " in ",
      desc));
  }

  static String unificationError(hydra.errors.UnificationError e) {
    hydra.core.Type lt = (e).leftType;
    String msg = (e).message;
    hydra.core.Type rt = (e).rightType;
    return hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      "unification error: cannot unify ",
      hydra.show.Core.type(lt),
      " with ",
      hydra.show.Core.type(rt),
      ": ",
      msg));
  }

  static String unsupportedTermVariantError(hydra.error.checking.UnsupportedTermVariantError e) {
    return hydra.lib.strings.Cat2.apply(
      "unsupported term variant: ",
      hydra.show.Variants.termVariant((e).termVariant));
  }

  static <T0> String untypedLambdaError(T0 ignored) {
    return "untyped lambda";
  }

  static String untypedLetBindingError(hydra.error.checking.UntypedLetBindingError e) {
    hydra.core.Binding b = (e).binding;
    return hydra.lib.strings.Cat2.apply(
      "untyped let binding: ",
      hydra.show.Core.binding(b));
  }
}
