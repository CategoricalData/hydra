// Note: this is an automatically generated file. Do not edit.

package hydra.encode.error.checking;

/**
 * Term encoders for hydra.error.checking
 */
public interface Checking {
  static hydra.core.Term checkingError(hydra.error.checking.CheckingError v1) {
    return (v1).accept(new hydra.error.checking.CheckingError.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.error.checking.CheckingError.IncorrectUnification y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.checking.CheckingError"), new hydra.core.Field(new hydra.core.Name("incorrectUnification"), hydra.encode.error.checking.Checking.incorrectUnificationError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.checking.CheckingError.NotAForallType y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.checking.CheckingError"), new hydra.core.Field(new hydra.core.Name("notAForallType"), hydra.encode.error.checking.Checking.notAForallTypeError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.checking.CheckingError.NotAFunctionType y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.checking.CheckingError"), new hydra.core.Field(new hydra.core.Name("notAFunctionType"), hydra.encode.error.checking.Checking.notAFunctionTypeError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.checking.CheckingError.TypeArityMismatch y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.checking.CheckingError"), new hydra.core.Field(new hydra.core.Name("typeArityMismatch"), hydra.encode.error.checking.Checking.typeArityMismatchError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.checking.CheckingError.TypeMismatch y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.checking.CheckingError"), new hydra.core.Field(new hydra.core.Name("typeMismatch"), hydra.encode.error.checking.Checking.typeMismatchError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.checking.CheckingError.UnboundTypeVariables y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.checking.CheckingError"), new hydra.core.Field(new hydra.core.Name("unboundTypeVariables"), hydra.encode.error.checking.Checking.unboundTypeVariablesError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.checking.CheckingError.UnequalTypes y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.checking.CheckingError"), new hydra.core.Field(new hydra.core.Name("unequalTypes"), hydra.encode.error.checking.Checking.unequalTypesError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.checking.CheckingError.UnsupportedTermVariant y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.checking.CheckingError"), new hydra.core.Field(new hydra.core.Name("unsupportedTermVariant"), hydra.encode.error.checking.Checking.unsupportedTermVariantError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.checking.CheckingError.UntypedLambda y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.checking.CheckingError"), new hydra.core.Field(new hydra.core.Name("untypedLambda"), hydra.encode.error.checking.Checking.untypedLambdaError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.checking.CheckingError.UntypedLetBinding y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.checking.CheckingError"), new hydra.core.Field(new hydra.core.Name("untypedLetBinding"), hydra.encode.error.checking.Checking.untypedLetBindingError((y).value))));
      }
    });
  }

  static hydra.core.Term incorrectUnificationError(hydra.error.checking.IncorrectUnificationError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.IncorrectUnificationError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("substitution"), hydra.encode.typing.Typing.typeSubst((x).substitution)))));
  }

  static hydra.core.Term notAForallTypeError(hydra.error.checking.NotAForallTypeError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.NotAForallTypeError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("type"), hydra.encode.core.Core.type((x).type)),
      new hydra.core.Field(new hydra.core.Name("typeArguments"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.core.Core::type,
        (x).typeArguments))))));
  }

  static hydra.core.Term notAFunctionTypeError(hydra.error.checking.NotAFunctionTypeError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.NotAFunctionTypeError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("type"), hydra.encode.core.Core.type((x).type)))));
  }

  static hydra.core.Term typeArityMismatchError(hydra.error.checking.TypeArityMismatchError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("type"), hydra.encode.core.Core.type((x).type)),
      new hydra.core.Field(new hydra.core.Name("expectedArity"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32((x).expectedArity)))),
      new hydra.core.Field(new hydra.core.Name("actualArity"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32((x).actualArity)))),
      new hydra.core.Field(new hydra.core.Name("typeArguments"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.core.Core::type,
        (x).typeArguments))))));
  }

  static hydra.core.Term typeMismatchError(hydra.error.checking.TypeMismatchError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.TypeMismatchError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedType"), hydra.encode.core.Core.type((x).expectedType)),
      new hydra.core.Field(new hydra.core.Name("actualType"), hydra.encode.core.Core.type((x).actualType)))));
  }

  static hydra.core.Term unboundTypeVariablesError(hydra.error.checking.UnboundTypeVariablesError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.UnboundTypeVariablesError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("variables"), new hydra.core.Term.Set(hydra.lib.sets.Map.apply(
        hydra.encode.core.Core::name,
        (x).variables))),
      new hydra.core.Field(new hydra.core.Name("type"), hydra.encode.core.Core.type((x).type)))));
  }

  static hydra.core.Term unequalTypesError(hydra.error.checking.UnequalTypesError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.UnequalTypesError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("types"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.core.Core::type,
        (x).types))),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).description))))));
  }

  static hydra.core.Term unsupportedTermVariantError(hydra.error.checking.UnsupportedTermVariantError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.UnsupportedTermVariantError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("termVariant"), hydra.encode.variants.Variants.termVariant((x).termVariant)))));
  }

  static <T0> hydra.core.Term untypedLambdaError(T0 x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.UntypedLambdaError"), (hydra.util.ConsList<hydra.core.Field>) (hydra.util.ConsList.<hydra.core.Field>empty())));
  }

  static hydra.core.Term untypedLetBindingError(hydra.error.checking.UntypedLetBindingError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.checking.UntypedLetBindingError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("binding"), hydra.encode.core.Core.binding((x).binding)))));
  }
}
