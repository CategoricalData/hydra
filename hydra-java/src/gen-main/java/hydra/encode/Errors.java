// Note: this is an automatically generated file. Do not edit.

package hydra.encode;

/**
 * Term encoders for hydra.errors
 */
public interface Errors {
  static hydra.core.Term decodingError(hydra.errors.DecodingError x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).value))));
  }

  static hydra.core.Term error(hydra.errors.Error_ v1) {
    return (v1).accept(new hydra.errors.Error_.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.errors.Error_.Checking y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("checking"), hydra.encode.error.Checking.checkingError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.Error_.Decoding y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("decoding"), hydra.encode.Errors.decodingError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.Error_.DuplicateBinding y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("duplicateBinding"), hydra.encode.error.Core.duplicateBindingError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.Error_.DuplicateField y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("duplicateField"), hydra.encode.error.Core.duplicateFieldError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.Error_.Other y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("other"), hydra.encode.Errors.otherError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.Error_.UndefinedField y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("undefinedField"), hydra.encode.error.Core.undefinedFieldError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.Error_.UndefinedTermVariable y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("undefinedTermVariable"), hydra.encode.error.Core.undefinedTermVariableError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.Error_.UntypedTermVariable y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("untypedTermVariable"), hydra.encode.error.Core.untypedTermVariableError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.Error_.UnexpectedTermVariant y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("unexpectedTermVariant"), hydra.encode.error.Core.unexpectedTermVariantError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.Error_.UnexpectedTypeVariant y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("unexpectedTypeVariant"), hydra.encode.error.Core.unexpectedTypeVariantError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.Error_.Unification y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("unification"), hydra.encode.Errors.unificationError((y).value))));
      }
    });
  }

  static hydra.core.Term otherError(hydra.errors.OtherError x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.OtherError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).value))));
  }

  static hydra.core.Term unificationError(hydra.errors.UnificationError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.UnificationError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("leftType"), hydra.encode.Core.type((x).leftType)),
      new hydra.core.Field(new hydra.core.Name("rightType"), hydra.encode.Core.type((x).rightType)),
      new hydra.core.Field(new hydra.core.Name("message"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).message))))));
  }
}
