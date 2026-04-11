// Note: this is an automatically generated file. Do not edit.

package hydra.encode;

/**
 * Term encoders for hydra.errors
 */
public interface Errors {
  static hydra.core.Term decodingError(hydra.errors.DecodingError x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.DecodingError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).value))));
  }

  static <T0> hydra.core.Term emptyListError(T0 ignored) {
    return new hydra.core.Term.Unit();
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
      public hydra.core.Term visit(hydra.errors.Error_.Extraction y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("extraction"), hydra.encode.Errors.extractionError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.Error_.Inference y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("inference"), hydra.encode.Errors.inferenceError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.Error_.Other y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("other"), hydra.encode.Errors.otherError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.Error_.Resolution y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.Error"), new hydra.core.Field(new hydra.core.Name("resolution"), hydra.encode.Errors.resolutionError((y).value))));
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

  static hydra.core.Term extractionError(hydra.errors.ExtractionError v1) {
    return (v1).accept(new hydra.errors.ExtractionError.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.errors.ExtractionError.EmptyList y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ExtractionError"), new hydra.core.Field(new hydra.core.Name("emptyList"), hydra.encode.Errors.emptyListError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.ExtractionError.MultipleBindings y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ExtractionError"), new hydra.core.Field(new hydra.core.Name("multipleBindings"), hydra.encode.Errors.multipleBindingsError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.ExtractionError.MultipleFields y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ExtractionError"), new hydra.core.Field(new hydra.core.Name("multipleFields"), hydra.encode.Errors.multipleFieldsError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.ExtractionError.NoMatchingField y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ExtractionError"), new hydra.core.Field(new hydra.core.Name("noMatchingField"), hydra.encode.Errors.noMatchingFieldError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.ExtractionError.NoSuchBinding y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ExtractionError"), new hydra.core.Field(new hydra.core.Name("noSuchBinding"), hydra.encode.Errors.noSuchBindingError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.ExtractionError.NotEnoughCases y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ExtractionError"), new hydra.core.Field(new hydra.core.Name("notEnoughCases"), hydra.encode.Errors.notEnoughCasesError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.ExtractionError.UnexpectedShape y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ExtractionError"), new hydra.core.Field(new hydra.core.Name("unexpectedShape"), hydra.encode.Errors.unexpectedShapeError((y).value))));
      }
    });
  }

  static hydra.core.Term inferenceError(hydra.errors.InferenceError v1) {
    return (v1).accept(new hydra.errors.InferenceError.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.errors.InferenceError.Checking y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.InferenceError"), new hydra.core.Field(new hydra.core.Name("checking"), hydra.encode.error.Checking.checkingError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.InferenceError.Other y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.InferenceError"), new hydra.core.Field(new hydra.core.Name("other"), hydra.encode.Errors.otherInferenceError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.InferenceError.Unification y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.InferenceError"), new hydra.core.Field(new hydra.core.Name("unification"), hydra.encode.Errors.unificationInferenceError((y).value))));
      }
    });
  }

  static hydra.core.Term multipleBindingsError(hydra.errors.MultipleBindingsError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.MultipleBindingsError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term multipleFieldsError(hydra.errors.MultipleFieldsError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.MultipleFieldsError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("fieldName"), hydra.encode.Core.name((x).fieldName)))));
  }

  static hydra.core.Term noMatchingFieldError(hydra.errors.NoMatchingFieldError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.NoMatchingFieldError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("fieldName"), hydra.encode.Core.name((x).fieldName)))));
  }

  static hydra.core.Term noSuchBindingError(hydra.errors.NoSuchBindingError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.NoSuchBindingError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term noSuchPrimitiveError(hydra.errors.NoSuchPrimitiveError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.NoSuchPrimitiveError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static <T0> hydra.core.Term notEnoughCasesError(T0 ignored) {
    return new hydra.core.Term.Unit();
  }

  static hydra.core.Term otherError(hydra.errors.OtherError x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.OtherError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).value))));
  }

  static hydra.core.Term otherInferenceError(hydra.errors.OtherInferenceError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.OtherInferenceError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("path"), hydra.encode.Paths.subtermPath((x).path)),
      new hydra.core.Field(new hydra.core.Name("message"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).message))))));
  }

  static hydra.core.Term otherResolutionError(hydra.errors.OtherResolutionError x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.errors.OtherResolutionError"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).value))));
  }

  static hydra.core.Term resolutionError(hydra.errors.ResolutionError v1) {
    return (v1).accept(new hydra.errors.ResolutionError.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.errors.ResolutionError.NoSuchBinding y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ResolutionError"), new hydra.core.Field(new hydra.core.Name("noSuchBinding"), hydra.encode.Errors.noSuchBindingError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.ResolutionError.NoSuchPrimitive y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ResolutionError"), new hydra.core.Field(new hydra.core.Name("noSuchPrimitive"), hydra.encode.Errors.noSuchPrimitiveError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.ResolutionError.NoMatchingField y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ResolutionError"), new hydra.core.Field(new hydra.core.Name("noMatchingField"), hydra.encode.Errors.noMatchingFieldError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.ResolutionError.Other y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ResolutionError"), new hydra.core.Field(new hydra.core.Name("other"), hydra.encode.Errors.otherResolutionError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.errors.ResolutionError.UnexpectedShape y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.errors.ResolutionError"), new hydra.core.Field(new hydra.core.Name("unexpectedShape"), hydra.encode.Errors.unexpectedShapeError((y).value))));
      }
    });
  }

  static hydra.core.Term unexpectedShapeError(hydra.errors.UnexpectedShapeError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.UnexpectedShapeError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).expected))),
      new hydra.core.Field(new hydra.core.Name("actual"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).actual))))));
  }

  static hydra.core.Term unificationError(hydra.errors.UnificationError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.UnificationError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("leftType"), hydra.encode.Core.type((x).leftType)),
      new hydra.core.Field(new hydra.core.Name("rightType"), hydra.encode.Core.type((x).rightType)),
      new hydra.core.Field(new hydra.core.Name("message"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).message))))));
  }

  static hydra.core.Term unificationInferenceError(hydra.errors.UnificationInferenceError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.errors.UnificationInferenceError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("path"), hydra.encode.Paths.subtermPath((x).path)),
      new hydra.core.Field(new hydra.core.Name("cause"), hydra.encode.Errors.unificationError((x).cause)))));
  }
}
