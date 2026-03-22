// Note: this is an automatically generated file. Do not edit.

package hydra.encode.error;

/**
 * Term encoders for hydra.error.core
 */
public interface Core {
  static hydra.core.Term duplicateBindingError(hydra.error.core.DuplicateBindingError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.DuplicateBindingError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Accessors.accessorPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term duplicateFieldError(hydra.error.core.DuplicateFieldError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.DuplicateFieldError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Accessors.accessorPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term invalidTermError(hydra.error.core.InvalidTermError v1) {
    return (v1).accept(new hydra.error.core.InvalidTermError.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.DuplicateBinding y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("duplicateBinding"), hydra.encode.error.Core.duplicateBindingError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.DuplicateField y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("duplicateField"), hydra.encode.error.Core.duplicateFieldError((y).value))));
      }
    });
  }

  static hydra.core.Term undefinedFieldError(hydra.error.core.UndefinedFieldError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedFieldError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("fieldName"), hydra.encode.Core.name((x).fieldName)),
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.encode.Core.name((x).typeName)))));
  }

  static hydra.core.Term undefinedTermError(hydra.error.core.UndefinedTermError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTermError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term undefinedTypeError(hydra.error.core.UndefinedTypeError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTypeError"), hydra.util.ConsList.of(new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term unexpectedTermVariantError(hydra.error.core.UnexpectedTermVariantError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedVariant"), hydra.encode.Variants.termVariant((x).expectedVariant)),
      new hydra.core.Field(new hydra.core.Name("actualTerm"), hydra.encode.Core.term((x).actualTerm)))));
  }

  static hydra.core.Term unexpectedTypeVariantError(hydra.error.core.UnexpectedTypeVariantError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("expectedVariant"), hydra.encode.Variants.typeVariant((x).expectedVariant)),
      new hydra.core.Field(new hydra.core.Name("actualType"), hydra.encode.Core.type((x).actualType)))));
  }
}
