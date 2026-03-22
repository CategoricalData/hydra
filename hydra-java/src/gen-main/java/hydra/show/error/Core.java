// Note: this is an automatically generated file. Do not edit.

package hydra.show.error;

/**
 * String representations of hydra.errors.core types
 */
public interface Core {
  static String duplicateBindingError(hydra.error.core.DuplicateBindingError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "duplicate binding: ",
      (e).name.value));
  }

  static String duplicateFieldError(hydra.error.core.DuplicateFieldError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "duplicate field: ",
      (e).name.value));
  }

  static String invalidTermError(hydra.error.core.InvalidTermError e) {
    return hydra.lib.strings.Cat2.apply(
      "invalid term: ",
      (e).accept(new hydra.error.core.InvalidTermError.PartialVisitor<>() {
        @Override
        public String visit(hydra.error.core.InvalidTermError.DuplicateBinding v1) {
          return hydra.show.error.Core.duplicateBindingError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTermError.DuplicateField v1) {
          return hydra.show.error.Core.duplicateFieldError((v1).value);
        }
      }));
  }

  static String undefinedFieldError(hydra.error.core.UndefinedFieldError e) {
    hydra.core.Name fname = (e).fieldName;
    hydra.core.Name tname = (e).typeName;
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "no such field \"",
      (fname).value,
      "\" in type \"",
      (tname).value,
      "\""));
  }

  static String undefinedTermError(hydra.error.core.UndefinedTermError e) {
    return hydra.lib.strings.Cat2.apply(
      "undefined term: ",
      (e).name.value);
  }

  static String undefinedTypeError(hydra.error.core.UndefinedTypeError e) {
    return hydra.lib.strings.Cat2.apply(
      "undefined type: ",
      (e).name.value);
  }

  static String unexpectedTermVariantError(hydra.error.core.UnexpectedTermVariantError e) {
    hydra.core.Term actual = (e).actualTerm;
    hydra.variants.TermVariant expected = (e).expectedVariant;
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "expected ",
      hydra.show.Meta.termVariant(expected),
      " term but found ",
      hydra.show.Core.term(actual)));
  }

  static String unexpectedTypeVariantError(hydra.error.core.UnexpectedTypeVariantError e) {
    hydra.core.Type actual = (e).actualType;
    hydra.variants.TypeVariant expected = (e).expectedVariant;
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "expected ",
      hydra.show.Meta.typeVariant(expected),
      " type but found ",
      hydra.show.Core.type(actual)));
  }
}
