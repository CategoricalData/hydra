// Note: this is an automatically generated file. Do not edit.

package hydra.show.error;

/**
 * String representations of hydra.error.core types
 */
public interface Core {
  static String constantConditionError(hydra.error.core.ConstantConditionError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "constant condition: ifElse with literal ",
      hydra.lib.literals.ShowBoolean.apply((e).value)));
  }

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

  static String duplicateRecordTypeFieldNamesError(hydra.error.core.DuplicateRecordTypeFieldNamesError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "duplicate field in record type: ",
      (e).name.value));
  }

  static String duplicateUnionTypeFieldNamesError(hydra.error.core.DuplicateUnionTypeFieldNamesError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "duplicate field in union type: ",
      (e).name.value));
  }

  static String emptyCaseStatementError(hydra.error.core.EmptyCaseStatementError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "empty case statement for type: ",
      (e).typeName.value));
  }

  static <T0> String emptyLetBindingsError(T0 e) {
    return "let expression with no bindings";
  }

  static <T0> String emptyRecordTypeError(T0 e) {
    return "record type with no fields (use TypeUnit instead)";
  }

  static <T0> String emptyTermAnnotationError(T0 e) {
    return "term annotation with empty annotation map";
  }

  static <T0> String emptyTypeAnnotationError(T0 e) {
    return "type annotation with empty annotation map";
  }

  static <T0> String emptyTypeNameInTermError(T0 e) {
    return "term with empty type name";
  }

  static <T0> String emptyUnionTypeError(T0 e) {
    return "union type with no alternatives (use TypeVoid instead)";
  }

  static String invalidForallParameterNameError(hydra.error.core.InvalidForallParameterNameError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "invalid forall parameter name: ",
      (e).name.value));
  }

  static String invalidLambdaParameterNameError(hydra.error.core.InvalidLambdaParameterNameError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "invalid lambda parameter name: ",
      (e).name.value));
  }

  static String invalidLetBindingNameError(hydra.error.core.InvalidLetBindingNameError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "invalid let binding name: ",
      (e).name.value));
  }

  static String invalidTermError(hydra.error.core.InvalidTermError e) {
    return hydra.lib.strings.Cat2.apply(
      "invalid term: ",
      (e).accept(new hydra.error.core.InvalidTermError.PartialVisitor<>() {
        @Override
        public String visit(hydra.error.core.InvalidTermError.ConstantCondition v1) {
          return hydra.show.error.Core.constantConditionError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTermError.DuplicateBinding v1) {
          return hydra.show.error.Core.duplicateBindingError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTermError.DuplicateField v1) {
          return hydra.show.error.Core.duplicateFieldError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTermError.EmptyCaseStatement v1) {
          return hydra.show.error.Core.emptyCaseStatementError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTermError.EmptyLetBindings v1) {
          return hydra.show.error.Core.emptyLetBindingsError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTermError.EmptyTermAnnotation v1) {
          return hydra.show.error.Core.emptyTermAnnotationError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTermError.EmptyTypeNameInTerm v1) {
          return hydra.show.error.Core.emptyTypeNameInTermError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTermError.InvalidLambdaParameterName v1) {
          return hydra.show.error.Core.invalidLambdaParameterNameError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTermError.InvalidLetBindingName v1) {
          return hydra.show.error.Core.invalidLetBindingNameError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTermError.InvalidTypeLambdaParameterName v1) {
          return hydra.show.error.Core.invalidTypeLambdaParameterNameError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTermError.NestedTermAnnotation v1) {
          return hydra.show.error.Core.nestedTermAnnotationError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTermError.RedundantWrapUnwrap v1) {
          return hydra.show.error.Core.redundantWrapUnwrapError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTermError.SelfApplication v1) {
          return hydra.show.error.Core.selfApplicationError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTermError.TermVariableShadowing v1) {
          return hydra.show.error.Core.termVariableShadowingError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTermError.TypeVariableShadowingInTypeLambda v1) {
          return hydra.show.error.Core.typeVariableShadowingInTypeLambdaError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTermError.UndefinedTermVariable v1) {
          return hydra.show.error.Core.undefinedTermVariableError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTermError.UndefinedTypeVariableInBindingType v1) {
          return hydra.show.error.Core.undefinedTypeVariableInBindingTypeError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTermError.UndefinedTypeVariableInLambdaDomain v1) {
          return hydra.show.error.Core.undefinedTypeVariableInLambdaDomainError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTermError.UndefinedTypeVariableInTypeApplication v1) {
          return hydra.show.error.Core.undefinedTypeVariableInTypeApplicationError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTermError.UnknownPrimitiveName v1) {
          return hydra.show.error.Core.unknownPrimitiveNameError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTermError.UnnecessaryIdentityApplication v1) {
          return hydra.show.error.Core.unnecessaryIdentityApplicationError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTermError.UntypedTermVariable v1) {
          return hydra.show.error.Core.untypedTermVariableError((v1).value);
        }
      }));
  }

  static String invalidTypeError(hydra.error.core.InvalidTypeError e) {
    return hydra.lib.strings.Cat2.apply(
      "invalid type: ",
      (e).accept(new hydra.error.core.InvalidTypeError.PartialVisitor<>() {
        @Override
        public String visit(hydra.error.core.InvalidTypeError.DuplicateRecordTypeFieldNames v1) {
          return hydra.show.error.Core.duplicateRecordTypeFieldNamesError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTypeError.DuplicateUnionTypeFieldNames v1) {
          return hydra.show.error.Core.duplicateUnionTypeFieldNamesError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTypeError.EmptyRecordType v1) {
          return hydra.show.error.Core.emptyRecordTypeError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTypeError.EmptyTypeAnnotation v1) {
          return hydra.show.error.Core.emptyTypeAnnotationError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTypeError.EmptyUnionType v1) {
          return hydra.show.error.Core.emptyUnionTypeError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTypeError.InvalidForallParameterName v1) {
          return hydra.show.error.Core.invalidForallParameterNameError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTypeError.InvalidTypeSchemeVariableName v1) {
          return hydra.show.error.Core.invalidTypeSchemeVariableNameError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTypeError.NestedTypeAnnotation v1) {
          return hydra.show.error.Core.nestedTypeAnnotationError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTypeError.NonComparableMapKeyType v1) {
          return hydra.show.error.Core.nonComparableMapKeyTypeError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTypeError.NonComparableSetElementType v1) {
          return hydra.show.error.Core.nonComparableSetElementTypeError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTypeError.SingleVariantUnion v1) {
          return hydra.show.error.Core.singleVariantUnionError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTypeError.TypeVariableShadowingInForall v1) {
          return hydra.show.error.Core.typeVariableShadowingInForallError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTypeError.UndefinedTypeVariable v1) {
          return hydra.show.error.Core.undefinedTypeVariableError((v1).value);
        }

        @Override
        public String visit(hydra.error.core.InvalidTypeError.VoidInNonBottomPosition v1) {
          return hydra.show.error.Core.voidInNonBottomPositionError((v1).value);
        }
      }));
  }

  static String invalidTypeLambdaParameterNameError(hydra.error.core.InvalidTypeLambdaParameterNameError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "invalid type lambda parameter name: ",
      (e).name.value));
  }

  static String invalidTypeSchemeVariableNameError(hydra.error.core.InvalidTypeSchemeVariableNameError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "invalid type scheme variable name: ",
      (e).name.value));
  }

  static <T0> String nestedTermAnnotationError(T0 e) {
    return "nested term annotations should be merged";
  }

  static <T0> String nestedTypeAnnotationError(T0 e) {
    return "nested type annotations should be merged";
  }

  static String nonComparableMapKeyTypeError(hydra.error.core.NonComparableMapKeyTypeError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "map key type contains a function type: ",
      hydra.show.Core.type((e).keyType)));
  }

  static String nonComparableSetElementTypeError(hydra.error.core.NonComparableSetElementTypeError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "set element type contains a function type: ",
      hydra.show.Core.type((e).elementType)));
  }

  static String redundantWrapUnwrapError(hydra.error.core.RedundantWrapUnwrapError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "redundant wrap/unwrap for type: ",
      (e).typeName.value));
  }

  static String selfApplicationError(hydra.error.core.SelfApplicationError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "self-application of variable: ",
      (e).name.value));
  }

  static String singleVariantUnionError(hydra.error.core.SingleVariantUnionError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "union type with single variant: ",
      (e).fieldName.value));
  }

  static String termVariableShadowingError(hydra.error.core.TermVariableShadowingError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "variable shadowing: ",
      (e).name.value));
  }

  static String typeVariableShadowingInForallError(hydra.error.core.TypeVariableShadowingInForallError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "type variable shadowing in forall: ",
      (e).name.value));
  }

  static String typeVariableShadowingInTypeLambdaError(hydra.error.core.TypeVariableShadowingInTypeLambdaError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "type variable shadowing in type lambda: ",
      (e).name.value));
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

  static String undefinedTermVariableError(hydra.error.core.UndefinedTermVariableError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "undefined term variable: ",
      (e).name.value));
  }

  static String undefinedTypeVariableError(hydra.error.core.UndefinedTypeVariableError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "undefined type variable: ",
      (e).name.value));
  }

  static String undefinedTypeVariableInBindingTypeError(hydra.error.core.UndefinedTypeVariableInBindingTypeError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "undefined type variable in binding type: ",
      (e).name.value));
  }

  static String undefinedTypeVariableInLambdaDomainError(hydra.error.core.UndefinedTypeVariableInLambdaDomainError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "undefined type variable in lambda domain: ",
      (e).name.value));
  }

  static String undefinedTypeVariableInTypeApplicationError(hydra.error.core.UndefinedTypeVariableInTypeApplicationError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "undefined type variable in type application: ",
      (e).name.value));
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

  static String unknownPrimitiveNameError(hydra.error.core.UnknownPrimitiveNameError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "unknown primitive: ",
      (e).name.value));
  }

  static <T0> String unnecessaryIdentityApplicationError(T0 e) {
    return "unnecessary application of identity lambda";
  }

  static String untypedTermVariableError(hydra.error.core.UntypedTermVariableError e) {
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "untyped term variable: ",
      (e).name.value));
  }

  static <T0> String voidInNonBottomPositionError(T0 e) {
    return "TypeVoid in a position where no value can be constructed";
  }
}
