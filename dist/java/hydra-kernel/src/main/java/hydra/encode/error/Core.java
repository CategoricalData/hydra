// Note: this is an automatically generated file. Do not edit.

package hydra.encode.error;

/**
 * Term encoders for hydra.error.core
 */
public interface Core {
  static hydra.core.Term constantConditionError(hydra.error.core.ConstantConditionError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.ConstantConditionError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("value"), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_((x).value))))));
  }

  static hydra.core.Term duplicateBindingError(hydra.error.core.DuplicateBindingError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.DuplicateBindingError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term duplicateFieldError(hydra.error.core.DuplicateFieldError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.DuplicateFieldError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term duplicateRecordTypeFieldNamesError(hydra.error.core.DuplicateRecordTypeFieldNamesError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.DuplicateRecordTypeFieldNamesError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term duplicateUnionTypeFieldNamesError(hydra.error.core.DuplicateUnionTypeFieldNamesError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.DuplicateUnionTypeFieldNamesError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term emptyCaseStatementError(hydra.error.core.EmptyCaseStatementError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.EmptyCaseStatementError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.encode.Core.name((x).typeName)))));
  }

  static hydra.core.Term emptyLetBindingsError(hydra.error.core.EmptyLetBindingsError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.EmptyLetBindingsError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)))));
  }

  static hydra.core.Term emptyRecordTypeError(hydra.error.core.EmptyRecordTypeError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.EmptyRecordTypeError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)))));
  }

  static hydra.core.Term emptyTermAnnotationError(hydra.error.core.EmptyTermAnnotationError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.EmptyTermAnnotationError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)))));
  }

  static hydra.core.Term emptyTypeAnnotationError(hydra.error.core.EmptyTypeAnnotationError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.EmptyTypeAnnotationError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)))));
  }

  static hydra.core.Term emptyTypeNameInTermError(hydra.error.core.EmptyTypeNameInTermError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.EmptyTypeNameInTermError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)))));
  }

  static hydra.core.Term emptyUnionTypeError(hydra.error.core.EmptyUnionTypeError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.EmptyUnionTypeError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)))));
  }

  static hydra.core.Term invalidForallParameterNameError(hydra.error.core.InvalidForallParameterNameError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.InvalidForallParameterNameError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term invalidLambdaParameterNameError(hydra.error.core.InvalidLambdaParameterNameError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.InvalidLambdaParameterNameError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term invalidLetBindingNameError(hydra.error.core.InvalidLetBindingNameError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.InvalidLetBindingNameError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term invalidTermError(hydra.error.core.InvalidTermError v1) {
    return (v1).accept(new hydra.error.core.InvalidTermError.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.ConstantCondition y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("constantCondition"), hydra.encode.error.Core.constantConditionError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.DuplicateBinding y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("duplicateBinding"), hydra.encode.error.Core.duplicateBindingError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.DuplicateField y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("duplicateField"), hydra.encode.error.Core.duplicateFieldError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.EmptyCaseStatement y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("emptyCaseStatement"), hydra.encode.error.Core.emptyCaseStatementError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.EmptyLetBindings y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("emptyLetBindings"), hydra.encode.error.Core.emptyLetBindingsError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.EmptyTermAnnotation y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("emptyTermAnnotation"), hydra.encode.error.Core.emptyTermAnnotationError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.EmptyTypeNameInTerm y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("emptyTypeNameInTerm"), hydra.encode.error.Core.emptyTypeNameInTermError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.InvalidLambdaParameterName y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("invalidLambdaParameterName"), hydra.encode.error.Core.invalidLambdaParameterNameError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.InvalidLetBindingName y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("invalidLetBindingName"), hydra.encode.error.Core.invalidLetBindingNameError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.InvalidTypeLambdaParameterName y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("invalidTypeLambdaParameterName"), hydra.encode.error.Core.invalidTypeLambdaParameterNameError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.NestedTermAnnotation y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("nestedTermAnnotation"), hydra.encode.error.Core.nestedTermAnnotationError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.RedundantWrapUnwrap y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("redundantWrapUnwrap"), hydra.encode.error.Core.redundantWrapUnwrapError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.SelfApplication y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("selfApplication"), hydra.encode.error.Core.selfApplicationError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.TermVariableShadowing y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("termVariableShadowing"), hydra.encode.error.Core.termVariableShadowingError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.TypeVariableShadowingInTypeLambda y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("typeVariableShadowingInTypeLambda"), hydra.encode.error.Core.typeVariableShadowingInTypeLambdaError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.UndefinedTermVariable y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("undefinedTermVariable"), hydra.encode.error.Core.undefinedTermVariableError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.UndefinedTypeVariableInBindingType y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("undefinedTypeVariableInBindingType"), hydra.encode.error.Core.undefinedTypeVariableInBindingTypeError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.UndefinedTypeVariableInLambdaDomain y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("undefinedTypeVariableInLambdaDomain"), hydra.encode.error.Core.undefinedTypeVariableInLambdaDomainError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.UndefinedTypeVariableInTypeApplication y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("undefinedTypeVariableInTypeApplication"), hydra.encode.error.Core.undefinedTypeVariableInTypeApplicationError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.UnknownPrimitiveName y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("unknownPrimitiveName"), hydra.encode.error.Core.unknownPrimitiveNameError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.UnnecessaryIdentityApplication y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("unnecessaryIdentityApplication"), hydra.encode.error.Core.unnecessaryIdentityApplicationError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTermError.UntypedTermVariable y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTermError"), new hydra.core.Field(new hydra.core.Name("untypedTermVariable"), hydra.encode.error.Core.untypedTermVariableError((y).value))));
      }
    });
  }

  static hydra.core.Term invalidTypeError(hydra.error.core.InvalidTypeError v1) {
    return (v1).accept(new hydra.error.core.InvalidTypeError.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTypeError.DuplicateRecordTypeFieldNames y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("duplicateRecordTypeFieldNames"), hydra.encode.error.Core.duplicateRecordTypeFieldNamesError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTypeError.DuplicateUnionTypeFieldNames y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("duplicateUnionTypeFieldNames"), hydra.encode.error.Core.duplicateUnionTypeFieldNamesError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTypeError.EmptyRecordType y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("emptyRecordType"), hydra.encode.error.Core.emptyRecordTypeError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTypeError.EmptyTypeAnnotation y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("emptyTypeAnnotation"), hydra.encode.error.Core.emptyTypeAnnotationError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTypeError.EmptyUnionType y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("emptyUnionType"), hydra.encode.error.Core.emptyUnionTypeError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTypeError.InvalidForallParameterName y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("invalidForallParameterName"), hydra.encode.error.Core.invalidForallParameterNameError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTypeError.InvalidTypeSchemeVariableName y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("invalidTypeSchemeVariableName"), hydra.encode.error.Core.invalidTypeSchemeVariableNameError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTypeError.NestedTypeAnnotation y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("nestedTypeAnnotation"), hydra.encode.error.Core.nestedTypeAnnotationError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTypeError.NonComparableMapKeyType y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("nonComparableMapKeyType"), hydra.encode.error.Core.nonComparableMapKeyTypeError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTypeError.NonComparableSetElementType y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("nonComparableSetElementType"), hydra.encode.error.Core.nonComparableSetElementTypeError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTypeError.SingleVariantUnion y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("singleVariantUnion"), hydra.encode.error.Core.singleVariantUnionError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTypeError.TypeVariableShadowingInForall y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("typeVariableShadowingInForall"), hydra.encode.error.Core.typeVariableShadowingInForallError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTypeError.UndefinedTypeVariable y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("undefinedTypeVariable"), hydra.encode.error.Core.undefinedTypeVariableError((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.error.core.InvalidTypeError.VoidInNonBottomPosition y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.error.core.InvalidTypeError"), new hydra.core.Field(new hydra.core.Name("voidInNonBottomPosition"), hydra.encode.error.Core.voidInNonBottomPositionError((y).value))));
      }
    });
  }

  static hydra.core.Term invalidTypeLambdaParameterNameError(hydra.error.core.InvalidTypeLambdaParameterNameError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.InvalidTypeLambdaParameterNameError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term invalidTypeSchemeVariableNameError(hydra.error.core.InvalidTypeSchemeVariableNameError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.InvalidTypeSchemeVariableNameError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term nestedTermAnnotationError(hydra.error.core.NestedTermAnnotationError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.NestedTermAnnotationError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)))));
  }

  static hydra.core.Term nestedTypeAnnotationError(hydra.error.core.NestedTypeAnnotationError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.NestedTypeAnnotationError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)))));
  }

  static hydra.core.Term nonComparableMapKeyTypeError(hydra.error.core.NonComparableMapKeyTypeError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.NonComparableMapKeyTypeError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("keyType"), hydra.encode.Core.type((x).keyType)))));
  }

  static hydra.core.Term nonComparableSetElementTypeError(hydra.error.core.NonComparableSetElementTypeError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.NonComparableSetElementTypeError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("elementType"), hydra.encode.Core.type((x).elementType)))));
  }

  static hydra.core.Term redundantWrapUnwrapError(hydra.error.core.RedundantWrapUnwrapError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.RedundantWrapUnwrapError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.encode.Core.name((x).typeName)))));
  }

  static hydra.core.Term selfApplicationError(hydra.error.core.SelfApplicationError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.SelfApplicationError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term singleVariantUnionError(hydra.error.core.SingleVariantUnionError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.SingleVariantUnionError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("fieldName"), hydra.encode.Core.name((x).fieldName)))));
  }

  static hydra.core.Term termVariableShadowingError(hydra.error.core.TermVariableShadowingError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.TermVariableShadowingError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term typeVariableShadowingInForallError(hydra.error.core.TypeVariableShadowingInForallError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.TypeVariableShadowingInForallError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term typeVariableShadowingInTypeLambdaError(hydra.error.core.TypeVariableShadowingInTypeLambdaError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.TypeVariableShadowingInTypeLambdaError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term undefinedFieldError(hydra.error.core.UndefinedFieldError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedFieldError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("fieldName"), hydra.encode.Core.name((x).fieldName)),
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.encode.Core.name((x).typeName)))));
  }

  static hydra.core.Term undefinedTermVariableError(hydra.error.core.UndefinedTermVariableError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTermVariableError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term undefinedTypeVariableError(hydra.error.core.UndefinedTypeVariableError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term undefinedTypeVariableInBindingTypeError(hydra.error.core.UndefinedTypeVariableInBindingTypeError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInBindingTypeError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term undefinedTypeVariableInLambdaDomainError(hydra.error.core.UndefinedTypeVariableInLambdaDomainError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInLambdaDomainError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term undefinedTypeVariableInTypeApplicationError(hydra.error.core.UndefinedTypeVariableInTypeApplicationError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInTypeApplicationError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term unexpectedTermVariantError(hydra.error.core.UnexpectedTermVariantError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("expectedVariant"), hydra.encode.Variants.termVariant((x).expectedVariant)),
      new hydra.core.Field(new hydra.core.Name("actualTerm"), hydra.encode.Core.term((x).actualTerm)))));
  }

  static hydra.core.Term unexpectedTypeVariantError(hydra.error.core.UnexpectedTypeVariantError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("expectedVariant"), hydra.encode.Variants.typeVariant((x).expectedVariant)),
      new hydra.core.Field(new hydra.core.Name("actualType"), hydra.encode.Core.type((x).actualType)))));
  }

  static hydra.core.Term unknownPrimitiveNameError(hydra.error.core.UnknownPrimitiveNameError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UnknownPrimitiveNameError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term unnecessaryIdentityApplicationError(hydra.error.core.UnnecessaryIdentityApplicationError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UnnecessaryIdentityApplicationError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)))));
  }

  static hydra.core.Term untypedTermVariableError(hydra.error.core.UntypedTermVariableError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.UntypedTermVariableError"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)),
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name((x).name)))));
  }

  static hydra.core.Term voidInNonBottomPositionError(hydra.error.core.VoidInNonBottomPositionError x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.error.core.VoidInNonBottomPositionError"), java.util.Arrays.asList(new hydra.core.Field(new hydra.core.Name("location"), hydra.encode.Paths.subtermPath((x).location)))));
  }
}
