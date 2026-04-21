// Note: this is an automatically generated file. Do not edit.

/**
 * Term encoders for hydra.error.core
 */



import * as Core from "../../core.js";
import * as EncodeCore from "../core.js";
import * as EncodePaths from "../paths.js";
import * as EncodeVariants from "../variants.js";
import * as ErrorCore from "../../error/core.js";

export function constantConditionError(x: ErrorCore.ConstantConditionError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.ConstantConditionError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "value",
    term: ({ tag: "literal", value: ({ tag: "boolean", value: ((_x) => _x.value)(x) }) })
  })]
  }) });
}

export function duplicateBindingError(x: ErrorCore.DuplicateBindingError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.DuplicateBindingError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function duplicateFieldError(x: ErrorCore.DuplicateFieldError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.DuplicateFieldError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function duplicateRecordTypeFieldNamesError(x: ErrorCore.DuplicateRecordTypeFieldNamesError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.DuplicateRecordTypeFieldNamesError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function duplicateUnionTypeFieldNamesError(x: ErrorCore.DuplicateUnionTypeFieldNamesError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.DuplicateUnionTypeFieldNamesError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function emptyCaseStatementError(x: ErrorCore.EmptyCaseStatementError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.EmptyCaseStatementError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "typeName",
    term: EncodeCore.name(((_x) => _x.typeName)(x))
  })]
  }) });
}

export function emptyLetBindingsError(x: ErrorCore.EmptyLetBindingsError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.EmptyLetBindingsError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  })]
  }) });
}

export function emptyRecordTypeError(x: ErrorCore.EmptyRecordTypeError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.EmptyRecordTypeError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  })]
  }) });
}

export function emptyTermAnnotationError(x: ErrorCore.EmptyTermAnnotationError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.EmptyTermAnnotationError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  })]
  }) });
}

export function emptyTypeAnnotationError(x: ErrorCore.EmptyTypeAnnotationError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.EmptyTypeAnnotationError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  })]
  }) });
}

export function emptyTypeNameInTermError(x: ErrorCore.EmptyTypeNameInTermError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.EmptyTypeNameInTermError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  })]
  }) });
}

export function emptyUnionTypeError(x: ErrorCore.EmptyUnionTypeError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.EmptyUnionTypeError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  })]
  }) });
}

export function invalidForallParameterNameError(x: ErrorCore.InvalidForallParameterNameError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.InvalidForallParameterNameError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function invalidLambdaParameterNameError(x: ErrorCore.InvalidLambdaParameterNameError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.InvalidLambdaParameterNameError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function invalidLetBindingNameError(x: ErrorCore.InvalidLetBindingNameError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.InvalidLetBindingNameError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function invalidTermError(v1: ErrorCore.InvalidTermError): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "constantCondition": return ((y: ErrorCore.ConstantConditionError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTermError",
    field: ({
    name: "constantCondition",
    term: constantConditionError(y)
  })
  }) }))((_m as any).value);
    case "duplicateBinding": return ((y: ErrorCore.DuplicateBindingError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTermError",
    field: ({
    name: "duplicateBinding",
    term: duplicateBindingError(y)
  })
  }) }))((_m as any).value);
    case "duplicateField": return ((y: ErrorCore.DuplicateFieldError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTermError",
    field: ({
    name: "duplicateField",
    term: duplicateFieldError(y)
  })
  }) }))((_m as any).value);
    case "emptyCaseStatement": return ((y: ErrorCore.EmptyCaseStatementError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTermError",
    field: ({
    name: "emptyCaseStatement",
    term: emptyCaseStatementError(y)
  })
  }) }))((_m as any).value);
    case "emptyLetBindings": return ((y: ErrorCore.EmptyLetBindingsError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTermError",
    field: ({
    name: "emptyLetBindings",
    term: emptyLetBindingsError(y)
  })
  }) }))((_m as any).value);
    case "emptyTermAnnotation": return ((y: ErrorCore.EmptyTermAnnotationError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTermError",
    field: ({
    name: "emptyTermAnnotation",
    term: emptyTermAnnotationError(y)
  })
  }) }))((_m as any).value);
    case "emptyTypeNameInTerm": return ((y: ErrorCore.EmptyTypeNameInTermError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTermError",
    field: ({
    name: "emptyTypeNameInTerm",
    term: emptyTypeNameInTermError(y)
  })
  }) }))((_m as any).value);
    case "invalidLambdaParameterName": return ((y: ErrorCore.InvalidLambdaParameterNameError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTermError",
    field: ({
    name: "invalidLambdaParameterName",
    term: invalidLambdaParameterNameError(y)
  })
  }) }))((_m as any).value);
    case "invalidLetBindingName": return ((y: ErrorCore.InvalidLetBindingNameError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTermError",
    field: ({
    name: "invalidLetBindingName",
    term: invalidLetBindingNameError(y)
  })
  }) }))((_m as any).value);
    case "invalidTypeLambdaParameterName": return ((y: ErrorCore.InvalidTypeLambdaParameterNameError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTermError",
    field: ({
    name: "invalidTypeLambdaParameterName",
    term: invalidTypeLambdaParameterNameError(y)
  })
  }) }))((_m as any).value);
    case "nestedTermAnnotation": return ((y: ErrorCore.NestedTermAnnotationError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTermError",
    field: ({
    name: "nestedTermAnnotation",
    term: nestedTermAnnotationError(y)
  })
  }) }))((_m as any).value);
    case "redundantWrapUnwrap": return ((y: ErrorCore.RedundantWrapUnwrapError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTermError",
    field: ({
    name: "redundantWrapUnwrap",
    term: redundantWrapUnwrapError(y)
  })
  }) }))((_m as any).value);
    case "selfApplication": return ((y: ErrorCore.SelfApplicationError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTermError",
    field: ({
    name: "selfApplication",
    term: selfApplicationError(y)
  })
  }) }))((_m as any).value);
    case "termVariableShadowing": return ((y: ErrorCore.TermVariableShadowingError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTermError",
    field: ({
    name: "termVariableShadowing",
    term: termVariableShadowingError(y)
  })
  }) }))((_m as any).value);
    case "typeVariableShadowingInTypeLambda": return ((y: ErrorCore.TypeVariableShadowingInTypeLambdaError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTermError",
    field: ({
    name: "typeVariableShadowingInTypeLambda",
    term: typeVariableShadowingInTypeLambdaError(y)
  })
  }) }))((_m as any).value);
    case "undefinedTermVariable": return ((y: ErrorCore.UndefinedTermVariableError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTermError",
    field: ({
    name: "undefinedTermVariable",
    term: undefinedTermVariableError(y)
  })
  }) }))((_m as any).value);
    case "undefinedTypeVariableInBindingType": return ((y: ErrorCore.UndefinedTypeVariableInBindingTypeError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTermError",
    field: ({
    name: "undefinedTypeVariableInBindingType",
    term: undefinedTypeVariableInBindingTypeError(y)
  })
  }) }))((_m as any).value);
    case "undefinedTypeVariableInLambdaDomain": return ((y: ErrorCore.UndefinedTypeVariableInLambdaDomainError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTermError",
    field: ({
    name: "undefinedTypeVariableInLambdaDomain",
    term: undefinedTypeVariableInLambdaDomainError(y)
  })
  }) }))((_m as any).value);
    case "undefinedTypeVariableInTypeApplication": return ((y: ErrorCore.UndefinedTypeVariableInTypeApplicationError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTermError",
    field: ({
    name: "undefinedTypeVariableInTypeApplication",
    term: undefinedTypeVariableInTypeApplicationError(y)
  })
  }) }))((_m as any).value);
    case "unknownPrimitiveName": return ((y: ErrorCore.UnknownPrimitiveNameError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTermError",
    field: ({
    name: "unknownPrimitiveName",
    term: unknownPrimitiveNameError(y)
  })
  }) }))((_m as any).value);
    case "unnecessaryIdentityApplication": return ((y: ErrorCore.UnnecessaryIdentityApplicationError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTermError",
    field: ({
    name: "unnecessaryIdentityApplication",
    term: unnecessaryIdentityApplicationError(y)
  })
  }) }))((_m as any).value);
    case "untypedTermVariable": return ((y: ErrorCore.UntypedTermVariableError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTermError",
    field: ({
    name: "untypedTermVariable",
    term: untypedTermVariableError(y)
  })
  }) }))((_m as any).value);
  }
})();
}

export function invalidTypeError(v1: ErrorCore.InvalidTypeError): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "duplicateRecordTypeFieldNames": return ((y: ErrorCore.DuplicateRecordTypeFieldNamesError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTypeError",
    field: ({
    name: "duplicateRecordTypeFieldNames",
    term: duplicateRecordTypeFieldNamesError(y)
  })
  }) }))((_m as any).value);
    case "duplicateUnionTypeFieldNames": return ((y: ErrorCore.DuplicateUnionTypeFieldNamesError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTypeError",
    field: ({
    name: "duplicateUnionTypeFieldNames",
    term: duplicateUnionTypeFieldNamesError(y)
  })
  }) }))((_m as any).value);
    case "emptyRecordType": return ((y: ErrorCore.EmptyRecordTypeError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTypeError",
    field: ({
    name: "emptyRecordType",
    term: emptyRecordTypeError(y)
  })
  }) }))((_m as any).value);
    case "emptyTypeAnnotation": return ((y: ErrorCore.EmptyTypeAnnotationError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTypeError",
    field: ({
    name: "emptyTypeAnnotation",
    term: emptyTypeAnnotationError(y)
  })
  }) }))((_m as any).value);
    case "emptyUnionType": return ((y: ErrorCore.EmptyUnionTypeError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTypeError",
    field: ({
    name: "emptyUnionType",
    term: emptyUnionTypeError(y)
  })
  }) }))((_m as any).value);
    case "invalidForallParameterName": return ((y: ErrorCore.InvalidForallParameterNameError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTypeError",
    field: ({
    name: "invalidForallParameterName",
    term: invalidForallParameterNameError(y)
  })
  }) }))((_m as any).value);
    case "invalidTypeSchemeVariableName": return ((y: ErrorCore.InvalidTypeSchemeVariableNameError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTypeError",
    field: ({
    name: "invalidTypeSchemeVariableName",
    term: invalidTypeSchemeVariableNameError(y)
  })
  }) }))((_m as any).value);
    case "nestedTypeAnnotation": return ((y: ErrorCore.NestedTypeAnnotationError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTypeError",
    field: ({
    name: "nestedTypeAnnotation",
    term: nestedTypeAnnotationError(y)
  })
  }) }))((_m as any).value);
    case "nonComparableMapKeyType": return ((y: ErrorCore.NonComparableMapKeyTypeError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTypeError",
    field: ({
    name: "nonComparableMapKeyType",
    term: nonComparableMapKeyTypeError(y)
  })
  }) }))((_m as any).value);
    case "nonComparableSetElementType": return ((y: ErrorCore.NonComparableSetElementTypeError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTypeError",
    field: ({
    name: "nonComparableSetElementType",
    term: nonComparableSetElementTypeError(y)
  })
  }) }))((_m as any).value);
    case "singleVariantUnion": return ((y: ErrorCore.SingleVariantUnionError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTypeError",
    field: ({
    name: "singleVariantUnion",
    term: singleVariantUnionError(y)
  })
  }) }))((_m as any).value);
    case "typeVariableShadowingInForall": return ((y: ErrorCore.TypeVariableShadowingInForallError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTypeError",
    field: ({
    name: "typeVariableShadowingInForall",
    term: typeVariableShadowingInForallError(y)
  })
  }) }))((_m as any).value);
    case "undefinedTypeVariable": return ((y: ErrorCore.UndefinedTypeVariableError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTypeError",
    field: ({
    name: "undefinedTypeVariable",
    term: undefinedTypeVariableError(y)
  })
  }) }))((_m as any).value);
    case "voidInNonBottomPosition": return ((y: ErrorCore.VoidInNonBottomPositionError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.core.InvalidTypeError",
    field: ({
    name: "voidInNonBottomPosition",
    term: voidInNonBottomPositionError(y)
  })
  }) }))((_m as any).value);
  }
})();
}

export function invalidTypeLambdaParameterNameError(x: ErrorCore.InvalidTypeLambdaParameterNameError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.InvalidTypeLambdaParameterNameError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function invalidTypeSchemeVariableNameError(x: ErrorCore.InvalidTypeSchemeVariableNameError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.InvalidTypeSchemeVariableNameError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function nestedTermAnnotationError(x: ErrorCore.NestedTermAnnotationError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.NestedTermAnnotationError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  })]
  }) });
}

export function nestedTypeAnnotationError(x: ErrorCore.NestedTypeAnnotationError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.NestedTypeAnnotationError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  })]
  }) });
}

export function nonComparableMapKeyTypeError(x: ErrorCore.NonComparableMapKeyTypeError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.NonComparableMapKeyTypeError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "keyType",
    term: EncodeCore.type(((_x) => _x.keyType)(x))
  })]
  }) });
}

export function nonComparableSetElementTypeError(x: ErrorCore.NonComparableSetElementTypeError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.NonComparableSetElementTypeError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "elementType",
    term: EncodeCore.type(((_x) => _x.elementType)(x))
  })]
  }) });
}

export function redundantWrapUnwrapError(x: ErrorCore.RedundantWrapUnwrapError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.RedundantWrapUnwrapError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "typeName",
    term: EncodeCore.name(((_x) => _x.typeName)(x))
  })]
  }) });
}

export function selfApplicationError(x: ErrorCore.SelfApplicationError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.SelfApplicationError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function singleVariantUnionError(x: ErrorCore.SingleVariantUnionError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.SingleVariantUnionError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "fieldName",
    term: EncodeCore.name(((_x) => _x.fieldName)(x))
  })]
  }) });
}

export function termVariableShadowingError(x: ErrorCore.TermVariableShadowingError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.TermVariableShadowingError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function typeVariableShadowingInForallError(x: ErrorCore.TypeVariableShadowingInForallError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.TypeVariableShadowingInForallError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function typeVariableShadowingInTypeLambdaError(x: ErrorCore.TypeVariableShadowingInTypeLambdaError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.TypeVariableShadowingInTypeLambdaError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function undefinedFieldError(x: ErrorCore.UndefinedFieldError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.UndefinedFieldError",
    fields: [({
    name: "fieldName",
    term: EncodeCore.name(((_x) => _x.fieldName)(x))
  }), ({
    name: "typeName",
    term: EncodeCore.name(((_x) => _x.typeName)(x))
  })]
  }) });
}

export function undefinedTermVariableError(x: ErrorCore.UndefinedTermVariableError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.UndefinedTermVariableError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function undefinedTypeVariableError(x: ErrorCore.UndefinedTypeVariableError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.UndefinedTypeVariableError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function undefinedTypeVariableInBindingTypeError(x: ErrorCore.UndefinedTypeVariableInBindingTypeError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.UndefinedTypeVariableInBindingTypeError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function undefinedTypeVariableInLambdaDomainError(x: ErrorCore.UndefinedTypeVariableInLambdaDomainError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.UndefinedTypeVariableInLambdaDomainError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function undefinedTypeVariableInTypeApplicationError(x: ErrorCore.UndefinedTypeVariableInTypeApplicationError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.UndefinedTypeVariableInTypeApplicationError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function unexpectedTermVariantError(x: ErrorCore.UnexpectedTermVariantError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.UnexpectedTermVariantError",
    fields: [({
    name: "expectedVariant",
    term: EncodeVariants.termVariant(((_x) => _x.expectedVariant)(x))
  }), ({
    name: "actualTerm",
    term: EncodeCore.term(((_x) => _x.actualTerm)(x))
  })]
  }) });
}

export function unexpectedTypeVariantError(x: ErrorCore.UnexpectedTypeVariantError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.UnexpectedTypeVariantError",
    fields: [({
    name: "expectedVariant",
    term: EncodeVariants.typeVariant(((_x) => _x.expectedVariant)(x))
  }), ({
    name: "actualType",
    term: EncodeCore.type(((_x) => _x.actualType)(x))
  })]
  }) });
}

export function unknownPrimitiveNameError(x: ErrorCore.UnknownPrimitiveNameError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.UnknownPrimitiveNameError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function unnecessaryIdentityApplicationError(x: ErrorCore.UnnecessaryIdentityApplicationError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.UnnecessaryIdentityApplicationError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  })]
  }) });
}

export function untypedTermVariableError(x: ErrorCore.UntypedTermVariableError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.UntypedTermVariableError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  }), ({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function voidInNonBottomPositionError(x: ErrorCore.VoidInNonBottomPositionError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.core.VoidInNonBottomPositionError",
    fields: [({
    name: "location",
    term: EncodePaths.subtermPath(((_x) => _x.location)(x))
  })]
  }) });
}
