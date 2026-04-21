// Note: this is an automatically generated file. Do not edit.

/**
 * Term encoders for hydra.errors
 */



import * as Core from "../core.js";
import * as EncodeContext from "./context.js";
import * as EncodeCore from "./core.js";
import * as EncodeErrorChecking from "./error/checking.js";
import * as EncodeErrorCore from "./error/core.js";
import * as EncodePaths from "./paths.js";
import * as EncodeTyping from "./typing.js";
import * as EncodeVariants from "./variants.js";
import * as Errors from "../errors.js";

export function decodingError(x: Errors.DecodingError): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.errors.DecodingError",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(x) }) })
  }) });
}

export function emptyListError<t0>(_: t0): Core.Term {
  return ({ tag: "unit" });
}

export function error(v1: Errors.Error): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "checking": return ((y: hydra.error.checking.CheckingError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.Error",
    field: ({
    name: "checking",
    term: EncodeErrorChecking.checkingError(y)
  })
  }) }))((_m as any).value);
    case "decoding": return ((y: Errors.DecodingError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.Error",
    field: ({
    name: "decoding",
    term: decodingError(y)
  })
  }) }))((_m as any).value);
    case "duplicateBinding": return ((y: hydra.error.core.DuplicateBindingError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.Error",
    field: ({
    name: "duplicateBinding",
    term: EncodeErrorCore.duplicateBindingError(y)
  })
  }) }))((_m as any).value);
    case "duplicateField": return ((y: hydra.error.core.DuplicateFieldError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.Error",
    field: ({
    name: "duplicateField",
    term: EncodeErrorCore.duplicateFieldError(y)
  })
  }) }))((_m as any).value);
    case "extraction": return ((y: Errors.ExtractionError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.Error",
    field: ({
    name: "extraction",
    term: extractionError(y)
  })
  }) }))((_m as any).value);
    case "inference": return ((y: Errors.InferenceError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.Error",
    field: ({
    name: "inference",
    term: inferenceError(y)
  })
  }) }))((_m as any).value);
    case "other": return ((y: Errors.OtherError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.Error",
    field: ({
    name: "other",
    term: otherError(y)
  })
  }) }))((_m as any).value);
    case "resolution": return ((y: Errors.ResolutionError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.Error",
    field: ({
    name: "resolution",
    term: resolutionError(y)
  })
  }) }))((_m as any).value);
    case "undefinedField": return ((y: hydra.error.core.UndefinedFieldError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.Error",
    field: ({
    name: "undefinedField",
    term: EncodeErrorCore.undefinedFieldError(y)
  })
  }) }))((_m as any).value);
    case "undefinedTermVariable": return ((y: hydra.error.core.UndefinedTermVariableError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.Error",
    field: ({
    name: "undefinedTermVariable",
    term: EncodeErrorCore.undefinedTermVariableError(y)
  })
  }) }))((_m as any).value);
    case "untypedTermVariable": return ((y: hydra.error.core.UntypedTermVariableError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.Error",
    field: ({
    name: "untypedTermVariable",
    term: EncodeErrorCore.untypedTermVariableError(y)
  })
  }) }))((_m as any).value);
    case "unexpectedTermVariant": return ((y: hydra.error.core.UnexpectedTermVariantError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.Error",
    field: ({
    name: "unexpectedTermVariant",
    term: EncodeErrorCore.unexpectedTermVariantError(y)
  })
  }) }))((_m as any).value);
    case "unexpectedTypeVariant": return ((y: hydra.error.core.UnexpectedTypeVariantError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.Error",
    field: ({
    name: "unexpectedTypeVariant",
    term: EncodeErrorCore.unexpectedTypeVariantError(y)
  })
  }) }))((_m as any).value);
    case "unification": return ((y: Errors.UnificationError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.Error",
    field: ({
    name: "unification",
    term: unificationError(y)
  })
  }) }))((_m as any).value);
  }
})();
}

export function extractionError(v1: Errors.ExtractionError): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "emptyList": return ((y: Errors.EmptyListError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.ExtractionError",
    field: ({
    name: "emptyList",
    term: emptyListError(y)
  })
  }) }))((_m as any).value);
    case "multipleBindings": return ((y: Errors.MultipleBindingsError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.ExtractionError",
    field: ({
    name: "multipleBindings",
    term: multipleBindingsError(y)
  })
  }) }))((_m as any).value);
    case "multipleFields": return ((y: Errors.MultipleFieldsError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.ExtractionError",
    field: ({
    name: "multipleFields",
    term: multipleFieldsError(y)
  })
  }) }))((_m as any).value);
    case "noMatchingField": return ((y: Errors.NoMatchingFieldError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.ExtractionError",
    field: ({
    name: "noMatchingField",
    term: noMatchingFieldError(y)
  })
  }) }))((_m as any).value);
    case "noSuchBinding": return ((y: Errors.NoSuchBindingError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.ExtractionError",
    field: ({
    name: "noSuchBinding",
    term: noSuchBindingError(y)
  })
  }) }))((_m as any).value);
    case "notEnoughCases": return ((y: Errors.NotEnoughCasesError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.ExtractionError",
    field: ({
    name: "notEnoughCases",
    term: notEnoughCasesError(y)
  })
  }) }))((_m as any).value);
    case "unexpectedShape": return ((y: Errors.UnexpectedShapeError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.ExtractionError",
    field: ({
    name: "unexpectedShape",
    term: unexpectedShapeError(y)
  })
  }) }))((_m as any).value);
  }
})();
}

export function inferenceError(v1: Errors.InferenceError): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "checking": return ((y: hydra.error.checking.CheckingError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.InferenceError",
    field: ({
    name: "checking",
    term: EncodeErrorChecking.checkingError(y)
  })
  }) }))((_m as any).value);
    case "other": return ((y: Errors.OtherInferenceError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.InferenceError",
    field: ({
    name: "other",
    term: otherInferenceError(y)
  })
  }) }))((_m as any).value);
    case "unification": return ((y: Errors.UnificationInferenceError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.InferenceError",
    field: ({
    name: "unification",
    term: unificationInferenceError(y)
  })
  }) }))((_m as any).value);
  }
})();
}

export function multipleBindingsError(x: Errors.MultipleBindingsError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.errors.MultipleBindingsError",
    fields: [({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function multipleFieldsError(x: Errors.MultipleFieldsError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.errors.MultipleFieldsError",
    fields: [({
    name: "fieldName",
    term: EncodeCore.name(((_x) => _x.fieldName)(x))
  })]
  }) });
}

export function noMatchingFieldError(x: Errors.NoMatchingFieldError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.errors.NoMatchingFieldError",
    fields: [({
    name: "fieldName",
    term: EncodeCore.name(((_x) => _x.fieldName)(x))
  })]
  }) });
}

export function noSuchBindingError(x: Errors.NoSuchBindingError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.errors.NoSuchBindingError",
    fields: [({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function noSuchPrimitiveError(x: Errors.NoSuchPrimitiveError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.errors.NoSuchPrimitiveError",
    fields: [({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function notEnoughCasesError<t0>(_: t0): Core.Term {
  return ({ tag: "unit" });
}

export function otherError(x: Errors.OtherError): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.errors.OtherError",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(x) }) })
  }) });
}

export function otherInferenceError(x: Errors.OtherInferenceError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.errors.OtherInferenceError",
    fields: [({
    name: "path",
    term: EncodePaths.subtermPath(((_x) => _x.path)(x))
  }), ({
    name: "message",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.message)(x) }) })
  })]
  }) });
}

export function otherResolutionError(x: Errors.OtherResolutionError): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.errors.OtherResolutionError",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(x) }) })
  }) });
}

export function resolutionError(v1: Errors.ResolutionError): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "noSuchBinding": return ((y: Errors.NoSuchBindingError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.ResolutionError",
    field: ({
    name: "noSuchBinding",
    term: noSuchBindingError(y)
  })
  }) }))((_m as any).value);
    case "noSuchPrimitive": return ((y: Errors.NoSuchPrimitiveError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.ResolutionError",
    field: ({
    name: "noSuchPrimitive",
    term: noSuchPrimitiveError(y)
  })
  }) }))((_m as any).value);
    case "noMatchingField": return ((y: Errors.NoMatchingFieldError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.ResolutionError",
    field: ({
    name: "noMatchingField",
    term: noMatchingFieldError(y)
  })
  }) }))((_m as any).value);
    case "other": return ((y: Errors.OtherResolutionError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.ResolutionError",
    field: ({
    name: "other",
    term: otherResolutionError(y)
  })
  }) }))((_m as any).value);
    case "unexpectedShape": return ((y: Errors.UnexpectedShapeError) => ({ tag: "inject", value: ({
    typeName: "hydra.errors.ResolutionError",
    field: ({
    name: "unexpectedShape",
    term: unexpectedShapeError(y)
  })
  }) }))((_m as any).value);
  }
})();
}

export function unexpectedShapeError(x: Errors.UnexpectedShapeError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.errors.UnexpectedShapeError",
    fields: [({
    name: "expected",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.expected)(x) }) })
  }), ({
    name: "actual",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.actual)(x) }) })
  })]
  }) });
}

export function unificationError(x: Errors.UnificationError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.errors.UnificationError",
    fields: [({
    name: "leftType",
    term: EncodeCore.type(((_x) => _x.leftType)(x))
  }), ({
    name: "rightType",
    term: EncodeCore.type(((_x) => _x.rightType)(x))
  }), ({
    name: "message",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.message)(x) }) })
  })]
  }) });
}

export function unificationInferenceError(x: Errors.UnificationInferenceError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.errors.UnificationInferenceError",
    fields: [({
    name: "path",
    term: EncodePaths.subtermPath(((_x) => _x.path)(x))
  }), ({
    name: "cause",
    term: unificationError(((_x) => _x.cause)(x))
  })]
  }) });
}
