// Note: this is an automatically generated file. Do not edit.

/**
 * Term encoders for hydra.error.checking
 */



import * as Core from "../../core.js";
import * as EncodeCore from "../core.js";
import * as EncodePaths from "../paths.js";
import * as EncodeTyping from "../typing.js";
import * as EncodeVariants from "../variants.js";
import * as ErrorChecking from "../../error/checking.js";
import * as LibLists from "../../lib/lists.js";
import * as LibSets from "../../lib/sets.js";

export function checkingError(v1: ErrorChecking.CheckingError): Core.Term {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "incorrectUnification": return ((y: ErrorChecking.IncorrectUnificationError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.checking.CheckingError",
    field: ({
    name: "incorrectUnification",
    term: incorrectUnificationError(y)
  })
  }) }))((_m as any).value);
    case "notAForallType": return ((y: ErrorChecking.NotAForallTypeError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.checking.CheckingError",
    field: ({
    name: "notAForallType",
    term: notAForallTypeError(y)
  })
  }) }))((_m as any).value);
    case "notAFunctionType": return ((y: ErrorChecking.NotAFunctionTypeError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.checking.CheckingError",
    field: ({
    name: "notAFunctionType",
    term: notAFunctionTypeError(y)
  })
  }) }))((_m as any).value);
    case "other": return ((y: ErrorChecking.OtherCheckingError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.checking.CheckingError",
    field: ({
    name: "other",
    term: otherCheckingError(y)
  })
  }) }))((_m as any).value);
    case "typeArityMismatch": return ((y: ErrorChecking.TypeArityMismatchError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.checking.CheckingError",
    field: ({
    name: "typeArityMismatch",
    term: typeArityMismatchError(y)
  })
  }) }))((_m as any).value);
    case "typeMismatch": return ((y: ErrorChecking.TypeMismatchError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.checking.CheckingError",
    field: ({
    name: "typeMismatch",
    term: typeMismatchError(y)
  })
  }) }))((_m as any).value);
    case "unboundTypeVariables": return ((y: ErrorChecking.UnboundTypeVariablesError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.checking.CheckingError",
    field: ({
    name: "unboundTypeVariables",
    term: unboundTypeVariablesError(y)
  })
  }) }))((_m as any).value);
    case "undefinedTermVariable": return ((y: ErrorChecking.UndefinedTermVariableCheckingError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.checking.CheckingError",
    field: ({
    name: "undefinedTermVariable",
    term: undefinedTermVariableCheckingError(y)
  })
  }) }))((_m as any).value);
    case "unequalTypes": return ((y: ErrorChecking.UnequalTypesError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.checking.CheckingError",
    field: ({
    name: "unequalTypes",
    term: unequalTypesError(y)
  })
  }) }))((_m as any).value);
    case "unsupportedTermVariant": return ((y: ErrorChecking.UnsupportedTermVariantError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.checking.CheckingError",
    field: ({
    name: "unsupportedTermVariant",
    term: unsupportedTermVariantError(y)
  })
  }) }))((_m as any).value);
    case "untypedLambda": return ((y: ErrorChecking.UntypedLambdaError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.checking.CheckingError",
    field: ({
    name: "untypedLambda",
    term: untypedLambdaError(y)
  })
  }) }))((_m as any).value);
    case "untypedLetBinding": return ((y: ErrorChecking.UntypedLetBindingError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.checking.CheckingError",
    field: ({
    name: "untypedLetBinding",
    term: untypedLetBindingError(y)
  })
  }) }))((_m as any).value);
    case "untypedTermVariable": return ((y: ErrorChecking.UntypedTermVariableCheckingError) => ({ tag: "inject", value: ({
    typeName: "hydra.error.checking.CheckingError",
    field: ({
    name: "untypedTermVariable",
    term: untypedTermVariableCheckingError(y)
  })
  }) }))((_m as any).value);
  }
})();
}

export function incorrectUnificationError(x: ErrorChecking.IncorrectUnificationError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.checking.IncorrectUnificationError",
    fields: [({
    name: "substitution",
    term: EncodeTyping.typeSubst(((_x) => _x.substitution)(x))
  })]
  }) });
}

export function notAForallTypeError(x: ErrorChecking.NotAForallTypeError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.checking.NotAForallTypeError",
    fields: [({
    name: "type",
    term: EncodeCore.type(((_x) => _x.type)(x))
  }), ({
    name: "typeArguments",
    term: ({ tag: "list", value: LibLists.map(EncodeCore.type)(((_x) => _x.typeArguments)(x)) })
  })]
  }) });
}

export function notAFunctionTypeError(x: ErrorChecking.NotAFunctionTypeError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.checking.NotAFunctionTypeError",
    fields: [({
    name: "type",
    term: EncodeCore.type(((_x) => _x.type)(x))
  })]
  }) });
}

export function otherCheckingError(x: ErrorChecking.OtherCheckingError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.checking.OtherCheckingError",
    fields: [({
    name: "path",
    term: EncodePaths.subtermPath(((_x) => _x.path)(x))
  }), ({
    name: "message",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.message)(x) }) })
  })]
  }) });
}

export function typeArityMismatchError(x: ErrorChecking.TypeArityMismatchError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.checking.TypeArityMismatchError",
    fields: [({
    name: "type",
    term: EncodeCore.type(((_x) => _x.type)(x))
  }), ({
    name: "expectedArity",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: ((_x) => _x.expectedArity)(x) }) }) })
  }), ({
    name: "actualArity",
    term: ({ tag: "literal", value: ({ tag: "integer", value: ({ tag: "int32", value: ((_x) => _x.actualArity)(x) }) }) })
  }), ({
    name: "typeArguments",
    term: ({ tag: "list", value: LibLists.map(EncodeCore.type)(((_x) => _x.typeArguments)(x)) })
  })]
  }) });
}

export function typeMismatchError(x: ErrorChecking.TypeMismatchError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.checking.TypeMismatchError",
    fields: [({
    name: "expectedType",
    term: EncodeCore.type(((_x) => _x.expectedType)(x))
  }), ({
    name: "actualType",
    term: EncodeCore.type(((_x) => _x.actualType)(x))
  })]
  }) });
}

export function unboundTypeVariablesError(x: ErrorChecking.UnboundTypeVariablesError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.checking.UnboundTypeVariablesError",
    fields: [({
    name: "variables",
    term: ({ tag: "set", value: LibSets.map(EncodeCore.name)(((_x) => _x.variables)(x)) })
  }), ({
    name: "type",
    term: EncodeCore.type(((_x) => _x.type)(x))
  })]
  }) });
}

export function undefinedTermVariableCheckingError(x: ErrorChecking.UndefinedTermVariableCheckingError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.checking.UndefinedTermVariableCheckingError",
    fields: [({
    name: "path",
    term: EncodePaths.subtermPath(((_x) => _x.path)(x))
  }), ({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}

export function unequalTypesError(x: ErrorChecking.UnequalTypesError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.checking.UnequalTypesError",
    fields: [({
    name: "types",
    term: ({ tag: "list", value: LibLists.map(EncodeCore.type)(((_x) => _x.types)(x)) })
  }), ({
    name: "description",
    term: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x.description)(x) }) })
  })]
  }) });
}

export function unsupportedTermVariantError(x: ErrorChecking.UnsupportedTermVariantError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.checking.UnsupportedTermVariantError",
    fields: [({
    name: "termVariant",
    term: EncodeVariants.termVariant(((_x) => _x.termVariant)(x))
  })]
  }) });
}

export function untypedLambdaError<t0>(x: t0): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.checking.UntypedLambdaError",
    fields: []
  }) });
}

export function untypedLetBindingError(x: ErrorChecking.UntypedLetBindingError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.checking.UntypedLetBindingError",
    fields: [({
    name: "binding",
    term: EncodeCore.binding(((_x) => _x.binding)(x))
  })]
  }) });
}

export function untypedTermVariableCheckingError(x: ErrorChecking.UntypedTermVariableCheckingError): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.error.checking.UntypedTermVariableCheckingError",
    fields: [({
    name: "path",
    term: EncodePaths.subtermPath(((_x) => _x.path)(x))
  }), ({
    name: "name",
    term: EncodeCore.name(((_x) => _x.name)(x))
  })]
  }) });
}
