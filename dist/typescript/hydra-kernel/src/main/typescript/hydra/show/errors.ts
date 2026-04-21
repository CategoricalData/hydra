// Note: this is an automatically generated file. Do not edit.

/**
 * String representations of hydra.error types
 */



import * as Ast from "../ast.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as Formatting from "../formatting.js";
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as LibLists from "../lib/lists.js";
import * as LibLiterals from "../lib/literals.js";
import * as LibSets from "../lib/sets.js";
import * as LibStrings from "../lib/strings.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as ShowCore from "./core.js";
import * as ShowErrorCore from "./error/core.js";
import * as ShowTyping from "./typing.js";
import * as ShowVariants from "./variants.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function checkingError(ce: ErrorChecking.CheckingError): string {
  return (() => {
  const _m = ce;
  switch (_m.tag) {
    case "incorrectUnification": return ((v1: ErrorChecking.IncorrectUnificationError) => incorrectUnificationError(v1))((_m as any).value);
    case "notAForallType": return ((v1: ErrorChecking.NotAForallTypeError) => notAForallTypeError(v1))((_m as any).value);
    case "notAFunctionType": return ((v1: ErrorChecking.NotAFunctionTypeError) => notAFunctionTypeError(v1))((_m as any).value);
    case "typeArityMismatch": return ((v1: ErrorChecking.TypeArityMismatchError) => typeArityMismatchError(v1))((_m as any).value);
    case "typeMismatch": return ((v1: ErrorChecking.TypeMismatchError) => typeMismatchError(v1))((_m as any).value);
    case "unboundTypeVariables": return ((v1: ErrorChecking.UnboundTypeVariablesError) => unboundTypeVariablesError(v1))((_m as any).value);
    case "unequalTypes": return ((v1: ErrorChecking.UnequalTypesError) => unequalTypesError(v1))((_m as any).value);
    case "unsupportedTermVariant": return ((v1: ErrorChecking.UnsupportedTermVariantError) => unsupportedTermVariantError(v1))((_m as any).value);
    case "untypedLambda": return ((v1: ErrorChecking.UntypedLambdaError) => untypedLambdaError(v1))((_m as any).value);
    case "untypedLetBinding": return ((v1: ErrorChecking.UntypedLetBindingError) => untypedLetBindingError(v1))((_m as any).value);
  }
})();
}

export function decodingError(de: Errors.DecodingError): string {
  return LibStrings.cat2("decoding error: ")(((_x) => _x)(de));
}

export function error(e: Errors.Error): string {
  return (() => {
  const _m = e;
  switch (_m.tag) {
    case "checking": return ((v1: ErrorChecking.CheckingError) => checkingError(v1))((_m as any).value);
    case "decoding": return ((v1: Errors.DecodingError) => decodingError(v1))((_m as any).value);
    case "duplicateBinding": return ((v1: ErrorCore.DuplicateBindingError) => ShowErrorCore.duplicateBindingError(v1))((_m as any).value);
    case "duplicateField": return ((v1: ErrorCore.DuplicateFieldError) => ShowErrorCore.duplicateFieldError(v1))((_m as any).value);
    case "extraction": return ((_: Errors.ExtractionError) => "extraction error")((_m as any).value);
    case "inference": return ((_: Errors.InferenceError) => "inference error")((_m as any).value);
    case "other": return ((v1: Errors.OtherError) => otherError(v1))((_m as any).value);
    case "resolution": return ((_: Errors.ResolutionError) => "resolution error")((_m as any).value);
    case "undefinedField": return ((v1: ErrorCore.UndefinedFieldError) => ShowErrorCore.undefinedFieldError(v1))((_m as any).value);
    case "undefinedTermVariable": return ((v1: ErrorCore.UndefinedTermVariableError) => ShowErrorCore.undefinedTermVariableError(v1))((_m as any).value);
    case "untypedTermVariable": return ((v1: ErrorCore.UntypedTermVariableError) => ShowErrorCore.untypedTermVariableError(v1))((_m as any).value);
    case "unexpectedTermVariant": return ((v1: ErrorCore.UnexpectedTermVariantError) => ShowErrorCore.unexpectedTermVariantError(v1))((_m as any).value);
    case "unexpectedTypeVariant": return ((v1: ErrorCore.UnexpectedTypeVariantError) => ShowErrorCore.unexpectedTypeVariantError(v1))((_m as any).value);
    case "unification": return ((v1: Errors.UnificationError) => unificationError(v1))((_m as any).value);
  }
})();
}

export function incorrectUnificationError(e: ErrorChecking.IncorrectUnificationError): string {
  return (() => {
  const subst = ((_x) => _x.substitution)(e);
  return LibStrings.cat2("incorrect unification: ")(ShowTyping.typeSubst(subst));
})();
}

export function notAForallTypeError(e: ErrorChecking.NotAForallTypeError): string {
  return (() => {
  const typ = ((_x) => _x.type)(e);
  return (() => {
  const args = ((_x) => _x.typeArguments)(e);
  return LibStrings.cat(["not a forall type: ", ShowCore.type(typ), ". Trying to apply ", LibLiterals.showInt32(LibLists.length(args)), " type argument(s): ", Formatting.showList(ShowCore.type)(args)]);
})();
})();
}

export function notAFunctionTypeError(e: ErrorChecking.NotAFunctionTypeError): string {
  return (() => {
  const typ = ((_x) => _x.type)(e);
  return LibStrings.cat2("not a function type: ")(ShowCore.type(typ));
})();
}

export function otherError(oe: Errors.OtherError): string {
  return ((_x) => _x)(oe);
}

export function typeArityMismatchError(e: ErrorChecking.TypeArityMismatchError): string {
  return (() => {
  const typ = ((_x) => _x.type)(e);
  return (() => {
  const expected = ((_x) => _x.expectedArity)(e);
  return (() => {
  const actual = ((_x) => _x.actualArity)(e);
  return (() => {
  const args = ((_x) => _x.typeArguments)(e);
  return LibStrings.cat(["type ", ShowCore.type(typ), " applied to the wrong number of type arguments (expected ", LibLiterals.showInt32(expected), ", got ", LibLiterals.showInt32(actual), "): ", Formatting.showList(ShowCore.type)(args)]);
})();
})();
})();
})();
}

export function typeMismatchError(e: ErrorChecking.TypeMismatchError): string {
  return (() => {
  const expected = ((_x) => _x.expectedType)(e);
  return (() => {
  const actual = ((_x) => _x.actualType)(e);
  return LibStrings.cat(["type mismatch: expected ", ShowCore.type(expected), " but found ", ShowCore.type(actual)]);
})();
})();
}

export function unboundTypeVariablesError(e: ErrorChecking.UnboundTypeVariablesError): string {
  return (() => {
  const vars = ((_x) => _x.variables)(e);
  return (() => {
  const typ = ((_x) => _x.type)(e);
  return LibStrings.cat(["unbound type variables: {", LibStrings.intercalate(", ")(LibLists.map(((_x) => _x))(LibSets.toList(vars))), "} in type ", ShowCore.type(typ)]);
})();
})();
}

export function unequalTypesError(e: ErrorChecking.UnequalTypesError): string {
  return (() => {
  const types = ((_x) => _x.types)(e);
  return (() => {
  const desc = ((_x) => _x.description)(e);
  return LibStrings.cat(["unequal types ", Formatting.showList(ShowCore.type)(types), " in ", desc]);
})();
})();
}

export function unificationError(e: Errors.UnificationError): string {
  return (() => {
  const lt = ((_x) => _x.leftType)(e);
  return (() => {
  const rt = ((_x) => _x.rightType)(e);
  return (() => {
  const msg = ((_x) => _x.message)(e);
  return LibStrings.cat(["unification error: cannot unify ", ShowCore.type(lt), " with ", ShowCore.type(rt), ": ", msg]);
})();
})();
})();
}

export function unsupportedTermVariantError(e: ErrorChecking.UnsupportedTermVariantError): string {
  return LibStrings.cat2("unsupported term variant: ")(ShowVariants.termVariant(((_x) => _x.termVariant)(e)));
}

export function untypedLambdaError<t0>(_: t0): string {
  return "untyped lambda";
}

export function untypedLetBindingError(e: ErrorChecking.UntypedLetBindingError): string {
  return (() => {
  const b = ((_x) => _x.binding)(e);
  return LibStrings.cat2("untyped let binding: ")(ShowCore.binding(b));
})();
}
