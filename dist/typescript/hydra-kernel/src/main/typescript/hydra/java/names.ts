// Note: this is an automatically generated file. Do not edit.

/**
 * Java naming constants and package name utilities
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
import * as Graph from "../graph.js";
import * as JavaSyntax from "./syntax.js";
import * as JsonModel from "../json/model.js";
import * as LibLists from "../lib/lists.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export const acceptMethodName: string = "accept";

export const applyMethodName: string = "apply";

export const compareToMethodName: string = "compareTo";

export const equalsMethodName: string = "equals";

export const getMethodName: string = "get";

export const hashCodeMethodName: string = "hashCode";

export const hydraCorePackageName: JavaSyntax.PackageName | null = javaPackageName(["hydra", "core"]);

export const hydraUtilPackageName: JavaSyntax.PackageName | null = javaPackageName(["hydra", "util"]);

export const instanceName: string = "instance";

export const javaLangPackageName: JavaSyntax.PackageName | null = javaPackageName(["java", "lang"]);

export function javaPackageName(parts: ReadonlyArray<string>): JavaSyntax.PackageName {
  return LibLists.map(((p: string) => p))(parts);
}

export const javaUtilFunctionPackageName: JavaSyntax.PackageName | null = javaPackageName(["java", "util", "function"]);

export const javaUtilPackageName: JavaSyntax.PackageName | null = javaPackageName(["java", "util"]);

export const otherInstanceName: string = "other";

export const otherwiseMethodName: string = "otherwise";

export const partialVisitorName: string = "PartialVisitor";

export const setMethodName: string = "set";

export const valueFieldName: string = "value";

export const visitMethodName: string = "visit";

export const visitorName: string = "Visitor";

export const visitorReturnParameter: string = "R";
