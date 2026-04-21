// Note: this is an automatically generated file. Do not edit.

/**
 * A Scala syntax model based on Scalameta (https://scalameta.org)
 */



import * as Core from "../core.js";

export type PredefString = string & { readonly __brand: "PredefString" };

export interface ScalaSymbol {
  readonly name: string;
}

export type Tree =
  | { readonly tag: "ref"; readonly value: Ref }
  | { readonly tag: "stat"; readonly value: Stat }
  | { readonly tag: "type"; readonly value: Type }
  | { readonly tag: "bounds"; readonly value: TypeBounds }
  | { readonly tag: "pat"; readonly value: Pat }
  | { readonly tag: "member"; readonly value: Member }
  | { readonly tag: "ctor"; readonly value: Ctor }
  | { readonly tag: "template"; readonly value: Template }
  | { readonly tag: "mod"; readonly value: Mod }
  | { readonly tag: "enumerator"; readonly value: Enumerator }
  | { readonly tag: "importer"; readonly value: Importer }
  | { readonly tag: "importee"; readonly value: Importee }
  | { readonly tag: "caseTree"; readonly value: CaseTree }
  | { readonly tag: "source"; readonly value: Source }
  | { readonly tag: "quasi"; readonly value: Quasi };

export type Ref =
  | { readonly tag: "name"; readonly value: Name }
  | { readonly tag: "init"; readonly value: Init };

export type Stat =
  | { readonly tag: "term"; readonly value: Data }
  | { readonly tag: "decl"; readonly value: Decl }
  | { readonly tag: "defn"; readonly value: Defn }
  | { readonly tag: "importExport"; readonly value: ImportExportStat };

export type Name =
  | { readonly tag: "value"; readonly value: string }
  | { readonly tag: "anonymous" }
  | { readonly tag: "indeterminate"; readonly value: PredefString };

export type Lit =
  | { readonly tag: "null" }
  | { readonly tag: "int"; readonly value: number }
  | { readonly tag: "double"; readonly value: number }
  | { readonly tag: "float"; readonly value: number }
  | { readonly tag: "byte"; readonly value: number }
  | { readonly tag: "short"; readonly value: bigint }
  | { readonly tag: "char"; readonly value: number }
  | { readonly tag: "long"; readonly value: bigint }
  | { readonly tag: "boolean"; readonly value: boolean }
  | { readonly tag: "unit" }
  | { readonly tag: "string"; readonly value: string }
  | { readonly tag: "bytes"; readonly value: ReadonlyArray<number> }
  | { readonly tag: "symbol"; readonly value: ScalaSymbol };

export type Data =
  | { readonly tag: "lit"; readonly value: Lit }
  | { readonly tag: "ref"; readonly value: Data_Ref }
  | { readonly tag: "interpolate"; readonly value: Data_Interpolate }
  | { readonly tag: "xml"; readonly value: Data_Xml }
  | { readonly tag: "apply"; readonly value: Data_Apply }
  | { readonly tag: "applyUsing"; readonly value: Data_ApplyUsing }
  | { readonly tag: "applyType"; readonly value: Data_ApplyType }
  | { readonly tag: "assign"; readonly value: Data_Assign }
  | { readonly tag: "return"; readonly value: Data_Return }
  | { readonly tag: "throw"; readonly value: Data_Throw }
  | { readonly tag: "ascribe"; readonly value: Data_Ascribe }
  | { readonly tag: "annotate"; readonly value: Data_Annotate }
  | { readonly tag: "tuple"; readonly value: Data_Tuple }
  | { readonly tag: "block"; readonly value: Data_Block }
  | { readonly tag: "endMarker"; readonly value: Data_EndMarker }
  | { readonly tag: "if"; readonly value: Data_If }
  | { readonly tag: "quotedMacroExpr"; readonly value: Data_QuotedMacroExpr }
  | { readonly tag: "quotedMacroType"; readonly value: Data_QuotedMacroType }
  | { readonly tag: "splicedMacroExpr"; readonly value: Data_SplicedMacroExpr }
  | { readonly tag: "match"; readonly value: Data_Match }
  | { readonly tag: "try"; readonly value: Data_Try }
  | { readonly tag: "tryWithHandler"; readonly value: Data_TryWithHandler }
  | { readonly tag: "functionData"; readonly value: Data_FunctionData }
  | { readonly tag: "polyFunction"; readonly value: Data_PolyFunction }
  | { readonly tag: "partialFunction"; readonly value: Data_PartialFunction }
  | { readonly tag: "while"; readonly value: Data_While }
  | { readonly tag: "do"; readonly value: Data_Do }
  | { readonly tag: "for"; readonly value: Data_For }
  | { readonly tag: "forYield"; readonly value: Data_ForYield }
  | { readonly tag: "new"; readonly value: Data_New }
  | { readonly tag: "newAnonymous"; readonly value: Data_NewAnonymous }
  | { readonly tag: "placeholder" }
  | { readonly tag: "eta"; readonly value: Data_Eta }
  | { readonly tag: "repeated"; readonly value: Data_Repeated }
  | { readonly tag: "param"; readonly value: Data_Param };

export type Data_Ref =
  | { readonly tag: "this"; readonly value: Data_This }
  | { readonly tag: "super"; readonly value: Data_Super }
  | { readonly tag: "name"; readonly value: Data_Name }
  | { readonly tag: "anonymous"; readonly value: Data_Anonymous }
  | { readonly tag: "select"; readonly value: Data_Select }
  | { readonly tag: "applyUnary"; readonly value: Data_ApplyUnary };

export type Data_This = void & { readonly __brand: "Data_This" };

export interface Data_Super {
  readonly thisp: Name;
  readonly superp: Name;
}

export interface Data_Name {
  readonly value: PredefString;
}

export type Data_Anonymous = void & { readonly __brand: "Data_Anonymous" };

export interface Data_Select {
  readonly qual: Data;
  readonly name: Data_Name;
}

export interface Data_Interpolate {
  readonly prefix: Data_Name;
  readonly parts: ReadonlyArray<Lit>;
  readonly args: ReadonlyArray<Data>;
}

export interface Data_Xml {
  readonly parts: ReadonlyArray<Lit>;
  readonly args: ReadonlyArray<Data>;
}

export interface Data_Apply {
  readonly fun: Data;
  readonly args: ReadonlyArray<Data>;
}

export interface Data_ApplyUsing {
  readonly fun: Data;
  readonly targs: ReadonlyArray<Data>;
}

export interface Data_ApplyType {
  readonly lhs: Data;
  readonly op: Data_Name;
  readonly targs: ReadonlyArray<Type>;
  readonly args: ReadonlyArray<Data>;
}

export interface Data_ApplyInfix {
  readonly lhs: Data;
  readonly op: Data_Name;
  readonly targs: ReadonlyArray<Type>;
  readonly args: ReadonlyArray<Data>;
}

export interface Data_ApplyUnary {
  readonly op: Data_Name;
  readonly arg: Data;
}

export interface Data_Assign {
  readonly lhs: Data;
  readonly rhs: Data;
}

export interface Data_Return {
  readonly expr: Data;
}

export interface Data_Throw {
  readonly expr: Data;
}

export interface Data_Ascribe {
  readonly expr: Data;
  readonly tpe: Type;
}

export interface Data_Annotate {
  readonly expr: Data;
  readonly annots: ReadonlyArray<Mod_Annot>;
}

export interface Data_Tuple {
  readonly args: ReadonlyArray<Data>;
}

export interface Data_Block {
  readonly stats: ReadonlyArray<Stat>;
}

export interface Data_EndMarker {
  readonly name: Data_Name;
}

export interface Data_If {
  readonly cond: Data;
  readonly thenp: Data;
  readonly elsep: Data;
}

export interface Data_QuotedMacroExpr {
  readonly body: Data;
}

export interface Data_QuotedMacroType {
  readonly tpe: Type;
}

export interface Data_SplicedMacroExpr {
  readonly body: Data;
}

export interface Data_Match {
  readonly expr: Data;
  readonly cases: ReadonlyArray<Case>;
}

export interface Data_Try {
  readonly expr: Data;
  readonly catchp: ReadonlyArray<Case>;
  readonly finallyp: Data | null;
}

export interface Data_TryWithHandler {
  readonly expr: Data;
  readonly catchp: Data;
  readonly finallyp: Data | null;
}

export type Data_FunctionData =
  | { readonly tag: "contextFunction"; readonly value: Data_ContextFunction }
  | { readonly tag: "function"; readonly value: Data_Function };

export interface Data_ContextFunction {
  readonly params: ReadonlyArray<Data_Param>;
  readonly body: Data;
}

export interface Data_Function {
  readonly params: ReadonlyArray<Data_Param>;
  readonly body: Data;
}

export interface Data_PolyFunction {
  readonly tparams: ReadonlyArray<Type_Param>;
  readonly body: Data;
}

export interface Data_PartialFunction {
  readonly cases: ReadonlyArray<Case>;
}

export interface Data_While {
  readonly expr: Data;
  readonly body: Data;
}

export interface Data_Do {
  readonly body: Data;
  readonly expr: Data;
}

export interface Data_For {
  readonly enums: ReadonlyArray<Enumerator>;
}

export interface Data_ForYield {
  readonly enums: ReadonlyArray<Enumerator>;
}

export interface Data_New {
  readonly init: Init;
}

export interface Data_NewAnonymous {
  readonly templ: Template;
}

export interface Data_Eta {
  readonly expr: Data;
}

export interface Data_Repeated {
  readonly expr: Data;
}

export interface Data_Param {
  readonly mods: ReadonlyArray<Mod>;
  readonly name: Name;
  readonly decltpe: Type | null;
  readonly default: Data | null;
}

export type Type =
  | { readonly tag: "ref"; readonly value: Type_Ref }
  | { readonly tag: "anonymousName"; readonly value: Type_AnonymousName }
  | { readonly tag: "apply"; readonly value: Type_Apply }
  | { readonly tag: "applyInfix"; readonly value: Type_ApplyInfix }
  | { readonly tag: "functionType"; readonly value: Type_FunctionType }
  | { readonly tag: "polyFunction"; readonly value: Type_PolyFunction }
  | { readonly tag: "implicitFunction"; readonly value: Type_ImplicitFunction }
  | { readonly tag: "tuple"; readonly value: Type_Tuple }
  | { readonly tag: "with"; readonly value: Type_With }
  | { readonly tag: "and"; readonly value: Type_And }
  | { readonly tag: "or"; readonly value: Type_Or }
  | { readonly tag: "refine"; readonly value: Type_Refine }
  | { readonly tag: "existential"; readonly value: Type_Existential }
  | { readonly tag: "annotate"; readonly value: Type_Annotate }
  | { readonly tag: "lambda"; readonly value: Type_Lambda }
  | { readonly tag: "macro"; readonly value: Type_Macro }
  | { readonly tag: "method"; readonly value: Type_Method }
  | { readonly tag: "placeholder"; readonly value: Type_Placeholder }
  | { readonly tag: "byName"; readonly value: Type_ByName }
  | { readonly tag: "repeated"; readonly value: Type_Repeated }
  | { readonly tag: "var"; readonly value: Type_Var }
  | { readonly tag: "typedParam"; readonly value: Type_TypedParam }
  | { readonly tag: "match"; readonly value: Type_Match };

export type Type_Ref =
  | { readonly tag: "name"; readonly value: Type_Name }
  | { readonly tag: "select"; readonly value: Type_Select }
  | { readonly tag: "project"; readonly value: Type_Project }
  | { readonly tag: "singleton"; readonly value: Type_Singleton };

export interface Type_Name {
  readonly value: string;
}

export type Type_AnonymousName = void & { readonly __brand: "Type_AnonymousName" };

export interface Type_Select {
  readonly qual: Data_Ref;
  readonly name: Type_Name;
}

export interface Type_Project {
  readonly qual: Type;
  readonly name: Type_Name;
}

export interface Type_Singleton {
  readonly ref: Data_Ref;
}

export interface Type_Apply {
  readonly tpe: Type;
  readonly args: ReadonlyArray<Type>;
}

export interface Type_ApplyInfix {
  readonly lhs: Type;
  readonly op: Type_Name;
  readonly rhs: Type;
}

export type Type_FunctionType =
  | { readonly tag: "function"; readonly value: Type_Function }
  | { readonly tag: "contextFunction"; readonly value: Type_ContextFunction };

export interface Type_Function {
  readonly params: ReadonlyArray<Type>;
  readonly res: Type;
}

export interface Type_PolyFunction {
  readonly tparams: ReadonlyArray<Type_Param>;
  readonly tpe: Type;
}

export interface Type_ContextFunction {
  readonly params: ReadonlyArray<Type>;
  readonly res: Type;
}

export interface Type_ImplicitFunction {
  readonly params: ReadonlyArray<Type>;
  readonly res: Type;
}

export interface Type_Tuple {
  readonly args: ReadonlyArray<Type>;
}

export interface Type_With {
  readonly lhs: Type;
  readonly rhs: Type;
}

export interface Type_And {
  readonly lhs: Type;
  readonly rhs: Type;
}

export interface Type_Or {
  readonly lhs: Type;
  readonly rhs: Type;
}

export interface Type_Refine {
  readonly tpe: Type | null;
  readonly stats: ReadonlyArray<Stat>;
}

export interface Type_Existential {
  readonly tpe: Type;
  readonly stats: ReadonlyArray<Stat>;
}

export interface Type_Annotate {
  readonly tpe: Type;
  readonly annots: ReadonlyArray<Mod_Annot>;
}

export interface Type_Lambda {
  readonly tparams: ReadonlyArray<Type_Param>;
  readonly tpe: Type;
}

export interface Type_Macro {
  readonly body: Data;
}

export interface Type_Method {
  readonly paramss: ReadonlyArray<ReadonlyArray<Data_Param>>;
  readonly tpe: Type;
}

export interface Type_Placeholder {
  readonly bounds: TypeBounds;
}

export interface TypeBounds {
  readonly lo: Type | null;
  readonly hi: Type | null;
}

export interface Type_ByName {
  readonly tpe: Type;
}

export interface Type_Repeated {
  readonly tpe: Type;
}

export interface Type_Var {
  readonly name: Type_Name;
}

export interface Type_TypedParam {
  readonly name: Name;
  readonly typ: Type;
}

export interface Type_Param {
  readonly mods: ReadonlyArray<Mod>;
  readonly name: Name;
  readonly tparams: ReadonlyArray<Type_Param>;
  readonly tbounds: ReadonlyArray<TypeBounds>;
  readonly vbounds: ReadonlyArray<Type>;
  readonly cbounds: ReadonlyArray<Type>;
}

export interface Type_Match {
  readonly tpe: Type;
  readonly cases: ReadonlyArray<TypeCase>;
}

export type Pat =
  | { readonly tag: "var"; readonly value: Pat_Var }
  | { readonly tag: "wildcard" }
  | { readonly tag: "seqWildcard" }
  | { readonly tag: "bind"; readonly value: Pat_Bind }
  | { readonly tag: "alternative"; readonly value: Pat_Alternative }
  | { readonly tag: "tuple"; readonly value: Pat_Tuple }
  | { readonly tag: "repeated"; readonly value: Pat_Repeated }
  | { readonly tag: "extract"; readonly value: Pat_Extract }
  | { readonly tag: "extractInfix"; readonly value: Pat_ExtractInfix }
  | { readonly tag: "interpolate"; readonly value: Pat_Interpolate }
  | { readonly tag: "xml"; readonly value: Pat_Xml }
  | { readonly tag: "typed"; readonly value: Pat_Typed }
  | { readonly tag: "macro"; readonly value: Pat_Macro }
  | { readonly tag: "given"; readonly value: Pat_Given };

export interface Pat_Var {
  readonly name: Data_Name;
}

export interface Pat_Bind {
  readonly lhs: Pat;
  readonly rhs: Pat;
}

export interface Pat_Alternative {
  readonly lhs: Pat;
  readonly rhs: Pat;
}

export interface Pat_Tuple {
  readonly args: ReadonlyArray<Pat>;
}

export interface Pat_Repeated {
  readonly name: Data_Name;
}

export interface Pat_Extract {
  readonly fun: Data;
  readonly args: ReadonlyArray<Pat>;
}

export interface Pat_ExtractInfix {
  readonly lhs: Pat;
  readonly op: Data_Name;
  readonly rhs: ReadonlyArray<Pat>;
}

export interface Pat_Interpolate {
  readonly prefix: Data_Name;
  readonly parts: ReadonlyArray<Lit>;
}

export interface Pat_Xml {
  readonly parts: ReadonlyArray<Lit>;
  readonly args: ReadonlyArray<Pat>;
}

export interface Pat_Typed {
  readonly lhs: Pat;
  readonly rhs: Type;
}

export interface Pat_Macro {
  readonly body: Data;
}

export interface Pat_Given {
  readonly tpe: Type;
}

export type Member =
  | { readonly tag: "term"; readonly value: Member_Data }
  | { readonly tag: "type"; readonly value: Member_Type }
  | { readonly tag: "termParam"; readonly value: Data_Param }
  | { readonly tag: "typeParam"; readonly value: Type_Param }
  | { readonly tag: "self"; readonly value: Self };

export type Member_Data =
  | { readonly tag: "pkg"; readonly value: Pkg }
  | { readonly tag: "object"; readonly value: Pkg_Object };

export interface Member_Type {
  readonly name: Type_Name;
}

export type Decl =
  | { readonly tag: "val"; readonly value: Decl_Val }
  | { readonly tag: "var"; readonly value: Decl_Var }
  | { readonly tag: "def"; readonly value: Decl_Def }
  | { readonly tag: "type"; readonly value: Decl_Type }
  | { readonly tag: "given"; readonly value: Decl_Given };

export interface Decl_Val {
  readonly mods: ReadonlyArray<Mod>;
  readonly pats: ReadonlyArray<Pat>;
  readonly decltpe: Type;
}

export interface Decl_Var {
  readonly mods: ReadonlyArray<Mod>;
  readonly pats: ReadonlyArray<Pat>;
  readonly decltpe: Type;
}

export interface Decl_Def {
  readonly mods: ReadonlyArray<Mod>;
  readonly name: Data_Name;
  readonly tparams: ReadonlyArray<Type_Param>;
  readonly paramss: ReadonlyArray<ReadonlyArray<Data_Param>>;
  readonly decltpe: Type;
}

export interface Decl_Type {
  readonly mods: ReadonlyArray<Mod>;
  readonly name: Type_Name;
  readonly tparams: ReadonlyArray<Type_Param>;
  readonly bounds: TypeBounds;
}

export interface Decl_Given {
  readonly mods: ReadonlyArray<Mod>;
  readonly name: Data_Name;
  readonly tparams: ReadonlyArray<Type_Param>;
  readonly sparams: ReadonlyArray<ReadonlyArray<Data_Param>>;
  readonly decltpe: Type;
}

export type Defn =
  | { readonly tag: "val"; readonly value: Defn_Val }
  | { readonly tag: "var"; readonly value: Defn_Var }
  | { readonly tag: "given"; readonly value: Defn_Given }
  | { readonly tag: "enum"; readonly value: Defn_Enum }
  | { readonly tag: "enumCase"; readonly value: Defn_EnumCase }
  | { readonly tag: "repeatedEnumCase"; readonly value: Defn_RepeatedEnumCase }
  | { readonly tag: "givenAlias"; readonly value: Defn_GivenAlias }
  | { readonly tag: "extensionGroup"; readonly value: Defn_ExtensionGroup }
  | { readonly tag: "def"; readonly value: Defn_Def }
  | { readonly tag: "macro"; readonly value: Defn_Macro }
  | { readonly tag: "type"; readonly value: Defn_Type }
  | { readonly tag: "class"; readonly value: Defn_Class }
  | { readonly tag: "trait"; readonly value: Defn_Trait }
  | { readonly tag: "object"; readonly value: Defn_Object };

export interface Defn_Val {
  readonly mods: ReadonlyArray<Mod>;
  readonly pats: ReadonlyArray<Pat>;
  readonly decltpe: Type | null;
  readonly rhs: Data;
}

export interface Defn_Var {
  readonly mods: ReadonlyArray<Mod>;
  readonly pats: ReadonlyArray<Pat>;
  readonly decltpe: Type;
  readonly rhs: Data | null;
}

export interface Defn_Given {
  readonly mods: ReadonlyArray<Mod>;
  readonly name: Name;
  readonly tparams: ReadonlyArray<ReadonlyArray<Type_Param>>;
  readonly sparams: ReadonlyArray<ReadonlyArray<Data_Param>>;
  readonly templ: Template;
}

export interface Defn_Enum {
  readonly mods: ReadonlyArray<Mod>;
  readonly name: Type_Name;
  readonly tparams: ReadonlyArray<Type_Param>;
  readonly ctor: Ctor_Primary;
  readonly template: Template;
}

export interface Defn_EnumCase {
  readonly mods: ReadonlyArray<Mod>;
  readonly name: Data_Name;
  readonly tparams: ReadonlyArray<Type_Param>;
  readonly ctor: Ctor_Primary;
  readonly inits: ReadonlyArray<Init>;
}

export interface Defn_RepeatedEnumCase {
  readonly mods: ReadonlyArray<Mod>;
  readonly cases: ReadonlyArray<Data_Name>;
}

export interface Defn_GivenAlias {
  readonly mods: ReadonlyArray<Mod>;
  readonly name: Name;
  readonly tparams: ReadonlyArray<ReadonlyArray<Type_Param>>;
  readonly sparams: ReadonlyArray<ReadonlyArray<Data_Param>>;
  readonly decltpe: Type;
  readonly body: Data;
}

export interface Defn_ExtensionGroup {
  readonly tparams: ReadonlyArray<Type_Param>;
  readonly parmss: ReadonlyArray<ReadonlyArray<Data_Param>>;
  readonly body: Stat;
}

export interface Defn_Def {
  readonly mods: ReadonlyArray<Mod>;
  readonly name: Data_Name;
  readonly tparams: ReadonlyArray<Type_Param>;
  readonly paramss: ReadonlyArray<ReadonlyArray<Data_Param>>;
  readonly decltpe: Type | null;
  readonly body: Data;
}

export interface Defn_Macro {
  readonly mods: ReadonlyArray<Mod>;
  readonly name: Data_Name;
  readonly tparams: ReadonlyArray<Type_Param>;
  readonly paramss: ReadonlyArray<ReadonlyArray<Data_Param>>;
  readonly decltpe: Type | null;
  readonly body: Data;
}

export interface Defn_Type {
  readonly mods: ReadonlyArray<Mod>;
  readonly name: Type_Name;
  readonly tparams: ReadonlyArray<Type_Param>;
  readonly body: Type;
}

export interface Defn_Class {
  readonly mods: ReadonlyArray<Mod>;
  readonly name: Type_Name;
  readonly tparams: ReadonlyArray<Type_Param>;
  readonly ctor: Ctor_Primary;
  readonly template: Template;
}

export interface Defn_Trait {
  readonly mods: ReadonlyArray<Mod>;
  readonly name: Type_Name;
  readonly tparams: ReadonlyArray<Type_Param>;
  readonly ctor: Ctor_Primary;
  readonly template: Template;
}

export interface Defn_Object {
  readonly name: Data_Name;
}

export interface Pkg {
  readonly name: Data_Name;
  readonly ref: Data_Ref;
  readonly stats: ReadonlyArray<Stat>;
}

export interface Pkg_Object {
  readonly mods: ReadonlyArray<Mod>;
  readonly name: Data_Name;
  readonly template: Template;
}

export type Ctor =
  | { readonly tag: "primary"; readonly value: Ctor_Primary }
  | { readonly tag: "secondary"; readonly value: Ctor_Secondary };

export interface Ctor_Primary {
  readonly mods: ReadonlyArray<Mod>;
  readonly name: Name;
  readonly paramss: ReadonlyArray<ReadonlyArray<Data_Param>>;
}

export interface Ctor_Secondary {
  readonly mods: ReadonlyArray<Mod>;
  readonly name: Name;
  readonly paramss: ReadonlyArray<ReadonlyArray<Data_Param>>;
  readonly init: Init;
  readonly stats: ReadonlyArray<Stat>;
}

export interface Init {
  readonly tpe: Type;
  readonly name: Name;
  readonly argss: ReadonlyArray<ReadonlyArray<Data>>;
}

export type Self = void & { readonly __brand: "Self" };

export interface Template {
  readonly early: ReadonlyArray<Stat>;
  readonly inits: ReadonlyArray<Init>;
  readonly self: Self;
  readonly stats: ReadonlyArray<Stat>;
}

export type Mod =
  | { readonly tag: "annot"; readonly value: Mod_Annot }
  | { readonly tag: "private"; readonly value: Mod_Private }
  | { readonly tag: "protected"; readonly value: Mod_Protected }
  | { readonly tag: "implicit" }
  | { readonly tag: "final" }
  | { readonly tag: "sealed" }
  | { readonly tag: "open" }
  | { readonly tag: "super" }
  | { readonly tag: "override" }
  | { readonly tag: "case" }
  | { readonly tag: "abstract" }
  | { readonly tag: "covariant" }
  | { readonly tag: "contravariant" }
  | { readonly tag: "lazy" }
  | { readonly tag: "valParam" }
  | { readonly tag: "varParam" }
  | { readonly tag: "infix" }
  | { readonly tag: "inline" }
  | { readonly tag: "using" }
  | { readonly tag: "opaque" }
  | { readonly tag: "transparent" };

export interface Mod_Annot {
  readonly init: Init;
}

export interface Mod_Private {
  readonly within: Ref;
}

export interface Mod_Protected {
  readonly within: Ref;
}

export type Enumerator =
  | { readonly tag: "generator"; readonly value: Enumerator_Generator }
  | { readonly tag: "caseGenerator"; readonly value: Enumerator_CaseGenerator }
  | { readonly tag: "val"; readonly value: Enumerator_Val }
  | { readonly tag: "guard"; readonly value: Enumerator_Guard };

export interface Enumerator_Generator {
  readonly pat: Pat;
  readonly rhs: Data;
}

export interface Enumerator_CaseGenerator {
  readonly pat: Pat;
  readonly rhs: Data;
}

export interface Enumerator_Val {
  readonly pat: Pat;
  readonly rhs: Data;
}

export interface Enumerator_Guard {
  readonly cond: Data;
}

export type ImportExportStat =
  | { readonly tag: "import"; readonly value: Import }
  | { readonly tag: "export"; readonly value: Export };

export interface Import {
  readonly importers: ReadonlyArray<Importer>;
}

export interface Export {
  readonly importers: ReadonlyArray<Importer>;
}

export interface Importer {
  readonly ref: Data_Ref;
  readonly importees: ReadonlyArray<Importee>;
}

export type Importee =
  | { readonly tag: "wildcard" }
  | { readonly tag: "given"; readonly value: Importee_Given }
  | { readonly tag: "givenAll" }
  | { readonly tag: "name"; readonly value: Importee_Name }
  | { readonly tag: "rename"; readonly value: Importee_Rename }
  | { readonly tag: "unimport"; readonly value: Importee_Unimport };

export interface Importee_Given {
  readonly tpe: Type;
}

export interface Importee_Name {
  readonly name: Name;
}

export interface Importee_Rename {
  readonly name: Name;
  readonly rename: Name;
}

export interface Importee_Unimport {
  readonly name: Name;
}

export type CaseTree =
  | { readonly tag: "case"; readonly value: Case }
  | { readonly tag: "typeCase"; readonly value: TypeCase };

export interface Case {
  readonly pat: Pat;
  readonly cond: Data | null;
  readonly body: Data;
}

export interface TypeCase {
  readonly pat: Type;
  readonly body: Type;
}

export interface Source {
  readonly stats: ReadonlyArray<Stat>;
}

export type Quasi = void & { readonly __brand: "Quasi" };
