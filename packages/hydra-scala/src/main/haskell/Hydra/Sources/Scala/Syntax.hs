module Hydra.Sources.Scala.Syntax where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations           (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.scala.syntax"

def :: String -> Type -> Binding
def = datatype ns

meta :: String -> Type
meta = typeref ns

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = (map toTypeDef definitions),
            moduleDependencies = [Core.ns, Core.ns],
            moduleDescription = Just "A Scala syntax model for Hydra, anchored on Scalameta (https://scalameta.org). Departs from Scalameta where Hydra's needs differ: Term is renamed to Data, the FunctionType/FunctionData wrappers are flattened, and arms Hydra never emits (xml literals, quasiquote/macro forms) are omitted."}
  where
    definitions = [
      predefString,
      scalaSymbol,
      tree,
      ref,
      stat,
      name,
      lit,
      data_,
      refData,
      thisData,
      superData,
      nameData,
      anonymousData,
      selectData,
      interpolateData,
      applyData,
      applyUsingData,
      applyTypeData,
      applyInfixData,
      applyUnaryData,
      assignData,
      returnData,
      throwData,
      ascribeData,
      annotateData,
      tupleData,
      blockData,
      endMarkerData,
      ifData,
      matchData,
      tryData,
      tryWithHandlerData,
      contextFunctionData,
      functionData,
      polyFunctionData,
      partialFunctionData,
      whileData,
      doData,
      forData,
      forYieldData,
      newData,
      newAnonymousData,
      etaData,
      repeatedData,
      paramData,
      type_,
      refType,
      nameType,
      anonymousNameType,
      selectType,
      projectType,
      singletonType,
      applyType,
      applyInfixType,
      functionType,
      polyFunctionType,
      contextFunctionType,
      implicitFunctionType,
      tupleType,
      withType,
      andType,
      orType,
      refineType,
      existentialType,
      annotateType,
      lambdaType,
      methodType,
      placeholderType,
      typeBounds,
      byNameType,
      repeatedType,
      varType,
      typedParamType,
      paramType,
      matchType,
      pat,
      varPat,
      bindPat,
      alternativePat,
      tuplePat,
      repeatedPat,
      extractPat,
      extractInfixPat,
      interpolatePat,
      typedPat,
      givenPat,
      member,
      dataMember,
      typeMember,
      decl,
      valDecl,
      varDecl,
      defDecl,
      typeDecl,
      givenDecl,
      defn,
      valDefn,
      varDefn,
      givenDefn,
      enumDefn,
      enumCaseDefn,
      repeatedEnumCaseDefn,
      givenAliasDefn,
      extensionGroupDefn,
      defDefn,
      typeDefn,
      classDefn,
      traitDefn,
      objectDefn,
      pkg,
      objectPkg,
      ctor,
      primaryCtor,
      secondaryCtor,
      init_,
      self,
      template,
      mod_,
      annotMod,
      privateMod,
      protectedMod,
      enumerator,
      generatorEnumerator,
      caseGeneratorEnumerator,
      valEnumerator,
      guardEnumerator,
      importExportStat,
      import_,
      export_,
      importer,
      importee,
      givenImportee,
      nameImportee,
      renameImportee,
      unimportImportee,
      caseTree,
      case_,
      typeCase,
      source]

predefString :: Binding
predefString = def "PredefString" $
  doc "A wrapper for strings used in scala.Predef contexts." $
  T.wrap T.string

scalaSymbol :: Binding
scalaSymbol = def "ScalaSymbol" $
  doc "A Scala 2 symbol literal (corresponds to scala.Symbol)." $
  T.record [
    "name">: T.string]

tree :: Binding
tree = def "Tree" $
  doc "The root of the Scalameta tree hierarchy. Each arm names a major AST category." $
  T.union [
    "ref">: meta "Ref",
    "stat">: meta "Stat",
    "type">: meta "Type",
    "bounds">: meta "TypeBounds",
    "pat">: meta "Pat",
    "member">: meta "Member",
    "ctor">: meta "Ctor",
    "template">: meta "Template",
    "mod">: meta "Mod",
    "enumerator">: meta "Enumerator",
    "importer">: meta "Importer",
    "importee">: meta "Importee",
    "caseTree">: meta "CaseTree",
    "source">: meta "Source"]

ref :: Binding
ref = def "Ref" $
  T.union [
    "name">: meta "Name",
    "init">: meta "Init"]

stat :: Binding
stat = def "Stat" $
  T.union [
    "term">: meta "Data",
    "decl">: meta "Decl",
    "defn">: meta "Defn",
    "importExport">: meta "ImportExportStat"]

name :: Binding
name = def "Name" $
  T.union [
    "value">: T.string,
    "anonymous">: T.unit,
    "indeterminate">: meta "PredefString"]

lit :: Binding
lit = def "Lit" $
  T.union [
    "null">: T.unit,
    "int">: T.int32,
    "double">: T.float64,
    "float">: T.float32,
    "byte">: T.int8,
    "short">: T.int16,
    "char">: T.uint16,
    "long">: T.int64,
    "boolean">: T.boolean,
    "unit">: T.unit,
    "string">: T.string,
    "bytes">: T.list T.int32,
    "symbol">: meta "ScalaSymbol"]

data_ :: Binding
data_ = def "Data" $
  T.union [
    "lit">: meta "Lit",
    "ref">: meta "RefData",
    "interpolate">: meta "InterpolateData",
    "apply">: meta "ApplyData",
    "applyUsing">: meta "ApplyUsingData",
    "applyType">: meta "ApplyTypeData",
    "assign">: meta "AssignData",
    "return">: meta "ReturnData",
    "throw">: meta "ThrowData",
    "ascribe">: meta "AscribeData",
    "annotate">: meta "AnnotateData",
    "tuple">: meta "TupleData",
    "block">: meta "BlockData",
    "endMarker">: meta "EndMarkerData",
    "if">: meta "IfData",
    "match">: meta "MatchData",
    "try">: meta "TryData",
    "tryWithHandler">: meta "TryWithHandlerData",
    "function">: meta "FunctionData",
    "contextFunction">: meta "ContextFunctionData",
    "polyFunction">: meta "PolyFunctionData",
    "partialFunction">: meta "PartialFunctionData",
    "while">: meta "WhileData",
    "do">: meta "DoData",
    "for">: meta "ForData",
    "forYield">: meta "ForYieldData",
    "new">: meta "NewData",
    "newAnonymous">: meta "NewAnonymousData",
    "placeholder">: T.unit,
    "eta">: meta "EtaData",
    "repeated">: meta "RepeatedData",
    "param">: meta "ParamData"]

refData :: Binding
refData = def "RefData" $
  T.union [
    "this">: meta "ThisData",
    "super">: meta "SuperData",
    "name">: meta "NameData",
    "anonymous">: meta "AnonymousData",
    "select">: meta "SelectData",
    "applyUnary">: meta "ApplyUnaryData"]

thisData :: Binding
thisData = def "ThisData" $
  T.wrap T.unit

superData :: Binding
superData = def "SuperData" $
  T.record [
    "thisp">: meta "Name",
    "superp">: meta "Name"]

nameData :: Binding
nameData = def "NameData" $
  T.record [
    "value">: meta "PredefString"]

anonymousData :: Binding
anonymousData = def "AnonymousData" $
  T.wrap T.unit

selectData :: Binding
selectData = def "SelectData" $
  T.record [
    "qual">: meta "Data",
    "name">: meta "NameData"]

interpolateData :: Binding
interpolateData = def "InterpolateData" $
  T.record [
    "prefix">: meta "NameData",
    "parts">: T.list $ meta "Lit",
    "args">: T.list $ meta "Data"]

applyData :: Binding
applyData = def "ApplyData" $
  T.record [
    "fun">: meta "Data",
    "args">: T.list $ meta "Data"]

applyUsingData :: Binding
applyUsingData = def "ApplyUsingData" $
  T.record [
    "fun">: meta "Data",
    "targs">: T.list $ meta "Data"]

applyTypeData :: Binding
applyTypeData = def "ApplyTypeData" $
  T.record [
    "lhs">: meta "Data",
    "op">: meta "NameData",
    "targs">: T.list $ meta "Type",
    "args">: T.list $ meta "Data"]

applyInfixData :: Binding
applyInfixData = def "ApplyInfixData" $
  T.record [
    "lhs">: meta "Data",
    "op">: meta "NameData",
    "targs">: T.list $ meta "Type",
    "args">: T.list $ meta "Data"]

applyUnaryData :: Binding
applyUnaryData = def "ApplyUnaryData" $
  T.record [
    "op">: meta "NameData",
    "arg">: meta "Data"]

assignData :: Binding
assignData = def "AssignData" $
  T.record [
    "lhs">: meta "Data",
    "rhs">: meta "Data"]

returnData :: Binding
returnData = def "ReturnData" $
  T.record [
    "expr">: meta "Data"]

throwData :: Binding
throwData = def "ThrowData" $
  T.record [
    "expr">: meta "Data"]

ascribeData :: Binding
ascribeData = def "AscribeData" $
  T.record [
    "expr">: meta "Data",
    "tpe">: meta "Type"]

annotateData :: Binding
annotateData = def "AnnotateData" $
  T.record [
    "expr">: meta "Data",
    "annots">: T.list $ meta "AnnotMod"]

tupleData :: Binding
tupleData = def "TupleData" $
  T.record [
    "args">: T.list $ meta "Data"]

blockData :: Binding
blockData = def "BlockData" $
  T.record [
    "stats">: T.list $ meta "Stat"]

endMarkerData :: Binding
endMarkerData = def "EndMarkerData" $
  T.record [
    "name">: meta "NameData"]

ifData :: Binding
ifData = def "IfData" $
  T.record [
    "cond">: meta "Data",
    "thenp">: meta "Data",
    "elsep">: meta "Data"]

matchData :: Binding
matchData = def "MatchData" $
  T.record [
    "expr">: meta "Data",
    "cases">: T.list $ meta "Case"]

tryData :: Binding
tryData = def "TryData" $
  T.record [
    "expr">: meta "Data",
    "catchp">: T.list $ meta "Case",
    "finallyp">: T.maybe $ meta "Data"]

tryWithHandlerData :: Binding
tryWithHandlerData = def "TryWithHandlerData" $
  T.record [
    "expr">: meta "Data",
    "catchp">: meta "Data",
    "finallyp">: T.maybe $ meta "Data"]

contextFunctionData :: Binding
contextFunctionData = def "ContextFunctionData" $
  T.record [
    "params">: T.list $ meta "ParamData",
    "body">: meta "Data"]

functionData :: Binding
functionData = def "FunctionData" $
  T.record [
    "params">: T.list $ meta "ParamData",
    "body">: meta "Data"]

polyFunctionData :: Binding
polyFunctionData = def "PolyFunctionData" $
  T.record [
    "tparams">: T.list $ meta "ParamType",
    "body">: meta "Data"]

partialFunctionData :: Binding
partialFunctionData = def "PartialFunctionData" $
  T.record [
    "cases">: T.list $ meta "Case"]

whileData :: Binding
whileData = def "WhileData" $
  T.record [
    "expr">: meta "Data",
    "body">: meta "Data"]

doData :: Binding
doData = def "DoData" $
  T.record [
    "body">: meta "Data",
    "expr">: meta "Data"]

forData :: Binding
forData = def "ForData" $
  T.record [
    "enums">: T.list $ meta "Enumerator"]

forYieldData :: Binding
forYieldData = def "ForYieldData" $
  T.record [
    "enums">: T.list $ meta "Enumerator"]

newData :: Binding
newData = def "NewData" $
  T.record [
    "init">: meta "Init"]

newAnonymousData :: Binding
newAnonymousData = def "NewAnonymousData" $
  T.record [
    "templ">: meta "Template"]

etaData :: Binding
etaData = def "EtaData" $
  T.record [
    "expr">: meta "Data"]

repeatedData :: Binding
repeatedData = def "RepeatedData" $
  T.record [
    "expr">: meta "Data"]

paramData :: Binding
paramData = def "ParamData" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Name",
    "decltpe">: T.maybe $ meta "Type",
    "default">: T.maybe $ meta "Data"]

type_ :: Binding
type_ = def "Type" $
  T.union [
    "ref">: meta "RefType",
    "anonymousName">: meta "AnonymousNameType",
    "apply">: meta "ApplyType",
    "applyInfix">: meta "ApplyInfixType",
    "function">: meta "FunctionType",
    "contextFunction">: meta "ContextFunctionType",
    "polyFunction">: meta "PolyFunctionType",
    "implicitFunction">: meta "ImplicitFunctionType",
    "tuple">: meta "TupleType",
    "with">: meta "WithType",
    "and">: meta "AndType",
    "or">: meta "OrType",
    "refine">: meta "RefineType",
    "existential">: meta "ExistentialType",
    "annotate">: meta "AnnotateType",
    "lambda">: meta "LambdaType",
    "method">: meta "MethodType",
    "placeholder">: meta "PlaceholderType",
    "byName">: meta "ByNameType",
    "repeated">: meta "RepeatedType",
    "var">: meta "VarType",
    "typedParam">: meta "TypedParamType",
    "match">: meta "MatchType"]

refType :: Binding
refType = def "RefType" $
  T.union [
    "name">: meta "NameType",
    "select">: meta "SelectType",
    "project">: meta "ProjectType",
    "singleton">: meta "SingletonType"]

nameType :: Binding
nameType = def "NameType" $
  T.record [
    "value">: T.string]

anonymousNameType :: Binding
anonymousNameType = def "AnonymousNameType" $
  T.wrap T.unit

selectType :: Binding
selectType = def "SelectType" $
  T.record [
    "qual">: meta "RefData",
    "name">: meta "NameType"]

projectType :: Binding
projectType = def "ProjectType" $
  T.record [
    "qual">: meta "Type",
    "name">: meta "NameType"]

singletonType :: Binding
singletonType = def "SingletonType" $
  T.record [
    "ref">: meta "RefData"]

applyType :: Binding
applyType = def "ApplyType" $
  T.record [
    "tpe">: meta "Type",
    "args">: T.list $ meta "Type"]

applyInfixType :: Binding
applyInfixType = def "ApplyInfixType" $
  T.record [
    "lhs">: meta "Type",
    "op">: meta "NameType",
    "rhs">: meta "Type"]

functionType :: Binding
functionType = def "FunctionType" $
  T.record [
    "params">: T.list $ meta "Type",
    "res">: meta "Type"]

polyFunctionType :: Binding
polyFunctionType = def "PolyFunctionType" $
  T.record [
    "tparams">: T.list $ meta "ParamType",
    "tpe">: meta "Type"]

contextFunctionType :: Binding
contextFunctionType = def "ContextFunctionType" $
  T.record [
    "params">: T.list $ meta "Type",
    "res">: meta "Type"]

implicitFunctionType :: Binding
implicitFunctionType = def "ImplicitFunctionType" $
  T.record [
    "params">: T.list $ meta "Type",
    "res">: meta "Type"]

tupleType :: Binding
tupleType = def "TupleType" $
  T.record [
    "args">: T.list $ meta "Type"]

withType :: Binding
withType = def "WithType" $
  T.record [
    "lhs">: meta "Type",
    "rhs">: meta "Type"]

andType :: Binding
andType = def "AndType" $
  T.record [
    "lhs">: meta "Type",
    "rhs">: meta "Type"]

orType :: Binding
orType = def "OrType" $
  T.record [
    "lhs">: meta "Type",
    "rhs">: meta "Type"]

refineType :: Binding
refineType = def "RefineType" $
  T.record [
    "tpe">: T.maybe $ meta "Type",
    "stats">: T.list $ meta "Stat"]

existentialType :: Binding
existentialType = def "ExistentialType" $
  T.record [
    "tpe">: meta "Type",
    "stats">: T.list $ meta "Stat"]

annotateType :: Binding
annotateType = def "AnnotateType" $
  T.record [
    "tpe">: meta "Type",
    "annots">: T.list $ meta "AnnotMod"]

lambdaType :: Binding
lambdaType = def "LambdaType" $
  T.record [
    "tparams">: T.list $ meta "ParamType",
    "tpe">: meta "Type"]

methodType :: Binding
methodType = def "MethodType" $
  T.record [
    "paramss">: T.list $ T.list $ meta "ParamData",
    "tpe">: meta "Type"]

placeholderType :: Binding
placeholderType = def "PlaceholderType" $
  T.record [
    "bounds">: meta "TypeBounds"]

typeBounds :: Binding
typeBounds = def "TypeBounds" $
  T.record [
    "lo">: T.maybe $ meta "Type",
    "hi">: T.maybe $ meta "Type"]

byNameType :: Binding
byNameType = def "ByNameType" $
  T.record [
    "tpe">: meta "Type"]

repeatedType :: Binding
repeatedType = def "RepeatedType" $
  T.record [
    "tpe">: meta "Type"]

varType :: Binding
varType = def "VarType" $
  T.record [
    "name">: meta "NameType"]

typedParamType :: Binding
typedParamType = def "TypedParamType" $
  T.record [
    "name">: meta "Name",
    "typ">: meta "Type"]

paramType :: Binding
paramType = def "ParamType" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Name",
    "tparams">: T.list $ meta "ParamType",
    "tbounds">: T.list $ meta "TypeBounds",
    "vbounds">: T.list $ meta "Type",
    "cbounds">: T.list $ meta "Type"]

matchType :: Binding
matchType = def "MatchType" $
  T.record [
    "tpe">: meta "Type",
    "cases">: T.list $ meta "TypeCase"]

pat :: Binding
pat = def "Pat" $
  T.union [
    "var">: meta "VarPat",
    "wildcard">: T.unit,
    "seqWildcard">: T.unit,
    "bind">: meta "BindPat",
    "alternative">: meta "AlternativePat",
    "tuple">: meta "TuplePat",
    "repeated">: meta "RepeatedPat",
    "extract">: meta "ExtractPat",
    "extractInfix">: meta "ExtractInfixPat",
    "interpolate">: meta "InterpolatePat",
    "typed">: meta "TypedPat",
    "given">: meta "GivenPat"]

varPat :: Binding
varPat = def "VarPat" $
  T.record [
    "name">: meta "NameData"]

bindPat :: Binding
bindPat = def "BindPat" $
  T.record [
    "lhs">: meta "Pat",
    "rhs">: meta "Pat"]

alternativePat :: Binding
alternativePat = def "AlternativePat" $
  T.record [
    "lhs">: meta "Pat",
    "rhs">: meta "Pat"]

tuplePat :: Binding
tuplePat = def "TuplePat" $
  T.record [
    "args">: T.list $ meta "Pat"]

repeatedPat :: Binding
repeatedPat = def "RepeatedPat" $
  T.record [
    "name">: meta "NameData"]

extractPat :: Binding
extractPat = def "ExtractPat" $
  T.record [
    "fun">: meta "Data",
    "args">: T.list $ meta "Pat"]

extractInfixPat :: Binding
extractInfixPat = def "ExtractInfixPat" $
  T.record [
    "lhs">: meta "Pat",
    "op">: meta "NameData",
    "rhs">: T.list $ meta "Pat"]

interpolatePat :: Binding
interpolatePat = def "InterpolatePat" $
  T.record [
    "prefix">: meta "NameData",
    "parts">: T.list $ meta "Lit"]

typedPat :: Binding
typedPat = def "TypedPat" $
  T.record [
    "lhs">: meta "Pat",
    "rhs">: meta "Type"]

givenPat :: Binding
givenPat = def "GivenPat" $
  T.record [
    "tpe">: meta "Type"]

member :: Binding
member = def "Member" $
  T.union [
    "term">: meta "DataMember",
    "type">: meta "TypeMember",
    "termParam">: meta "ParamData",
    "typeParam">: meta "ParamType",
    "self">: meta "Self"]

dataMember :: Binding
dataMember = def "DataMember" $
  T.union [
    "pkg">: meta "Pkg",
    "object">: meta "ObjectPkg"]

typeMember :: Binding
typeMember = def "TypeMember" $
  T.record [
    "name">: meta "NameType"]

decl :: Binding
decl = def "Decl" $
  T.union [
    "val">: meta "ValDecl",
    "var">: meta "VarDecl",
    "def">: meta "DefDecl",
    "type">: meta "TypeDecl",
    "given">: meta "GivenDecl"]

valDecl :: Binding
valDecl = def "ValDecl" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "pats">: T.list $ meta "Pat",
    "decltpe">: meta "Type"]

varDecl :: Binding
varDecl = def "VarDecl" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "pats">: T.list $ meta "Pat",
    "decltpe">: meta "Type"]

defDecl :: Binding
defDecl = def "DefDecl" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "NameData",
    "tparams">: T.list $ meta "ParamType",
    "paramss">: T.list $ T.list $ meta "ParamData",
    "decltpe">: meta "Type"]

typeDecl :: Binding
typeDecl = def "TypeDecl" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "NameType",
    "tparams">: T.list $ meta "ParamType",
    "bounds">: meta "TypeBounds"]

givenDecl :: Binding
givenDecl = def "GivenDecl" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "NameData",
    "tparams">: T.list $ meta "ParamType",
    "sparams">: T.list $ T.list $ meta "ParamData",
    "decltpe">: meta "Type"]

defn :: Binding
defn = def "Defn" $
  T.union [
    "val">: meta "ValDefn",
    "var">: meta "VarDefn",
    "given">: meta "GivenDefn",
    "enum">: meta "EnumDefn",
    "enumCase">: meta "EnumCaseDefn",
    "repeatedEnumCase">: meta "RepeatedEnumCaseDefn",
    "givenAlias">: meta "GivenAliasDefn",
    "extensionGroup">: meta "ExtensionGroupDefn",
    "def">: meta "DefDefn",
    "type">: meta "TypeDefn",
    "class">: meta "ClassDefn",
    "trait">: meta "TraitDefn",
    "object">: meta "ObjectDefn"]

valDefn :: Binding
valDefn = def "ValDefn" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "pats">: T.list $ meta "Pat",
    "decltpe">: T.maybe $ meta "Type",
    "rhs">: meta "Data"]

varDefn :: Binding
varDefn = def "VarDefn" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "pats">: T.list $ meta "Pat",
    "decltpe">: meta "Type",
    "rhs">: T.maybe $ meta "Data"]

givenDefn :: Binding
givenDefn = def "GivenDefn" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Name",
    "tparams">: T.list $ T.list $ meta "ParamType",
    "sparams">: T.list $ T.list $ meta "ParamData",
    "templ">: meta "Template"]

enumDefn :: Binding
enumDefn = def "EnumDefn" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "NameType",
    "tparams">: T.list $ meta "ParamType",
    "ctor">: meta "PrimaryCtor",
    "template">: meta "Template"]

enumCaseDefn :: Binding
enumCaseDefn = def "EnumCaseDefn" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "NameData",
    "tparams">: T.list $ meta "ParamType",
    "ctor">: meta "PrimaryCtor",
    "inits">: T.list $ meta "Init"]

repeatedEnumCaseDefn :: Binding
repeatedEnumCaseDefn = def "RepeatedEnumCaseDefn" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "cases">: T.list $ meta "NameData"]

givenAliasDefn :: Binding
givenAliasDefn = def "GivenAliasDefn" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Name",
    "tparams">: T.list $ T.list $ meta "ParamType",
    "sparams">: T.list $ T.list $ meta "ParamData",
    "decltpe">: meta "Type",
    "body">: meta "Data"]

extensionGroupDefn :: Binding
extensionGroupDefn = def "ExtensionGroupDefn" $
  T.record [
    "tparams">: T.list $ meta "ParamType",
    "parmss">: T.list $ T.list $ meta "ParamData",
    "body">: meta "Stat"]

defDefn :: Binding
defDefn = def "DefDefn" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "NameData",
    "tparams">: T.list $ meta "ParamType",
    "paramss">: T.list $ T.list $ meta "ParamData",
    "decltpe">: T.maybe $ meta "Type",
    "body">: meta "Data"]

typeDefn :: Binding
typeDefn = def "TypeDefn" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "NameType",
    "tparams">: T.list $ meta "ParamType",
    "body">: meta "Type"]

classDefn :: Binding
classDefn = def "ClassDefn" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "NameType",
    "tparams">: T.list $ meta "ParamType",
    "ctor">: meta "PrimaryCtor",
    "template">: meta "Template"]

traitDefn :: Binding
traitDefn = def "TraitDefn" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "NameType",
    "tparams">: T.list $ meta "ParamType",
    "ctor">: meta "PrimaryCtor",
    "template">: meta "Template"]

objectDefn :: Binding
objectDefn = def "ObjectDefn" $
  T.record [
    "name">: meta "NameData"]

pkg :: Binding
pkg = def "Pkg" $
  T.record [
    "name">: meta "NameData",
    "ref">: meta "RefData",
    "stats">: T.list $ meta "Stat"]

objectPkg :: Binding
objectPkg = def "ObjectPkg" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "NameData",
    "template">: meta "Template"]

ctor :: Binding
ctor = def "Ctor" $
  T.union [
    "primary">: meta "PrimaryCtor",
    "secondary">: meta "SecondaryCtor"]

primaryCtor :: Binding
primaryCtor = def "PrimaryCtor" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Name",
    "paramss">: T.list $ T.list $ meta "ParamData"]

secondaryCtor :: Binding
secondaryCtor = def "SecondaryCtor" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Name",
    "paramss">: T.list $ T.list $ meta "ParamData",
    "init">: meta "Init",
    "stats">: T.list $ meta "Stat"]

init_ :: Binding
init_ = def "Init" $
  T.record [
    "tpe">: meta "Type",
    "name">: meta "Name",
    "argss">: T.list $ T.list $ meta "Data"]

self :: Binding
self = def "Self" $
  T.wrap T.unit

template :: Binding
template = def "Template" $
  T.record [
    "early">: T.list $ meta "Stat",
    "inits">: T.list $ meta "Init",
    "self">: meta "Self",
    "stats">: T.list $ meta "Stat"]

mod_ :: Binding
mod_ = def "Mod" $
  T.union [
    "annot">: meta "AnnotMod",
    "private">: meta "PrivateMod",
    "protected">: meta "ProtectedMod",
    "implicit">: T.unit,
    "final">: T.unit,
    "sealed">: T.unit,
    "open">: T.unit,
    "super">: T.unit,
    "override">: T.unit,
    "case">: T.unit,
    "abstract">: T.unit,
    "covariant">: T.unit,
    "contravariant">: T.unit,
    "lazy">: T.unit,
    "valParam">: T.unit,
    "varParam">: T.unit,
    "infix">: T.unit,
    "inline">: T.unit,
    "using">: T.unit,
    "opaque">: T.unit,
    "transparent">: T.unit]

annotMod :: Binding
annotMod = def "AnnotMod" $
  T.record [
    "init">: meta "Init"]

privateMod :: Binding
privateMod = def "PrivateMod" $
  T.record [
    "within">: meta "Ref"]

protectedMod :: Binding
protectedMod = def "ProtectedMod" $
  T.record [
    "within">: meta "Ref"]

enumerator :: Binding
enumerator = def "Enumerator" $
  T.union [
    "generator">: meta "GeneratorEnumerator",
    "caseGenerator">: meta "CaseGeneratorEnumerator",
    "val">: meta "ValEnumerator",
    "guard">: meta "GuardEnumerator"]

generatorEnumerator :: Binding
generatorEnumerator = def "GeneratorEnumerator" $
  T.record [
    "pat">: meta "Pat",
    "rhs">: meta "Data"]

caseGeneratorEnumerator :: Binding
caseGeneratorEnumerator = def "CaseGeneratorEnumerator" $
  T.record [
    "pat">: meta "Pat",
    "rhs">: meta "Data"]

valEnumerator :: Binding
valEnumerator = def "ValEnumerator" $
  T.record [
    "pat">: meta "Pat",
    "rhs">: meta "Data"]

guardEnumerator :: Binding
guardEnumerator = def "GuardEnumerator" $
  T.record [
    "cond">: meta "Data"]

importExportStat :: Binding
importExportStat = def "ImportExportStat" $
  T.union [
    "import">: meta "Import",
    "export">: meta "Export"]

import_ :: Binding
import_ = def "Import" $
  T.record [
    "importers">: T.list $ meta "Importer"]

export_ :: Binding
export_ = def "Export" $
  T.record [
    "importers">: T.list $ meta "Importer"]

importer :: Binding
importer = def "Importer" $
  T.record [
    "ref">: meta "RefData",
    "importees">: T.list $ meta "Importee"]

importee :: Binding
importee = def "Importee" $
  T.union [
    "wildcard">: T.unit,
    "given">: meta "GivenImportee",
    "givenAll">: T.unit,
    "name">: meta "NameImportee",
    "rename">: meta "RenameImportee",
    "unimport">: meta "UnimportImportee"]

givenImportee :: Binding
givenImportee = def "GivenImportee" $
  T.record [
    "tpe">: meta "Type"]

nameImportee :: Binding
nameImportee = def "NameImportee" $
  T.record [
    "name">: meta "Name"]

renameImportee :: Binding
renameImportee = def "RenameImportee" $
  T.record [
    "name">: meta "Name",
    "rename">: meta "Name"]

unimportImportee :: Binding
unimportImportee = def "UnimportImportee" $
  T.record [
    "name">: meta "Name"]

caseTree :: Binding
caseTree = def "CaseTree" $
  T.union [
    "case">: meta "Case",
    "typeCase">: meta "TypeCase"]

case_ :: Binding
case_ = def "Case" $
  T.record [
    "pat">: meta "Pat",
    "cond">: T.maybe $ meta "Data",
    "body">: meta "Data"]

typeCase :: Binding
typeCase = def "TypeCase" $
  T.record [
    "pat">: meta "Type",
    "body">: meta "Type"]

source :: Binding
source = def "Source" $
  T.record [
    "stats">: T.list $ meta "Stat"]
