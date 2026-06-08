module Hydra.Sources.Scala.Syntax where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations           (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: ModuleName
ns = ModuleName "hydra.scala.syntax"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just "A Scala syntax model for Hydra, anchored on Scalameta (https://scalameta.org). Departs from Scalameta where Hydra's needs differ: Term is renamed to Data, the FunctionType/FunctionData wrappers are flattened, and arms Hydra never emits (xml literals, quasiquote/macro forms) are omitted.")}
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

self :: TypeDefinition
self = def "Self" $
  T.wrap T.unit

alternativePat :: TypeDefinition
alternativePat = def "AlternativePat" $
  T.record [
    "lhs">: meta "Pat",
    "rhs">: meta "Pat"]

andType :: TypeDefinition
andType = def "AndType" $
  T.record [
    "lhs">: meta "Type",
    "rhs">: meta "Type"]

annotMod :: TypeDefinition
annotMod = def "AnnotMod" $
  T.record [
    "init">: meta "Init"]

annotateData :: TypeDefinition
annotateData = def "AnnotateData" $
  T.record [
    "expr">: meta "Data",
    "annots">: T.list $ meta "AnnotMod"]

annotateType :: TypeDefinition
annotateType = def "AnnotateType" $
  T.record [
    "tpe">: meta "Type",
    "annots">: T.list $ meta "AnnotMod"]

anonymousData :: TypeDefinition
anonymousData = def "AnonymousData" $
  T.wrap T.unit

anonymousNameType :: TypeDefinition
anonymousNameType = def "AnonymousNameType" $
  T.wrap T.unit

applyData :: TypeDefinition
applyData = def "ApplyData" $
  T.record [
    "fun">: meta "Data",
    "args">: T.list $ meta "Data"]

applyInfixData :: TypeDefinition
applyInfixData = def "ApplyInfixData" $
  T.record [
    "lhs">: meta "Data",
    "op">: meta "NameData",
    "targs">: T.list $ meta "Type",
    "args">: T.list $ meta "Data"]

applyInfixType :: TypeDefinition
applyInfixType = def "ApplyInfixType" $
  T.record [
    "lhs">: meta "Type",
    "op">: meta "NameType",
    "rhs">: meta "Type"]

applyType :: TypeDefinition
applyType = def "ApplyType" $
  T.record [
    "tpe">: meta "Type",
    "args">: T.list $ meta "Type"]

applyTypeData :: TypeDefinition
applyTypeData = def "ApplyTypeData" $
  T.record [
    "lhs">: meta "Data",
    "op">: meta "NameData",
    "targs">: T.list $ meta "Type",
    "args">: T.list $ meta "Data"]

applyUnaryData :: TypeDefinition
applyUnaryData = def "ApplyUnaryData" $
  T.record [
    "op">: meta "NameData",
    "arg">: meta "Data"]

applyUsingData :: TypeDefinition
applyUsingData = def "ApplyUsingData" $
  T.record [
    "fun">: meta "Data",
    "targs">: T.list $ meta "Data"]

ascribeData :: TypeDefinition
ascribeData = def "AscribeData" $
  T.record [
    "expr">: meta "Data",
    "tpe">: meta "Type"]

assignData :: TypeDefinition
assignData = def "AssignData" $
  T.record [
    "lhs">: meta "Data",
    "rhs">: meta "Data"]

bindPat :: TypeDefinition
bindPat = def "BindPat" $
  T.record [
    "lhs">: meta "Pat",
    "rhs">: meta "Pat"]

blockData :: TypeDefinition
blockData = def "BlockData" $
  T.record [
    "stats">: T.list $ meta "Stat"]

byNameType :: TypeDefinition
byNameType = def "ByNameType" $
  T.record [
    "tpe">: meta "Type"]

caseGeneratorEnumerator :: TypeDefinition
caseGeneratorEnumerator = def "CaseGeneratorEnumerator" $
  T.record [
    "pat">: meta "Pat",
    "rhs">: meta "Data"]

caseTree :: TypeDefinition
caseTree = def "CaseTree" $
  T.union [
    "case">: meta "Case",
    "typeCase">: meta "TypeCase"]

case_ :: TypeDefinition
case_ = def "Case" $
  T.record [
    "pat">: meta "Pat",
    "cond">: T.optional $ meta "Data",
    "body">: meta "Data"]

classDefn :: TypeDefinition
classDefn = def "ClassDefn" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "NameType",
    "tparams">: T.list $ meta "ParamType",
    "ctor">: meta "PrimaryCtor",
    "template">: meta "Template"]

contextFunctionData :: TypeDefinition
contextFunctionData = def "ContextFunctionData" $
  T.record [
    "params">: T.list $ meta "ParamData",
    "body">: meta "Data"]

contextFunctionType :: TypeDefinition
contextFunctionType = def "ContextFunctionType" $
  T.record [
    "params">: T.list $ meta "Type",
    "res">: meta "Type"]

ctor :: TypeDefinition
ctor = def "Ctor" $
  T.union [
    "primary">: meta "PrimaryCtor",
    "secondary">: meta "SecondaryCtor"]

dataMember :: TypeDefinition
dataMember = def "DataMember" $
  T.union [
    "pkg">: meta "Pkg",
    "object">: meta "ObjectPkg"]

data_ :: TypeDefinition
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

decl :: TypeDefinition
decl = def "Decl" $
  T.union [
    "val">: meta "ValDecl",
    "var">: meta "VarDecl",
    "def">: meta "DefDecl",
    "type">: meta "TypeDecl",
    "given">: meta "GivenDecl"]

def :: String -> Type -> TypeDefinition
def = datatype ns

defDecl :: TypeDefinition
defDecl = def "DefDecl" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "NameData",
    "tparams">: T.list $ meta "ParamType",
    "paramss">: T.list $ T.list $ meta "ParamData",
    "decltpe">: meta "Type"]

defDefn :: TypeDefinition
defDefn = def "DefDefn" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "NameData",
    "tparams">: T.list $ meta "ParamType",
    "paramss">: T.list $ T.list $ meta "ParamData",
    "decltpe">: T.optional $ meta "Type",
    "body">: meta "Data"]

defn :: TypeDefinition
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

doData :: TypeDefinition
doData = def "DoData" $
  T.record [
    "body">: meta "Data",
    "expr">: meta "Data"]

endMarkerData :: TypeDefinition
endMarkerData = def "EndMarkerData" $
  T.record [
    "name">: meta "NameData"]

enumCaseDefn :: TypeDefinition
enumCaseDefn = def "EnumCaseDefn" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "NameData",
    "tparams">: T.list $ meta "ParamType",
    "ctor">: meta "PrimaryCtor",
    "inits">: T.list $ meta "Init"]

enumDefn :: TypeDefinition
enumDefn = def "EnumDefn" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "NameType",
    "tparams">: T.list $ meta "ParamType",
    "ctor">: meta "PrimaryCtor",
    "template">: meta "Template"]

enumerator :: TypeDefinition
enumerator = def "Enumerator" $
  T.union [
    "generator">: meta "GeneratorEnumerator",
    "caseGenerator">: meta "CaseGeneratorEnumerator",
    "val">: meta "ValEnumerator",
    "guard">: meta "GuardEnumerator"]

etaData :: TypeDefinition
etaData = def "EtaData" $
  T.record [
    "expr">: meta "Data"]

existentialType :: TypeDefinition
existentialType = def "ExistentialType" $
  T.record [
    "tpe">: meta "Type",
    "stats">: T.list $ meta "Stat"]

export_ :: TypeDefinition
export_ = def "Export" $
  T.record [
    "importers">: T.list $ meta "Importer"]

extensionGroupDefn :: TypeDefinition
extensionGroupDefn = def "ExtensionGroupDefn" $
  T.record [
    "tparams">: T.list $ meta "ParamType",
    "parmss">: T.list $ T.list $ meta "ParamData",
    "body">: meta "Stat"]

extractInfixPat :: TypeDefinition
extractInfixPat = def "ExtractInfixPat" $
  T.record [
    "lhs">: meta "Pat",
    "op">: meta "NameData",
    "rhs">: T.list $ meta "Pat"]

extractPat :: TypeDefinition
extractPat = def "ExtractPat" $
  T.record [
    "fun">: meta "Data",
    "args">: T.list $ meta "Pat"]

forData :: TypeDefinition
forData = def "ForData" $
  T.record [
    "enums">: T.list $ meta "Enumerator"]

forYieldData :: TypeDefinition
forYieldData = def "ForYieldData" $
  T.record [
    "enums">: T.list $ meta "Enumerator"]

functionData :: TypeDefinition
functionData = def "FunctionData" $
  T.record [
    "params">: T.list $ meta "ParamData",
    "body">: meta "Data"]

functionType :: TypeDefinition
functionType = def "FunctionType" $
  T.record [
    "params">: T.list $ meta "Type",
    "res">: meta "Type"]

generatorEnumerator :: TypeDefinition
generatorEnumerator = def "GeneratorEnumerator" $
  T.record [
    "pat">: meta "Pat",
    "rhs">: meta "Data"]

givenAliasDefn :: TypeDefinition
givenAliasDefn = def "GivenAliasDefn" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Name",
    "tparams">: T.list $ T.list $ meta "ParamType",
    "sparams">: T.list $ T.list $ meta "ParamData",
    "decltpe">: meta "Type",
    "body">: meta "Data"]

givenDecl :: TypeDefinition
givenDecl = def "GivenDecl" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "NameData",
    "tparams">: T.list $ meta "ParamType",
    "sparams">: T.list $ T.list $ meta "ParamData",
    "decltpe">: meta "Type"]

givenDefn :: TypeDefinition
givenDefn = def "GivenDefn" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Name",
    "tparams">: T.list $ T.list $ meta "ParamType",
    "sparams">: T.list $ T.list $ meta "ParamData",
    "templ">: meta "Template"]

givenImportee :: TypeDefinition
givenImportee = def "GivenImportee" $
  T.record [
    "tpe">: meta "Type"]

givenPat :: TypeDefinition
givenPat = def "GivenPat" $
  T.record [
    "tpe">: meta "Type"]

guardEnumerator :: TypeDefinition
guardEnumerator = def "GuardEnumerator" $
  T.record [
    "cond">: meta "Data"]

ifData :: TypeDefinition
ifData = def "IfData" $
  T.record [
    "cond">: meta "Data",
    "thenp">: meta "Data",
    "elsep">: meta "Data"]

implicitFunctionType :: TypeDefinition
implicitFunctionType = def "ImplicitFunctionType" $
  T.record [
    "params">: T.list $ meta "Type",
    "res">: meta "Type"]

importExportStat :: TypeDefinition
importExportStat = def "ImportExportStat" $
  T.union [
    "import">: meta "Import",
    "export">: meta "Export"]

import_ :: TypeDefinition
import_ = def "Import" $
  T.record [
    "importers">: T.list $ meta "Importer"]

importee :: TypeDefinition
importee = def "Importee" $
  T.union [
    "wildcard">: T.unit,
    "given">: meta "GivenImportee",
    "givenAll">: T.unit,
    "name">: meta "NameImportee",
    "rename">: meta "RenameImportee",
    "unimport">: meta "UnimportImportee"]

importer :: TypeDefinition
importer = def "Importer" $
  T.record [
    "ref">: meta "RefData",
    "importees">: T.list $ meta "Importee"]

init_ :: TypeDefinition
init_ = def "Init" $
  T.record [
    "tpe">: meta "Type",
    "name">: meta "Name",
    "argss">: T.list $ T.list $ meta "Data"]

interpolateData :: TypeDefinition
interpolateData = def "InterpolateData" $
  T.record [
    "prefix">: meta "NameData",
    "parts">: T.list $ meta "Lit",
    "args">: T.list $ meta "Data"]

interpolatePat :: TypeDefinition
interpolatePat = def "InterpolatePat" $
  T.record [
    "prefix">: meta "NameData",
    "parts">: T.list $ meta "Lit"]

lambdaType :: TypeDefinition
lambdaType = def "LambdaType" $
  T.record [
    "tparams">: T.list $ meta "ParamType",
    "tpe">: meta "Type"]

lit :: TypeDefinition
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

matchData :: TypeDefinition
matchData = def "MatchData" $
  T.record [
    "expr">: meta "Data",
    "cases">: T.list $ meta "Case"]

matchType :: TypeDefinition
matchType = def "MatchType" $
  T.record [
    "tpe">: meta "Type",
    "cases">: T.list $ meta "TypeCase"]

member :: TypeDefinition
member = def "Member" $
  T.union [
    "term">: meta "DataMember",
    "type">: meta "TypeMember",
    "termParam">: meta "ParamData",
    "typeParam">: meta "ParamType",
    "self">: meta "Self"]

meta :: String -> Type
meta = typeref ns

methodType :: TypeDefinition
methodType = def "MethodType" $
  T.record [
    "paramss">: T.list $ T.list $ meta "ParamData",
    "tpe">: meta "Type"]

mod_ :: TypeDefinition
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

name :: TypeDefinition
name = def "Name" $
  T.union [
    "value">: T.string,
    "anonymous">: T.unit,
    "indeterminate">: meta "PredefString"]

nameData :: TypeDefinition
nameData = def "NameData" $
  T.record [
    "value">: meta "PredefString"]

nameImportee :: TypeDefinition
nameImportee = def "NameImportee" $
  T.record [
    "name">: meta "Name"]

nameType :: TypeDefinition
nameType = def "NameType" $
  T.record [
    "value">: T.string]

newAnonymousData :: TypeDefinition
newAnonymousData = def "NewAnonymousData" $
  T.record [
    "templ">: meta "Template"]

newData :: TypeDefinition
newData = def "NewData" $
  T.record [
    "init">: meta "Init"]

objectDefn :: TypeDefinition
objectDefn = def "ObjectDefn" $
  T.record [
    "name">: meta "NameData"]

objectPkg :: TypeDefinition
objectPkg = def "ObjectPkg" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "NameData",
    "template">: meta "Template"]

orType :: TypeDefinition
orType = def "OrType" $
  T.record [
    "lhs">: meta "Type",
    "rhs">: meta "Type"]

paramData :: TypeDefinition
paramData = def "ParamData" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Name",
    "decltpe">: T.optional $ meta "Type",
    "default">: T.optional $ meta "Data"]

paramType :: TypeDefinition
paramType = def "ParamType" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Name",
    "tparams">: T.list $ meta "ParamType",
    "tbounds">: T.list $ meta "TypeBounds",
    "vbounds">: T.list $ meta "Type",
    "cbounds">: T.list $ meta "Type"]

partialFunctionData :: TypeDefinition
partialFunctionData = def "PartialFunctionData" $
  T.record [
    "cases">: T.list $ meta "Case"]

pat :: TypeDefinition
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

pkg :: TypeDefinition
pkg = def "Pkg" $
  T.record [
    "name">: meta "NameData",
    "ref">: meta "RefData",
    "stats">: T.list $ meta "Stat"]

placeholderType :: TypeDefinition
placeholderType = def "PlaceholderType" $
  T.record [
    "bounds">: meta "TypeBounds"]

polyFunctionData :: TypeDefinition
polyFunctionData = def "PolyFunctionData" $
  T.record [
    "tparams">: T.list $ meta "ParamType",
    "body">: meta "Data"]

polyFunctionType :: TypeDefinition
polyFunctionType = def "PolyFunctionType" $
  T.record [
    "tparams">: T.list $ meta "ParamType",
    "tpe">: meta "Type"]

predefString :: TypeDefinition
predefString = def "PredefString" $
  doc "A wrapper for strings used in scala.Predef contexts." $
  T.wrap T.string

primaryCtor :: TypeDefinition
primaryCtor = def "PrimaryCtor" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Name",
    "paramss">: T.list $ T.list $ meta "ParamData"]

privateMod :: TypeDefinition
privateMod = def "PrivateMod" $
  T.record [
    "within">: meta "Ref"]

projectType :: TypeDefinition
projectType = def "ProjectType" $
  T.record [
    "qual">: meta "Type",
    "name">: meta "NameType"]

protectedMod :: TypeDefinition
protectedMod = def "ProtectedMod" $
  T.record [
    "within">: meta "Ref"]

ref :: TypeDefinition
ref = def "Ref" $
  T.union [
    "name">: meta "Name",
    "init">: meta "Init"]

refData :: TypeDefinition
refData = def "RefData" $
  T.union [
    "this">: meta "ThisData",
    "super">: meta "SuperData",
    "name">: meta "NameData",
    "anonymous">: meta "AnonymousData",
    "select">: meta "SelectData",
    "applyUnary">: meta "ApplyUnaryData"]

refType :: TypeDefinition
refType = def "RefType" $
  T.union [
    "name">: meta "NameType",
    "select">: meta "SelectType",
    "project">: meta "ProjectType",
    "singleton">: meta "SingletonType"]

refineType :: TypeDefinition
refineType = def "RefineType" $
  T.record [
    "tpe">: T.optional $ meta "Type",
    "stats">: T.list $ meta "Stat"]

renameImportee :: TypeDefinition
renameImportee = def "RenameImportee" $
  T.record [
    "name">: meta "Name",
    "rename">: meta "Name"]

repeatedData :: TypeDefinition
repeatedData = def "RepeatedData" $
  T.record [
    "expr">: meta "Data"]

repeatedEnumCaseDefn :: TypeDefinition
repeatedEnumCaseDefn = def "RepeatedEnumCaseDefn" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "cases">: T.list $ meta "NameData"]

repeatedPat :: TypeDefinition
repeatedPat = def "RepeatedPat" $
  T.record [
    "name">: meta "NameData"]

repeatedType :: TypeDefinition
repeatedType = def "RepeatedType" $
  T.record [
    "tpe">: meta "Type"]

returnData :: TypeDefinition
returnData = def "ReturnData" $
  T.record [
    "expr">: meta "Data"]

scalaSymbol :: TypeDefinition
scalaSymbol = def "ScalaSymbol" $
  doc "A Scala 2 symbol literal (corresponds to scala.Symbol)." $
  T.record [
    "name">: T.string]

secondaryCtor :: TypeDefinition
secondaryCtor = def "SecondaryCtor" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "Name",
    "paramss">: T.list $ T.list $ meta "ParamData",
    "init">: meta "Init",
    "stats">: T.list $ meta "Stat"]

selectData :: TypeDefinition
selectData = def "SelectData" $
  T.record [
    "qual">: meta "Data",
    "name">: meta "NameData"]

selectType :: TypeDefinition
selectType = def "SelectType" $
  T.record [
    "qual">: meta "RefData",
    "name">: meta "NameType"]

singletonType :: TypeDefinition
singletonType = def "SingletonType" $
  T.record [
    "ref">: meta "RefData"]

source :: TypeDefinition
source = def "Source" $
  T.record [
    "stats">: T.list $ meta "Stat"]

stat :: TypeDefinition
stat = def "Stat" $
  T.union [
    "term">: meta "Data",
    "decl">: meta "Decl",
    "defn">: meta "Defn",
    "importExport">: meta "ImportExportStat"]

superData :: TypeDefinition
superData = def "SuperData" $
  T.record [
    "thisp">: meta "Name",
    "superp">: meta "Name"]

template :: TypeDefinition
template = def "Template" $
  T.record [
    "early">: T.list $ meta "Stat",
    "inits">: T.list $ meta "Init",
    "self">: meta "Self",
    "stats">: T.list $ meta "Stat"]

thisData :: TypeDefinition
thisData = def "ThisData" $
  T.wrap T.unit

throwData :: TypeDefinition
throwData = def "ThrowData" $
  T.record [
    "expr">: meta "Data"]

traitDefn :: TypeDefinition
traitDefn = def "TraitDefn" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "NameType",
    "tparams">: T.list $ meta "ParamType",
    "ctor">: meta "PrimaryCtor",
    "template">: meta "Template"]

tree :: TypeDefinition
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

tryData :: TypeDefinition
tryData = def "TryData" $
  T.record [
    "expr">: meta "Data",
    "catchp">: T.list $ meta "Case",
    "finallyp">: T.optional $ meta "Data"]

tryWithHandlerData :: TypeDefinition
tryWithHandlerData = def "TryWithHandlerData" $
  T.record [
    "expr">: meta "Data",
    "catchp">: meta "Data",
    "finallyp">: T.optional $ meta "Data"]

tupleData :: TypeDefinition
tupleData = def "TupleData" $
  T.record [
    "args">: T.list $ meta "Data"]

tuplePat :: TypeDefinition
tuplePat = def "TuplePat" $
  T.record [
    "args">: T.list $ meta "Pat"]

tupleType :: TypeDefinition
tupleType = def "TupleType" $
  T.record [
    "args">: T.list $ meta "Type"]

typeBounds :: TypeDefinition
typeBounds = def "TypeBounds" $
  T.record [
    "lo">: T.optional $ meta "Type",
    "hi">: T.optional $ meta "Type"]

typeCase :: TypeDefinition
typeCase = def "TypeCase" $
  T.record [
    "pat">: meta "Type",
    "body">: meta "Type"]

typeDecl :: TypeDefinition
typeDecl = def "TypeDecl" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "NameType",
    "tparams">: T.list $ meta "ParamType",
    "bounds">: meta "TypeBounds"]

typeDefn :: TypeDefinition
typeDefn = def "TypeDefn" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "name">: meta "NameType",
    "tparams">: T.list $ meta "ParamType",
    "body">: meta "Type"]

typeMember :: TypeDefinition
typeMember = def "TypeMember" $
  T.record [
    "name">: meta "NameType"]

type_ :: TypeDefinition
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

typedParamType :: TypeDefinition
typedParamType = def "TypedParamType" $
  T.record [
    "name">: meta "Name",
    "typ">: meta "Type"]

typedPat :: TypeDefinition
typedPat = def "TypedPat" $
  T.record [
    "lhs">: meta "Pat",
    "rhs">: meta "Type"]

unimportImportee :: TypeDefinition
unimportImportee = def "UnimportImportee" $
  T.record [
    "name">: meta "Name"]

valDecl :: TypeDefinition
valDecl = def "ValDecl" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "pats">: T.list $ meta "Pat",
    "decltpe">: meta "Type"]

valDefn :: TypeDefinition
valDefn = def "ValDefn" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "pats">: T.list $ meta "Pat",
    "decltpe">: T.optional $ meta "Type",
    "rhs">: meta "Data"]

valEnumerator :: TypeDefinition
valEnumerator = def "ValEnumerator" $
  T.record [
    "pat">: meta "Pat",
    "rhs">: meta "Data"]

varDecl :: TypeDefinition
varDecl = def "VarDecl" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "pats">: T.list $ meta "Pat",
    "decltpe">: meta "Type"]

varDefn :: TypeDefinition
varDefn = def "VarDefn" $
  T.record [
    "mods">: T.list $ meta "Mod",
    "pats">: T.list $ meta "Pat",
    "decltpe">: meta "Type",
    "rhs">: T.optional $ meta "Data"]

varPat :: TypeDefinition
varPat = def "VarPat" $
  T.record [
    "name">: meta "NameData"]

varType :: TypeDefinition
varType = def "VarType" $
  T.record [
    "name">: meta "NameType"]

whileData :: TypeDefinition
whileData = def "WhileData" $
  T.record [
    "expr">: meta "Data",
    "body">: meta "Data"]

withType :: TypeDefinition
withType = def "WithType" $
  T.record [
    "lhs">: meta "Type",
    "rhs">: meta "Type"]
