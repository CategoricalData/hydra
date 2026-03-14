module Hydra.Ext.Sources.Scala.Utils where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.Grammars                        as Grammars
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Meta.Accessors                  as Accessors
import qualified Hydra.Dsl.Meta.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Meta.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Compute                    as Compute
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Grammar                    as Grammar
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Meta.Json                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Meta.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Meta.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Meta.Typing                     as Typing
import qualified Hydra.Dsl.Meta.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt.Simple   as AdaptSimple
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars       as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta      as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan         as Tarjan
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import qualified Hydra.Ext.Scala.Meta as Scala
import qualified Hydra.Ext.Sources.Scala.Meta as ScalaMeta
import qualified Hydra.Ext.Sources.Scala.Language as ScalaLanguageSource


def :: String -> TTerm a -> TBinding a
def = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.scala.utils"

scalaLanguageNs :: Namespace
scalaLanguageNs = moduleNamespace ScalaLanguageSource.scalaLanguageModule

module_ :: Module
module_ = Module ns elements
    [scalaLanguageNs, Names.ns, Formatting.ns]
    (ScalaMeta.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Utility functions for constructing Scala AST nodes"
  where
    elements = [
      toBinding nameOfType,
      toBinding qualifyUnionFieldName,
      toBinding scalaTypeName,
      toBinding sapply,
      toBinding sassign,
      toBinding slambda,
      toBinding sname,
      toBinding sprim,
      toBinding stapply,
      toBinding stapply1,
      toBinding stapply2,
      toBinding stparam,
      toBinding stref,
      toBinding svar,
      toBinding scalaReservedWordsRef]


nameOfType :: TBinding (Graph -> Type -> Y.Maybe Name)
nameOfType = def "nameOfType" $
  doc "Extract the name from a type, if it is a named type" $
  lambda "cx" $ lambda "t" $
    (cases _Type (Rewriting.deannotateType @@ var "t")
      (Just nothing) [
      _Type_variable>>: ("name" ~> just (var "name")),
      _Type_forall>>: ("ft" ~> nameOfType @@ var "cx" @@ (project _ForallType _ForallType_body @@ var "ft"))])

qualifyUnionFieldName :: TBinding (String -> Y.Maybe Name -> Name -> String)
qualifyUnionFieldName = def "qualifyUnionFieldName" $
  doc "Qualify a union field name, optionally prefixing with the Scala type name" $
  lambda "dlft" $ lambda "sname" $ lambda "fname" $
    Maybes.maybe
      (var "dlft")
      ("n" ~> (scalaTypeName @@ true @@ var "n") ++ string ".")
      (var "sname")
    ++ (Core.unName $ var "fname")

scalaTypeName :: TBinding (Bool -> Name -> String)
scalaTypeName = def "scalaTypeName" $
  doc "Convert a Hydra name to a Scala type name" $
  lambda "qualify" $ lambda "name" $
    Logic.ifElse
      (Logic.or (var "qualify") (Sets.member (Names.localNameOf @@ var "name") (asTerm scalaReservedWordsRef)))
      (Core.unName $ var "name")
      (Names.localNameOf @@ var "name")

scalaReservedWordsRef :: TBinding (S.Set String)
scalaReservedWordsRef = def "scalaReservedWords" $
  doc "Reference to scalaReservedWords from the language module" $
  TTerm $ TermVariable $ Name "hydra.ext.scala.language.scalaReservedWords"

sapply :: TBinding (Scala.Data -> [Scala.Data] -> Scala.Data)
sapply = def "sapply" $
  doc "Apply a Scala data expression to a list of arguments" $
  lambda "fun" $ lambda "args" $
    inject _Data _Data_apply (
      record _Data_Apply [
        _Data_Apply_fun>>: var "fun",
        _Data_Apply_args>>: var "args"])

sassign :: TBinding (Scala.Data -> Scala.Data -> Scala.Data)
sassign = def "sassign" $
  doc "Create a Scala assignment expression" $
  lambda "lhs" $ lambda "rhs" $
    inject _Data _Data_assign (
      record _Data_Assign [
        _Data_Assign_lhs>>: var "lhs",
        _Data_Assign_rhs>>: var "rhs"])

slambda :: TBinding (String -> Scala.Data -> Y.Maybe Scala.Type -> Scala.Data)
slambda = def "slambda" $
  doc "Create a Scala lambda (function) expression" $
  lambda "v" $ lambda "body" $ lambda "sdom" $
    inject _Data _Data_functionData (
      inject _Data_FunctionData _Data_FunctionData_function (
        record _Data_Function [
          _Data_Function_params>>: list [
            record _Data_Param [
              _Data_Param_mods>>: list ([] :: [TTerm Scala.Mod]),
              _Data_Param_name>>: inject Scala._Name Scala._Name_value (var "v"),
              _Data_Param_decltpe>>: var "sdom",
              _Data_Param_default>>: nothing]],
          _Data_Function_body>>: var "body"]))

sname :: TBinding (String -> Scala.Data)
sname = def "sname" $
  doc "Create a Scala name reference" $
  lambda "s" $
    inject _Data _Data_ref (
      inject _Data_Ref _Data_Ref_name (
        record _Data_Name [
          _Data_Name_value>>: wrap _PredefString (var "s")]))

sprim :: TBinding (Name -> Scala.Data)
sprim = def "sprim" $
  doc "Create a Scala primitive reference from a Hydra name" $
  lambda "name" $ lets [
    "qname">: Names.qualifyName @@ var "name",
    "prefix">: Lists.last (Strings.splitOn (string ".") (Core.unNamespace (Maybes.fromJust (Module.qualifiedNameNamespace $ var "qname")))),
    "local">: Module.qualifiedNameLocal $ var "qname"] $
    sname @@ (var "prefix" ++ string "." ++ var "local")

stapply :: TBinding (Scala.Type -> [Scala.Type] -> Scala.Type)
stapply = def "stapply" $
  doc "Apply a Scala type to a list of type arguments" $
  lambda "t" $ lambda "args" $
    inject Scala._Type Scala._Type_apply (
      record _Type_Apply [
        _Type_Apply_tpe>>: var "t",
        _Type_Apply_args>>: var "args"])

stapply1 :: TBinding (Scala.Type -> Scala.Type -> Scala.Type)
stapply1 = def "stapply1" $
  doc "Apply a Scala type to one type argument" $
  lambda "t1" $ lambda "t2" $
    stapply @@ var "t1" @@ list [var "t2"]

stapply2 :: TBinding (Scala.Type -> Scala.Type -> Scala.Type -> Scala.Type)
stapply2 = def "stapply2" $
  doc "Apply a Scala type to two type arguments" $
  lambda "t1" $ lambda "t2" $ lambda "t3" $
    stapply @@ var "t1" @@ list [var "t2", var "t3"]

stparam :: TBinding (Name -> Scala.Type_Param)
stparam = def "stparam" $
  doc "Create a Scala type parameter from a Hydra name" $
  lambda "name" $ lets [
    "v">: Core.unName $ var "name"] $
    record _Type_Param [
      _Type_Param_mods>>: list ([] :: [TTerm Scala.Mod]),
      _Type_Param_name>>: inject Scala._Name Scala._Name_value (var "v"),
      _Type_Param_tparams>>: list ([] :: [TTerm Scala.Type_Param]),
      _Type_Param_tbounds>>: list ([] :: [TTerm Scala.TypeBounds]),
      _Type_Param_vbounds>>: list ([] :: [TTerm Scala.Type]),
      _Type_Param_cbounds>>: list ([] :: [TTerm Scala.Type])]

stref :: TBinding (String -> Scala.Type)
stref = def "stref" $
  doc "Create a Scala type reference by name" $
  lambda "s" $
    inject Scala._Type Scala._Type_ref (
      inject _Type_Ref _Type_Ref_name (
        record _Type_Name [
          _Type_Name_value>>: var "s"]))

svar :: TBinding (Name -> Scala.Pat)
svar = def "svar" $
  doc "Create a Scala pattern variable" $
  lambda "name" $ lets [
    "v">: Core.unName $ var "name"] $
    inject _Pat _Pat_var (
      record _Pat_Var [
        _Pat_Var_name>>: record _Data_Name [
          _Data_Name_value>>: wrap _PredefString (var "v")]])


-- Scala Meta type/constructor name references

_Data = Scala._Data
_Data_Apply = Scala._Data_Apply
_Data_Apply_fun = Name "fun"
_Data_Apply_args = Name "args"
_Data_Assign = Scala._Data_Assign
_Data_Assign_lhs = Name "lhs"
_Data_Assign_rhs = Name "rhs"
_Data_FunctionData = Scala._Data_FunctionData
_Data_FunctionData_function = Name "function"
_Data_Function = Scala._Data_Function
_Data_Function_params = Name "params"
_Data_Function_body = Name "body"
_Data_Match = Scala._Data_Match
_Data_Name = Scala._Data_Name
_Data_Name_value = Name "value"
_Data_Param = Scala._Data_Param
_Data_Param_mods = Name "mods"
_Data_Param_name = Name "name"
_Data_Param_decltpe = Name "decltpe"
_Data_Param_default = Name "default"
_Data_Ref = Scala._Data_Ref
_Data_Ref_name = Name "name"

_Data_apply = Scala._Data_apply
_Data_assign = Scala._Data_assign
_Data_functionData = Scala._Data_functionData
_Data_ref = Scala._Data_ref
_Data_lit = Scala._Data_lit
_Data_match = Scala._Data_match

-- Note: _ForallType and _ForallType_body come from Hydra.Kernel (Hydra.Core)

-- Note: _Name and _Name_value from Scala.Meta are used via the Scala. prefix
-- to avoid ambiguity with Hydra.Kernel._Name

-- Note: Scala._Type is used via the Scala. prefix to avoid ambiguity with Hydra.Kernel._Type
_Type_Apply = Scala._Type_Apply
_Type_Apply_tpe = Name "tpe"
_Type_Apply_args = Name "args"
_Type_FunctionType = Scala._Type_FunctionType
_Type_Function = Scala._Type_Function
_Type_Function_params = Name "params"
_Type_Function_res = Name "res"
_Type_Lambda = Scala._Type_Lambda
_Type_Lambda_tparams = Name "tparams"
_Type_Lambda_tpe = Name "tpe"
_Type_Name = Scala._Type_Name
_Type_Name_value = Name "value"
_Type_Param = Scala._Type_Param
_Type_Param_mods = Name "mods"
_Type_Param_name = Name "name"
_Type_Param_tparams = Name "tparams"
_Type_Param_tbounds = Name "tbounds"
_Type_Param_vbounds = Name "vbounds"
_Type_Param_cbounds = Name "cbounds"
_Type_Ref = Scala._Type_Ref
_Type_Ref_name = Name "name"
_Type_Var = Scala._Type_Var
_Type_Var_name = Name "name"

_Type_apply = Scala._Type_apply
_Type_functionType = Scala._Type_functionType
_Type_lambda = Scala._Type_lambda
_Type_ref = Scala._Type_ref
_Type_var = Scala._Type_var

_Pat = Scala._Pat
_Pat_Var = Scala._Pat_Var
_Pat_Var_name = Name "name"
_Pat_Extract = Scala._Pat_Extract
_Pat_Extract_fun = Name "fun"
_Pat_Extract_args = Name "args"

_Pat_var = Scala._Pat_var
_Pat_extract = Name "extract"

_PredefString = Scala._PredefString

_Lit = Scala._Lit
_Lit_boolean = Scala._Lit_boolean
_Lit_float = Scala._Lit_float
_Lit_double = Scala._Lit_double
_Lit_short = Scala._Lit_short
_Lit_int = Scala._Lit_int
_Lit_long = Scala._Lit_long
_Lit_byte = Scala._Lit_byte
_Lit_string = Scala._Lit_string

_Stat = Scala._Stat
_Stat_defn = Scala._Stat_defn
_Stat_importExport = Scala._Stat_importExport

_Defn = Scala._Defn
_Defn_Class = Scala._Defn_Class
_Defn_Def = Scala._Defn_Def
_Defn_Enum = Scala._Defn_Enum
_Defn_EnumCase = Scala._Defn_EnumCase
_Defn_Type = Scala._Defn_Type
_Defn_Val = Scala._Defn_Val

_Ctor_Primary = Scala._Ctor_Primary
_Template = Scala._Template
_Self = Scala._Self
_Mod = Scala._Mod
_Mod_case = Name "case"
_Pkg = Scala._Pkg
_Import = Scala._Import
_Importer = Scala._Importer
_Importee = Scala._Importee
_ImportExportStat = Scala._ImportExportStat
_Init = Scala._Init
_Case = Scala._Case
