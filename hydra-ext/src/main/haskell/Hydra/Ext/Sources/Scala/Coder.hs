-- | Scala code generator in Hydra DSL.
-- This module provides DSL versions of Scala code generation functions.

module Hydra.Ext.Sources.Scala.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Meta.Error                      as Error
import qualified Hydra.Dsl.Meta.Module                     as Module
import qualified Hydra.Dsl.Meta.Util                       as Util
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Meta.Typing                     as Typing
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import qualified Hydra.Sources.CoderUtils                  as CoderUtils
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Kernel.Terms.Arity           as Arity
import qualified Hydra.Sources.Kernel.Terms.Serialization  as SerializationSource
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import           Prelude hiding ((++))

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Coerce (coerce)

-- Additional imports
import qualified Hydra.Ext.Scala.Meta as Scala
import qualified Hydra.Ext.Sources.Scala.Meta as ScalaMeta
import qualified Hydra.Ext.Sources.Scala.Language as ScalaLanguageSource
import qualified Hydra.Ext.Sources.Scala.Utils as ScalaUtilsSource
import qualified Hydra.Ext.Sources.Scala.Prepare as ScalaPrepareSource
import qualified Hydra.Ext.Sources.Scala.Serde as ScalaSerdeSource


def :: String -> TTerm a -> TBinding a
def = definitionInModule module_

-- | An empty list term, avoiding ambiguous type variable issues with 'emptyList'
emptyList :: TTerm [a]
emptyList = TTerm $ TermList []


ns :: Namespace
ns = Namespace "hydra.ext.scala.coder"

module_ :: Module
module_ = Module ns elements
    [ScalaUtilsSource.ns, ScalaPrepareSource.ns, ScalaSerdeSource.ns, Formatting.ns, Names.ns, Rewriting.ns, CoderUtils.ns, Schemas.ns, ShowCore.ns, Annotations.ns, Constants.ns,
      Inference.ns, Sorting.ns, Arity.ns, SerializationSource.ns, Reduction.ns]
    (ScalaMeta.ns:moduleNamespace ScalaLanguageSource.scalaLanguageModule:KernelTypes.kernelTypesNamespaces) $
    Just "Scala code generator: converts Hydra modules to Scala source code"
  where
    elements = [
      toBinding moduleToScala,
      toBinding constructModule,
      toBinding findImports,
      toBinding toElImport,
      toBinding toPrimImport,
      toBinding encodeTypeDefinition,
      toBinding encodeFunction,
      toBinding encodeLiteral,
      toBinding encodeTerm,
      toBinding encodeType,
      toBinding encodeUntypeApplicationTerm,
      toBinding fieldToParam,
      toBinding fieldToEnumCase,
      toBinding typeParamToTypeVar,
      toBinding encodeTermDefinition,
      toBinding findSdom,
      toBinding findDomain,
      toBinding encodeCase,
      toBinding applyVar]


-- | Type alias for Result
-- type Result a = Either (InContext Error) a

-- | Get a type annotation, converting DecodingError to InContext Error.
getTypeE :: TTerm Context -> TTerm Graph -> TTerm (M.Map Name Term) -> TTerm (Either (InContext Error) (Maybe Type))
getTypeE cx g ann = Eithers.bimap
  ("__de" ~> Ctx.inContext (Error.errorOther $ Error.otherError (Error.unDecodingError @@ var "__de")) cx)
  ("__a" ~> var "__a")
  (Annotations.getType @@ g @@ ann)


moduleToScala :: TBinding (Module -> [Definition] -> Context -> Graph -> Either (InContext Error) (M.Map FilePath String))
moduleToScala = def "moduleToScala" $
  doc "Convert a Hydra module to Scala source code" $
  lambda "mod" $ lambda "defs" $ lambda "cx" $ lambda "g" $
    Eithers.bind
      (asTerm constructModule @@ var "cx" @@ var "g" @@ var "mod" @@ var "defs")
      ("pkg" ~> lets [
        "s">: SerializationSource.printExpr @@ (SerializationSource.parenthesize @@ (TTerm (TermVariable (Name "hydra.ext.scala.serde.writePkg")) @@ var "pkg"))] $
        right (Maps.singleton
          (Names.namespaceToFilePath @@ Util.caseConventionCamel @@ wrap _FileExtension (string "scala") @@ Module.moduleNamespace (var "mod"))
          (var "s")))

constructModule :: TBinding (Context -> Graph -> Module -> [Definition] -> Either (InContext Error) Scala.Pkg)
constructModule = def "constructModule" $
  doc "Construct a Scala package from a Hydra module and its definitions" $
  lambda "cx" $ lambda "g" $ lambda "mod" $ lambda "defs" $ lets [
    "partitioned">: Schemas.partitionDefinitions @@ var "defs",
    "typeDefs">: Pairs.first (var "partitioned"),
    "termDefs">: Pairs.second (var "partitioned"),
    "nsName">: Core.unNamespace (Module.moduleNamespace (var "mod")),
    "pname">: toScalaName (var "nsName"),
    "pref">: inject _Data_Ref _Data_Ref_name (var "pname")] $
    Eithers.bind
      (Eithers.mapList ("td" ~> asTerm encodeTypeDefinition @@ var "cx" @@ var "g" @@ var "td") (var "typeDefs"))
      ("typeDeclStats" ~>
        Eithers.bind
          (Eithers.mapList ("td" ~> asTerm encodeTermDefinition @@ var "cx" @@ var "g" @@ var "td") (var "termDefs"))
          ("termDeclStats" ~>
            Eithers.bind
              (asTerm findImports @@ var "cx" @@ var "g" @@ var "mod")
              ("imports" ~>
                right (record _Pkg [
                  _Pkg_name>>: var "pname",
                  _Pkg_ref>>: var "pref",
                  _Pkg_stats>>: Lists.concat (list [var "imports", var "typeDeclStats", var "termDeclStats"])]))))
  where
    toScalaName n = record _Data_Name [
      _Data_Name_value>>: wrap _PredefString (Strings.intercalate (string ".") (Strings.splitOn (string ".") n))]

findImports :: TBinding (Context -> Graph -> Module -> Either (InContext Error) [Scala.Stat])
findImports = def "findImports" $
  doc "Find import statements for the module" $
  lambda "cx" $ lambda "g" $ lambda "mod" $
    Eithers.bind
      (Schemas.moduleDependencyNamespaces @@ var "cx" @@ var "g" @@ false @@ false @@ true @@ false @@ var "mod")
      ("elImps" ~>
        Eithers.bind
          (Schemas.moduleDependencyNamespaces @@ var "cx" @@ var "g" @@ false @@ true @@ false @@ false @@ var "mod")
          ("primImps" ~>
            right (Lists.concat (list [
              Lists.map (asTerm toElImport) (Sets.toList (var "elImps")),
              Lists.map (asTerm toPrimImport) (Sets.toList (var "primImps"))]))))

toElImport :: TBinding (Namespace -> Scala.Stat)
toElImport = def "toElImport" $
  doc "Create an element import statement" $
  lambda "ns" $
    inject _Stat _Stat_importExport (
      inject _ImportExportStat _ImportExportStat_import (
        record _Import [
          _Import_importers>>: list [
            record _Importer [
              _Importer_ref>>: inject _Data_Ref _Data_Ref_name (
                record _Data_Name [
                  _Data_Name_value>>: wrap _PredefString (
                    Strings.intercalate (string ".") (Strings.splitOn (string ".") (Core.unNamespace (var "ns"))))]),
              _Importer_importees>>: list [inject _Importee _Importee_wildcard unit]]]]))

toPrimImport :: TBinding (Namespace -> Scala.Stat)
toPrimImport = def "toPrimImport" $
  doc "Create a primitive import statement" $
  lambda "ns" $
    inject _Stat _Stat_importExport (
      inject _ImportExportStat _ImportExportStat_import (
        record _Import [
          _Import_importers>>: list [
            record _Importer [
              _Importer_ref>>: inject _Data_Ref _Data_Ref_name (
                record _Data_Name [
                  _Data_Name_value>>: wrap _PredefString (
                    Strings.intercalate (string ".") (Strings.splitOn (string ".") (Core.unNamespace (var "ns"))))]),
              _Importer_importees>>: emptyList]]]))

encodeTypeDefinition :: TBinding (Context -> Graph -> TypeDefinition -> Either (InContext Error) Scala.Stat)
encodeTypeDefinition = def "encodeTypeDefinition" $
  doc "Encode a type definition as a Scala statement" $
  lambda "cx" $ lambda "g" $ lambda "td" $ lets [
    "name">: project _TypeDefinition _TypeDefinition_name @@ var "td",
    "typ">: project _TypeDefinition _TypeDefinition_type @@ var "td",
    "lname">: Names.localNameOf @@ var "name",
    "tname">: record _Type_Name [_Type_Name_value>>: var "lname"],
    "dname">: record _Data_Name [_Data_Name_value>>: wrap _PredefString (var "lname")],
    "freeVars">: Lists.filter ("v" ~> Logic.not (Lists.elem (int32 46) (Strings.toList (Core.unName (var "v"))))) (Sets.toList (Rewriting.freeVariablesInType @@ var "typ")),
    "tparams">: Lists.map ("__v" ~> stparam (var "__v")) (var "freeVars")] $
    (cases _Type (Rewriting.deannotateType @@ var "typ") (Just $ defaultTypeCase (var "lname") (var "tparams") (var "cx") (var "g") (var "typ")) [
      _Type_record>>: ("rt" ~>
        Eithers.bind
          (Eithers.mapList ("f" ~> asTerm fieldToParam @@ var "cx" @@ var "g" @@ var "f") (var "rt"))
          ("params" ~>
            right (inject _Stat _Stat_defn (inject _Defn _Defn_class (
              record _Defn_Class [
                _Defn_Class_mods>>: list [inject _Mod _Mod_case unit],
                _Defn_Class_name>>: var "tname",
                _Defn_Class_tparams>>: var "tparams",
                _Defn_Class_ctor>>: record _Ctor_Primary [
                  _Ctor_Primary_mods>>: emptyList,
                  _Ctor_Primary_name>>: inject Scala._Name _Name_value (string ""),
                  _Ctor_Primary_paramss>>: list [var "params"]],
                _Defn_Class_template>>: emptyTemplate]))))),
      _Type_union>>: ("rt" ~>
        Eithers.bind
          (Eithers.mapList ("f" ~> asTerm fieldToEnumCase @@ var "cx" @@ var "g" @@ var "lname" @@ var "tparams" @@ var "f") (var "rt"))
          ("cases" ~>
            right (inject _Stat _Stat_defn (inject _Defn _Defn_enum (
              record _Defn_Enum [
                _Defn_Enum_mods>>: emptyList,
                _Defn_Enum_name>>: var "tname",
                _Defn_Enum_tparams>>: var "tparams",
                _Defn_Enum_ctor>>: record _Ctor_Primary [
                  _Ctor_Primary_mods>>: emptyList,
                  _Ctor_Primary_name>>: inject Scala._Name _Name_value (string ""),
                  _Ctor_Primary_paramss>>: emptyList],
                _Defn_Enum_template>>: record _Template [
                  _Template_early>>: emptyList,
                  _Template_inits>>: emptyList,
                  _Template_self>>: wrap _Self unit,
                  _Template_stats>>: var "cases"]]))))),
      _Type_wrap>>: ("wt" ~>
        Eithers.bind
          (asTerm encodeType @@ var "cx" @@ var "g" @@ var "wt")
          ("styp" ~>
            right (inject _Stat _Stat_defn (inject _Defn _Defn_type (
              record _Defn_Type [
                _Defn_Type_mods>>: emptyList,
                _Defn_Type_name>>: var "tname",
                _Defn_Type_tparams>>: var "tparams",
                _Defn_Type_body>>: var "styp"])))))])
  where
    defaultTypeCase :: TTerm String -> TTerm [Scala.Type_Param] -> TTerm Context -> TTerm Graph -> TTerm Type -> TTerm (Either (InContext Error) Scala.Stat)
    defaultTypeCase lname tparams cx g typ =
      Eithers.bind
        (asTerm encodeType @@ cx @@ g @@ typ)
        ("styp" ~>
          right (inject _Stat _Stat_defn (inject _Defn _Defn_type (
            record _Defn_Type [
              _Defn_Type_mods>>: emptyList,
              _Defn_Type_name>>: record _Type_Name [_Type_Name_value>>: lname],
              _Defn_Type_tparams>>: tparams,
              _Defn_Type_body>>: var "styp"]))))

    emptyTemplate = record _Template [
      _Template_early>>: emptyList,
      _Template_inits>>: emptyList,
      _Template_self>>: wrap _Self unit,
      _Template_stats>>: emptyList]

    stparam v = lets [
      "vn">: Core.unName v] $
      record _Type_Param [
        _Type_Param_mods>>: emptyList,
        _Type_Param_name>>: inject Scala._Name _Name_value (var "vn"),
        _Type_Param_tparams>>: emptyList,
        _Type_Param_tbounds>>: emptyList,
        _Type_Param_vbounds>>: emptyList,
        _Type_Param_cbounds>>: emptyList]

fieldToParam :: TBinding (Context -> Graph -> FieldType -> Either (InContext Error) Scala.Data_Param)
fieldToParam = def "fieldToParam" $
  doc "Convert a field type to a Scala parameter" $
  lambda "cx" $ lambda "g" $ lambda "ft" $ lets [
    "fname">: Core.unName (project _FieldType _FieldType_name @@ var "ft"),
    "ftyp">: project _FieldType _FieldType_type @@ var "ft"] $
    Eithers.bind
      (asTerm encodeType @@ var "cx" @@ var "g" @@ var "ftyp")
      ("sftyp" ~>
        right (record _Data_Param [
          _Data_Param_mods>>: emptyList,
          _Data_Param_name>>: inject Scala._Name _Name_value (var "fname"),
          _Data_Param_decltpe>>: just (var "sftyp"),
          _Data_Param_default>>: nothing]))

fieldToEnumCase :: TBinding (Context -> Graph -> String -> [Scala.Type_Param] -> FieldType -> Either (InContext Error) Scala.Stat)
fieldToEnumCase = def "fieldToEnumCase" $
  doc "Convert a field type to a Scala enum case" $
  lambda "cx" $ lambda "g" $ lambda "parentName" $ lambda "tparams" $ lambda "ft" $ lets [
    "fname">: Core.unName (project _FieldType _FieldType_name @@ var "ft"),
    "ftyp">: project _FieldType _FieldType_type @@ var "ft",
    "caseName">: record _Data_Name [_Data_Name_value>>: wrap _PredefString (var "fname")],
    "isUnit">: cases _Type (Rewriting.deannotateType @@ var "ftyp") (Just false) [
      _Type_unit>>: (constant true),
      _Type_record>>: ("rt" ~> Equality.equal (Lists.length (var "rt")) (int32 0))],
    "parentType">: Logic.ifElse (Lists.null (var "tparams"))
      (stref (var "parentName"))
      (inject Scala._Type _Type_apply (record _Type_Apply [
        _Type_Apply_tpe>>: stref (var "parentName"),
        _Type_Apply_args>>: Lists.map (asTerm typeParamToTypeVar) (var "tparams")]))] $
    Eithers.bind
      (asTerm encodeType @@ var "cx" @@ var "g" @@ var "ftyp")
      ("sftyp" ~>
        right (inject _Stat _Stat_defn (inject _Defn _Defn_enumCase (
          record _Defn_EnumCase [
            _Defn_EnumCase_mods>>: emptyList,
            _Defn_EnumCase_name>>: var "caseName",
            _Defn_EnumCase_tparams>>: emptyList,
            _Defn_EnumCase_ctor>>: record _Ctor_Primary [
              _Ctor_Primary_mods>>: emptyList,
              _Ctor_Primary_name>>: inject Scala._Name _Name_value (string ""),
              _Ctor_Primary_paramss>>: list [
                Logic.ifElse (var "isUnit")
                  (emptyList)
                  (list [record _Data_Param [
                    _Data_Param_mods>>: emptyList,
                    _Data_Param_name>>: inject Scala._Name _Name_value (string "value"),
                    _Data_Param_decltpe>>: just (var "sftyp"),
                    _Data_Param_default>>: nothing]])]],
            _Defn_EnumCase_inits>>: list [record _Init [
              _Init_tpe>>: var "parentType",
              _Init_name>>: inject Scala._Name _Name_value (string ""),
              _Init_argss>>: emptyList]]]))))
  where
    stref s = inject Scala._Type _Type_ref (inject _Type_Ref _Type_Ref_name (record _Type_Name [_Type_Name_value>>: s]))

typeParamToTypeVar :: TBinding (Scala.Type_Param -> Scala.Type)
typeParamToTypeVar = def "typeParamToTypeVar" $
  doc "Convert a type parameter to a type variable reference" $
  lambda "tp" $ lets [
    "n">: project _Type_Param _Type_Param_name @@ var "tp",
    "s">: cases Scala._Name (var "n") (Just $ string "") [
      Scala._Name_value>>: ("v" ~> var "v")]] $
    inject Scala._Type _Type_var (record _Type_Var [
      _Type_Var_name>>: record _Type_Name [_Type_Name_value>>: var "s"]])

encodeTermDefinition :: TBinding (Context -> Graph -> TermDefinition -> Either (InContext Error) Scala.Stat)
encodeTermDefinition = def "encodeTermDefinition" $
  doc "Encode a term definition as a Scala statement" $
  lambda "cx" $ lambda "g" $ lambda "td" $ lets [
    "name">: project _TermDefinition _TermDefinition_name @@ var "td",
    "term">: project _TermDefinition _TermDefinition_term @@ var "td",
    "typ">: project _TermDefinition _TermDefinition_type @@ var "td",
    "lname">: Names.localNameOf @@ var "name",
    "typ'">: Core.typeSchemeType $ var "typ",
    "toVal">: "ln" ~> "r" ~>
      inject _Defn _Defn_val (record _Defn_Val [
        _Defn_Val_mods>>: emptyList,
        _Defn_Val_pats>>: list [inject _Pat _Pat_var (record _Pat_Var [
          _Pat_Var_name>>: record _Data_Name [_Data_Name_value>>: wrap _PredefString (var "ln")]])],
        _Defn_Val_decltpe>>: nothing,
        _Defn_Val_rhs>>: var "r"])] $
    Eithers.bind
      (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "term")
      ("rhs" ~>
        -- If the RHS is function data and the type is a function type, use def; otherwise use val
        cases _Data (var "rhs") (Just $ right (inject _Stat _Stat_defn (var "toVal" @@ var "lname" @@ var "rhs"))) [
          _Data_functionData>>: "fun" ~>
            cases _Type (Rewriting.deannotateType @@ var "typ'") (Just $ right (inject _Stat _Stat_defn (var "toVal" @@ var "lname" @@ var "rhs"))) [
              _Type_function>>: "ft" ~> lets [
                "cod">: Core.functionTypeCodomain $ var "ft",
                "freeTypeVars">: Sets.toList (Rewriting.freeVariablesInType @@ var "typ'"),
                "tparams">: Lists.map (lambda "tv" $ ScalaUtilsSource.stparam @@ var "tv") (var "freeTypeVars")] $
                Eithers.bind (asTerm encodeType @@ var "cx" @@ var "g" @@ var "cod")
                  ("scod" ~>
                    cases _Data_FunctionData (var "fun") (Just $ right (inject _Stat _Stat_defn (var "toVal" @@ var "lname" @@ var "rhs"))) [
                      _Data_FunctionData_function>>: "f" ~> lets [
                        "params">: project _Data_Function _Data_Function_params @@ var "f",
                        "body">: project _Data_Function _Data_Function_body @@ var "f"] $
                        right (inject _Stat _Stat_defn (inject _Defn _Defn_def (record _Defn_Def [
                          _Defn_Def_mods>>: emptyList,
                          _Defn_Def_name>>: record _Data_Name [_Data_Name_value>>: wrap _PredefString (var "lname")],
                          _Defn_Def_tparams>>: var "tparams",
                          _Defn_Def_paramss>>: list [var "params"],
                          _Defn_Def_decltpe>>: just (var "scod"),
                          _Defn_Def_body>>: var "body"])))
                    ])
              ]])

encodeFunction :: TBinding (Context -> Graph -> M.Map Name Term -> Function -> Maybe Term -> Either (InContext Error) Scala.Data)
encodeFunction = def "encodeFunction" $
  doc "Encode a Hydra function as a Scala expression" $
  lambda "cx" $ lambda "g" $ lambda "meta" $ lambda "fun" $ lambda "arg" $
    (cases _Function (var "fun") (Just $ left (Ctx.inContext (Error.errorOther $ Error.otherError (string "unsupported function")) (var "cx"))) [
      _Function_lambda>>: ("lam" ~> lets [
        "v">: Core.unName (project _Lambda _Lambda_parameter @@ var "lam"),
        "body">: project _Lambda _Lambda_body @@ var "lam"] $
        Eithers.bind
          (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "body")
          ("sbody" ~>
            Eithers.bind
              (asTerm findSdom @@ var "cx" @@ var "g" @@ var "meta")
              ("sdom" ~>
                right (ScalaUtilsSource.slambda @@ var "v" @@ var "sbody" @@ var "sdom")))),
      _Function_primitive>>: ("name" ~> right (ScalaUtilsSource.sprim @@ var "name")),
      _Function_elimination>>: ("e" ~>
        cases _Elimination (var "e") (Just $ left (Ctx.inContext (Error.errorOther $ Error.otherError (string "unsupported elimination")) (var "cx"))) [
          _Elimination_wrap>>: ("name" ~> right (ScalaUtilsSource.sname @@ (string "ELIM-NOMINAL(" ++ Core.unName (var "name") ++ string ")"))),
          _Elimination_record>>: (constant $ left (Ctx.inContext (Error.errorOther $ Error.otherError (string "unapplied projection not yet supported")) (var "cx"))),
          _Elimination_union>>: ("cs" ~> lets [
            "v">: string "v"] $
            Eithers.bind
              (asTerm findDomain @@ var "cx" @@ var "g" @@ var "meta")
              ("dom" ~>
                Eithers.bind
                  (Schemas.fieldTypes @@ var "cx" @@ var "g" @@ var "dom")
                  ("ftypes" ~> lets [
                    "sn">: ScalaUtilsSource.nameOfType @@ var "g" @@ var "dom",
                    "cases">: project _CaseStatement _CaseStatement_cases @@ var "cs"] $
                    Eithers.bind
                      (Eithers.mapList ("f" ~> asTerm encodeCase @@ var "cx" @@ var "g" @@ var "ftypes" @@ var "sn" @@ var "f") (var "cases"))
                      ("scases" ~>
                        Maybes.maybe
                          (Eithers.bind
                            (asTerm findSdom @@ var "cx" @@ var "g" @@ var "meta")
                            ("sdom" ~>
                              right (ScalaUtilsSource.slambda @@ var "v" @@ (inject _Data _Data_match (record _Data_Match [
                                _Data_Match_expr>>: ScalaUtilsSource.sname @@ var "v",
                                _Data_Match_cases>>: var "scases"])) @@ var "sdom")))
                          ("a" ~>
                            Eithers.bind
                              (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "a")
                              ("sa" ~>
                                right (inject _Data _Data_match (record _Data_Match [
                                  _Data_Match_expr>>: var "sa",
                                  _Data_Match_cases>>: var "scases"]))))
                          (var "arg")))))])])

findSdom :: TBinding (Context -> Graph -> M.Map Name Term -> Either (InContext Error) (Maybe Scala.Type))
findSdom = def "findSdom" $
  doc "Find the Scala domain type for a function" $
  lambda "cx" $ lambda "g" $ lambda "meta" $
    Eithers.bind
      (asTerm findDomain @@ var "cx" @@ var "g" @@ var "meta")
      ("dom" ~>
        Eithers.bind
          (asTerm encodeType @@ var "cx" @@ var "g" @@ var "dom")
          ("sdom" ~> right (just (var "sdom"))))

findDomain :: TBinding (Context -> Graph -> M.Map Name Term -> Either (InContext Error) Type)
findDomain = def "findDomain" $
  doc "Find the domain type from annotations" $
  lambda "cx" $ lambda "g" $ lambda "meta" $
    Eithers.bind
      (getTypeE (var "cx") (var "g") (var "meta"))
      ("r" ~> Maybes.maybe
        (left (Ctx.inContext (Error.errorOther $ Error.otherError (string "expected a typed term")) (var "cx")))
        ("t" ~> cases _Type (Rewriting.deannotateType @@ var "t") (Just $ left (Ctx.inContext (Error.errorOther $ Error.otherError (string "expected a function type")) (var "cx"))) [
          _Type_function>>: ("ft" ~> right (project _FunctionType _FunctionType_domain @@ var "ft"))])
        (var "r"))

encodeCase :: TBinding (Context -> Graph -> M.Map Name Type -> Maybe Name -> Field -> Either (InContext Error) Scala.Case)
encodeCase = def "encodeCase" $
  doc "Encode a case branch" $
  lambda "cx" $ lambda "g" $ lambda "ftypes" $ lambda "sn" $ lambda "f" $ lets [
    "fname">: project _Field _Field_name @@ var "f",
    "fterm">: project _Field _Field_term @@ var "f",
    "dom">: Maybes.fromJust (Maps.lookup (var "fname") (var "ftypes")),
    "v">: Core.name (string "y"),
    "patArgs">: Logic.ifElse (Equality.equal (var "dom") (Core.typeUnit))
      (emptyList)
      (list [ScalaUtilsSource.svar @@ var "v"]),
    "pat">: inject _Pat _Pat_extract (record _Pat_Extract [
      _Pat_Extract_fun>>: ScalaUtilsSource.sname @@ (ScalaUtilsSource.qualifyUnionFieldName @@ string "MATCHED." @@ var "sn" @@ var "fname"),
      _Pat_Extract_args>>: var "patArgs"]),
    "applied">: asTerm applyVar @@ var "fterm" @@ var "v"] $
    Eithers.bind
      (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "applied")
      ("body" ~>
        right (record _Case [
          _Case_pat>>: var "pat",
          _Case_cond>>: nothing,
          _Case_body>>: var "body"]))

applyVar :: TBinding (Term -> Name -> Term)
applyVar = def "applyVar" $
  doc "Apply a variable to a term, performing substitution for lambdas" $
  lambda "fterm" $ lambda "avar" $ lets [
    "v">: Core.unName (var "avar")] $
    cases _Term (Rewriting.deannotateTerm @@ var "fterm") (Just $ Core.termApplication (record _Application [_Application_function>>: var "fterm", _Application_argument>>: Core.termVariable (var "avar")])) [
      _Term_function>>: ("f" ~> cases _Function (var "f")
        (Just $ Core.termApplication (record _Application [_Application_function>>: var "fterm", _Application_argument>>: Core.termVariable (var "avar")])) [
          _Function_lambda>>: ("lam" ~> lets [
            "lamParam">: project _Lambda _Lambda_parameter @@ var "lam",
            "lamBody">: project _Lambda _Lambda_body @@ var "lam"] $
            Logic.ifElse (Rewriting.isFreeVariableInTerm @@ var "lamParam" @@ var "lamBody")
              (var "lamBody")
              (Rewriting.substituteVariable @@ var "lamParam" @@ var "avar" @@ var "lamBody"))])]

encodeLiteral :: TBinding (Context -> Graph -> Literal -> Either (InContext Error) Scala.Lit)
encodeLiteral = def "encodeLiteral" $
  doc "Encode a literal value as a Scala literal" $
  lambda "cx" $ lambda "g" $ lambda "av" $
    (cases _Literal (var "av") (Just $ left (Ctx.inContext (Error.errorOther $ Error.otherError (string "unexpected literal")) (var "cx"))) [
      _Literal_boolean>>: ("b" ~> right (inject _Lit _Lit_boolean (var "b"))),
      _Literal_float>>: ("fv" ~> cases _FloatValue (var "fv") (Just $ left (Ctx.inContext (Error.errorOther $ Error.otherError (string "unexpected float value")) (var "cx"))) [
        _FloatValue_float32>>: ("f" ~> right (inject _Lit _Lit_float (var "f"))),
        _FloatValue_float64>>: ("f" ~> right (inject _Lit _Lit_double (var "f")))]),
      _Literal_integer>>: ("iv" ~> cases _IntegerValue (var "iv") (Just $ left (Ctx.inContext (Error.errorOther $ Error.otherError (string "unexpected integer value")) (var "cx"))) [
        _IntegerValue_int16>>: ("i" ~> right (inject _Lit _Lit_short (var "i"))),
        _IntegerValue_int32>>: ("i" ~> right (inject _Lit _Lit_int (var "i"))),
        _IntegerValue_int64>>: ("i" ~> right (inject _Lit _Lit_long (var "i"))),
        _IntegerValue_uint8>>: ("i" ~> right (inject _Lit _Lit_byte (Literals.bigintToInt8 (Literals.uint8ToBigint (var "i")))))]),
      _Literal_string>>: ("s" ~> right (inject _Lit _Lit_string (var "s")))])

encodeTerm :: TBinding (Context -> Graph -> Term -> Either (InContext Error) Scala.Data)
encodeTerm = def "encodeTerm" $
  doc "Encode a Hydra term as a Scala expression" $
  lambda "cx" $ lambda "g" $ lambda "term" $
    (cases _Term (Rewriting.deannotateTerm @@ var "term") (Just $ left (Ctx.inContext (Error.errorOther $ Error.otherError (string "unexpected term")) (var "cx"))) [
      _Term_application>>: ("app" ~> lets [
        "fun">: project _Application _Application_function @@ var "app",
        "arg">: project _Application _Application_argument @@ var "app"] $
        cases _Term (Rewriting.deannotateTerm @@ var "fun")
          (Just $ Eithers.bind
            (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "fun")
            ("sfun" ~>
              Eithers.bind
                (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "arg")
                ("sarg" ~>
                  right (ScalaUtilsSource.sapply @@ var "sfun" @@ list [var "sarg"])))) [
          _Term_function>>: ("f" ~> cases _Function (var "f")
            (Just $ Eithers.bind
              (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "fun")
              ("sfun" ~>
                Eithers.bind
                  (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "arg")
                  ("sarg" ~>
                    right (ScalaUtilsSource.sapply @@ var "sfun" @@ list [var "sarg"])))) [
              _Function_elimination>>: ("e" ~> cases _Elimination (var "e")
                (Just $ Eithers.bind
                  (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "fun")
                  ("sfun" ~>
                    Eithers.bind
                      (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "arg")
                      ("sarg" ~>
                        right (ScalaUtilsSource.sapply @@ var "sfun" @@ list [var "sarg"])))) [
                  _Elimination_record>>: ("proj" ~> lets [
                    "fname">: Core.unName (project _Projection _Projection_field @@ var "proj")] $
                    Eithers.bind
                      (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "arg")
                      ("sarg" ~>
                        right (inject _Data _Data_ref (inject _Data_Ref _Data_Ref_select (
                          record _Data_Select [
                            _Data_Select_qual>>: var "sarg",
                            _Data_Select_name>>: record _Data_Name [
                              _Data_Name_value>>: wrap _PredefString (var "fname")]]))))),
                  _Elimination_union>>: (constant $
                    asTerm encodeFunction @@ var "cx" @@ var "g" @@ (Annotations.termAnnotationInternal @@ var "fun") @@ var "f" @@ just (var "arg"))])])]),
      _Term_function>>: ("f" ~>
        asTerm encodeFunction @@ var "cx" @@ var "g" @@ (Annotations.termAnnotationInternal @@ var "term") @@ var "f" @@ nothing),
      _Term_list>>: ("els" ~>
        Eithers.bind
          (Eithers.mapList ("e" ~> asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "e") (var "els"))
          ("sels" ~>
            right (ScalaUtilsSource.sapply @@ (ScalaUtilsSource.sname @@ string "Seq") @@ var "sels"))),
      _Term_literal>>: ("v" ~>
        Eithers.bind
          (asTerm encodeLiteral @@ var "cx" @@ var "g" @@ var "v")
          ("slit" ~> right (inject _Data _Data_lit (var "slit")))),
      _Term_map>>: ("m" ~>
        Eithers.bind
          (Eithers.mapList ("kv" ~>
            Eithers.bind
              (asTerm encodeTerm @@ var "cx" @@ var "g" @@ (Pairs.first (var "kv")))
              ("sk" ~>
                Eithers.bind
                  (asTerm encodeTerm @@ var "cx" @@ var "g" @@ (Pairs.second (var "kv")))
                  ("sv" ~>
                    right (ScalaUtilsSource.sassign @@ var "sk" @@ var "sv"))))
            (Maps.toList (var "m")))
          ("spairs" ~>
            right (ScalaUtilsSource.sapply @@ (ScalaUtilsSource.sname @@ string "Map") @@ var "spairs"))),
      _Term_wrap>>: ("wt" ~>
        asTerm encodeTerm @@ var "cx" @@ var "g" @@ (project _WrappedTerm _WrappedTerm_body @@ var "wt")),
      _Term_maybe>>: ("m" ~>
        Maybes.maybe
          (right (ScalaUtilsSource.sname @@ string "None"))
          ("t" ~>
            Eithers.bind
              (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "t")
              ("s" ~> right (ScalaUtilsSource.sapply @@ (ScalaUtilsSource.sname @@ string "Some") @@ list [var "s"])))
          (var "m")),
      _Term_record>>: ("rec" ~> lets [
        "rname">: project _Record _Record_typeName @@ var "rec",
        "fields">: project _Record _Record_fields @@ var "rec",
        "n">: ScalaUtilsSource.scalaTypeName @@ false @@ var "rname"] $
        Eithers.bind
          (Eithers.mapList ("f" ~> asTerm encodeTerm @@ var "cx" @@ var "g" @@ (project _Field _Field_term @@ var "f")) (var "fields"))
          ("args" ~>
            right (ScalaUtilsSource.sapply @@ (ScalaUtilsSource.sname @@ var "n") @@ var "args"))),
      _Term_set>>: ("s" ~>
        Eithers.bind
          (Eithers.mapList ("e" ~> asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "e") (Sets.toList (var "s")))
          ("sels" ~>
            right (ScalaUtilsSource.sapply @@ (ScalaUtilsSource.sname @@ string "Set") @@ var "sels"))),
      _Term_union>>: ("inj" ~> lets [
        "sn">: project _Injection _Injection_typeName @@ var "inj",
        "fn">: project _Field _Field_name @@ (project _Injection _Injection_field @@ var "inj"),
        "ft">: project _Field _Field_term @@ (project _Injection _Injection_field @@ var "inj"),
        "lhs">: ScalaUtilsSource.sname @@ (ScalaUtilsSource.qualifyUnionFieldName @@ string "UNION." @@ just (var "sn") @@ var "fn")] $
        cases _Term (Rewriting.deannotateTerm @@ var "ft") (Just $
          Eithers.bind
            (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "ft")
            ("sarg" ~> right (ScalaUtilsSource.sapply @@ var "lhs" @@ list [var "sarg"]))) [
          _Term_record>>: ("rec" ~>
            Logic.ifElse (Equality.equal (Lists.length (project _Record _Record_fields @@ var "rec")) (int32 0))
              (right (ScalaUtilsSource.sapply @@ var "lhs" @@ emptyList))
              (Eithers.bind
                (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "ft")
                ("sarg" ~> right (ScalaUtilsSource.sapply @@ var "lhs" @@ list [var "sarg"]))))]),
      _Term_variable>>: ("v" ~> right (ScalaUtilsSource.sname @@ (Core.unName (var "v"))))])


encodeType :: TBinding (Context -> Graph -> Type -> Either (InContext Error) Scala.Type)
encodeType = def "encodeType" $
  doc "Encode a Hydra type as a Scala type" $
  lambda "cx" $ lambda "g" $ lambda "t" $
    (cases _Type (Rewriting.deannotateType @@ var "t") (Just $ left (Ctx.inContext (Error.errorOther $ Error.otherError (string "unsupported type")) (var "cx"))) [
      _Type_unit>>: (constant $ right (stref (string "Unit"))),
      _Type_either>>: ("et" ~> lets [
        "lt">: project _EitherType _EitherType_left @@ var "et",
        "rt">: project _EitherType _EitherType_right @@ var "et"] $
        Eithers.bind
          (asTerm encodeType @@ var "cx" @@ var "g" @@ var "lt")
          ("slt" ~>
            Eithers.bind
              (asTerm encodeType @@ var "cx" @@ var "g" @@ var "rt")
              ("srt" ~>
                right (ScalaUtilsSource.stapply2 @@ stref (string "Either") @@ var "slt" @@ var "srt")))),
      _Type_function>>: ("ft" ~> lets [
        "dom">: project _FunctionType _FunctionType_domain @@ var "ft",
        "cod">: project _FunctionType _FunctionType_codomain @@ var "ft"] $
        Eithers.bind
          (asTerm encodeType @@ var "cx" @@ var "g" @@ var "dom")
          ("sdom" ~>
            Eithers.bind
              (asTerm encodeType @@ var "cx" @@ var "g" @@ var "cod")
              ("scod" ~>
                right (inject Scala._Type _Type_functionType (inject _Type_FunctionType _Type_FunctionType_function (
                  record _Type_Function [
                    _Type_Function_params>>: list [var "sdom"],
                    _Type_Function_res>>: var "scod"])))))),
      _Type_list>>: ("lt" ~>
        Eithers.bind
          (asTerm encodeType @@ var "cx" @@ var "g" @@ var "lt")
          ("slt" ~>
            right (ScalaUtilsSource.stapply1 @@ stref (string "Seq") @@ var "slt"))),
      _Type_literal>>: ("lt" ~> cases _LiteralType (var "lt") (Just $ left (Ctx.inContext (Error.errorOther $ Error.otherError (string "unsupported literal type")) (var "cx"))) [
        _LiteralType_binary>>: (constant $ right (ScalaUtilsSource.stapply @@ stref (string "Array") @@ list [stref (string "Byte")])),
        _LiteralType_boolean>>: (constant $ right (stref (string "Boolean"))),
        _LiteralType_float>>: ("ft" ~> cases _FloatType (var "ft") (Just $ left (Ctx.inContext (Error.errorOther $ Error.otherError (string "unsupported float type")) (var "cx"))) [
          _FloatType_bigfloat>>: (constant $ right (stref (string "BigDecimal"))),
          _FloatType_float32>>: (constant $ right (stref (string "Float"))),
          _FloatType_float64>>: (constant $ right (stref (string "Double")))]),
        _LiteralType_integer>>: ("it" ~> cases _IntegerType (var "it") (Just $ left (Ctx.inContext (Error.errorOther $ Error.otherError (string "unsupported integer type")) (var "cx"))) [
          _IntegerType_bigint>>: (constant $ right (stref (string "BigInt"))),
          _IntegerType_int8>>: (constant $ right (stref (string "Byte"))),
          _IntegerType_int16>>: (constant $ right (stref (string "Short"))),
          _IntegerType_int32>>: (constant $ right (stref (string "Int"))),
          _IntegerType_int64>>: (constant $ right (stref (string "Long"))),
          _IntegerType_uint8>>: (constant $ right (stref (string "Byte"))),
          _IntegerType_uint16>>: (constant $ right (stref (string "Int"))),
          _IntegerType_uint32>>: (constant $ right (stref (string "Long"))),
          _IntegerType_uint64>>: (constant $ right (stref (string "BigInt")))]),
        _LiteralType_string>>: (constant $ right (stref (string "String")))]),
      _Type_map>>: ("mt" ~> lets [
        "kt">: project _MapType _MapType_keys @@ var "mt",
        "vt">: project _MapType _MapType_values @@ var "mt"] $
        Eithers.bind
          (asTerm encodeType @@ var "cx" @@ var "g" @@ var "kt")
          ("skt" ~>
            Eithers.bind
              (asTerm encodeType @@ var "cx" @@ var "g" @@ var "vt")
              ("svt" ~>
                right (ScalaUtilsSource.stapply2 @@ stref (string "Map") @@ var "skt" @@ var "svt")))),
      _Type_maybe>>: ("ot" ~>
        Eithers.bind
          (asTerm encodeType @@ var "cx" @@ var "g" @@ var "ot")
          ("sot" ~>
            right (ScalaUtilsSource.stapply1 @@ stref (string "Option") @@ var "sot"))),
      _Type_pair>>: ("pt" ~> lets [
        "ft">: project _PairType _PairType_first @@ var "pt",
        "st">: project _PairType _PairType_second @@ var "pt"] $
        Eithers.bind
          (asTerm encodeType @@ var "cx" @@ var "g" @@ var "ft")
          ("sft" ~>
            Eithers.bind
              (asTerm encodeType @@ var "cx" @@ var "g" @@ var "st")
              ("sst" ~>
                right (ScalaUtilsSource.stapply2 @@ stref (string "Tuple2") @@ var "sft" @@ var "sst")))),
      _Type_record>>: (constant $
        Ctx.failInContext (Error.errorOther $ Error.otherError (string "unexpected anonymous record type")) (var "cx")),
      _Type_set>>: ("st" ~>
        Eithers.bind
          (asTerm encodeType @@ var "cx" @@ var "g" @@ var "st")
          ("sst" ~>
            right (ScalaUtilsSource.stapply1 @@ stref (string "Set") @@ var "sst"))),
      _Type_union>>: (constant $
        Ctx.failInContext (Error.errorOther $ Error.otherError (string "unexpected anonymous union type")) (var "cx")),
      _Type_wrap>>: (constant $
        Ctx.failInContext (Error.errorOther $ Error.otherError (string "unexpected anonymous wrap type")) (var "cx")),
      _Type_forall>>: ("ft" ~> lets [
        "v">: project _ForallType _ForallType_parameter @@ var "ft",
        "body">: project _ForallType _ForallType_body @@ var "ft"] $
        Eithers.bind
          (asTerm encodeType @@ var "cx" @@ var "g" @@ var "body")
          ("sbody" ~>
            right (inject Scala._Type _Type_lambda (record _Type_Lambda [
              _Type_Lambda_tparams>>: list [ScalaUtilsSource.stparam @@ var "v"],
              _Type_Lambda_tpe>>: var "sbody"])))),
      _Type_variable>>: ("v" ~> right (inject Scala._Type _Type_var (record _Type_Var [
        _Type_Var_name>>: record _Type_Name [_Type_Name_value>>: Core.unName (var "v")]])))])
  where
    stref s = inject Scala._Type _Type_ref (inject _Type_Ref _Type_Ref_name (record _Type_Name [_Type_Name_value>>: s]))

encodeUntypeApplicationTerm :: TBinding (Context -> Graph -> Term -> Either (InContext Error) Scala.Data)
encodeUntypeApplicationTerm = def "encodeUntypeApplicationTerm" $
  doc "Encode an untyped application term by first inferring types" $
  lambda "cx" $ lambda "g" $ lambda "term" $
    Eithers.bind
      (Inference.inferInGraphContext @@ var "cx" @@ var "g" @@ var "term")
      ("result" ~>
        asTerm encodeTerm @@ var "cx" @@ var "g" @@ Typing.inferenceResultTerm (var "result"))


-- Name references used by Coder

-- Scala Meta names (re-exported from Utils for convenience)
_Data = Scala._Data
_Data_Apply = Scala._Data_Apply
_Data_Assign = Scala._Data_Assign
_Data_FunctionData = Scala._Data_FunctionData
_Data_FunctionData_function = Scala._Data_FunctionData_function
_Data_Function = Scala._Data_Function
_Data_Function_params = Name "params"
_Data_Function_body = Name "body"
_Data_Match = Scala._Data_Match
_Data_Match_expr = Name "expr"
_Data_Match_cases = Name "cases"
_Data_Name = Scala._Data_Name
_Data_Name_value = Name "value"
_Data_Param = Scala._Data_Param
_Data_Param_mods = Name "mods"
_Data_Param_name = Name "name"
_Data_Param_decltpe = Name "decltpe"
_Data_Param_default = Name "default"
_Data_Ref = Scala._Data_Ref
_Data_Ref_name = Name "name"
_Data_Ref_select = Name "select"
_Data_Select = Scala._Data_Select
_Data_Select_qual = Name "qual"
_Data_Select_name = Name "name"

_Data_apply = Scala._Data_apply
_Data_assign = Scala._Data_assign
_Data_functionData = Scala._Data_functionData
_Data_ref = Scala._Data_ref
_Data_lit = Scala._Data_lit
_Data_match = Scala._Data_match

_Name_value = Scala._Name_value

_Type_Apply = Scala._Type_Apply
_Type_Apply_tpe = Name "tpe"
_Type_Apply_args = Name "args"
_Type_FunctionType = Scala._Type_FunctionType
_Type_FunctionType_function = Name "function"
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
_Defn_Class_mods = Name "mods"
_Defn_Class_name = Name "name"
_Defn_Class_tparams = Name "tparams"
_Defn_Class_ctor = Name "ctor"
_Defn_Class_template = Name "template"
_Defn_Def = Scala._Defn_Def
_Defn_Def_mods = Name "mods"
_Defn_Def_name = Name "name"
_Defn_Def_tparams = Name "tparams"
_Defn_Def_paramss = Name "paramss"
_Defn_Def_decltpe = Name "decltpe"
_Defn_Def_body = Name "body"
_Defn_Enum = Scala._Defn_Enum
_Defn_Enum_mods = Name "mods"
_Defn_Enum_name = Name "name"
_Defn_Enum_tparams = Name "tparams"
_Defn_Enum_ctor = Name "ctor"
_Defn_Enum_template = Name "template"
_Defn_EnumCase = Scala._Defn_EnumCase
_Defn_EnumCase_mods = Name "mods"
_Defn_EnumCase_name = Name "name"
_Defn_EnumCase_tparams = Name "tparams"
_Defn_EnumCase_ctor = Name "ctor"
_Defn_EnumCase_inits = Name "inits"
_Defn_Type = Scala._Defn_Type
_Defn_Type_mods = Name "mods"
_Defn_Type_name = Name "name"
_Defn_Type_tparams = Name "tparams"
_Defn_Type_body = Name "body"
_Defn_Val = Scala._Defn_Val
_Defn_Val_mods = Name "mods"
_Defn_Val_pats = Name "pats"
_Defn_Val_decltpe = Name "decltpe"
_Defn_Val_rhs = Name "rhs"

_Defn_class = Name "class"
_Defn_def = Name "def"
_Defn_enum = Name "enum"
_Defn_enumCase = Name "enumCase"
_Defn_type = Name "type"
_Defn_val = Name "val"

_Ctor_Primary = Scala._Ctor_Primary
_Ctor_Primary_mods = Name "mods"
_Ctor_Primary_name = Name "name"
_Ctor_Primary_paramss = Name "paramss"
_Template = Scala._Template
_Template_early = Name "early"
_Template_inits = Name "inits"
_Template_self = Name "self"
_Template_stats = Name "stats"
_Self = Scala._Self
_Mod = Scala._Mod
_Mod_case = Name "case"
_Pkg = Scala._Pkg
_Pkg_name = Name "name"
_Pkg_ref = Name "ref"
_Pkg_stats = Name "stats"
_Import = Scala._Import
_Import_importers = Name "importers"
_Importer = Scala._Importer
_Importer_ref = Name "ref"
_Importer_importees = Name "importees"
_Importee = Scala._Importee
_Importee_wildcard = Name "wildcard"
_ImportExportStat = Scala._ImportExportStat
_ImportExportStat_import = Name "import"
_Init = Scala._Init
_Init_tpe = Name "tpe"
_Init_name = Name "name"
_Init_argss = Name "argss"
_Case = Scala._Case
_Case_pat = Name "pat"
_Case_cond = Name "cond"
_Case_body = Name "body"


