
module Hydra.Sources.Haskell.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import qualified Hydra.Sources.Kernel.Terms.All             as KernelTerms
import qualified Hydra.Sources.Kernel.Types.All             as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals  as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules   as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Simple    as AdaptSimple
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms     as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils     as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.Annotations     as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity           as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking        as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Decode.Core               as DecodeCore
import qualified Hydra.Sources.Encode.Core                as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Core    as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util    as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting      as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars        as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference       as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages       as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical         as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals        as Literals
import qualified Hydra.Sources.Kernel.Terms.Monads          as Monads
import qualified Hydra.Sources.Kernel.Terms.Names           as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction       as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect         as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting       as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas         as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization   as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors  as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core       as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph      as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta       as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing     as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting         as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution    as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan          as Tarjan
import qualified Hydra.Sources.Kernel.Terms.Templates       as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification     as Unification
import           Prelude hiding ((++))
import qualified Data.Int                                   as I
import qualified Data.List                                  as L
import qualified Data.Map                                   as M
import qualified Data.Set                                   as S
import qualified Data.Maybe                                 as Y

-- Additional imports
import qualified Hydra.Ext.Haskell.Ast as H
import qualified Hydra.Sources.Haskell.Language as HaskellLanguage
import qualified Hydra.Sources.Haskell.Ast as HaskellAst
import qualified Hydra.Sources.Haskell.Serde as HaskellSerde
import qualified Hydra.Sources.Haskell.Utils as HaskellUtils


type HaskellNamespaces = Namespaces H.ModuleName

haskellCoderDefinition :: String -> TTerm a -> TBinding a
haskellCoderDefinition = definitionInModule module_

module_ :: Module
module_ = Module ns elements
    [HaskellSerde.ns, HaskellUtils.ns,
      AdaptModules.ns, Rewriting.ns, Serialization.ns]
    (HaskellAst.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Functions for encoding Hydra modules as Haskell modules"
  where
    ns = Namespace "hydra.ext.haskell.coder"
    elements = [
      toBinding includeTypeDefinitions,
      toBinding useCoreImport,
      toBinding keyHaskellVar,
      toBinding adaptTypeToHaskellAndEncode,
      toBinding constantForFieldName,
      toBinding constantForTypeName,
      toBinding constructModule,
      toBinding encodeFunction,
      toBinding encodeLiteral,
      toBinding encodeTerm,
      toBinding encodeType,
      toBinding encodeTypeWithClassAssertions,
      toBinding findOrdVariables,
      toBinding getImplicitTypeClasses,
      toBinding moduleToHaskellModule,
      toBinding moduleToHaskell,
      toBinding nameDecls,
      toBinding toDataDeclaration,
--      toBinding toTypeDeclarations,
      toBinding toTypeDeclarationsFrom,
      toBinding typeDecl]

-- TODO: make these settings configurable
includeTypeDefinitions :: TBinding Bool
includeTypeDefinitions = haskellCoderDefinition "includeTypeDefinitions" $
  false
useCoreImport :: TBinding Bool
useCoreImport = haskellCoderDefinition "useCoreImport" $
  true

keyHaskellVar :: TBinding Name
keyHaskellVar = haskellCoderDefinition "keyHaskellVar" $
  wrap _Name $ string "haskellVar"

adaptTypeToHaskellAndEncode :: TBinding (HaskellNamespaces -> Type -> Flow Graph H.Type)
adaptTypeToHaskellAndEncode = haskellCoderDefinition "adaptTypeToHaskellAndEncode" $
  "namespaces" ~>
    AdaptModules.adaptTypeToLanguageAndEncode @@ (HaskellLanguage.haskellLanguage) @@ (encodeType @@ var "namespaces")

constantForFieldName :: TBinding (Name -> Name -> String)
constantForFieldName = haskellCoderDefinition "constantForFieldName" $
  "tname" ~> "fname" ~>
    Strings.cat $ list [
      string "_",
      Names.localNameOf @@ var "tname",
      string "_",
      Core.unName $ var "fname"]

constantForTypeName :: TBinding (Name -> String)
constantForTypeName = haskellCoderDefinition "constantForTypeName" $
  "tname" ~>
    Strings.cat2 (string "_") (Names.localNameOf @@ var "tname")

constructModule :: TBinding (HaskellNamespaces -> Module -> [Definition] -> Flow Graph H.Module)
constructModule = haskellCoderDefinition "constructModule" $ "namespaces" ~> "mod" ~> "defs" ~> lets [
  "h">: "namespace" ~>
    unwrap _Namespace @@ var "namespace",
  "createDeclarations">: "g" ~> "def" ~>
    cases _Definition (var "def") Nothing [
      _Definition_type>>: "type" ~> lets [
        "name">: Module.typeDefinitionName $ var "type",
        "typ">: Module.typeDefinitionType $ var "type"] $
        toTypeDeclarationsFrom @@ var "namespaces" @@ var "name" @@ var "typ",
      _Definition_term>>: "term" ~>
        "d" <<~ toDataDeclaration @@ var "namespaces" @@ var "term" $
        Flows.pure $ list [var "d"]],
    "importName">: "name" ~>
      wrap H._ModuleName $ Strings.intercalate (string ".") (Lists.map (Formatting.capitalize) (Strings.splitOn (string ".") (var "name"))),
    "imports">: Lists.concat2 (var "domainImports") (var "standardImports"),
    "domainImports">: lets [
      "toImport">: "pair" ~> lets [
        "namespace">: Pairs.first $ var "pair",
        "alias">: Pairs.second $ var "pair",
        "name">: var "h" @@ var "namespace"] $
        record H._Import [
          H._Import_qualified>>: true,
          H._Import_module>>: var "importName" @@ var "name",
          H._Import_as>>: just $ var "alias",
          H._Import_spec>>: nothing]] $
      Lists.map (var "toImport") (Maps.toList $ Module.namespacesMapping $ var "namespaces"),
    "standardImports">: lets [
      "toImport">: "triple" ~> lets [
        "name">: Pairs.first $ Pairs.first $ var "triple",
        "malias">: Pairs.second $ Pairs.first $ var "triple",
        "hidden">: Pairs.second $ var "triple",
        "spec">: Logic.ifElse (Lists.null $ var "hidden")
          nothing
          (just $ inject H._SpecImport H._SpecImport_hiding $ Lists.map ("n" ~> record H._ImportExportSpec [
            H._ImportExportSpec_modifier>>: nothing,
            H._ImportExportSpec_name>>: HaskellUtils.simpleName @@ var "n",
            H._ImportExportSpec_subspec>>: nothing]) (var "hidden"))] $
        record H._Import [
          H._Import_qualified>>: Maybes.isJust $ var "malias",
          H._Import_module>>: wrap H._ModuleName $ var "name",
          H._Import_as>>: Maybes.map (unaryFunction $ wrap H._ModuleName) (var "malias"),
          H._Import_spec>>: var "spec"]] $
      Lists.map (var "toImport") $ list [
        pair (pair (string "Prelude") nothing) (list $ string <$> [
          "Enum", "Ordering", "fail", "map", "pure", "sum"]),
        pair (pair (string "Data.Int") (just $ string "I")) (list ([] :: [TTerm String])),
        pair (pair (string "Data.List") (just $ string "L")) (list ([] :: [TTerm String])),
        pair (pair (string "Data.Map") (just $ string "M")) (list ([] :: [TTerm String])),
        pair (pair (string "Data.Set") (just $ string "S")) (list ([] :: [TTerm String]))]] $
    "g" <<~ Monads.getState $
    "declLists" <<~ Flows.mapList (var "createDeclarations" @@ var "g") (var "defs") $ lets [
    "decls">: Lists.concat $ var "declLists",
    "mc">: Module.moduleDescription $ var "mod"] $
    Flows.pure $ record H._Module [
      H._Module_head>>: just $ record H._ModuleHead [
        H._ModuleHead_comments>>: var "mc",
        H._ModuleHead_name>>: var "importName" @@ (var "h" @@ (Module.moduleNamespace $ var "mod")),
        H._ModuleHead_exports>>: list ([] :: [TTerm H.Export])],
      H._Module_imports>>: var "imports",
      H._Module_declarations>>: var "decls"]

encodeFunction :: TBinding (HaskellNamespaces -> Function -> Flow Graph H.Expression)
encodeFunction = haskellCoderDefinition "encodeFunction" $
  "namespaces" ~> "fun" ~>
    cases _Function (var "fun") Nothing [
      _Function_elimination>>: "e" ~>
        cases _Elimination (var "e") Nothing [
          _Elimination_wrap>>: "name" ~>
            Flows.pure $ inject H._Expression H._Expression_variable $ HaskellUtils.elementReference @@ var "namespaces" @@
              (Names.qname @@ (Maybes.fromJust $ Names.namespaceOf @@ var "name") @@ (HaskellUtils.newtypeAccessorName @@ var "name")),
          _Elimination_record>>: "proj" ~> lets [
            "dn">: Core.projectionTypeName $ var "proj",
            "fname">: Core.projectionField $ var "proj"] $
            Flows.pure $ inject H._Expression H._Expression_variable $ HaskellUtils.recordFieldReference @@ var "namespaces" @@ var "dn" @@ var "fname",
          _Elimination_union>>: "stmt" ~> lets [
            "dn">: Core.caseStatementTypeName $ var "stmt",
            "def">: Core.caseStatementDefault $ var "stmt",
            "fields">: Core.caseStatementCases $ var "stmt",
            "caseExpr">:
              "rt" <<~ Lexical.withSchemaContext @@ (Schemas.requireUnionType @@ var "dn") $ lets [
              "fieldMap">: Maps.fromList $ Lists.map (var "toFieldMapEntry") (Core.rowTypeFields $ var "rt"),
              "toFieldMapEntry">: "f" ~>
                pair (Core.fieldTypeName $ var "f") (var "f")] $
              "ecases" <<~ Flows.mapList (var "toAlt" @@ var "fieldMap") (var "fields") $
              "dcases" <<~ (Maybes.cases (var "def")
                (Flows.pure $ list ([] :: [TTerm H.CaseRhs])) $
                "d" ~>
                  "cs" <<~ (Flows.map (unaryFunction $ wrap H._CaseRhs) $ encodeTerm @@ var "namespaces" @@ var "d") $ lets [
                  "lhs">: inject H._Pattern H._Pattern_name $ HaskellUtils.rawName @@ (Constants.ignoredVariable),
                  "alt">: record H._Alternative [
                    H._Alternative_pattern>>: var "lhs",
                    H._Alternative_rhs>>: var "cs",
                    H._Alternative_binds>>: nothing]] $
                  Flows.pure $ list [var "alt"]) $
              Flows.pure $ inject H._Expression H._Expression_case $ record H._CaseExpression [
                H._CaseExpression_case>>: HaskellUtils.hsvar @@ string "x",
                H._CaseExpression_alternatives>>: Lists.concat2 (var "ecases") (var "dcases")],
              "toAlt">: "fieldMap" ~> "field" ~> lets [
                "fn">: Core.fieldName $ var "field",
                "fun'">: Core.fieldTerm $ var "field"] $
                Annotations.withDepth @@ keyHaskellVar @@ (
                  "depth" ~> lets [
                    "v0">: Strings.cat2 (string "v") (Literals.showInt32 $ var "depth"),
                    "raw">: MetaTerms.apply (var "fun'") (Core.termVariable $ Core.name $ var "v0"),
                    "rhsTerm">: Rewriting.simplifyTerm @@ var "raw",
                    "v1">: Logic.ifElse (Rewriting.isFreeVariableInTerm @@ (wrap _Name $ var "v0") @@ var "rhsTerm")
                      (Constants.ignoredVariable)
                      (var "v0"),
                    "hname">: HaskellUtils.unionFieldReference @@ var "namespaces" @@ var "dn" @@ var "fn"] $
                    "args" <<~ (Maybes.cases (Maps.lookup (var "fn") (var "fieldMap"))
                        (Flows.fail $ Strings.cat $ list [string "field ", Literals.showString $ (Core.unName $ var "fn"),
                          string " not found in ", Literals.showString $ (Core.unName $ var "dn")]) $
                        "fieldType" ~> lets [
                          "ft">: Core.fieldTypeType $ var "fieldType",
                          "noArgs">: Flows.pure $ list ([] :: [TTerm H.Pattern]),
                          "singleArg">: Flows.pure $ list [inject H._Pattern H._Pattern_name $ HaskellUtils.rawName @@ var "v1"]] $
                          cases _Type (Rewriting.deannotateType @@ var "ft")
                            (Just $ var "singleArg") [
                            _Type_unit>>: constant $ var "noArgs"]) $ lets [
                    "lhs">: HaskellUtils.applicationPattern @@ var "hname" @@ var "args"] $
                    "rhs" <<~ (Flows.map (unaryFunction $ wrap H._CaseRhs) $ encodeTerm @@ var "namespaces" @@ var "rhsTerm") $
                    Flows.pure $ record H._Alternative [
                      H._Alternative_pattern>>: var "lhs",
                      H._Alternative_rhs>>: var "rhs",
                      H._Alternative_binds>>: nothing])] $
            Flows.map (HaskellUtils.hslambda @@ (HaskellUtils.rawName @@ string "x")) (var "caseExpr")],
      _Function_lambda>>: "lam" ~> lets [
        "v">: Core.lambdaParameter $ var "lam",
        "body">: Core.lambdaBody $ var "lam"] $
        Flows.bind (encodeTerm @@ var "namespaces" @@ var "body") $ "hbody" ~>
          Flows.pure $ HaskellUtils.hslambda @@ (HaskellUtils.elementReference @@ var "namespaces" @@ var "v") @@ var "hbody",
      _Function_primitive>>: "name" ~>
        Flows.pure $ inject H._Expression H._Expression_variable $ HaskellUtils.elementReference @@ var "namespaces" @@ var "name"]

encodeLiteral :: TBinding (Literal -> Flow Graph H.Expression)
encodeLiteral = haskellCoderDefinition "encodeLiteral" $
  "l" ~>
    cases _Literal (var "l")
      (Just $ Flows.fail $ Strings.cat2 (string "literal value ") (ShowCore.literal @@ var "l")) [
      _Literal_boolean>>: "b" ~>
        Flows.pure $ HaskellUtils.hsvar @@ Logic.ifElse (var "b") (string "True") (string "False"),
      _Literal_float>>: "fv" ~>
        cases _FloatValue (var "fv") Nothing [
          _FloatValue_float32>>: "f" ~>
            Flows.pure $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_float $ var "f"),
          _FloatValue_float64>>: "f" ~>
            Flows.pure $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_double $ var "f"),
          _FloatValue_bigfloat>>: "f" ~>
            Flows.pure $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_double $ Literals.bigfloatToFloat64 $ var "f")],
      _Literal_integer>>: "iv" ~>
        cases _IntegerValue (var "iv") Nothing [
          _IntegerValue_bigint>>: "i" ~>
            Flows.pure $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_integer $ var "i"),
          _IntegerValue_int8>>: "i" ~>
            Flows.pure $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_integer $ Literals.int8ToBigint $ var "i"),
          _IntegerValue_int16>>: "i" ~>
            Flows.pure $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_integer $ Literals.int16ToBigint $ var "i"),
          _IntegerValue_int32>>: "i" ~>
            Flows.pure $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_int $ var "i"),
          _IntegerValue_int64>>: "i" ~>
            Flows.pure $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_integer $ Literals.int64ToBigint $ var "i"),
          _IntegerValue_uint8>>: "i" ~>
            Flows.pure $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_integer $ Literals.uint8ToBigint $ var "i"),
          _IntegerValue_uint16>>: "i" ~>
            Flows.pure $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_integer $ Literals.uint16ToBigint $ var "i"),
          _IntegerValue_uint32>>: "i" ~>
            Flows.pure $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_integer $ Literals.uint32ToBigint $ var "i"),
          _IntegerValue_uint64>>: "i" ~>
            Flows.pure $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_integer $ Literals.uint64ToBigint $ var "i")],
      _Literal_string>>: "s" ~>
        Flows.pure $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_string $ var "s")]

encodeTerm :: TBinding (HaskellNamespaces -> Term -> Flow Graph H.Expression)
encodeTerm = haskellCoderDefinition "encodeTerm" $
  "namespaces" ~> "term" ~> lets [
    "encode">: encodeTerm @@ var "namespaces"] $
    "nonemptyMap" <~ ("m" ~> lets [
      "lhs">: HaskellUtils.hsvar @@ string "M.fromList",
      "encodePair">: "pair" ~> lets [
        "k">: Pairs.first $ var "pair",
        "v">: Pairs.second $ var "pair",
        "hk">: var "encode" @@ var "k",
        "hv">: var "encode" @@ var "v"] $
        Flows.map
          (unaryFunction $ inject H._Expression H._Expression_tuple)
          (Flows.sequence $ list [var "hk", var "hv"])] $
      "rhs" <<~ Flows.map
        (unaryFunction $ inject H._Expression H._Expression_list)
        (Flows.mapList (var "encodePair") $ Maps.toList (var "m")) $
      Flows.pure $ HaskellUtils.hsapp @@ var "lhs" @@ var "rhs") $
    "nonemptySet" <~ ("s" ~> lets [
      "lhs">: HaskellUtils.hsvar @@ string "S.fromList" ] $
      "rhs" <<~ encodeTerm @@ var "namespaces" @@ (inject _Term _Term_list $ Sets.toList $ var "s") $
      Flows.pure $ HaskellUtils.hsapp @@ var "lhs" @@ var "rhs") $
    cases _Term (Rewriting.deannotateTerm @@ var "term")
      (Just $ Flows.fail $ Strings.cat2 (string "unexpected term: ") (ShowCore.term @@ var "term")) [
      _Term_application>>: "app" ~> lets [
        "fun">: Core.applicationFunction $ var "app",
        "arg">: Core.applicationArgument $ var "app"] $
        Flows.bind (var "encode" @@ var "fun") $ "hfun" ~>
          Flows.bind (var "encode" @@ var "arg") $ "harg" ~>
            Flows.pure $ HaskellUtils.hsapp @@ var "hfun" @@ var "harg",
      _Term_either>>: "e" ~> Eithers.either_
          ("l" ~>
            Flows.bind (var "encode" @@ var "l") $ "hl" ~>
              Flows.pure $ HaskellUtils.hsapp @@ (HaskellUtils.hsvar @@ string "Left") @@ var "hl")
          ("r" ~>
            Flows.bind (var "encode" @@ var "r") $ "hr" ~>
              Flows.pure $ HaskellUtils.hsapp @@ (HaskellUtils.hsvar @@ string "Right") @@ var "hr")
          (var "e"),
      _Term_function>>: "f" ~>
        encodeFunction @@ var "namespaces" @@ var "f",
      _Term_let>>: "letTerm" ~> lets [
        "bindings">: Core.letBindings $ var "letTerm",
        "env">: Core.letBody $ var "letTerm",
        "encodeBinding">: "binding" ~> lets [
          "name">: Core.bindingName $ var "binding",
          "term'">: Core.bindingTerm $ var "binding",
          "hname">: HaskellUtils.simpleName @@ (Core.unName $ var "name")] $
          "hexpr" <<~ var "encode" @@ var "term'" $
          Flows.pure $ inject H._LocalBinding H._LocalBinding_value $ HaskellUtils.simpleValueBinding @@ var "hname" @@ var "hexpr" @@ nothing] $
        "hbindings" <<~ Flows.mapList (var "encodeBinding") (var "bindings") $
        "hinner" <<~ var "encode" @@ var "env" $
        Flows.pure $ inject H._Expression H._Expression_let $ record H._LetExpression [
          H._LetExpression_bindings>>: var "hbindings",
          H._LetExpression_inner>>: var "hinner"],
      _Term_list>>: "els" ~>
        Flows.bind (Flows.mapList (var "encode") (var "els")) $ "helems" ~>
          Flows.pure $ inject H._Expression H._Expression_list $ var "helems",
      _Term_literal>>: "v" ~>
        encodeLiteral @@ var "v",
      _Term_map>>: "m" ~> Logic.ifElse (Maps.null $ var "m")
        (produce $ HaskellUtils.hsvar @@ string "M.empty")
        (var "nonemptyMap" @@ var "m"),
      _Term_maybe>>: "m" ~>
        Maybes.cases (var "m")
          (Flows.pure $ HaskellUtils.hsvar @@ string "Nothing") $
          "t" ~>
            Flows.bind (var "encode" @@ var "t") $ "ht" ~>
              Flows.pure $ HaskellUtils.hsapp @@ (HaskellUtils.hsvar @@ string "Just") @@ var "ht",
      _Term_pair>>: "p" ~>
        "f" <<~ var "encode" @@ Pairs.first (var "p") $
        "s" <<~ var "encode" @@ Pairs.second (var "p") $
        Flows.pure $ inject H._Expression H._Expression_tuple $ list [var "f", var "s"],
      _Term_record>>: "record" ~> lets [
        "sname">: Core.recordTypeName $ var "record",
        "fields">: Core.recordFields $ var "record",
        "toFieldUpdate">: "field" ~> lets [
          "fn">: Core.fieldName $ var "field",
          "ft">: Core.fieldTerm $ var "field",
          "fieldRef">: HaskellUtils.recordFieldReference @@ var "namespaces" @@ var "sname" @@ var "fn"] $
          "hft" <<~ var "encode" @@ var "ft" $
          Flows.pure $ record H._FieldUpdate [
            H._FieldUpdate_name>>: var "fieldRef",
            H._FieldUpdate_value>>: var "hft"],
          "typeName">: HaskellUtils.elementReference @@ var "namespaces" @@ var "sname"] $
          "updates" <<~ Flows.mapList (var "toFieldUpdate") (var "fields") $
          Flows.pure $ inject H._Expression H._Expression_constructRecord $ record H._ConstructRecordExpression [
            H._ConstructRecordExpression_name>>: var "typeName",
            H._ConstructRecordExpression_fields>>: var "updates"],
      _Term_set>>: "s" ~> Logic.ifElse (Sets.null $ var "s")
        (produce $ HaskellUtils.hsvar @@ string "S.empty")
        (var "nonemptySet" @@ var "s"),
      _Term_typeLambda>>: "abs" ~> lets [
        "term1">: Core.typeLambdaBody $ var "abs"] $
        var "encode" @@ var "term1",
      _Term_typeApplication>>: "typed" ~> lets [
        "term1">: Core.typeApplicationTermBody $ var "typed"] $
        var "encode" @@ var "term1",
      _Term_union>>: "injection" ~> lets [
        "sname">: Core.injectionTypeName $ var "injection",
        "field">: Core.injectionField $ var "injection",
        "fn">: Core.fieldName $ var "field",
        "ft">: Core.fieldTerm $ var "field",
        "lhs">: inject H._Expression H._Expression_variable $ HaskellUtils.unionFieldReference @@ var "namespaces" @@ var "sname" @@ var "fn",
        "dflt">: Flows.map (HaskellUtils.hsapp @@ var "lhs") (var "encode" @@ var "ft")] $
        "ftyp" <<~ Schemas.requireUnionField_ @@ var "sname" @@ var "fn" $
        cases _Type (Rewriting.deannotateType @@ var "ftyp")
          (Just $ var "dflt") [
          _Type_unit>>: constant $ Flows.pure $ var "lhs"],
      _Term_unit>>: constant $ Flows.pure $ inject H._Expression H._Expression_tuple $ list ([] :: [TTerm H.Expression]),
      _Term_variable>>: "name" ~>
        Flows.pure $ inject H._Expression H._Expression_variable $ HaskellUtils.elementReference @@ var "namespaces" @@ var "name",
      _Term_wrap>>: "wrapped" ~> lets [
        "tname">: Core.wrappedTermTypeName $ var "wrapped",
        "term'">: Core.wrappedTermBody $ var "wrapped",
        "lhs">: inject H._Expression H._Expression_variable $ HaskellUtils.elementReference @@ var "namespaces" @@ var "tname"] $
        "rhs" <<~ var "encode" @@ var "term'" $
        Flows.pure $ HaskellUtils.hsapp @@ var "lhs" @@ var "rhs"]

encodeType :: TBinding (HaskellNamespaces -> Type -> Flow Graph H.Type)
encodeType = haskellCoderDefinition "encodeType" $
  "namespaces" ~>
  "typ" ~> lets [
  "encode">: encodeType @@ var "namespaces",
  "ref">: "name" ~>
    Flows.pure $ inject H._Type H._Type_variable $ HaskellUtils.elementReference @@ var "namespaces" @@ var "name",
  "unitTuple">: inject H._Type H._Type_tuple $ list ([] :: [TTerm H.Type])] $
  trace (string "encode type") $
  cases _Type (Rewriting.deannotateType @@ var "typ")
    (Just $ Flows.fail $ Strings.cat2 (string "unexpected type: ") (ShowCore.type_ @@ var "typ")) [
    _Type_application>>: "app" ~> lets [
      "lhs">: Core.applicationTypeFunction $ var "app",
      "rhs">: Core.applicationTypeArgument $ var "app"] $
      Flows.bind (var "encode" @@ var "lhs") $ "hlhs" ~>
        Flows.bind (var "encode" @@ var "rhs") $ "hrhs" ~>
          Flows.pure $ HaskellUtils.toTypeApplication @@ list [var "hlhs", var "hrhs"],
    _Type_either>>: "eitherType" ~> lets [
      "left">: Core.eitherTypeLeft $ var "eitherType",
      "right">: Core.eitherTypeRight $ var "eitherType"] $
      Flows.map
        (HaskellUtils.toTypeApplication)
        (Flows.sequence $ list [
          Flows.pure $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "Either",
          var "encode" @@ var "left",
          var "encode" @@ var "right"]),
    _Type_function>>: "funType" ~> lets [
      "dom">: Core.functionTypeDomain $ var "funType",
      "cod">: Core.functionTypeCodomain $ var "funType"] $
      Flows.bind (var "encode" @@ var "dom") $ "hdom" ~>
        Flows.bind (var "encode" @@ var "cod") $ "hcod" ~>
          Flows.pure $ inject H._Type H._Type_function $ record H._FunctionType [
            H._FunctionType_domain>>: var "hdom",
            H._FunctionType_codomain>>: var "hcod"],
    _Type_forall>>: "forallType" ~> lets [
      "v">: Core.forallTypeParameter $ var "forallType",
      "body">: Core.forallTypeBody $ var "forallType"] $
      var "encode" @@ var "body",
    _Type_list>>: "lt" ~>
      Flows.bind (var "encode" @@ var "lt") $ "hlt" ~>
        Flows.pure $ inject H._Type H._Type_list $ var "hlt",
    _Type_literal>>: "lt" ~>
      cases _LiteralType (var "lt")
        (Just $ Flows.fail $ Strings.cat2 (string "unexpected literal type: ") (ShowCore.literalType @@ var "lt")) [
        _LiteralType_boolean>>: constant $
          Flows.pure $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "Bool",
        _LiteralType_float>>: "ft" ~>
          cases _FloatType (var "ft") Nothing [
            _FloatType_float32>>: constant $
              Flows.pure $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "Float",
            _FloatType_float64>>: constant $
              Flows.pure $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "Double",
            _FloatType_bigfloat>>: constant $
              Flows.pure $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "Double"],
        _LiteralType_integer>>: "it" ~>
          cases _IntegerType (var "it")
            (Just $ Flows.fail $ Strings.cat2 (string "unexpected integer type: ") (ShowCore.integerType @@ var "it")) [
            _IntegerType_bigint>>: constant $
              Flows.pure $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "Integer",
            _IntegerType_int8>>: constant $
              Flows.pure $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "I.Int8",
            _IntegerType_int16>>: constant $
              Flows.pure $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "I.Int16",
            _IntegerType_int32>>: constant $
              Flows.pure $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "Int",
            _IntegerType_int64>>: constant $
              Flows.pure $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "I.Int64"],
        _LiteralType_string>>: constant $
          Flows.pure $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "String"],
    _Type_map>>: "mapType" ~> lets [
      "kt">: Core.mapTypeKeys $ var "mapType",
      "vt">: Core.mapTypeValues $ var "mapType"] $
      Flows.map
        (HaskellUtils.toTypeApplication)
        (Flows.sequence $ list [
          Flows.pure $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "M.Map",
          var "encode" @@ var "kt",
          var "encode" @@ var "vt"]),
    _Type_maybe>>: "ot" ~>
      Flows.map
        (HaskellUtils.toTypeApplication)
        (Flows.sequence $ list [
          Flows.pure $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "Maybe",
          var "encode" @@ var "ot"]),
    _Type_pair>>: "pt" ~>
      "f" <<~ var "encode" @@ (Core.pairTypeFirst $ var "pt") $
      "s" <<~ var "encode" @@ (Core.pairTypeSecond $ var "pt") $
        Flows.pure $ inject H._Type H._Type_tuple $ list [var "f", var "s"],
    _Type_record>>: "rt" ~> var "ref" @@ (Core.rowTypeTypeName $ var "rt"),
    _Type_set>>: "st" ~>
      Flows.map
        (HaskellUtils.toTypeApplication)
        (Flows.sequence $ list [
          Flows.pure $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "S.Set",
          var "encode" @@ var "st"]),
    _Type_union>>: "rt" ~> lets [
      "typeName">: Core.rowTypeTypeName $ var "rt"] $
      var "ref" @@ var "typeName",
    _Type_unit>>: constant $ Flows.pure $ var "unitTuple",
    _Type_variable>>: "v1" ~> var "ref" @@ var "v1",
    _Type_wrap>>: "wrapped" ~> lets [
      "name">: Core.wrappedTypeTypeName $ var "wrapped"] $
      var "ref" @@ var "name"]

encodeTypeWithClassAssertions :: TBinding (HaskellNamespaces -> M.Map Name (S.Set TypeClass) -> Type -> Flow Graph H.Type)
encodeTypeWithClassAssertions = haskellCoderDefinition "encodeTypeWithClassAssertions" $
  "namespaces" ~> "explicitClasses" ~> "typ" ~> lets [
    "classes">: Maps.union (var "explicitClasses") (getImplicitTypeClasses @@ var "typ"),
    "implicitClasses">: getImplicitTypeClasses @@ var "typ",
    "encodeAssertion">: "pair" ~> lets [
      "name">: Pairs.first $ var "pair",
      "cls">: Pairs.second $ var "pair",
      "hname">: HaskellUtils.rawName @@ cases _TypeClass (var "cls") Nothing [
        _TypeClass_equality>>: constant $ string "Eq",
        _TypeClass_ordering>>: constant $ string "Ord"],
      "htype">: inject H._Type H._Type_variable $ HaskellUtils.rawName @@ (Core.unName $ var "name")] $
      inject H._Assertion H._Assertion_class $ record H._ClassAssertion [
        H._ClassAssertion_name>>: var "hname",
        H._ClassAssertion_types>>: list [var "htype"]],
    "assertPairs">: Lists.concat $ Lists.map (var "toPairs") (Maps.toList $ var "classes"),
    "toPairs">: "mapEntry" ~> lets [
      "name">: Pairs.first $ var "mapEntry",
      "clsSet">: Pairs.second $ var "mapEntry",
      "toPair">: "c" ~>
        pair (var "name") (var "c")] $
      Lists.map (var "toPair") (Sets.toList $ var "clsSet")] $
    Monads.withTrace @@ (string "encode with assertions") @@
      ("htyp" <<~ adaptTypeToHaskellAndEncode @@ var "namespaces" @@ var "typ" $
      Logic.ifElse (Lists.null $ var "assertPairs")
        (Flows.pure $ var "htyp") (lets [
          "encoded">: Lists.map (var "encodeAssertion") (var "assertPairs"),
          "hassert">: Logic.ifElse (Equality.gt (Lists.length $ var "encoded") (int32 1))
            (Lists.head $ var "encoded")
            (inject H._Assertion H._Assertion_tuple $ var "encoded")] $
          Flows.pure $ inject H._Type H._Type_ctx $ record H._ContextType [
            H._ContextType_ctx>>: var "hassert",
            H._ContextType_type>>: var "htyp"]))

findOrdVariables :: TBinding (Type -> S.Set Name)
findOrdVariables = haskellCoderDefinition "findOrdVariables" $
  "typ" ~> lets [
    "fold">: "names" ~> "typ'" ~>
      cases _Type (var "typ'")
        (Just $ var "names") [
        _Type_map>>: "mapType" ~> lets [
          "kt">: Core.mapTypeKeys $ var "mapType"] $
          var "tryType" @@ var "names" @@ var "kt",
        _Type_set>>: "et" ~>
          var "tryType" @@ var "names" @@ var "et"],
    "isTypeVariable">: "v" ~> lets [
      "nameStr">: Core.unName $ var "v",
      "hasNoNamespace">: Maybes.isNothing $ Names.namespaceOf @@ var "v",
      "startsWithT">: Equality.equal (Strings.charAt (int32 0) (var "nameStr")) (int32 116)] $ -- 't' character
      Logic.and (var "hasNoNamespace") (var "startsWithT"),
    "tryType">: "names" ~> "t" ~>
      cases _Type (Rewriting.deannotateType @@ var "t")
        (Just $ var "names") [
        _Type_variable>>: "v" ~>
          Logic.ifElse (var "isTypeVariable" @@ var "v")
            (Sets.insert (var "v") (var "names"))
            (var "names")]] $
    Rewriting.foldOverType @@ Coders.traversalOrderPre @@ var "fold" @@ Sets.empty @@ var "typ"

getImplicitTypeClasses :: TBinding (Type -> M.Map Name (S.Set TypeClass))
getImplicitTypeClasses = haskellCoderDefinition "getImplicitTypeClasses" $
  "typ" ~> lets [
    "toPair">: "name" ~>
      pair (var "name") (Sets.fromList $ list [Graph.typeClassOrdering])] $
    Maps.fromList $ Lists.map (var "toPair") (Sets.toList $ findOrdVariables @@ var "typ")

moduleToHaskellModule :: TBinding (Module -> [Definition] -> Flow Graph H.Module)
moduleToHaskellModule = haskellCoderDefinition "moduleToHaskellModule" $
  "mod" ~> "defs" ~>
    "namespaces" <<~ HaskellUtils.namespacesForModule @@ var "mod" $
      constructModule @@ var "namespaces" @@ var "mod" @@ var "defs"

moduleToHaskell :: TBinding (Module -> [Definition] -> Flow Graph (M.Map String String))
moduleToHaskell = haskellCoderDefinition "moduleToHaskell" $ "mod" ~> "defs" ~>
  "hsmod" <<~ moduleToHaskellModule @@ var "mod" @@ var "defs" $ lets [
  "s">: Serialization.printExpr @@ (Serialization.parenthesize @@ (HaskellSerde.moduleToExpr @@ var "hsmod")),
  "filepath">: Names.namespaceToFilePath @@ Util.caseConventionPascal @@ (wrap _FileExtension $ string "hs") @@ (Module.moduleNamespace $ var "mod")] $
  Flows.pure $ Maps.singleton (var "filepath") (var "s")

nameDecls :: TBinding (Graph -> HaskellNamespaces -> Name -> Type -> [H.DeclarationWithComments])
nameDecls = haskellCoderDefinition "nameDecls" $
  "g" ~> "namespaces" ~> "name" ~> "typ" ~> lets [
    "nm">: Core.unName $ var "name",
    "toDecl">: "n" ~> "pair" ~> lets [
      "k">: Pairs.first $ var "pair",
      "v">: Pairs.second $ var "pair",
      "decl">: inject H._Declaration H._Declaration_valueBinding $ inject H._ValueBinding H._ValueBinding_simple $ record H._SimpleValueBinding [
        H._SimpleValueBinding_pattern>>: HaskellUtils.applicationPattern @@ (HaskellUtils.simpleName @@ var "k") @@ list ([] :: [TTerm H.Pattern]),
        H._SimpleValueBinding_rhs>>: wrap H._RightHandSide $ inject H._Expression H._Expression_application $ record H._ApplicationExpression [
          H._ApplicationExpression_function>>: inject H._Expression H._Expression_variable $ HaskellUtils.elementReference @@ var "namespaces" @@ var "n",
          H._ApplicationExpression_argument>>: inject H._Expression H._Expression_literal $ inject H._Literal H._Literal_string $ var "v"],
        H._SimpleValueBinding_localBindings>>: nothing]] $
      record H._DeclarationWithComments [
        H._DeclarationWithComments_body>>: var "decl",
        H._DeclarationWithComments_comments>>: nothing],
    "nameDecl">: pair (constantForTypeName @@ var "name") (var "nm"),
    "fieldDecls">: Lists.map (var "toConstant") (Lexical.fieldsOf @@ var "typ"),
    "toConstant">: "fieldType" ~> lets [
      "fname">: Core.fieldTypeName $ var "fieldType"] $
      pair (constantForFieldName @@ var "name" @@ var "fname") (Core.unName $ var "fname")] $
    Logic.ifElse (useCoreImport)
      (Lists.cons (var "toDecl" @@ Core.nameLift _Name @@ var "nameDecl") (Lists.map (var "toDecl" @@ Core.nameLift _Name) (var "fieldDecls")))
      (list ([] :: [TTerm H.DeclarationWithComments]))

toDataDeclaration :: TBinding (HaskellNamespaces -> TermDefinition -> Flow Graph H.DeclarationWithComments)
toDataDeclaration = haskellCoderDefinition "toDataDeclaration" $
  "namespaces" ~> "def" ~> lets [
    "name">: Module.termDefinitionName $ var "def",
    "term">: Module.termDefinitionTerm $ var "def",
    "typ">: Module.termDefinitionType $ var "def",
    "hname">: HaskellUtils.simpleName @@ (Names.localNameOf @@ var "name"),
    "rewriteValueBinding">: "vb" ~>
      cases H._ValueBinding (var "vb") Nothing [
        H._ValueBinding_simple>>: "simple" ~> lets [
          "pattern'">: project H._SimpleValueBinding H._SimpleValueBinding_pattern @@ var "simple",
          "rhs">: project H._SimpleValueBinding H._SimpleValueBinding_rhs @@ var "simple",
          "bindings">: project H._SimpleValueBinding H._SimpleValueBinding_localBindings @@ var "simple"] $
          cases H._Pattern (var "pattern'")
            (Just $ var "vb") [
            H._Pattern_application>>: "appPat" ~> lets [
              "name'">: project H._ApplicationPattern H._ApplicationPattern_name @@ var "appPat",
              "args">: project H._ApplicationPattern H._ApplicationPattern_args @@ var "appPat",
              "rhsExpr">: unwrap H._RightHandSide @@ var "rhs"] $
              cases H._Expression (var "rhsExpr")
                (Just $ var "vb") [
                H._Expression_lambda>>: "lambda'" ~> lets [
                  "vars">: project H._LambdaExpression H._LambdaExpression_bindings @@ var "lambda'",
                  "body">: project H._LambdaExpression H._LambdaExpression_inner @@ var "lambda'",
                  "newPattern">: HaskellUtils.applicationPattern @@ var "name'" @@ (Lists.concat2 (var "args") (var "vars")),
                  "newRhs">: wrap H._RightHandSide $ var "body"] $
                  var "rewriteValueBinding" @@ (inject H._ValueBinding H._ValueBinding_simple $ record H._SimpleValueBinding [
                    H._SimpleValueBinding_pattern>>: var "newPattern",
                    H._SimpleValueBinding_rhs>>: var "newRhs",
                    H._SimpleValueBinding_localBindings>>: var "bindings"])]]],
    "toDecl">: "comments" ~> "hname'" ~> "term'" ~> "bindings" ~>
      cases _Term (Rewriting.deannotateTerm @@ var "term'")
        (Just $
          "hterm" <<~ encodeTerm @@ var "namespaces" @@ var "term'" $ lets [
         "vb">: HaskellUtils.simpleValueBinding @@ var "hname'" @@ var "hterm" @@ var "bindings"] $
         "explicitClasses" <<~ Annotations.getTypeClasses @@ (Rewriting.removeTypesFromTerm @@ var "term") $
         "htype" <<~ encodeTypeWithClassAssertions @@ var "namespaces" @@ var "explicitClasses" @@ var "typ" $ lets [
         "decl">: inject H._Declaration H._Declaration_typedBinding $ record H._TypedBinding [
           H._TypedBinding_typeSignature>>: record H._TypeSignature [
             H._TypeSignature_name>>: var "hname'",
             H._TypeSignature_type>>: var "htype"],
           H._TypedBinding_valueBinding>>: var "rewriteValueBinding" @@ var "vb"]] $
          Flows.pure $ record H._DeclarationWithComments [
            H._DeclarationWithComments_body>>: var "decl",
            H._DeclarationWithComments_comments>>: var "comments"]) [
        _Term_let>>: "letTerm" ~> lets [
          -- For let terms, encode each binding's term directly
          "lbindings">: Core.letBindings $ var "letTerm",
          "env">: Core.letBody $ var "letTerm",
          "toBinding">: "hname''" ~> "hterm'" ~>
            inject H._LocalBinding H._LocalBinding_value $ HaskellUtils.simpleValueBinding @@ var "hname''" @@ var "hterm'" @@ nothing,
          "hnames">: Lists.map ("binding" ~> HaskellUtils.simpleName @@ (Core.unName $ Core.bindingName $ var "binding")) (var "lbindings"),
          "terms">: Lists.map (unaryFunction $ Core.bindingTerm) (var "lbindings")] $
          "hterms" <<~ Flows.mapList (encodeTerm @@ var "namespaces") (var "terms") $ lets [
          "hbindings">: Lists.zipWith (var "toBinding") (var "hnames") (var "hterms")] $
          var "toDecl" @@ var "comments" @@ var "hname'" @@ var "env" @@ (just $ wrap H._LocalBindings $ var "hbindings")]] $
    "comments" <<~ Annotations.getTermDescription @@ var "term" $
    var "toDecl" @@ var "comments" @@ var "hname" @@ var "term" @@ nothing

--toTypeDeclarations :: TBinding (HaskellNamespaces -> Binding -> Term -> Flow Graph [H.DeclarationWithComments])
--toTypeDeclarations = haskellCoderDefinition "toTypeDeclarations" $
--  "namespaces" ~> "el" ~> "term" ~> lets [
--    "elementName">: Core.bindingName $ var "el",
--    "lname">: Names.localNameOf @@ var "elementName",
--    "hname">: HaskellUtils.simpleName @@ var "lname",
--    "declHead">: "name" ~> "vars'" ~> Logic.ifElse (Lists.null $ var "vars'")
--      (inject H._DeclarationHead H._DeclarationHead_simple $ var "name")
--      (lets [
--        "h">: Lists.head $ var "vars'",
--        "rest">: Lists.tail $ var "vars'",
--        "hvar">: wrap H._Variable $ HaskellUtils.simpleName @@ (Core.unName $ var "h")] $
--        inject H._DeclarationHead H._DeclarationHead_application $ record H._ApplicationDeclarationHead [
--          H._ApplicationDeclarationHead_function>>: var "declHead" @@ var "name" @@ var "rest",
--          H._ApplicationDeclarationHead_operand>>: var "hvar"]),
--    "newtypeCons">: "el'" ~> "typ'" ~> lets [
--      "hname">: HaskellUtils.simpleName @@ (HaskellUtils.newtypeAccessorName @@ (Core.bindingName $ var "el'"))] $
--      "htype" <<~ adaptTypeToHaskellAndEncode @@ var "namespaces" @@ var "typ'" $ lets [
--      "hfield">: record H._FieldWithComments [
--        H._FieldWithComments_field>>: record H._Field [
--          H._Field_name>>: var "hname",
--          H._Field_type>>: var "htype"],
--        H._FieldWithComments_comments>>: nothing],
--      "constructorName">: HaskellUtils.simpleName @@ (Names.localNameOf @@ (Core.bindingName $ var "el'"))] $
--      Flows.pure $ record H._ConstructorWithComments [
--        H._ConstructorWithComments_body>>: inject H._Constructor H._Constructor_record $ record H._RecordConstructor [
--          H._RecordConstructor_name>>: var "constructorName",
--          H._RecordConstructor_fields>>: list [var "hfield"]],
--        H._ConstructorWithComments_comments>>: nothing],
--    "recordCons">: "lname'" ~> "fields" ~> lets [
--      "toField">: "fieldType" ~> lets [
--        "fname">: Core.fieldTypeName $ var "fieldType",
--        "ftype">: Core.fieldTypeType $ var "fieldType",
--        "hname'">: HaskellUtils.simpleName @@ Strings.cat2
--          (Formatting.decapitalize @@ var "lname'")
--          (Formatting.capitalize @@ (Core.unName $ var "fname"))] $
--        "htype" <<~ adaptTypeToHaskellAndEncode @@ var "namespaces" @@ var "ftype" $
--        "comments" <<~ Annotations.getTypeDescription @@ var "ftype" $
--        Flows.pure $ record H._FieldWithComments [
--          H._FieldWithComments_field>>: record H._Field [
--            H._Field_name>>: var "hname'",
--            H._Field_type>>: var "htype"],
--          H._FieldWithComments_comments>>: var "comments"]] $
--      "hFields" <<~ Flows.mapList (var "toField") (var "fields") $
--      Flows.pure $ record H._ConstructorWithComments [
--        H._ConstructorWithComments_body>>: inject H._Constructor H._Constructor_record $ record H._RecordConstructor [
--          H._RecordConstructor_name>>: HaskellUtils.simpleName @@ var "lname'",
--          H._RecordConstructor_fields>>: var "hFields"],
--        H._ConstructorWithComments_comments>>: nothing],
--    "unionCons">: "g'" ~> "lname'" ~> "fieldType" ~> lets [
--      "fname">: Core.fieldTypeName $ var "fieldType",
--      "ftype">: Core.fieldTypeType $ var "fieldType",
--      "deconflict">: "name" ~> lets [
--        "tname">: Names.unqualifyName @@ record _QualifiedName [
--          _QualifiedName_namespace>>: just $ Pairs.first $ Module.namespacesFocus $ var "namespaces",
--          _QualifiedName_local>>: var "name"]] $
--        Logic.ifElse (Maybes.isJust $ Maps.lookup (var "tname") (Graph.graphElements $ var "g'"))
--          (var "deconflict" @@ Strings.cat2 (var "name") (string "_"))
--          (var "name")] $
--      "comments" <<~ Annotations.getTypeDescription @@ var "ftype" $ lets [
--      "nm">: var "deconflict" @@ Strings.cat2 (Formatting.capitalize @@ var "lname'") (Formatting.capitalize @@ (Core.unName $ var "fname"))] $
--      "typeList" <<~ (Logic.ifElse (Equality.equal (Rewriting.deannotateType @@ var "ftype") MetaTypes.unit)
--        (Flows.pure $ list ([] :: [TTerm H.CaseRhs])) $
--        Flows.bind (adaptTypeToHaskellAndEncode @@ var "namespaces" @@ var "ftype") $ "htype" ~>
--          Flows.pure $ list [var "htype"]) $
--      Flows.pure $ record H._ConstructorWithComments [
--        H._ConstructorWithComments_body>>: inject H._Constructor H._Constructor_ordinary $ record H._OrdinaryConstructor [
--          H._OrdinaryConstructor_name>>: HaskellUtils.simpleName @@ var "nm",
--          H._OrdinaryConstructor_fields>>: var "typeList"],
--        H._ConstructorWithComments_comments>>: var "comments"]] $
--    Monads.withTrace @@ (Strings.cat2 (string "type element ") (Core.unName $ var "elementName")) @@ (
--      "g" <<~ Monads.getState $
--      "t" <<~ (trace (string "to type declarations") $ DecodeCore.type_ @@ var "term") $
--      "isSer" <<~ (Schemas.isSerializable @@ var "el") $ lets [
--      "deriv">: wrap H._Deriving $ Logic.ifElse (var "isSer")
--        (Lists.map (HaskellUtils.rawName) (list [string "Eq", string "Ord", string "Read", string "Show"]))
--        (list ([] :: [TTerm H.Name])),
--      "unpackResult">: HaskellUtils.unpackForallType @@ var "g" @@ var "t",
--      "vars">: Pairs.first $ var "unpackResult",
--      "t'">: Pairs.second $ var "unpackResult",
--      "hd">: var "declHead" @@ var "hname" @@ (Lists.reverse $ var "vars")] $
--      "decl" <<~ (cases _Type (Rewriting.deannotateType @@ var "t'")
--        (Just $ "htype" <<~ adaptTypeToHaskellAndEncode @@ var "namespaces" @@ var "t" $
--          Flows.pure $ inject H._Declaration H._Declaration_type $ record H._TypeDeclaration [
--            H._TypeDeclaration_name>>: var "hd",
--            H._TypeDeclaration_type>>: var "htype"]) [
--        _Type_record>>: "rt" ~>
--          "cons" <<~ (var "recordCons" @@ var "lname" @@ (Core.rowTypeFields $ var "rt")) $
--          Flows.pure $ inject H._Declaration H._Declaration_data $ record H._DataDeclaration [
--            H._DataDeclaration_keyword>>: injectUnit H._DataOrNewtype H._DataOrNewtype_data,
--            H._DataDeclaration_context>>: list ([] :: [TTerm H.Assertion]),
--            H._DataDeclaration_head>>: var "hd",
--            H._DataDeclaration_constructors>>: list [var "cons"],
--            H._DataDeclaration_deriving>>: list [var "deriv"]],
--        _Type_union>>: "rt" ~>
--          "cons" <<~ Flows.mapList (var "unionCons" @@ var "g" @@ var "lname") (Core.rowTypeFields $ var "rt") $
--          Flows.pure $ inject H._Declaration H._Declaration_data $ record H._DataDeclaration [
--            H._DataDeclaration_keyword>>: injectUnit H._DataOrNewtype H._DataOrNewtype_data,
--            H._DataDeclaration_context>>: list ([] :: [TTerm H.Assertion]),
--            H._DataDeclaration_head>>: var "hd",
--            H._DataDeclaration_constructors>>: var "cons",
--            H._DataDeclaration_deriving>>: list [var "deriv"]],
--        _Type_wrap>>: "wrapped" ~> lets [
--          "tname">: Core.wrappedTypeTypeName $ var "wrapped",
--          "wt">: Core.wrappedTypeBody $ var "wrapped"] $
--          "cons" <<~ var "newtypeCons" @@ var "el" @@ var "wt" $
--            Flows.pure $ inject H._Declaration H._Declaration_data $ record H._DataDeclaration [
--              H._DataDeclaration_keyword>>: injectUnit H._DataOrNewtype H._DataOrNewtype_newtype,
--              H._DataDeclaration_context>>: list ([] :: [TTerm H.Assertion]),
--              H._DataDeclaration_head>>: var "hd",
--              H._DataDeclaration_constructors>>: list [var "cons"],
--              H._DataDeclaration_deriving>>: list [var "deriv"]]]) $
--      "comments" <<~ Annotations.getTermDescription @@ var "term" $
--      "tdecls" <<~ (Logic.ifElse (includeTypeDefinitions)
--        (Flows.bind (typeDecl @@ var "namespaces" @@ var "elementName" @@ var "t") $ "decl'" ~>
--          Flows.pure $ list [var "decl'"])
--        (Flows.pure $ list ([] :: [TTerm H.DeclarationWithComments]))) $ lets [
--      "mainDecl">: record H._DeclarationWithComments [
--        H._DeclarationWithComments_body>>: var "decl",
--        H._DeclarationWithComments_comments>>: var "comments"],
--      "nameDecls'">: nameDecls @@ var "g" @@ var "namespaces" @@ var "elementName" @@ var "t"] $
--      Flows.pure $ Lists.concat $ list [list [var "mainDecl"], var "nameDecls'", var "tdecls"])

-- | Simplified version of toTypeDeclarations that works with Name and Type directly
-- This is used with the new Definition-based API
toTypeDeclarationsFrom :: TBinding (HaskellNamespaces -> Name -> Type -> Flow Graph [H.DeclarationWithComments])
toTypeDeclarationsFrom = haskellCoderDefinition "toTypeDeclarationsFrom" $
  "namespaces" ~> "elementName" ~> "typ" ~> lets [
    "lname">: Names.localNameOf @@ var "elementName",
    "hname">: HaskellUtils.simpleName @@ var "lname",
    "declHead">: "name" ~> "vars'" ~> Logic.ifElse (Lists.null $ var "vars'")
      (inject H._DeclarationHead H._DeclarationHead_simple $ var "name")
      (lets [
        "h">: Lists.head $ var "vars'",
        "rest">: Lists.tail $ var "vars'",
        "hvar">: wrap H._Variable $ HaskellUtils.simpleName @@ (Core.unName $ var "h")] $
        inject H._DeclarationHead H._DeclarationHead_application $ record H._ApplicationDeclarationHead [
          H._ApplicationDeclarationHead_function>>: var "declHead" @@ var "name" @@ var "rest",
          H._ApplicationDeclarationHead_operand>>: var "hvar"]),
    "newtypeCons">: "tname" ~> "typ'" ~> lets [
      "hname">: HaskellUtils.simpleName @@ (HaskellUtils.newtypeAccessorName @@ var "tname")] $
      "htype" <<~ adaptTypeToHaskellAndEncode @@ var "namespaces" @@ var "typ'" $ lets [
      "hfield">: record H._FieldWithComments [
        H._FieldWithComments_field>>: record H._Field [
          H._Field_name>>: var "hname",
          H._Field_type>>: var "htype"],
        H._FieldWithComments_comments>>: nothing],
      "constructorName">: HaskellUtils.simpleName @@ (Names.localNameOf @@ var "tname")] $
      Flows.pure $ record H._ConstructorWithComments [
        H._ConstructorWithComments_body>>: inject H._Constructor H._Constructor_record $ record H._RecordConstructor [
          H._RecordConstructor_name>>: var "constructorName",
          H._RecordConstructor_fields>>: list [var "hfield"]],
        H._ConstructorWithComments_comments>>: nothing],
    "recordCons">: "lname'" ~> "fields" ~> lets [
      "toField">: "fieldType" ~> lets [
        "fname">: Core.fieldTypeName $ var "fieldType",
        "ftype">: Core.fieldTypeType $ var "fieldType",
        "hname'">: HaskellUtils.simpleName @@ Strings.cat2
          (Formatting.decapitalize @@ var "lname'")
          (Formatting.capitalize @@ (Core.unName $ var "fname"))] $
        "htype" <<~ adaptTypeToHaskellAndEncode @@ var "namespaces" @@ var "ftype" $
        "comments" <<~ Annotations.getTypeDescription @@ var "ftype" $
        Flows.pure $ record H._FieldWithComments [
          H._FieldWithComments_field>>: record H._Field [
            H._Field_name>>: var "hname'",
            H._Field_type>>: var "htype"],
          H._FieldWithComments_comments>>: var "comments"]] $
      "hFields" <<~ Flows.mapList (var "toField") (var "fields") $
      Flows.pure $ record H._ConstructorWithComments [
        H._ConstructorWithComments_body>>: inject H._Constructor H._Constructor_record $ record H._RecordConstructor [
          H._RecordConstructor_name>>: HaskellUtils.simpleName @@ var "lname'",
          H._RecordConstructor_fields>>: var "hFields"],
        H._ConstructorWithComments_comments>>: nothing],
    "unionCons">: "g'" ~> "lname'" ~> "fieldType" ~> lets [
      "fname">: Core.fieldTypeName $ var "fieldType",
      "ftype">: Core.fieldTypeType $ var "fieldType",
      "deconflict">: "name" ~> lets [
        "tname">: Names.unqualifyName @@ record _QualifiedName [
          _QualifiedName_namespace>>: just $ Pairs.first $ Module.namespacesFocus $ var "namespaces",
          _QualifiedName_local>>: var "name"]] $
        Logic.ifElse (Maybes.isJust $ Maps.lookup (var "tname") (Graph.graphElements $ var "g'"))
          (var "deconflict" @@ Strings.cat2 (var "name") (string "_"))
          (var "name")] $
      "comments" <<~ Annotations.getTypeDescription @@ var "ftype" $ lets [
      "nm">: var "deconflict" @@ Strings.cat2 (Formatting.capitalize @@ var "lname'") (Formatting.capitalize @@ (Core.unName $ var "fname"))] $
      "typeList" <<~ (Logic.ifElse (Equality.equal (Rewriting.deannotateType @@ var "ftype") MetaTypes.unit)
        (Flows.pure $ list ([] :: [TTerm H.CaseRhs])) $
        Flows.bind (adaptTypeToHaskellAndEncode @@ var "namespaces" @@ var "ftype") $ "htype" ~>
          Flows.pure $ list [var "htype"]) $
      Flows.pure $ record H._ConstructorWithComments [
        H._ConstructorWithComments_body>>: inject H._Constructor H._Constructor_ordinary $ record H._OrdinaryConstructor [
          H._OrdinaryConstructor_name>>: HaskellUtils.simpleName @@ var "nm",
          H._OrdinaryConstructor_fields>>: var "typeList"],
        H._ConstructorWithComments_comments>>: var "comments"]] $
    Monads.withTrace @@ (Strings.cat2 (string "type definition ") (Core.unName $ var "elementName")) @@ (
      "g" <<~ Monads.getState $
      "isSer" <<~ (Schemas.isSerializableByName @@ var "elementName") $ lets [
      "deriv">: wrap H._Deriving $ Logic.ifElse (var "isSer")
        (Lists.map (HaskellUtils.rawName) (list [string "Eq", string "Ord", string "Read", string "Show"]))
        (list ([] :: [TTerm H.Name])),
      "unpackResult">: HaskellUtils.unpackForallType @@ var "g" @@ var "typ",
      "vars">: Pairs.first $ var "unpackResult",
      "t'">: Pairs.second $ var "unpackResult",
      "hd">: var "declHead" @@ var "hname" @@ (Lists.reverse $ var "vars")] $
      "decl" <<~ (cases _Type (Rewriting.deannotateType @@ var "t'")
        (Just $ "htype" <<~ (adaptTypeToHaskellAndEncode @@ var "namespaces" @@ var "typ") $
          Flows.pure $ inject H._Declaration H._Declaration_type $ record H._TypeDeclaration [
            H._TypeDeclaration_name>>: var "hd",
            H._TypeDeclaration_type>>: var "htype"]) [
        _Type_record>>: "rt" ~>
          "cons" <<~ (var "recordCons" @@ var "lname" @@ (Core.rowTypeFields $ var "rt")) $
          Flows.pure $ inject H._Declaration H._Declaration_data $ record H._DataDeclaration [
            H._DataDeclaration_keyword>>: injectUnit H._DataOrNewtype H._DataOrNewtype_data,
            H._DataDeclaration_context>>: list ([] :: [TTerm H.Assertion]),
            H._DataDeclaration_head>>: var "hd",
            H._DataDeclaration_constructors>>: list [var "cons"],
            H._DataDeclaration_deriving>>: list [var "deriv"]],
        _Type_union>>: "rt" ~>
          "cons" <<~ Flows.mapList (var "unionCons" @@ var "g" @@ var "lname") (Core.rowTypeFields $ var "rt") $
          Flows.pure $ inject H._Declaration H._Declaration_data $ record H._DataDeclaration [
            H._DataDeclaration_keyword>>: injectUnit H._DataOrNewtype H._DataOrNewtype_data,
            H._DataDeclaration_context>>: list ([] :: [TTerm H.Assertion]),
            H._DataDeclaration_head>>: var "hd",
            H._DataDeclaration_constructors>>: var "cons",
            H._DataDeclaration_deriving>>: list [var "deriv"]],
        _Type_wrap>>: "wrapped" ~> lets [
          "wt">: Core.wrappedTypeBody $ var "wrapped"] $
          "cons" <<~ var "newtypeCons" @@ var "elementName" @@ var "wt" $
            Flows.pure $ inject H._Declaration H._Declaration_data $ record H._DataDeclaration [
              H._DataDeclaration_keyword>>: injectUnit H._DataOrNewtype H._DataOrNewtype_newtype,
              H._DataDeclaration_context>>: list ([] :: [TTerm H.Assertion]),
              H._DataDeclaration_head>>: var "hd",
              H._DataDeclaration_constructors>>: list [var "cons"],
              H._DataDeclaration_deriving>>: list [var "deriv"]]]) $
      "comments" <<~ Annotations.getTypeDescription @@ var "typ" $
      "tdecls" <<~ (Logic.ifElse (includeTypeDefinitions)
        (Flows.bind (typeDecl @@ var "namespaces" @@ var "elementName" @@ var "typ") $ "decl'" ~>
          Flows.pure $ list [var "decl'"])
        (Flows.pure $ list ([] :: [TTerm H.DeclarationWithComments]))) $ lets [
      "mainDecl">: record H._DeclarationWithComments [
        H._DeclarationWithComments_body>>: var "decl",
        H._DeclarationWithComments_comments>>: var "comments"],
      "nameDecls'">: nameDecls @@ var "g" @@ var "namespaces" @@ var "elementName" @@ var "typ"] $
      Flows.pure $ Lists.concat $ list [list [var "mainDecl"], var "nameDecls'", var "tdecls"])

typeDecl :: TBinding (HaskellNamespaces -> Name -> Type -> Flow Graph H.DeclarationWithComments)
typeDecl = haskellCoderDefinition "typeDecl" $
  "namespaces" ~> "name" ~> "typ" ~> lets [
    "typeName">: "ns" ~> "name'" ~>
      Names.qname @@ var "ns" @@ (var "typeNameLocal" @@ var "name'"),
    "typeNameLocal">: "name'" ~>
      Strings.cat $ list [string "_", Names.localNameOf @@ var "name'", string "_type_"],
    "rawTerm">: encoderFor _Type @@ var "typ",
    "rewrite">: "recurse" ~> "term" ~> lets [
      "variantResult">: cases _Term (Rewriting.deannotateTerm @@ var "term")
        (Just nothing) [
        _Term_union>>: "inj" ~> Logic.ifElse (Equality.equal (Core.injectionTypeName $ var "inj") (Core.nameLift _Type))
          (just $ Core.injectionField $ var "inj")
          nothing],
      "decodeString">: "term" ~> (cases _Term (Rewriting.deannotateTerm @@ var "term")
        (Just nothing) [
        _Term_literal>>: "lit" ~> cases _Literal (var "lit")
          (Just nothing) [
          _Literal_string>>: "s" ~> just (var "s")]]),
      "decodeName">: "term" ~> (cases _Term (Rewriting.deannotateTerm @@ var "term")
        (Just nothing) [
        _Term_wrap>>: "wt" ~> Logic.ifElse (Equality.equal (Core.wrappedTermTypeName $ var "wt") (Core.nameLift _Name))
          (Maybes.map (unaryFunction Core.name) $ var "decodeString" @@ (Core.wrappedTermBody $ var "wt"))
          nothing]),
      "forType">: "field" ~> lets [
        "fname">: Core.fieldName $ var "field",
        "fterm">: Core.fieldTerm $ var "field"] $
        Logic.ifElse (Equality.equal (var "fname") $ Core.nameLift _Type_record)
          nothing
          (Logic.ifElse (Equality.equal (var "fname") $ Core.nameLift _Type_variable)
            (Maybes.bind (var "decodeName" @@ var "fterm") (var "forVariableType"))
            nothing),
      "forVariableType">: "vname" ~> lets [
        "qname">: Names.qualifyName @@ var "vname",
        "mns">: Module.qualifiedNameNamespace $ var "qname",
        "local">: Module.qualifiedNameLocal $ var "qname"] $
        Maybes.map ("ns" ~> Core.termVariable $ Names.qname @@ var "ns" @@ (Strings.cat $ list [string "_", var "local", string "_type_"])) (var "mns")] $
      Maybes.fromMaybe (var "recurse" @@ var "term") (Maybes.bind (var "variantResult") (var "forType")),
    "finalTerm">: Rewriting.rewriteTerm @@ var "rewrite" @@ var "rawTerm"] $
    -- Note: consider constructing this coder just once, then reusing it
    "coder" <<~ AdaptModules.constructCoder @@ (HaskellLanguage.haskellLanguage) @@ (encodeTerm @@ var "namespaces") @@ (Core.typeVariable $ Core.nameLift _Type) $
    "expr" <<~ Compute.coderEncode (var "coder") @@ var "finalTerm" $ lets [
    "rhs">: wrap H._RightHandSide $ var "expr",
    "hname">: HaskellUtils.simpleName @@ (var "typeNameLocal" @@ var "name"),
    "pat">: HaskellUtils.applicationPattern @@ var "hname" @@ list ([] :: [TTerm H.Pattern]),
    "decl">: inject H._Declaration H._Declaration_valueBinding $ inject H._ValueBinding H._ValueBinding_simple $ record H._SimpleValueBinding [
      H._SimpleValueBinding_pattern>>: var "pat",
      H._SimpleValueBinding_rhs>>: var "rhs",
      H._SimpleValueBinding_localBindings>>: nothing]] $
    Flows.pure $ record H._DeclarationWithComments [
      H._DeclarationWithComments_body>>: var "decl",
      H._DeclarationWithComments_comments>>: nothing]
