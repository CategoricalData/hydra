module Hydra.Sources.Tier3.Ext.Haskell.Coder where

-- Standard Tier-3 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors              as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Compute                    as Compute
import qualified Hydra.Dsl.Core                       as Core
import qualified Hydra.Dsl.Graph                      as Graph
import qualified Hydra.Dsl.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Lib.Equality               as Equality
import qualified Hydra.Dsl.Lib.Flows                  as Flows
import qualified Hydra.Dsl.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Lib.Literals               as Literals
import qualified Hydra.Dsl.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Lib.Math                   as Math
import qualified Hydra.Dsl.Lib.Optionals              as Optionals
import           Hydra.Dsl.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Lib.Strings                as Strings
import qualified Hydra.Dsl.Mantle                     as Mantle
import qualified Hydra.Dsl.Module                     as Module
import qualified Hydra.Dsl.TTerms                     as TTerms
import qualified Hydra.Dsl.TTypes                     as TTypes
import qualified Hydra.Dsl.Terms                      as Terms
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Types                      as Types
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Sources.Tier1.All              as Tier1
import qualified Hydra.Sources.Tier2.Constants        as Constants
import qualified Hydra.Sources.Tier2.Decoding           as Decoding
import qualified Hydra.Sources.Tier2.Encode.Core      as EncodeCore
import qualified Hydra.Sources.Tier2.Formatting       as Formatting
import qualified Hydra.Sources.Tier2.Literals         as Literals
import qualified Hydra.Sources.Tier2.Annotations            as Annotations
import qualified Hydra.Sources.Tier2.Adapt.Utils     as AdaptUtils
import qualified Hydra.Sources.Tier2.Adapt.Modules         as AdaptModules
import qualified Hydra.Sources.Tier2.Annotations      as Annotations
import qualified Hydra.Sources.Tier2.Arity            as Arity
import qualified Hydra.Sources.Tier2.Languages     as Languages
import qualified Hydra.Sources.Tier2.Decode.Core      as DecodeCore
import qualified Hydra.Sources.Tier2.Describe.Core    as DescribeCore
import qualified Hydra.Sources.Tier2.Extract.Core     as ExtractCore
import qualified Hydra.Sources.Tier2.Monads           as Monads
import qualified Hydra.Sources.Tier2.Grammars  as Grammars
import qualified Hydra.Sources.Tier2.Inference        as Inference
import qualified Hydra.Sources.Tier2.Lexical          as Lexical
import qualified Hydra.Sources.Tier2.Adapt.Literals  as AdaptLiterals
import qualified Hydra.Sources.Tier2.Names           as Names
import qualified Hydra.Sources.Tier2.Reduction        as Reduction
import qualified Hydra.Sources.Tier2.Rewriting        as Rewriting
import qualified Hydra.Sources.Tier2.Schemas          as Schemas
import qualified Hydra.Sources.Tier2.Serialization    as Serialization
import qualified Hydra.Sources.Tier2.Show.Accessors   as ShowAccessors
import qualified Hydra.Sources.Tier2.Show.Core        as ShowCore
import qualified Hydra.Sources.Tier2.Sorting          as Sorting
import qualified Hydra.Sources.Tier2.Substitution     as Substitution
import qualified Hydra.Sources.Tier2.Tarjan           as Tarjan
import qualified Hydra.Sources.Tier2.Templates       as Templates
import qualified Hydra.Sources.Tier2.Adapt.Terms     as AdaptTerms
import qualified Hydra.Sources.Tier2.Unification      as Unification
import qualified Hydra.Sources.Tier2.Variants         as Variants
import qualified Data.Int                             as I
import qualified Data.List                            as L
import qualified Data.Map                             as M
import qualified Data.Set                             as S
import qualified Data.Maybe                           as Y

-- Additional imports
import qualified Hydra.Ext.Haskell.Ast as H
import qualified Hydra.Sources.Tier3.Ext.Haskell.Language as HaskellLanguage
import qualified Hydra.Sources.Tier3.Ext.Haskell.Ast as HaskellAst
import qualified Hydra.Sources.Tier3.Ext.Haskell.Serde as HaskellSerde
import qualified Hydra.Sources.Tier3.Ext.Haskell.Utils as HaskellUtils


type HaskellNamespaces = Namespaces H.ModuleName

haskellCoderDefinition :: String -> TTerm a -> TElement a
haskellCoderDefinition = definitionInModule haskellCoderModule

haskellCoderModule :: Module
haskellCoderModule = Module ns elements
    [HaskellSerde.haskellSerdeModule, HaskellUtils.haskellUtilsModule,
      AdaptModules.adaptModulesModule, Rewriting.hydraRewritingModule, Serialization.hydraSerializationModule]
    [HaskellAst.haskellAstModule,
      Tier1.hydraAstModule, Tier1.hydraCodersModule, Tier1.hydraComputeModule, Tier1.hydraGraphModule,
      Tier1.hydraMantleModule, Tier1.hydraModuleModule, Tier1.hydraTopologyModule, Tier1.hydraTypingModule] $
    Just "Functions for encoding Hydra modules as Haskell modules"
  where
    ns = Namespace "hydra.ext.haskell.coder"
    elements = [
      el includeTypeDefinitionsDef,
      el useCoreImportDef,
      el keyHaskellVarDef,
      el adaptTypeToHaskellAndEncodeDef,
      el constantForFieldNameDef,
      el constantForTypeNameDef,
      el constructModuleDef,
      el encodeFunctionDef,
      el encodeLiteralDef,
      el encodeTermDef,
      el encodeTypeDef,
      el encodeTypeWithClassAssertionsDef,
      el findOrdVariablesDef,
      el getImplicitTypeClassesDef,
      el moduleToHaskellModuleDef,
      el moduleToHaskellDef,
      el nameDeclsDef,
      el toDataDeclarationDef,
      el toTypeDeclarationsDef,
      el typeDeclDef]

-- TODO: make these settings configurable
includeTypeDefinitionsDef :: TElement Bool
includeTypeDefinitionsDef = haskellCoderDefinition "includeTypeDefinitions" $
  false
useCoreImportDef :: TElement Bool
useCoreImportDef = haskellCoderDefinition "useCoreImport" $
  true

keyHaskellVarDef :: TElement Name
keyHaskellVarDef = haskellCoderDefinition "keyHaskellVar" $
  wrap _Name $ string "haskellVar"

adaptTypeToHaskellAndEncodeDef :: TElement (HaskellNamespaces -> Type -> Flow Graph H.Type)
adaptTypeToHaskellAndEncodeDef = haskellCoderDefinition "adaptTypeToHaskellAndEncode" $
  lambda "namespaces" $
    ref AdaptModules.adaptAndEncodeTypeDef @@ (ref HaskellLanguage.haskellLanguageDef) @@ (ref encodeTypeDef @@ var "namespaces")

constantForFieldNameDef :: TElement (Name -> Name -> String)
constantForFieldNameDef = haskellCoderDefinition "constantForFieldName" $
  lambda "tname" $ lambda "fname" $
    Strings.cat $ list [
      string "_",
      ref Names.localNameOfDef @@ var "tname",
      string "_",
      Core.unName $ var "fname"]

constantForTypeNameDef :: TElement (Name -> String)
constantForTypeNameDef = haskellCoderDefinition "constantForTypeName" $
  lambda "tname" $
    Strings.cat2 (string "_") (ref Names.localNameOfDef @@ var "tname")

constructModuleDef :: TElement (HaskellNamespaces -> Module -> M.Map Type (Coder Graph Graph Term H.Expression) -> [(Element, TypedTerm)] -> Flow Graph H.Module)
constructModuleDef = haskellCoderDefinition "constructModule" $ lambdas ["namespaces", "mod", "coders", "pairs"] $ lets [
  "h">: lambda "namespace" $
    unwrap _Namespace @@ var "namespace",
  "createDeclarations">: lambda "g" $ lambda "pair" $ lets [
    "el">: first $ var "pair",
    "tt">: second $ var "pair",
    "term">: Core.typedTermTerm $ var "tt",
    "typ">: Core.typedTermType $ var "tt"] $
    Logic.ifElse (ref Annotations.isNativeTypeDef @@ var "el")
      (ref toTypeDeclarationsDef @@ var "namespaces" @@ var "el" @@ var "term")
      (withVar "d" (ref toDataDeclarationDef @@ var "coders" @@ var "namespaces" @@ var "pair") $
        Flows.pure $ list [var "d"]),
    "importName">: lambda "name" $
      wrap H._ModuleName $ Strings.intercalate (string ".") (Lists.map (ref Formatting.capitalizeDef) (Strings.splitOn (string ".") (var "name"))),
    "imports">: Lists.concat2 (var "domainImports") (var "standardImports"),
    "domainImports">: lets [
      "toImport">: lambda "pair" $ lets [
        "namespace">: first $ var "pair",
        "alias">: second $ var "pair",
        "name">: var "h" @@ var "namespace"] $
        record H._Import [
          H._Import_qualified>>: true,
          H._Import_module>>: var "importName" @@ var "name",
          H._Import_as>>: just $ var "alias",
          H._Import_spec>>: nothing]] $
      Lists.map (var "toImport") (Maps.toList $ Module.namespacesMapping $ var "namespaces"),
    "standardImports">: lets [
      "toImport">: lambda "triple" $ lets [
        "name">: first $ first $ var "triple",
        "malias">: second $ first $ var "triple",
        "hidden">: second $ var "triple",
        "spec">: Logic.ifElse (Lists.null $ var "hidden")
          nothing
          (just $ inject H._SpecImport H._SpecImport_hiding $ Lists.map (lambda "n" $ record H._ImportExportSpec [
            H._ImportExportSpec_modifier>>: nothing,
            H._ImportExportSpec_name>>: ref HaskellUtils.simpleNameDef @@ var "n",
            H._ImportExportSpec_subspec>>: nothing]) (var "hidden"))] $
        record H._Import [
          H._Import_qualified>>: Optionals.isJust $ var "malias",
          H._Import_module>>: wrap H._ModuleName $ var "name",
          H._Import_as>>: Optionals.map (unaryFunction $ wrap H._ModuleName) (var "malias"),
          H._Import_spec>>: var "spec"]] $
      Lists.map (var "toImport") $ list [
        pair (pair (string "Prelude") nothing) (list $ string <$> [
          "Enum", "Ordering", "fail", "map", "pure", "sum"]),
        pair (pair (string "Data.Int") (just $ string "I")) (list []),
        pair (pair (string "Data.List") (just $ string "L")) (list []),
        pair (pair (string "Data.Map") (just $ string "M")) (list []),
        pair (pair (string "Data.Set") (just $ string "S")) (list [])]] $
    withVar "g" (ref Monads.getStateDef) $
    withVar "declLists" (Flows.mapList (var "createDeclarations" @@ var "g") (var "pairs")) $ lets [
    "decls">: Lists.concat $ var "declLists",
    "mc">: Module.moduleDescription $ var "mod"] $
    Flows.pure $ record H._Module [
      H._Module_head>>: just $ record H._ModuleHead [
        H._ModuleHead_comments>>: var "mc",
        H._ModuleHead_name>>: var "importName" @@ (var "h" @@ (Module.moduleNamespace $ var "mod")),
        H._ModuleHead_exports>>: list []],
      H._Module_imports>>: var "imports",
      H._Module_declarations>>: var "decls"]

encodeFunctionDef :: TElement (HaskellNamespaces -> Function -> Flow Graph H.Expression)
encodeFunctionDef = haskellCoderDefinition "encodeFunction" $
  lambda "namespaces" $ lambda "fun" $
    cases _Function (var "fun") Nothing [
      _Function_elimination>>: lambda "e" $
        cases _Elimination (var "e") Nothing [
          _Elimination_wrap>>: lambda "name" $
            Flows.pure $ inject H._Expression H._Expression_variable $ ref HaskellUtils.elementReferenceDef @@ var "namespaces" @@
              (ref Names.qnameDef @@ (Optionals.fromJust $ ref Names.namespaceOfDef @@ var "name") @@ (ref HaskellUtils.newtypeAccessorNameDef @@ var "name")),
          _Elimination_product>>: lambda "proj" $ lets [
            "arity">: Core.tupleProjectionArity $ var "proj",
            "idx">: Core.tupleProjectionIndex $ var "proj"] $
            Logic.ifElse (Equality.equal (var "arity") (int32 2))
              (Flows.pure $ ref HaskellUtils.hsvarDef @@ Logic.ifElse (Equality.equal (var "idx") (int32 0)) (string "fst") (string "snd"))
              (Flows.fail $ string "Eliminations for tuples of arity > 2 are not supported yet in the Haskell coder"),
          _Elimination_record>>: lambda "proj" $ lets [
            "dn">: Core.projectionTypeName $ var "proj",
            "fname">: Core.projectionField $ var "proj"] $
            Flows.pure $ inject H._Expression H._Expression_variable $ ref HaskellUtils.recordFieldReferenceDef @@ var "namespaces" @@ var "dn" @@ var "fname",
          _Elimination_union>>: lambda "stmt" $ lets [
            "dn">: Core.caseStatementTypeName $ var "stmt",
            "def">: Core.caseStatementDefault $ var "stmt",
            "fields">: Core.caseStatementCases $ var "stmt",
            "caseExpr">:
              withVar "rt" (ref Lexical.withSchemaContextDef @@ (ref Schemas.requireUnionTypeDef @@ var "dn")) $ lets [
              "fieldMap">: Maps.fromList $ Lists.map (var "toFieldMapEntry") (Core.rowTypeFields $ var "rt"),
              "toFieldMapEntry">: lambda "f" $
                pair (Core.fieldTypeName $ var "f") (var "f")] $
              withVar "ecases" (Flows.mapList (var "toAlt" @@ var "fieldMap") (var "fields")) $
              withVar "dcases" (Optionals.cases (var "def")
                (Flows.pure $ list []) $
                (lambda "d" $
                  withVar "cs" (Flows.map (unaryFunction $ wrap H._CaseRhs) $ ref encodeTermDef @@ var "namespaces" @@ var "d") $ lets [
                  "lhs">: inject H._Pattern H._Pattern_name $ ref HaskellUtils.rawNameDef @@ (ref Constants.ignoredVariableDef),
                  "alt">: record H._Alternative [
                    H._Alternative_pattern>>: var "lhs",
                    H._Alternative_rhs>>: var "cs",
                    H._Alternative_binds>>: nothing]] $
                  Flows.pure $ list [var "alt"])) $
              Flows.pure $ inject H._Expression H._Expression_case $ record H._CaseExpression [
                H._CaseExpression_case>>: ref HaskellUtils.hsvarDef @@ string "x",
                H._CaseExpression_alternatives>>: Lists.concat2 (var "ecases") (var "dcases")],
              "toAlt">: lambdas ["fieldMap", "field"] $ lets [
                "fn">: Core.fieldName $ var "field",
                "fun'">: Core.fieldTerm $ var "field"] $
                ref Annotations.withDepthDef @@ ref keyHaskellVarDef @@ (
                  lambda "depth" $ lets [
                    "v0">: Strings.cat2 (string "v") (Literals.showInt32 $ var "depth"),
                    "raw">: TTerms.apply (var "fun'") (Core.termVariable $ Core.name $ var "v0"),
                    "rhsTerm">: ref Rewriting.simplifyTermDef @@ var "raw",
                    "v1">: Logic.ifElse (ref Rewriting.isFreeVariableInTermDef @@ (wrap _Name $ var "v0") @@ var "rhsTerm")
                      (ref Constants.ignoredVariableDef)
                      (var "v0"),
                    "hname">: ref HaskellUtils.unionFieldReferenceDef @@ var "namespaces" @@ var "dn" @@ var "fn"] $
                    withVar "args"
                      (Optionals.cases (Maps.lookup (var "fn") (var "fieldMap"))
                        (Flows.fail $ Strings.cat $ list [string "field ", Literals.showString $ (Core.unName $ var "fn"),
                          string " not found in ", Literals.showString $ (Core.unName $ var "dn")])
                        (lambda "fieldType" $ lets [
                          "ft">: Core.fieldTypeType $ var "fieldType",
                          "noArgs">: Flows.pure $ list [],
                          "singleArg">: Flows.pure $ list [inject H._Pattern H._Pattern_name $ ref HaskellUtils.rawNameDef @@ var "v1"]] $
                          cases _Type (ref Rewriting.stripTypeDef @@ var "ft")
                            (Just $ var "singleArg") [
                            _Type_unit>>: constant $ var "noArgs"])) $ lets [
                    "lhs">: ref HaskellUtils.applicationPatternDef @@ var "hname" @@ var "args"] $
                    withVar "rhs" (Flows.map (unaryFunction $ wrap H._CaseRhs) $ ref encodeTermDef @@ var "namespaces" @@ var "rhsTerm") $
                    Flows.pure $ record H._Alternative [
                      H._Alternative_pattern>>: var "lhs",
                      H._Alternative_rhs>>: var "rhs",
                      H._Alternative_binds>>: nothing])] $
            Flows.map (ref HaskellUtils.hslambdaDef @@ (ref HaskellUtils.rawNameDef @@ string "x")) (var "caseExpr")],
      _Function_lambda>>: lambda "lam" $ lets [
        "v">: Core.lambdaParameter $ var "lam",
        "body">: Core.lambdaBody $ var "lam"] $
        Flows.bind (ref encodeTermDef @@ var "namespaces" @@ var "body") $ lambda "hbody" $
          Flows.pure $ ref HaskellUtils.hslambdaDef @@ (ref HaskellUtils.elementReferenceDef @@ var "namespaces" @@ var "v") @@ var "hbody",
      _Function_primitive>>: lambda "name" $
        Flows.pure $ inject H._Expression H._Expression_variable $ ref HaskellUtils.elementReferenceDef @@ var "namespaces" @@ var "name"]

encodeLiteralDef :: TElement (Literal -> Flow Graph H.Expression)
encodeLiteralDef = haskellCoderDefinition "encodeLiteral" $
  lambda "l" $
    cases _Literal (var "l")
      (Just $ Flows.fail $ Strings.cat2 (string "literal value ") (ref ShowCore.literalDef @@ var "l")) [
      _Literal_boolean>>: lambda "b" $
        Flows.pure $ ref HaskellUtils.hsvarDef @@ Logic.ifElse (var "b") (string "True") (string "False"),
      _Literal_float>>: lambda "fv" $
        cases _FloatValue (var "fv") Nothing [
          _FloatValue_float32>>: lambda "f" $
            Flows.pure $ ref HaskellUtils.hslitDef @@ (inject H._Literal H._Literal_float $ var "f"),
          _FloatValue_float64>>: lambda "f" $
            Flows.pure $ ref HaskellUtils.hslitDef @@ (inject H._Literal H._Literal_double $ var "f"),
          _FloatValue_bigfloat>>: lambda "f" $
            Flows.pure $ ref HaskellUtils.hslitDef @@ (inject H._Literal H._Literal_double $ Literals.bigfloatToFloat64 $ var "f")],
      _Literal_integer>>: lambda "iv" $
        cases _IntegerValue (var "iv") Nothing [
          _IntegerValue_bigint>>: lambda "i" $
            Flows.pure $ ref HaskellUtils.hslitDef @@ (inject H._Literal H._Literal_integer $ var "i"),
          _IntegerValue_int8>>: lambda "i" $
            Flows.pure $ ref HaskellUtils.hslitDef @@ (inject H._Literal H._Literal_integer $ Literals.int8ToBigint $ var "i"),
          _IntegerValue_int16>>: lambda "i" $
            Flows.pure $ ref HaskellUtils.hslitDef @@ (inject H._Literal H._Literal_integer $ Literals.int16ToBigint $ var "i"),
          _IntegerValue_int32>>: lambda "i" $
            Flows.pure $ ref HaskellUtils.hslitDef @@ (inject H._Literal H._Literal_int $ var "i"),
          _IntegerValue_int64>>: lambda "i" $
            Flows.pure $ ref HaskellUtils.hslitDef @@ (inject H._Literal H._Literal_integer $ Literals.int64ToBigint $ var "i"),
          _IntegerValue_uint8>>: lambda "i" $
            Flows.pure $ ref HaskellUtils.hslitDef @@ (inject H._Literal H._Literal_integer $ Literals.uint8ToBigint $ var "i"),
          _IntegerValue_uint16>>: lambda "i" $
            Flows.pure $ ref HaskellUtils.hslitDef @@ (inject H._Literal H._Literal_integer $ Literals.uint16ToBigint $ var "i"),
          _IntegerValue_uint32>>: lambda "i" $
            Flows.pure $ ref HaskellUtils.hslitDef @@ (inject H._Literal H._Literal_integer $ Literals.uint32ToBigint $ var "i"),
          _IntegerValue_uint64>>: lambda "i" $
            Flows.pure $ ref HaskellUtils.hslitDef @@ (inject H._Literal H._Literal_integer $ Literals.uint64ToBigint $ var "i")],
      _Literal_string>>: lambda "s" $
        Flows.pure $ ref HaskellUtils.hslitDef @@ (inject H._Literal H._Literal_string $ var "s")]

encodeTermDef :: TElement (HaskellNamespaces -> Term -> Flow Graph H.Expression)
encodeTermDef = haskellCoderDefinition "encodeTerm" $
  lambda "namespaces" $ lambda "term" $ lets [
    "encode">: ref encodeTermDef @@ var "namespaces"] $
    cases _Term (ref Rewriting.stripTermDef @@ var "term")
      (Just $ Flows.fail $ Strings.cat2 (string "unexpected term: ") (ref ShowCore.termDef @@ var "term")) [
      _Term_application>>: lambda "app" $ lets [
        "fun">: Core.applicationFunction $ var "app",
        "arg">: Core.applicationArgument $ var "app"] $
        Flows.bind (var "encode" @@ var "fun") $ lambda "hfun" $
          Flows.bind (var "encode" @@ var "arg") $ lambda "harg" $
            Flows.pure $ ref HaskellUtils.hsappDef @@ var "hfun" @@ var "harg",
      _Term_function>>: lambda "f" $
        ref encodeFunctionDef @@ var "namespaces" @@ var "f",
      _Term_let>>: lambda "letTerm" $ lets [
        "bindings">: Core.letBindings $ var "letTerm",
        "env">: Core.letEnvironment $ var "letTerm",
        "encodeBinding">: lambda "binding" $ lets [
          "name">: Core.letBindingName $ var "binding",
          "term'">: Core.letBindingTerm $ var "binding",
          "hname">: ref HaskellUtils.simpleNameDef @@ (Core.unName $ var "name")] $
          withVar "hexpr" (var "encode" @@ var "term'") $
          Flows.pure $ inject H._LocalBinding H._LocalBinding_value $ ref HaskellUtils.simpleValueBindingDef @@ var "hname" @@ var "hexpr" @@ nothing] $
        withVar "hbindings" (Flows.mapList (var "encodeBinding") (var "bindings")) $
        withVar "hinner" (var "encode" @@ var "env") $
        Flows.pure $ inject H._Expression H._Expression_let $ record H._LetExpression [
          H._LetExpression_bindings>>: var "hbindings",
          H._LetExpression_inner>>: var "hinner"],
      _Term_list>>: lambda "els" $
        Flows.bind (Flows.mapList (var "encode") (var "els")) $ lambda "helems" $
          Flows.pure $ inject H._Expression H._Expression_list $ var "helems",
      _Term_literal>>: lambda "v" $
        ref encodeLiteralDef @@ var "v",
      _Term_map>>: lambda "m" $ lets [
        "lhs">: ref HaskellUtils.hsvarDef @@ string "M.fromList",
        "encodePair">: lambda "pair" $ lets [
          "k">: first $ var "pair",
          "v">: second $ var "pair",
          "hk">: var "encode" @@ var "k",
          "hv">: var "encode" @@ var "v"] $
          Flows.map
            (unaryFunction $ inject H._Expression H._Expression_tuple)
            (Flows.sequence $ list [var "hk", var "hv"])] $
        withVar "rhs" (Flows.map
          (unaryFunction $ inject H._Expression H._Expression_list)
          (Flows.mapList (var "encodePair") $ Maps.toList (var "m"))) $
        Flows.pure $ ref HaskellUtils.hsappDef @@ var "lhs" @@ var "rhs",
      _Term_optional>>: lambda "m" $
        Optionals.cases (var "m")
          (Flows.pure $ ref HaskellUtils.hsvarDef @@ string "Nothing") $
          lambda "t" $
            Flows.bind (var "encode" @@ var "t") $ lambda "ht" $
              Flows.pure $ ref HaskellUtils.hsappDef @@ (ref HaskellUtils.hsvarDef @@ string "Just") @@ var "ht",
      _Term_product>>: lambda "terms" $
        Flows.bind (Flows.mapList (var "encode") (var "terms")) $ lambda "hterms" $
          Flows.pure $ inject H._Expression H._Expression_tuple $ var "hterms",
      _Term_record>>: lambda "record" $ lets [
        "sname">: Core.recordTypeName $ var "record",
        "fields">: Core.recordFields $ var "record",
        "toFieldUpdate">: lambda "field" $ lets [
          "fn">: Core.fieldName $ var "field",
          "ft">: Core.fieldTerm $ var "field",
          "fieldRef">: ref HaskellUtils.recordFieldReferenceDef @@ var "namespaces" @@ var "sname" @@ var "fn"] $
          withVar "hft" (var "encode" @@ var "ft") $
          Flows.pure $ record H._FieldUpdate [
            H._FieldUpdate_name>>: var "fieldRef",
            H._FieldUpdate_value>>: var "hft"],
          "typeName">: ref HaskellUtils.elementReferenceDef @@ var "namespaces" @@ var "sname"] $
          withVar "updates" (Flows.mapList (var "toFieldUpdate") (var "fields")) $
          Flows.pure $ inject H._Expression H._Expression_constructRecord $ record H._ConstructRecordExpression [
            H._ConstructRecordExpression_name>>: var "typeName",
            H._ConstructRecordExpression_fields>>: var "updates"],
      _Term_set>>: lambda "s" $ lets [
        "lhs">: ref HaskellUtils.hsvarDef @@ string "S.fromList" ] $
        withVar "rhs" (ref encodeTermDef @@ var "namespaces" @@ (inject _Term _Term_list $ Sets.toList $ var "s")) $
        Flows.pure $ ref HaskellUtils.hsappDef @@ var "lhs" @@ var "rhs",
      _Term_typeAbstraction>>: lambda "abs" $ lets [
        "term1">: Core.typeAbstractionBody $ var "abs"] $
        var "encode" @@ var "term1",
      _Term_typeApplication>>: lambda "typed" $ lets [
        "term1">: Core.typedTermTerm $ var "typed"] $
        var "encode" @@ var "term1",
      _Term_union>>: lambda "injection" $ lets [
        "sname">: Core.injectionTypeName $ var "injection",
        "field">: Core.injectionField $ var "injection",
        "fn">: Core.fieldName $ var "field",
        "ft">: Core.fieldTerm $ var "field",
        "lhs">: inject H._Expression H._Expression_variable $ ref HaskellUtils.unionFieldReferenceDef @@ var "namespaces" @@ var "sname" @@ var "fn",
        "dflt">: Flows.map (ref HaskellUtils.hsappDef @@ var "lhs") (var "encode" @@ var "ft")] $
        cases _Term (ref Rewriting.stripTermDef @@ var "ft")
          (Just $ var "dflt") [
          _Term_unit>>: constant $ Flows.pure $ var "lhs"],
      _Term_unit>>: constant $ Flows.pure $ inject H._Expression H._Expression_tuple $ list [],
      _Term_variable>>: lambda "name" $
        Flows.pure $ inject H._Expression H._Expression_variable $ ref HaskellUtils.elementReferenceDef @@ var "namespaces" @@ var "name",
      _Term_wrap>>: lambda "wrapped" $ lets [
        "tname">: Core.wrappedTermTypeName $ var "wrapped",
        "term'">: Core.wrappedTermObject $ var "wrapped",
        "lhs">: inject H._Expression H._Expression_variable $ ref HaskellUtils.elementReferenceDef @@ var "namespaces" @@ var "tname"] $
        withVar "rhs" (var "encode" @@ var "term'") $
        Flows.pure $ ref HaskellUtils.hsappDef @@ var "lhs" @@ var "rhs"]

encodeTypeDef :: TElement (HaskellNamespaces -> Type -> Flow Graph H.Type)
encodeTypeDef = haskellCoderDefinition "encodeType" $
  lambda "namespaces" $ lambda "typ" $ lets [
    "encode">: ref encodeTypeDef @@ var "namespaces",
    "ref">: lambda "name" $
      Flows.pure $ inject H._Type H._Type_variable $ ref HaskellUtils.elementReferenceDef @@ var "namespaces" @@ var "name",
    "unitTuple">: inject H._Type H._Type_tuple $ list []] $
    ref Monads.withTraceDef @@ (string "encode type") @@
      (cases _Type (ref Rewriting.stripTypeDef @@ var "typ")
        (Just $ Flows.fail $ Strings.cat2 (string "unexpected type: ") (ref ShowCore.typeDef @@ var "typ")) [
        _Type_application>>: lambda "app" $ lets [
          "lhs">: Core.applicationTypeFunction $ var "app",
          "rhs">: Core.applicationTypeArgument $ var "app"] $
          Flows.bind (var "encode" @@ var "lhs") $ lambda "hlhs" $
            Flows.bind (var "encode" @@ var "rhs") $ lambda "hrhs" $
              Flows.pure $ ref HaskellUtils.toTypeApplicationDef @@ list [var "hlhs", var "hrhs"],
        _Type_function>>: lambda "funType" $ lets [
          "dom">: Core.functionTypeDomain $ var "funType",
          "cod">: Core.functionTypeCodomain $ var "funType"] $
          Flows.bind (var "encode" @@ var "dom") $ lambda "hdom" $
            Flows.bind (var "encode" @@ var "cod") $ lambda "hcod" $
              Flows.pure $ inject H._Type H._Type_function $ record H._FunctionType [
                H._FunctionType_domain>>: var "hdom",
                H._FunctionType_codomain>>: var "hcod"],
        _Type_forall>>: lambda "forallType" $ lets [
          "v">: Core.forallTypeParameter $ var "forallType",
          "body">: Core.forallTypeBody $ var "forallType"] $
          var "encode" @@ var "body",
        _Type_list>>: lambda "lt" $
          Flows.bind (var "encode" @@ var "lt") $ lambda "hlt" $
            Flows.pure $ inject H._Type H._Type_list $ var "hlt",
        _Type_literal>>: lambda "lt" $
          cases _LiteralType (var "lt")
            (Just $ Flows.fail $ Strings.cat2 (string "unexpected literal type: ") (ref ShowCore.literalTypeDef @@ var "lt")) [
            _LiteralType_boolean>>: constant $
              Flows.pure $ inject H._Type H._Type_variable $ ref HaskellUtils.rawNameDef @@ string "Bool",
            _LiteralType_float>>: lambda "ft" $
              cases _FloatType (var "ft") Nothing [
                _FloatType_float32>>: constant $
                  Flows.pure $ inject H._Type H._Type_variable $ ref HaskellUtils.rawNameDef @@ string "Float",
                _FloatType_float64>>: constant $
                  Flows.pure $ inject H._Type H._Type_variable $ ref HaskellUtils.rawNameDef @@ string "Double",
                _FloatType_bigfloat>>: constant $
                  Flows.pure $ inject H._Type H._Type_variable $ ref HaskellUtils.rawNameDef @@ string "Double"],
            _LiteralType_integer>>: lambda "it" $
              cases _IntegerType (var "it")
                (Just $ Flows.fail $ Strings.cat2 (string "unexpected integer type: ") (ref ShowCore.integerTypeDef @@ var "it")) [
                _IntegerType_bigint>>: constant $
                  Flows.pure $ inject H._Type H._Type_variable $ ref HaskellUtils.rawNameDef @@ string "Integer",
                _IntegerType_int8>>: constant $
                  Flows.pure $ inject H._Type H._Type_variable $ ref HaskellUtils.rawNameDef @@ string "I.Int8",
                _IntegerType_int16>>: constant $
                  Flows.pure $ inject H._Type H._Type_variable $ ref HaskellUtils.rawNameDef @@ string "I.Int16",
                _IntegerType_int32>>: constant $
                  Flows.pure $ inject H._Type H._Type_variable $ ref HaskellUtils.rawNameDef @@ string "Int",
                _IntegerType_int64>>: constant $
                  Flows.pure $ inject H._Type H._Type_variable $ ref HaskellUtils.rawNameDef @@ string "I.Int64"],
            _LiteralType_string>>: constant $
              Flows.pure $ inject H._Type H._Type_variable $ ref HaskellUtils.rawNameDef @@ string "String"],
        _Type_map>>: lambda "mapType" $ lets [
          "kt">: Core.mapTypeKeys $ var "mapType",
          "vt">: Core.mapTypeValues $ var "mapType"] $
          Flows.map
            (ref HaskellUtils.toTypeApplicationDef)
            (Flows.sequence $ list [
              Flows.pure $ inject H._Type H._Type_variable $ ref HaskellUtils.rawNameDef @@ string "M.Map",
              var "encode" @@ var "kt",
              var "encode" @@ var "vt"]),
        _Type_optional>>: lambda "ot" $
          Flows.map
            (ref HaskellUtils.toTypeApplicationDef)
            (Flows.sequence $ list [
              Flows.pure $ inject H._Type H._Type_variable $ ref HaskellUtils.rawNameDef @@ string "Maybe",
              var "encode" @@ var "ot"]),
        _Type_product>>: lambda "types" $
          Flows.bind (Flows.mapList (var "encode") (var "types")) $ lambda "htypes" $
            Flows.pure $ inject H._Type H._Type_tuple $ var "htypes",
        _Type_record>>: lambda "rt" $ var "ref" @@ (Core.rowTypeTypeName $ var "rt"),
        _Type_set>>: lambda "st" $
          Flows.map
            (ref HaskellUtils.toTypeApplicationDef)
            (Flows.sequence $ list [
              Flows.pure $ inject H._Type H._Type_variable $ ref HaskellUtils.rawNameDef @@ string "S.Set",
              var "encode" @@ var "st"]),
        _Type_union>>: lambda "rt" $ lets [
          "typeName">: Core.rowTypeTypeName $ var "rt"] $
          var "ref" @@ var "typeName",
        _Type_unit>>: constant $ Flows.pure $ var "unitTuple",
        _Type_variable>>: lambda "v1" $ var "ref" @@ var "v1",
        _Type_wrap>>: lambda "wrapped" $ lets [
          "name">: Core.wrappedTypeTypeName $ var "wrapped"] $
          var "ref" @@ var "name"])

encodeTypeWithClassAssertionsDef :: TElement (HaskellNamespaces -> M.Map Name (S.Set TypeClass) -> Type -> Flow Graph H.Type)
encodeTypeWithClassAssertionsDef = haskellCoderDefinition "encodeTypeWithClassAssertions" $
  lambda "namespaces" $ lambda "explicitClasses" $ lambda "typ" $ lets [
    "classes">: Maps.union (var "explicitClasses") (ref getImplicitTypeClassesDef @@ var "typ"),
    "implicitClasses">: ref getImplicitTypeClassesDef @@ var "typ",
    "encodeAssertion">: lambda "pair" $ lets [
      "name">: first $ var "pair",
      "cls">: second $ var "pair",
      "hname">: ref HaskellUtils.rawNameDef @@ cases _TypeClass (var "cls") Nothing [
        _TypeClass_equality>>: constant $ string "Eq",
        _TypeClass_ordering>>: constant $ string "Ord"],
      "htype">: inject H._Type H._Type_variable $ ref HaskellUtils.rawNameDef @@ (Core.unName $ var "name")] $
      inject H._Assertion H._Assertion_class $ record H._ClassAssertion [
        H._ClassAssertion_name>>: var "hname",
        H._ClassAssertion_types>>: list [var "htype"]],
    "assertPairs">: Lists.concat $ Lists.map (var "toPairs") (Maps.toList $ var "classes"),
    "toPairs">: lambda "mapEntry" $ lets [
      "name">: first $ var "mapEntry",
      "clsSet">: second $ var "mapEntry",
      "toPair">: lambda "c" $
        pair (var "name") (var "c")] $
      Lists.map (var "toPair") (Sets.toList $ var "clsSet")] $
    ref Monads.withTraceDef @@ (string "encode with assertions") @@
      (withVar "htyp" (ref adaptTypeToHaskellAndEncodeDef @@ var "namespaces" @@ var "typ") $
      Logic.ifElse (Lists.null $ var "assertPairs")
        (Flows.pure $ var "htyp") (lets [
          "encoded">: Lists.map (var "encodeAssertion") (var "assertPairs"),
          "hassert">: Logic.ifElse (Equality.gt (Lists.length $ var "encoded") (int32 1))
            (Lists.head $ var "encoded")
            (inject H._Assertion H._Assertion_tuple $ var "encoded")] $
          Flows.pure $ inject H._Type H._Type_ctx $ record H._ContextType [
            H._ContextType_ctx>>: var "hassert",
            H._ContextType_type>>: var "htyp"]))

findOrdVariablesDef :: TElement (Type -> S.Set Name)
findOrdVariablesDef = haskellCoderDefinition "findOrdVariables" $
  lambda "typ" $ lets [
    "fold">: lambda "names" $ lambda "typ'" $
      cases _Type (var "typ'")
        (Just $ var "names") [
        _Type_map>>: lambda "mapType" $ lets [
          "kt">: Core.mapTypeKeys $ var "mapType"] $
          var "tryType" @@ var "names" @@ var "kt",
        _Type_set>>: lambda "et" $
          var "tryType" @@ var "names" @@ var "et"],
    "isTypeVariable">: lambda "v" $ lets [
      "nameStr">: Core.unName $ var "v",
      "hasNoNamespace">: Optionals.isNothing $ ref Names.namespaceOfDef @@ var "v",
      "startsWithT">: Equality.equal (Strings.charAt (int32 0) (var "nameStr")) (int32 116)] $ -- 't' character
      Logic.and (var "hasNoNamespace") (var "startsWithT"),
    "tryType">: lambda "names" $ lambda "t" $
      cases _Type (ref Rewriting.stripTypeDef @@ var "t")
        (Just $ var "names") [
        _Type_variable>>: lambda "v" $
          Logic.ifElse (var "isTypeVariable" @@ var "v")
            (Sets.insert (var "v") (var "names"))
            (var "names")]] $
    ref Rewriting.foldOverTypeDef @@ Coders.traversalOrderPre @@ var "fold" @@ Sets.empty @@ var "typ"

getImplicitTypeClassesDef :: TElement (Type -> M.Map Name (S.Set TypeClass))
getImplicitTypeClassesDef = haskellCoderDefinition "getImplicitTypeClasses" $
  lambda "typ" $ lets [
    "toPair">: lambda "name" $
      pair (var "name") (Sets.fromList $ list [Graph.typeClassOrdering])] $
    Maps.fromList $ Lists.map (var "toPair") (Sets.toList $ ref findOrdVariablesDef @@ var "typ")

moduleToHaskellModuleDef :: TElement (Module -> Flow Graph H.Module)
moduleToHaskellModuleDef = haskellCoderDefinition "moduleToHaskellModule" $
  lambda "mod" $
    withVar "namespaces" (ref HaskellUtils.namespacesForModuleDef @@ var "mod") $
      ref AdaptModules.transformModuleDef @@ (ref HaskellLanguage.haskellLanguageDef) @@ (ref encodeTermDef @@ var "namespaces") @@ (ref constructModuleDef @@ var "namespaces") @@ var "mod"

moduleToHaskellDef :: TElement (Module -> Flow Graph (M.Map String String))
moduleToHaskellDef = haskellCoderDefinition "moduleToHaskell" $ lambda "mod" $
  withVar "hsmod" (ref moduleToHaskellModuleDef @@ var "mod") $ lets [
  "s">: ref Serialization.printExprDef @@ (ref Serialization.parenthesizeDef @@ (ref HaskellSerde.moduleToExprDef @@ var "hsmod")),
  "filepath">: ref Names.namespaceToFilePathDef @@ Mantle.caseConventionPascal @@ (wrap _FileExtension $ string "hs") @@ (Module.moduleNamespace $ var "mod")] $
  Flows.pure $ Maps.singleton (var "filepath") (var "s")

nameDeclsDef :: TElement (Graph -> HaskellNamespaces -> Name -> Type -> [H.DeclarationWithComments])
nameDeclsDef = haskellCoderDefinition "nameDecls" $
  lambda "g" $ lambda "namespaces" $ lambda "name" $ lambda "typ" $ lets [
    "nm">: Core.unName $ var "name",
    "toDecl">: lambda "n" $ lambda "pair" $ lets [
      "k">: first $ var "pair",
      "v">: second $ var "pair",
      "decl">: inject H._Declaration H._Declaration_valueBinding $ inject H._ValueBinding H._ValueBinding_simple $ record H._SimpleValueBinding [
        H._SimpleValueBinding_pattern>>: ref HaskellUtils.applicationPatternDef @@ (ref HaskellUtils.simpleNameDef @@ var "k") @@ list [],
        H._SimpleValueBinding_rhs>>: wrap H._RightHandSide $ inject H._Expression H._Expression_application $ record H._ApplicationExpression [
          H._ApplicationExpression_function>>: inject H._Expression H._Expression_variable $ ref HaskellUtils.elementReferenceDef @@ var "namespaces" @@ var "n",
          H._ApplicationExpression_argument>>: inject H._Expression H._Expression_literal $ inject H._Literal H._Literal_string $ var "v"],
        H._SimpleValueBinding_localBindings>>: nothing]] $
      record H._DeclarationWithComments [
        H._DeclarationWithComments_body>>: var "decl",
        H._DeclarationWithComments_comments>>: nothing],
    "nameDecl">: pair (ref constantForTypeNameDef @@ var "name") (var "nm"),
    "fieldDecls">: Lists.map (var "toConstant") (ref Lexical.fieldsOfDef @@ var "typ"),
    "toConstant">: lambda "fieldType" $ lets [
      "fname">: Core.fieldTypeName $ var "fieldType"] $
      pair (ref constantForFieldNameDef @@ var "name" @@ var "fname") (Core.unName $ var "fname")] $
    Logic.ifElse (ref useCoreImportDef)
      (Lists.cons (var "toDecl" @@ Core.nameLift _Name @@ var "nameDecl") (Lists.map (var "toDecl" @@ Core.nameLift _Name) (var "fieldDecls")))
      (list [])

toDataDeclarationDef :: TElement (M.Map Type (Coder Graph Graph Term H.Expression) -> HaskellNamespaces -> (Element, TypedTerm) -> Flow Graph H.DeclarationWithComments)
toDataDeclarationDef = haskellCoderDefinition "toDataDeclaration" $
  lambdas ["coders", "namespaces", "pair"] $ lets [
    "el">: first $ var "pair",
    "tt">: second $ var "pair",
    "term">: Core.typedTermTerm $ var "tt",
    "typ">: Core.typedTermType $ var "tt",
    "coder">: Optionals.fromJust $ Maps.lookup (var "typ") (var "coders"),
    "hname">: ref HaskellUtils.simpleNameDef @@ (ref Names.localNameOfDef @@ (Graph.elementName $ var "el")),
    "rewriteValueBinding">: lambda "vb" $
      cases H._ValueBinding (var "vb") Nothing [
        H._ValueBinding_simple>>: lambda "simple" $ lets [
          "pattern'">: project H._SimpleValueBinding H._SimpleValueBinding_pattern @@ var "simple",
          "rhs">: project H._SimpleValueBinding H._SimpleValueBinding_rhs @@ var "simple",
          "bindings">: project H._SimpleValueBinding H._SimpleValueBinding_localBindings @@ var "simple"] $
          cases H._Pattern (var "pattern'")
            (Just $ var "vb") [
            H._Pattern_application>>: lambda "appPat" $ lets [
              "name'">: project H._ApplicationPattern H._ApplicationPattern_name @@ var "appPat",
              "args">: project H._ApplicationPattern H._ApplicationPattern_args @@ var "appPat",
              "rhsExpr">: unwrap H._RightHandSide @@ var "rhs"] $
              cases H._Expression (var "rhsExpr")
                (Just $ var "vb") [
                H._Expression_lambda>>: lambda "lambda'" $ lets [
                  "vars">: project H._LambdaExpression H._LambdaExpression_bindings @@ var "lambda'",
                  "body">: project H._LambdaExpression H._LambdaExpression_inner @@ var "lambda'",
                  "newPattern">: ref HaskellUtils.applicationPatternDef @@ var "name'" @@ (Lists.concat2 (var "args") (var "vars")),
                  "newRhs">: wrap H._RightHandSide $ var "body"] $
                  var "rewriteValueBinding" @@ (inject H._ValueBinding H._ValueBinding_simple $ record H._SimpleValueBinding [
                    H._SimpleValueBinding_pattern>>: var "newPattern",
                    H._SimpleValueBinding_rhs>>: var "newRhs",
                    H._SimpleValueBinding_localBindings>>: var "bindings"])]]],
    "toDecl">: lambda "comments" $ lambda "hname'" $ lambda "term'" $ lambda "coder'" $ lambda "bindings" $
      cases _Term (ref Rewriting.stripTermDef @@ var "term'")
        (Just $
          withVar "hterm" (Compute.coderEncode (var "coder'") @@ (var "term'")) $ lets [
         "vb">: ref HaskellUtils.simpleValueBindingDef @@ var "hname'" @@ var "hterm" @@ var "bindings"] $
         withVar "explicitClasses" (ref Annotations.getTypeClassesDef @@ (ref Rewriting.stripTypesFromTermDef @@ (Graph.elementTerm $ var "el"))) $
         withVar "htype" (ref encodeTypeWithClassAssertionsDef @@ var "namespaces" @@ var "explicitClasses" @@ var "typ") $ lets [
         "decl">: inject H._Declaration H._Declaration_typedBinding $ record H._TypedBinding [
           H._TypedBinding_typeSignature>>: record H._TypeSignature [
             H._TypeSignature_name>>: var "hname'",
             H._TypeSignature_type>>: var "htype"],
           H._TypedBinding_valueBinding>>: var "rewriteValueBinding" @@ var "vb"]] $
          Flows.pure $ record H._DeclarationWithComments [
            H._DeclarationWithComments_body>>: var "decl",
            H._DeclarationWithComments_comments>>: var "comments"]) [
        _Term_let>>: lambda "letTerm" $ lets [
          -- A "let" constant cannot be predicted in advance, so we construct a coder on the fly.
          -- This makes program code with "let" terms more expensive to transform than simple data.
          "lbindings">: Core.letBindings $ var "letTerm",
          "env">: Core.letEnvironment $ var "letTerm",
          "toBinding">: lambda "hname''" $ lambda "hterm'" $
            inject H._LocalBinding H._LocalBinding_value $ ref HaskellUtils.simpleValueBindingDef @@ var "hname''" @@ var "hterm'" @@ nothing,
          "ts">: Lists.map (lambda "binding" $ Core.typeSchemeType $ Optionals.fromJust $ Core.letBindingType $ var "binding") (var "lbindings")] $
          withVar "coders'" (Flows.mapList (lambda "t" $ ref AdaptModules.constructCoderDef @@ (ref HaskellLanguage.haskellLanguageDef) @@ (ref encodeTermDef @@ var "namespaces") @@ var "t") (var "ts")) $ lets [
          "hnames">: Lists.map (lambda "binding" $ ref HaskellUtils.simpleNameDef @@ (Core.unName $ Core.letBindingName $ var "binding")) (var "lbindings"),
          "terms">: Lists.map (unaryFunction $ Core.letBindingTerm) (var "lbindings")] $
          withVar "hterms" (Flows.sequence $ Lists.zipWith (lambdas ["e", "t"] $ Compute.coderEncode (var "e") @@ var "t") (var "coders'") (var "terms")) $ lets [
          "hbindings">: Lists.zipWith (var "toBinding") (var "hnames") (var "hterms")] $
          var "toDecl" @@ var "comments" @@ var "hname'" @@ var "env" @@ var "coder'" @@ (just $ wrap H._LocalBindings $ var "hbindings")]] $
    withVar "comments" (ref Annotations.getTermDescriptionDef @@ var "term") $
    var "toDecl" @@ var "comments" @@ var "hname" @@ var "term" @@ var "coder" @@ nothing

toTypeDeclarationsDef :: TElement (HaskellNamespaces -> Element -> Term -> Flow Graph [H.DeclarationWithComments])
toTypeDeclarationsDef = haskellCoderDefinition "toTypeDeclarations" $
  lambda "namespaces" $ lambda "el" $ lambda "term" $ lets [
    "elementName">: Graph.elementName $ var "el",
    "lname">: ref Names.localNameOfDef @@ var "elementName",
    "hname">: ref HaskellUtils.simpleNameDef @@ var "lname",
    "declHead">: lambdas ["name", "vars'"] $ Logic.ifElse (Lists.null $ var "vars'")
      (inject H._DeclarationHead H._DeclarationHead_simple $ var "name")
      (lets [
        "h">: Lists.head $ var "vars'",
        "rest">: Lists.tail $ var "vars'",
        "hvar">: wrap H._Variable $ ref HaskellUtils.simpleNameDef @@ (Core.unName $ var "h")] $
        inject H._DeclarationHead H._DeclarationHead_application $ record H._ApplicationDeclarationHead [
          H._ApplicationDeclarationHead_function>>: var "declHead" @@ var "name" @@ var "rest",
          H._ApplicationDeclarationHead_operand>>: var "hvar"]),
    "newtypeCons">: lambda "el'" $ lambda "typ'" $ lets [
      "hname">: ref HaskellUtils.simpleNameDef @@ (ref HaskellUtils.newtypeAccessorNameDef @@ (Graph.elementName $ var "el'"))] $
      withVar "htype" (ref adaptTypeToHaskellAndEncodeDef @@ var "namespaces" @@ var "typ'") $ lets [
      "hfield">: record H._FieldWithComments [
        H._FieldWithComments_field>>: record H._Field [
          H._Field_name>>: var "hname",
          H._Field_type>>: var "htype"],
        H._FieldWithComments_comments>>: nothing],
      "constructorName">: ref HaskellUtils.simpleNameDef @@ (ref Names.localNameOfDef @@ (Graph.elementName $ var "el'"))] $
      Flows.pure $ record H._ConstructorWithComments [
        H._ConstructorWithComments_body>>: inject H._Constructor H._Constructor_record $ record H._RecordConstructor [
          H._RecordConstructor_name>>: var "constructorName",
          H._RecordConstructor_fields>>: list [var "hfield"]],
        H._ConstructorWithComments_comments>>: nothing],
    "recordCons">: lambda "lname'" $ lambda "fields" $ lets [
      "toField">: lambda "fieldType" $ lets [
        "fname">: Core.fieldTypeName $ var "fieldType",
        "ftype">: Core.fieldTypeType $ var "fieldType",
        "hname'">: ref HaskellUtils.simpleNameDef @@ Strings.cat2
          (ref Formatting.decapitalizeDef @@ var "lname'")
          (ref Formatting.capitalizeDef @@ (Core.unName $ var "fname"))] $
        withVar "htype" (ref adaptTypeToHaskellAndEncodeDef @@ var "namespaces" @@ var "ftype") $
        withVar "comments" (ref Annotations.getTypeDescriptionDef @@ var "ftype") $
        Flows.pure $ record H._FieldWithComments [
          H._FieldWithComments_field>>: record H._Field [
            H._Field_name>>: var "hname'",
            H._Field_type>>: var "htype"],
          H._FieldWithComments_comments>>: var "comments"]] $
      withVar "hFields" (Flows.mapList (var "toField") (var "fields")) $
      Flows.pure $ record H._ConstructorWithComments [
        H._ConstructorWithComments_body>>: inject H._Constructor H._Constructor_record $ record H._RecordConstructor [
          H._RecordConstructor_name>>: ref HaskellUtils.simpleNameDef @@ var "lname'",
          H._RecordConstructor_fields>>: var "hFields"],
        H._ConstructorWithComments_comments>>: nothing],
    "unionCons">: lambda "g'" $ lambda "lname'" $ lambda "fieldType" $ lets [
      "fname">: Core.fieldTypeName $ var "fieldType",
      "ftype">: Core.fieldTypeType $ var "fieldType",
      "deconflict">: lambda "name" $ lets [
        "tname">: ref Names.unqualifyNameDef @@ record _QualifiedName [
          _QualifiedName_namespace>>: just $ first $ Module.namespacesFocus $ var "namespaces",
          _QualifiedName_local>>: var "name"]] $
        Logic.ifElse (Optionals.isJust $ Maps.lookup (var "tname") (Graph.graphElements $ var "g'"))
          (var "deconflict" @@ Strings.cat2 (var "name") (string "_"))
          (var "name")] $
      withVar "comments" (ref Annotations.getTypeDescriptionDef @@ var "ftype") $ lets [
      "nm">: var "deconflict" @@ Strings.cat2 (ref Formatting.capitalizeDef @@ var "lname'") (ref Formatting.capitalizeDef @@ (Core.unName $ var "fname"))] $
      withVar "typeList" (Logic.ifElse (Equality.equal (ref Rewriting.stripTypeDef @@ var "ftype") TTypes.unit)
        (Flows.pure $ list []) $
        Flows.bind (ref adaptTypeToHaskellAndEncodeDef @@ var "namespaces" @@ var "ftype") $ lambda "htype" $
          Flows.pure $ list [var "htype"]) $
      Flows.pure $ record H._ConstructorWithComments [
        H._ConstructorWithComments_body>>: inject H._Constructor H._Constructor_ordinary $ record H._OrdinaryConstructor [
          H._OrdinaryConstructor_name>>: ref HaskellUtils.simpleNameDef @@ var "nm",
          H._OrdinaryConstructor_fields>>: var "typeList"],
        H._ConstructorWithComments_comments>>: var "comments"]] $
    ref Monads.withTraceDef @@ (Strings.cat2 (string "type element ") (Core.unName $ var "elementName")) @@ (
      withVar "g" (ref Monads.getStateDef) $
      withVar "t" (ref DecodeCore.typeDef @@ var "term") $
      withVar "isSer" (ref Schemas.isSerializableDef @@ var "el") $ lets [
      "deriv">: wrap H._Deriving $ Logic.ifElse (var "isSer")
        (Lists.map (ref HaskellUtils.rawNameDef) (list [string "Eq", string "Ord", string "Read", string "Show"]))
        (list []),
      "unpackResult">: ref HaskellUtils.unpackForallTypeDef @@ var "g" @@ var "t",
      "vars">: first $ var "unpackResult",
      "t'">: second $ var "unpackResult",
      "hd">: var "declHead" @@ var "hname" @@ (Lists.reverse $ var "vars")] $
      withVar "decl" (cases _Type (ref Rewriting.stripTypeDef @@ var "t'")
        (Just $ withVar "htype" (ref adaptTypeToHaskellAndEncodeDef @@ var "namespaces" @@ var "t") $
          Flows.pure $ inject H._Declaration H._Declaration_type $ record H._TypeDeclaration [
            H._TypeDeclaration_name>>: var "hd",
            H._TypeDeclaration_type>>: var "htype"]) [
        _Type_record>>: lambda "rt" $
          withVar "cons" (var "recordCons" @@ var "lname" @@ (Core.rowTypeFields $ var "rt")) $
          Flows.pure $ inject H._Declaration H._Declaration_data $ record H._DataDeclaration [
            H._DataDeclaration_keyword>>: unitVariant H._DataOrNewtype H._DataOrNewtype_data,
            H._DataDeclaration_context>>: list [],
            H._DataDeclaration_head>>: var "hd",
            H._DataDeclaration_constructors>>: list [var "cons"],
            H._DataDeclaration_deriving>>: list [var "deriv"]],
        _Type_union>>: lambda "rt" $
          withVar "cons" (Flows.mapList (var "unionCons" @@ var "g" @@ var "lname") (Core.rowTypeFields $ var "rt")) $
          Flows.pure $ inject H._Declaration H._Declaration_data $ record H._DataDeclaration [
            H._DataDeclaration_keyword>>: unitVariant H._DataOrNewtype H._DataOrNewtype_data,
            H._DataDeclaration_context>>: list [],
            H._DataDeclaration_head>>: var "hd",
            H._DataDeclaration_constructors>>: var "cons",
            H._DataDeclaration_deriving>>: list [var "deriv"]],
        _Type_wrap>>: lambda "wrapped" $ lets [
          "tname">: Core.wrappedTypeTypeName $ var "wrapped",
          "wt">: Core.wrappedTypeObject $ var "wrapped"] $
          withVar "cons" (var "newtypeCons" @@ var "el" @@ var "wt") $
            Flows.pure $ inject H._Declaration H._Declaration_data $ record H._DataDeclaration [
              H._DataDeclaration_keyword>>: unitVariant H._DataOrNewtype H._DataOrNewtype_newtype,
              H._DataDeclaration_context>>: list [],
              H._DataDeclaration_head>>: var "hd",
              H._DataDeclaration_constructors>>: list [var "cons"],
              H._DataDeclaration_deriving>>: list [var "deriv"]]]) $
      withVar "comments" (ref Annotations.getTermDescriptionDef @@ var "term") $
      withVar "tdecls" (Logic.ifElse (ref includeTypeDefinitionsDef)
        (Flows.bind (ref typeDeclDef @@ var "namespaces" @@ var "elementName" @@ var "t") $ lambda "decl'" $
          Flows.pure $ list [var "decl'"])
        (Flows.pure $ list [])) $ lets [
      "mainDecl">: record H._DeclarationWithComments [
        H._DeclarationWithComments_body>>: var "decl",
        H._DeclarationWithComments_comments>>: var "comments"],
      "nameDecls'">: ref nameDeclsDef @@ var "g" @@ var "namespaces" @@ var "elementName" @@ var "t"] $
      Flows.pure $ Lists.concat $ list [list [var "mainDecl"], var "nameDecls'", var "tdecls"])

typeDeclDef :: TElement (HaskellNamespaces -> Name -> Type -> Flow Graph H.DeclarationWithComments)
typeDeclDef = haskellCoderDefinition "typeDecl" $
  lambda "namespaces" $ lambda "name" $ lambda "typ" $ lets [
    "typeName">: lambda "ns" $ lambda "name'" $
      ref Names.qnameDef @@ var "ns" @@ (var "typeNameLocal" @@ var "name'"),
    "typeNameLocal">: lambda "name'" $
      Strings.cat $ list [string "_", ref Names.localNameOfDef @@ var "name'", string "_type_"],
    "rawTerm">: ref EncodeCore.typeDef @@ var "typ",
    "rewrite">: lambda "recurse" $ lambda "term" $ lets [
      "variantResult">: ref Decoding.variantDef @@ Core.nameLift _Type @@ var "term",
      "forType">: lambda "field" $ lets [
        "fname">: Core.fieldName $ var "field",
        "fterm">: Core.fieldTerm $ var "field"] $
        Logic.ifElse (Equality.equal (var "fname") $ Core.nameLift _Type_record)
          nothing
          (Logic.ifElse (Equality.equal (var "fname") $ Core.nameLift _Type_variable)
            (Optionals.bind (ref Decoding.nameDef @@ var "fterm") (var "forVariableType"))
            nothing),
      "forVariableType">: lambda "name''" $ lets [
        "qname">: ref Names.qualifyNameDef @@ var "name''",
        "mns">: Module.qualifiedNameNamespace $ var "qname",
        "local">: Module.qualifiedNameLocal $ var "qname"] $
        Optionals.map (lambda "ns" $ Core.termVariable $ ref Names.qnameDef @@ var "ns" @@ (Strings.cat $ list [string "_", var "local", string "_type_"])) (var "mns")] $
      Optionals.fromMaybe (var "recurse" @@ var "term") (Optionals.bind (var "variantResult") (var "forType")),
    "finalTerm">: ref Rewriting.rewriteTermDef @@ var "rewrite" @@ var "rawTerm"] $
    -- Note: consider constructing this coder just once, then reusing it
    withVar "coder" (ref AdaptModules.constructCoderDef @@ (ref HaskellLanguage.haskellLanguageDef) @@ (ref encodeTermDef @@ var "namespaces") @@ (Core.typeVariable $ Core.nameLift _Type)) $
    withVar "expr" (Compute.coderEncode (var "coder") @@ var "finalTerm") $ lets [
    "rhs">: wrap H._RightHandSide $ var "expr",
    "hname">: ref HaskellUtils.simpleNameDef @@ (var "typeNameLocal" @@ var "name"),
    "pat">: ref HaskellUtils.applicationPatternDef @@ var "hname" @@ list [],
    "decl">: inject H._Declaration H._Declaration_valueBinding $ inject H._ValueBinding H._ValueBinding_simple $ record H._SimpleValueBinding [
      H._SimpleValueBinding_pattern>>: var "pat",
      H._SimpleValueBinding_rhs>>: var "rhs",
      H._SimpleValueBinding_localBindings>>: nothing]] $
    Flows.pure $ record H._DeclarationWithComments [
      H._DeclarationWithComments_body>>: var "decl",
      H._DeclarationWithComments_comments>>: nothing]
