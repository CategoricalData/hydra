
module Hydra.Sources.Haskell.Coder where

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
import qualified Hydra.Dsl.Accessors                  as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Grammar                    as Grammar
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
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
import qualified Hydra.Dsl.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Errors                     as Error
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
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
import qualified Hydra.Ext.Haskell.Syntax as H
import qualified Hydra.Sources.Haskell.Language as HaskellLanguage
import qualified Hydra.Sources.Haskell.Syntax as HaskellSyntax
import qualified Hydra.Sources.Haskell.Serde as HaskellSerde
import qualified Hydra.Sources.Haskell.Utils as HaskellUtils
import qualified Hydra.Sources.Kernel.Terms.Show.Errors as ShowError


formatError :: TTerm (InContext Error -> String)
formatError = "ic" ~> ShowError.error_ @@ Ctx.inContextObject (var "ic")

-- | Lift Either String to Either (InContext Error) using a context
liftStringError :: TTerm Context -> TTerm (Either String a) -> TTerm (Either (InContext Error) a)
liftStringError cx = Eithers.bimap ("_s" ~> Ctx.inContext (Error.errorOther $ Error.otherError $ var "_s") cx) ("_x" ~> var "_x")

type HaskellNamespaces = Namespaces H.ModuleName

haskellCoderDefinition :: String -> TTerm a -> TBinding a
haskellCoderDefinition = definitionInModule module_

module_ :: Module
module_ = Module ns elements
    [HaskellSerde.ns, HaskellUtils.ns,
      Adapt.ns, Rewriting.ns, Serialization.ns, ShowError.ns]
    (HaskellSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Functions for encoding Hydra modules as Haskell modules"
  where
    ns = Namespace "hydra.ext.haskell.coder"
    elements = [
      toTermDefinition includeTypeDefinitions,
      toTermDefinition useCoreImport,
      toTermDefinition keyHaskellVar,
      toTermDefinition adaptTypeToHaskellAndEncode,
      toTermDefinition constantForFieldName,
      toTermDefinition constantForTypeName,
      toTermDefinition constructModule,
      toTermDefinition encodeCaseExpression,
      toTermDefinition encodeFunction,
      toTermDefinition encodeLiteral,
      toTermDefinition encodeTerm,
      toTermDefinition encodeType,
      toTermDefinition encodeTypeWithClassAssertions,
      toTermDefinition findOrdVariables,
      toTermDefinition getImplicitTypeClasses,
      toTermDefinition moduleToHaskellModule,
      toTermDefinition moduleToHaskell,
      toTermDefinition nameDecls,
      toTermDefinition toDataDeclaration,
--      toTermDefinition toTypeDeclarations,
      toTermDefinition toTypeDeclarationsFrom,
      toTermDefinition typeDecl,
      toTermDefinition typeSchemeConstraintsToClassMap]

-- TODO: make these settings configurable
includeTypeDefinitions :: TBinding Bool
includeTypeDefinitions = haskellCoderDefinition "includeTypeDefinitions" $
  doc "Whether to include type definitions in generated Haskell modules" $
  false
useCoreImport :: TBinding Bool
useCoreImport = haskellCoderDefinition "useCoreImport" $
  doc "Whether to use the Hydra core import in generated modules" $
  true

keyHaskellVar :: TBinding Name
keyHaskellVar = haskellCoderDefinition "keyHaskellVar" $
  doc "The key used to track Haskell variable depth in annotations" $
  wrap _Name $ string "haskellVar"

adaptTypeToHaskellAndEncode :: TBinding (HaskellNamespaces -> Type -> Context -> Graph -> Either (InContext Error) H.Type)
adaptTypeToHaskellAndEncode = haskellCoderDefinition "adaptTypeToHaskellAndEncode" $
  doc "Adapt a Hydra type to Haskell's type system and encode it" $
  "namespaces" ~> "typ" ~> "cx" ~> "g" ~>
  "enc" <~ ("t" ~> encodeType @@ var "namespaces" @@ var "t" @@ var "cx" @@ var "g") $
  cases _Type (Rewriting.deannotateType @@ var "typ")
    (Just (
      "adaptedType" <<~ liftStringError (var "cx") (Adapt.adaptTypeForLanguage @@ HaskellLanguage.haskellLanguage @@ var "typ") $
      var "enc" @@ var "adaptedType")) [
    _Type_variable>>: constant (var "enc" @@ var "typ")]

constantForFieldName :: TBinding (Name -> Name -> String)
constantForFieldName = haskellCoderDefinition "constantForFieldName" $
  doc "Generate a constant name for a field (e.g., '_TypeName_fieldName')" $
  "tname" ~> "fname" ~>
    Strings.cat $ list [
      string "_",
      Names.localNameOf @@ var "tname",
      string "_",
      Core.unName $ var "fname"]

constantForTypeName :: TBinding (Name -> String)
constantForTypeName = haskellCoderDefinition "constantForTypeName" $
  doc "Generate a constant name for a type (e.g., '_TypeName')" $
  "tname" ~>
    Strings.cat2 (string "_") (Names.localNameOf @@ var "tname")

constructModule :: TBinding (HaskellNamespaces -> Module -> [Definition] -> Context -> Graph -> Either (InContext Error) H.Module)
constructModule = haskellCoderDefinition "constructModule" $
  doc "Construct a Haskell module from a Hydra module and its definitions" $
  "namespaces" ~> "mod" ~> "defs" ~> "cx" ~> "g" ~> lets [
  "h">: "namespace" ~>
    unwrap _Namespace @@ var "namespace",
  "createDeclarations">: "def" ~>
    cases _Definition (var "def") Nothing [
      _Definition_type>>: "type" ~> lets [
        "name">: Module.typeDefinitionName $ var "type",
        "typ">: Module.typeDefinitionType $ var "type"] $
        toTypeDeclarationsFrom @@ var "namespaces" @@ var "name" @@ var "typ" @@ var "cx" @@ var "g",
      _Definition_term>>: "term" ~>
        "d" <<~ toDataDeclaration @@ var "namespaces" @@ var "term" @@ var "cx" @@ var "g" $
        right $ list [var "d"]],
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
      Lists.map (var "toImport") $ Lists.concat2
        (list [
          pair (pair (string "Prelude") nothing) (list $ string <$> [
            "Enum", "Ordering", "decodeFloat", "encodeFloat", "fail", "map", "pure", "sum"]),
          pair (pair (string "Data.ByteString") (just $ string "B")) (list ([] :: [TTerm String])),
          pair (pair (string "Data.Int") (just $ string "I")) (list ([] :: [TTerm String])),
          pair (pair (string "Data.List") (just $ string "L")) (list ([] :: [TTerm String])),
          pair (pair (string "Data.Map") (just $ string "M")) (list ([] :: [TTerm String])),
          pair (pair (string "Data.Set") (just $ string "S")) (list ([] :: [TTerm String]))])
        -- Conditionally add Hydra.Lib.Literals import if binary literals are present
        (Logic.ifElse (Schemas.moduleContainsBinaryLiterals @@ var "mod")
          (list [pair (pair (string "Hydra.Lib.Literals") (just $ string "Literals")) (list ([] :: [TTerm String]))])
          (list ([] :: [TTerm ((String, Maybe String), [String])])))] $
    "declLists" <<~ Eithers.mapList (var "createDeclarations") (var "defs") $ lets [
    "decls">: Lists.concat $ var "declLists",
    "mc">: Module.moduleDescription $ var "mod"] $
    right $ record H._Module [
      H._Module_head>>: just $ record H._ModuleHead [
        H._ModuleHead_comments>>: var "mc",
        H._ModuleHead_name>>: var "importName" @@ (var "h" @@ (Module.moduleNamespace $ var "mod")),
        H._ModuleHead_exports>>: list ([] :: [TTerm H.Export])],
      H._Module_imports>>: var "imports",
      H._Module_declarations>>: var "decls"]

encodeCaseExpression :: TBinding (Int -> HaskellNamespaces -> CaseStatement -> H.Expression -> Context -> Graph -> Either (InContext Error) H.Expression)
encodeCaseExpression = haskellCoderDefinition "encodeCaseExpression" $
  doc "Encode a Hydra case statement as a Haskell case expression with a given scrutinee" $
  "depth" ~> "namespaces" ~> "stmt" ~> "scrutinee" ~> "cx" ~> "g" ~> lets [
    "dn">: Core.caseStatementTypeName $ var "stmt",
    "def">: Core.caseStatementDefault $ var "stmt",
    "fields">: Core.caseStatementCases $ var "stmt",
    "toAlt">: "fieldMap" ~> "field" ~> lets [
      "fn">: Core.fieldName $ var "field",
      "fun'">: Core.fieldTerm $ var "field",
      "v0">: Strings.cat2 (string "v") (Literals.showInt32 $ var "depth"),
      "raw">: MetaTerms.apply (var "fun'") (Core.termVariable $ Core.name $ var "v0"),
      "rhsTerm">: Rewriting.simplifyTerm @@ var "raw",
      "v1">: Logic.ifElse (Rewriting.isFreeVariableInTerm @@ (wrap _Name $ var "v0") @@ var "rhsTerm")
        (Constants.ignoredVariable)
        (var "v0"),
      "hname">: HaskellUtils.unionFieldReference @@ (Sets.union (Sets.fromList (Maps.keys (Graph.graphBoundTerms $ var "g"))) (Sets.fromList (Maps.keys (Graph.graphSchemaTypes $ var "g")))) @@ var "namespaces" @@ var "dn" @@ var "fn"] $
          "args" <<~ (Maybes.cases (Maps.lookup (var "fn") (var "fieldMap"))
              (Ctx.failInContext (Error.errorOther $ Error.otherError (Strings.cat $ list [string "field ", Literals.showString $ (Core.unName $ var "fn"),
                string " not found in ", Literals.showString $ (Core.unName $ var "dn")])) (var "cx")) $
              "fieldType" ~> lets [
                "ft">: Core.fieldTypeType $ var "fieldType",
                "noArgs">: list ([] :: [TTerm H.Pattern]),
                "singleArg">: list [inject H._Pattern H._Pattern_name $ HaskellUtils.rawName @@ var "v1"]] $
                cases _Type (Rewriting.deannotateType @@ var "ft")
                  (Just $ right $ var "singleArg") [
                  _Type_unit>>: constant $ right $ var "noArgs"]) $ lets [
          "lhs">: HaskellUtils.applicationPattern @@ var "hname" @@ var "args"] $
          "rhs" <<~ Eithers.map (unaryFunction $ wrap H._CaseRhs) (encodeTerm @@ (Math.add (var "depth") (int32 1)) @@ var "namespaces" @@ var "rhsTerm" @@ var "cx" @@ var "g") $
          right $ record H._Alternative [
            H._Alternative_pattern>>: var "lhs",
            H._Alternative_rhs>>: var "rhs",
            H._Alternative_binds>>: nothing]] $
    "rt" <<~ Schemas.requireUnionType @@ var "cx" @@ var "g" @@ var "dn" $ lets [
    "toFieldMapEntry">: "f" ~>
      pair (Core.fieldTypeName $ var "f") (var "f"),
    "fieldMap">: Maps.fromList $ Lists.map (var "toFieldMapEntry") (var "rt")] $
    "ecases" <<~ Eithers.mapList (var "toAlt" @@ var "fieldMap") (var "fields") $
    "dcases" <<~ (Maybes.cases (var "def")
      (right $ list ([] :: [TTerm H.CaseRhs])) $
      "d" ~>
        "cs" <<~ Eithers.map (unaryFunction $ wrap H._CaseRhs) (encodeTerm @@ var "depth" @@ var "namespaces" @@ var "d" @@ var "cx" @@ var "g") $ lets [
        "lhs">: inject H._Pattern H._Pattern_name $ HaskellUtils.rawName @@ (Constants.ignoredVariable),
        "alt">: record H._Alternative [
          H._Alternative_pattern>>: var "lhs",
          H._Alternative_rhs>>: var "cs",
          H._Alternative_binds>>: nothing]] $
        right $ list [var "alt"]) $
    right $ inject H._Expression H._Expression_case $ record H._CaseExpression [
      H._CaseExpression_case>>: var "scrutinee",
      H._CaseExpression_alternatives>>: Lists.concat2 (var "ecases") (var "dcases")]

encodeFunction :: TBinding (Int -> HaskellNamespaces -> Function -> Context -> Graph -> Either (InContext Error) H.Expression)
encodeFunction = haskellCoderDefinition "encodeFunction" $
  doc "Encode a Hydra function as a Haskell expression" $
  "depth" ~> "namespaces" ~> "fun" ~> "cx" ~> "g" ~>
    cases _Function (var "fun") Nothing [
      _Function_elimination>>: "e" ~>
        cases _Elimination (var "e") Nothing [
          _Elimination_wrap>>: "name" ~>
            right $ inject H._Expression H._Expression_variable $ HaskellUtils.elementReference @@ var "namespaces" @@
              (Names.qname @@ (Maybes.fromJust $ Names.namespaceOf @@ var "name") @@ (HaskellUtils.newtypeAccessorName @@ var "name")),
          _Elimination_record>>: "proj" ~> lets [
            "dn">: Core.projectionTypeName $ var "proj",
            "fname">: Core.projectionField $ var "proj"] $
            right $ inject H._Expression H._Expression_variable $ HaskellUtils.recordFieldReference @@ var "namespaces" @@ var "dn" @@ var "fname",
          _Elimination_union>>: "stmt" ~>
            -- When used standalone (not applied to an argument), wrap in a lambda
            Eithers.map (HaskellUtils.hslambda @@ (HaskellUtils.rawName @@ string "x"))
              (encodeCaseExpression @@ var "depth" @@ var "namespaces" @@ var "stmt" @@ (HaskellUtils.hsvar @@ string "x") @@ var "cx" @@ var "g")],
      _Function_lambda>>: "lam" ~> lets [
        "v">: Core.lambdaParameter $ var "lam",
        "body">: Core.lambdaBody $ var "lam"] $
        "hbody" <<~ encodeTerm @@ var "depth" @@ var "namespaces" @@ var "body" @@ var "cx" @@ var "g" $
          right $ HaskellUtils.hslambda @@ (HaskellUtils.elementReference @@ var "namespaces" @@ var "v") @@ var "hbody",
      _Function_primitive>>: "name" ~>
        right $ inject H._Expression H._Expression_variable $ HaskellUtils.elementReference @@ var "namespaces" @@ var "name"]

encodeLiteral :: TBinding (Literal -> Context -> Either (InContext Error) H.Expression)
encodeLiteral = haskellCoderDefinition "encodeLiteral" $
  doc "Encode a Hydra literal as a Haskell expression" $
  "l" ~> "cx" ~>
    cases _Literal (var "l")
      (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError (Strings.cat2 (string "literal value ") (ShowCore.literal @@ var "l"))) (var "cx")) [
      _Literal_binary>>: "bs" ~>
        right $ HaskellUtils.hsapp
          @@ (HaskellUtils.hsvar @@ string "Literals.stringToBinary")
          @@ (HaskellUtils.hslit @@ (inject H._Literal H._Literal_string
              $ Literals.binaryToString $ var "bs")),
      _Literal_boolean>>: "b" ~>
        right $ HaskellUtils.hsvar @@ Logic.ifElse (var "b") (string "True") (string "False"),
      _Literal_float>>: "fv" ~>
        cases _FloatValue (var "fv") Nothing [
          _FloatValue_float32>>: "f" ~>
            right $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_float $ var "f"),
          _FloatValue_float64>>: "f" ~>
            right $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_double $ var "f"),
          _FloatValue_bigfloat>>: "f" ~>
            right $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_double $ Literals.bigfloatToFloat64 $ var "f")],
      _Literal_integer>>: "iv" ~>
        cases _IntegerValue (var "iv") Nothing [
          _IntegerValue_bigint>>: "i" ~>
            right $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_integer $ var "i"),
          _IntegerValue_int8>>: "i" ~>
            right $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_integer $ Literals.int8ToBigint $ var "i"),
          _IntegerValue_int16>>: "i" ~>
            right $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_integer $ Literals.int16ToBigint $ var "i"),
          _IntegerValue_int32>>: "i" ~>
            right $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_int $ var "i"),
          _IntegerValue_int64>>: "i" ~>
            right $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_integer $ Literals.int64ToBigint $ var "i"),
          _IntegerValue_uint8>>: "i" ~>
            right $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_integer $ Literals.uint8ToBigint $ var "i"),
          _IntegerValue_uint16>>: "i" ~>
            right $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_integer $ Literals.uint16ToBigint $ var "i"),
          _IntegerValue_uint32>>: "i" ~>
            right $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_integer $ Literals.uint32ToBigint $ var "i"),
          _IntegerValue_uint64>>: "i" ~>
            right $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_integer $ Literals.uint64ToBigint $ var "i")],
      _Literal_string>>: "s" ~>
        right $ HaskellUtils.hslit @@ (inject H._Literal H._Literal_string $ var "s")]

encodeTerm :: TBinding (Int -> HaskellNamespaces -> Term -> Context -> Graph -> Either (InContext Error) H.Expression)
encodeTerm = haskellCoderDefinition "encodeTerm" $
  doc "Encode a Hydra term as a Haskell expression" $
  "depth" ~> "namespaces" ~> "term" ~> "cx" ~> "g" ~> lets [
    "encode">: "t" ~> encodeTerm @@ var "depth" @@ var "namespaces" @@ var "t" @@ var "cx" @@ var "g"] $
    "nonemptyMap" <~ ("m" ~> lets [
      "lhs">: HaskellUtils.hsvar @@ string "M.fromList",
      "encodePair">: "pair" ~> lets [
        "k">: Pairs.first $ var "pair",
        "v">: Pairs.second $ var "pair"] $
        "hk" <<~ var "encode" @@ var "k" $
        "hv" <<~ var "encode" @@ var "v" $
        right $ inject H._Expression H._Expression_tuple $ list [var "hk", var "hv"]] $
      "rhs" <<~ Eithers.map
        (unaryFunction $ inject H._Expression H._Expression_list)
        (Eithers.mapList (var "encodePair") $ Maps.toList (var "m")) $
      right $ HaskellUtils.hsapp @@ var "lhs" @@ var "rhs") $
    "nonemptySet" <~ ("s" ~> lets [
      "lhs">: HaskellUtils.hsvar @@ string "S.fromList" ] $
      "rhs" <<~ encodeTerm @@ var "depth" @@ var "namespaces" @@ (inject _Term _Term_list $ Sets.toList $ var "s") @@ var "cx" @@ var "g" $
      right $ HaskellUtils.hsapp @@ var "lhs" @@ var "rhs") $
    cases _Term (Rewriting.deannotateTerm @@ var "term")
      (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError (Strings.cat2 (string "unexpected term: ") (ShowCore.term @@ var "term"))) (var "cx")) [
      _Term_application>>: "app" ~> lets [
        "fun">: Core.applicationFunction $ var "app",
        "arg">: Core.applicationArgument $ var "app",
        "deannotatedFun">: Rewriting.deannotateTerm @@ (var "fun")] $
        -- When the function is a union elimination, encode as a direct case expression
        -- instead of (\x -> case x of ...) arg
        cases _Term (var "deannotatedFun")
          (Just $
            "hfun" <<~ var "encode" @@ var "fun" $
              "harg" <<~ var "encode" @@ var "arg" $
                right $ HaskellUtils.hsapp @@ var "hfun" @@ var "harg") [
          _Term_function>>: "f" ~>
            cases _Function (var "f")
              (Just $
                "hfun" <<~ var "encode" @@ var "fun" $
                  "harg" <<~ var "encode" @@ var "arg" $
                    right $ HaskellUtils.hsapp @@ var "hfun" @@ var "harg") [
              _Function_elimination>>: "e" ~>
                cases _Elimination (var "e")
                  (Just $
                    "hfun" <<~ var "encode" @@ var "fun" $
                      "harg" <<~ var "encode" @@ var "arg" $
                        right $ HaskellUtils.hsapp @@ var "hfun" @@ var "harg") [
                  _Elimination_union>>: "stmt" ~>
                    "harg" <<~ var "encode" @@ var "arg" $
                      encodeCaseExpression @@ var "depth" @@ var "namespaces" @@ var "stmt" @@ var "harg" @@ var "cx" @@ var "g"]]],
      _Term_either>>: "e" ~> Eithers.either_
          ("l" ~>
            "hl" <<~ var "encode" @@ var "l" $
              right $ HaskellUtils.hsapp @@ (HaskellUtils.hsvar @@ string "Left") @@ var "hl")
          ("r" ~>
            "hr" <<~ var "encode" @@ var "r" $
              right $ HaskellUtils.hsapp @@ (HaskellUtils.hsvar @@ string "Right") @@ var "hr")
          (var "e"),
      _Term_function>>: "f" ~>
        encodeFunction @@ var "depth" @@ var "namespaces" @@ var "f" @@ var "cx" @@ var "g",
      _Term_let>>: "letTerm" ~> lets [
        "collectBindings">: "lt" ~>
          "bs" <~ Core.letBindings (var "lt") $
          "body" <~ Core.letBody (var "lt") $
          cases _Term (Rewriting.deannotateTerm @@ var "body")
            (Just $ pair (var "bs") (var "body")) [
            _Term_let>>: "innerLt" ~>
              "innerResult" <~ var "collectBindings" @@ var "innerLt" $
              pair (Lists.concat2 (var "bs") (Pairs.first $ var "innerResult")) (Pairs.second $ var "innerResult")],
        "collected">: var "collectBindings" @@ var "letTerm",
        "allBindings">: Pairs.first $ var "collected",
        "finalBody">: Pairs.second $ var "collected",
        "encodeBinding">: "binding" ~> lets [
          "name">: Core.bindingName $ var "binding",
          "term'">: Core.bindingTerm $ var "binding",
          "hname">: HaskellUtils.simpleName @@ (Core.unName $ var "name")] $
          "hexpr" <<~ var "encode" @@ var "term'" $
          right $ inject H._LocalBinding H._LocalBinding_value $ HaskellUtils.simpleValueBinding @@ var "hname" @@ var "hexpr" @@ nothing] $
        "hbindings" <<~ Eithers.mapList (var "encodeBinding") (var "allBindings") $
        "hinner" <<~ var "encode" @@ var "finalBody" $
        right $ inject H._Expression H._Expression_let $ record H._LetExpression [
          H._LetExpression_bindings>>: var "hbindings",
          H._LetExpression_inner>>: var "hinner"],
      _Term_list>>: "els" ~>
        "helems" <<~ Eithers.mapList (var "encode") (var "els") $
          right $ inject H._Expression H._Expression_list $ var "helems",
      _Term_literal>>: "v" ~>
        encodeLiteral @@ var "v" @@ var "cx",
      _Term_map>>: "m" ~> Logic.ifElse (Maps.null $ var "m")
        (right $ HaskellUtils.hsvar @@ string "M.empty")
        (var "nonemptyMap" @@ var "m"),
      _Term_maybe>>: "m" ~>
        Maybes.cases (var "m")
          (right $ HaskellUtils.hsvar @@ string "Nothing") $
          "t" ~>
            "ht" <<~ var "encode" @@ var "t" $
              right $ HaskellUtils.hsapp @@ (HaskellUtils.hsvar @@ string "Just") @@ var "ht",
      _Term_pair>>: "p" ~>
        "f" <<~ var "encode" @@ Pairs.first (var "p") $
        "s" <<~ var "encode" @@ Pairs.second (var "p") $
        right $ inject H._Expression H._Expression_tuple $ list [var "f", var "s"],
      _Term_record>>: "record" ~> lets [
        "sname">: Core.recordTypeName $ var "record",
        "fields">: Core.recordFields $ var "record",
        "toFieldUpdate">: "field" ~> lets [
          "fn">: Core.fieldName $ var "field",
          "ft">: Core.fieldTerm $ var "field",
          "fieldRef">: HaskellUtils.recordFieldReference @@ var "namespaces" @@ var "sname" @@ var "fn"] $
          "hft" <<~ var "encode" @@ var "ft" $
          right $ record H._FieldUpdate [
            H._FieldUpdate_name>>: var "fieldRef",
            H._FieldUpdate_value>>: var "hft"],
          "typeName">: HaskellUtils.elementReference @@ var "namespaces" @@ var "sname"] $
          "updates" <<~ Eithers.mapList (var "toFieldUpdate") (var "fields") $
          right $ inject H._Expression H._Expression_constructRecord $ record H._ConstructRecordExpression [
            H._ConstructRecordExpression_name>>: var "typeName",
            H._ConstructRecordExpression_fields>>: var "updates"],
      _Term_set>>: "s" ~> Logic.ifElse (Sets.null $ var "s")
        (right $ HaskellUtils.hsvar @@ string "S.empty")
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
        "lhs">: inject H._Expression H._Expression_variable $ HaskellUtils.unionFieldReference @@ (Sets.union (Sets.fromList (Maps.keys (Graph.graphBoundTerms $ var "g"))) (Sets.fromList (Maps.keys (Graph.graphSchemaTypes $ var "g")))) @@ var "namespaces" @@ var "sname" @@ var "fn",
        "dflt">: Eithers.map (HaskellUtils.hsapp @@ var "lhs") (var "encode" @@ var "ft")] $
        "ftyp" <<~ Schemas.requireUnionField_ @@ var "cx" @@ var "g" @@ var "sname" @@ var "fn" $
        cases _Type (Rewriting.deannotateType @@ var "ftyp")
          (Just $ var "dflt") [
          _Type_unit>>: constant $ right $ var "lhs"],
      _Term_unit>>: constant $ right $ inject H._Expression H._Expression_tuple $ list ([] :: [TTerm H.Expression]),
      _Term_variable>>: "name" ~>
        right $ inject H._Expression H._Expression_variable $ HaskellUtils.elementReference @@ var "namespaces" @@ var "name",
      _Term_wrap>>: "wrapped" ~> lets [
        "tname">: Core.wrappedTermTypeName $ var "wrapped",
        "term'">: Core.wrappedTermBody $ var "wrapped",
        "lhs">: inject H._Expression H._Expression_variable $ HaskellUtils.elementReference @@ var "namespaces" @@ var "tname"] $
        "rhs" <<~ var "encode" @@ var "term'" $
        right $ HaskellUtils.hsapp @@ var "lhs" @@ var "rhs"]

encodeType :: TBinding (HaskellNamespaces -> Type -> Context -> Graph -> Either (InContext Error) H.Type)
encodeType = haskellCoderDefinition "encodeType" $
  doc "Encode a Hydra type as a Haskell type" $
  "namespaces" ~>
  "typ" ~> "cx" ~> "g" ~> lets [
  "encode">: "t" ~> encodeType @@ var "namespaces" @@ var "t" @@ var "cx" @@ var "g",
  "ref">: "name" ~>
    right $ inject H._Type H._Type_variable $ HaskellUtils.elementReference @@ var "namespaces" @@ var "name",
  "unitTuple">: inject H._Type H._Type_tuple $ list ([] :: [TTerm H.Type])] $
  cases _Type (Rewriting.deannotateType @@ var "typ")
    (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError (Strings.cat2 (string "unexpected type: ") (ShowCore.type_ @@ var "typ"))) (var "cx")) [
    _Type_application>>: "app" ~> lets [
      "lhs">: Core.applicationTypeFunction $ var "app",
      "rhs">: Core.applicationTypeArgument $ var "app"] $
      "hlhs" <<~ var "encode" @@ var "lhs" $
        "hrhs" <<~ var "encode" @@ var "rhs" $
          right $ HaskellUtils.toTypeApplication @@ list [var "hlhs", var "hrhs"],
    _Type_either>>: "eitherType" ~> lets [
      "left'">: Core.eitherTypeLeft $ var "eitherType",
      "right'">: Core.eitherTypeRight $ var "eitherType"] $
      "hleft" <<~ var "encode" @@ var "left'" $
      "hright" <<~ var "encode" @@ var "right'" $
      right $ HaskellUtils.toTypeApplication @@ list [
          inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "Either",
          var "hleft",
          var "hright"],
    _Type_function>>: "funType" ~> lets [
      "dom">: Core.functionTypeDomain $ var "funType",
      "cod">: Core.functionTypeCodomain $ var "funType"] $
      "hdom" <<~ var "encode" @@ var "dom" $
        "hcod" <<~ var "encode" @@ var "cod" $
          right $ inject H._Type H._Type_function $ record H._FunctionType [
            H._FunctionType_domain>>: var "hdom",
            H._FunctionType_codomain>>: var "hcod"],
    _Type_forall>>: "forallType" ~> lets [
      "v">: Core.forallTypeParameter $ var "forallType",
      "body">: Core.forallTypeBody $ var "forallType"] $
      var "encode" @@ var "body",
    _Type_list>>: "lt" ~>
      "hlt" <<~ var "encode" @@ var "lt" $
        right $ inject H._Type H._Type_list $ var "hlt",
    _Type_literal>>: "lt" ~>
      cases _LiteralType (var "lt")
        (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError (Strings.cat2 (string "unexpected literal type: ") (ShowCore.literalType @@ var "lt"))) (var "cx")) [
        _LiteralType_binary>>: constant $
          right $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "B.ByteString",
        _LiteralType_boolean>>: constant $
          right $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "Bool",
        _LiteralType_float>>: "ft" ~>
          cases _FloatType (var "ft") Nothing [
            _FloatType_float32>>: constant $
              right $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "Float",
            _FloatType_float64>>: constant $
              right $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "Double",
            _FloatType_bigfloat>>: constant $
              right $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "Double"],
        _LiteralType_integer>>: "it" ~>
          cases _IntegerType (var "it")
            (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError (Strings.cat2 (string "unexpected integer type: ") (ShowCore.integerType @@ var "it"))) (var "cx")) [
            _IntegerType_bigint>>: constant $
              right $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "Integer",
            _IntegerType_int8>>: constant $
              right $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "I.Int8",
            _IntegerType_int16>>: constant $
              right $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "I.Int16",
            _IntegerType_int32>>: constant $
              right $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "Int",
            _IntegerType_int64>>: constant $
              right $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "I.Int64"],
        _LiteralType_string>>: constant $
          right $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "String"],
    _Type_map>>: "mapType" ~> lets [
      "kt">: Core.mapTypeKeys $ var "mapType",
      "vt">: Core.mapTypeValues $ var "mapType"] $
      "hkt" <<~ var "encode" @@ var "kt" $
      "hvt" <<~ var "encode" @@ var "vt" $
      right $ HaskellUtils.toTypeApplication @@ list [
          inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "M.Map",
          var "hkt",
          var "hvt"],
    _Type_maybe>>: "ot" ~>
      "hot" <<~ var "encode" @@ var "ot" $
      right $ HaskellUtils.toTypeApplication @@ list [
          inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "Maybe",
          var "hot"],
    _Type_pair>>: "pt" ~>
      "f" <<~ var "encode" @@ (Core.pairTypeFirst $ var "pt") $
      "s" <<~ var "encode" @@ (Core.pairTypeSecond $ var "pt") $
        right $ inject H._Type H._Type_tuple $ list [var "f", var "s"],
    _Type_record>>: constant (var "ref" @@ Core.name (string "placeholder")),
    _Type_set>>: "st" ~>
      "hst" <<~ var "encode" @@ var "st" $
      right $ HaskellUtils.toTypeApplication @@ list [
          inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "S.Set",
          var "hst"],
    _Type_union>>: constant (var "ref" @@ Core.name (string "placeholder")),
    _Type_unit>>: constant $ right $ var "unitTuple",
    _Type_variable>>: "v1" ~> var "ref" @@ var "v1",
    _Type_void>>: constant $
      right $ inject H._Type H._Type_variable $ HaskellUtils.rawName @@ string "Void",
    _Type_wrap>>: constant (var "ref" @@ Core.name (string "placeholder"))]

encodeTypeWithClassAssertions :: TBinding (HaskellNamespaces -> M.Map Name (S.Set TypeClass) -> Type -> Context -> Graph -> Either (InContext Error) H.Type)
encodeTypeWithClassAssertions = haskellCoderDefinition "encodeTypeWithClassAssertions" $
  doc "Encode a Hydra type as a Haskell type with typeclass assertions" $
  "namespaces" ~> "explicitClasses" ~> "typ" ~> "cx" ~> "g" ~> lets [
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
      "htyp" <<~ adaptTypeToHaskellAndEncode @@ var "namespaces" @@ var "typ" @@ var "cx" @@ var "g" $
      Logic.ifElse (Lists.null $ var "assertPairs")
        (right $ var "htyp") (lets [
          "encoded">: Lists.map (var "encodeAssertion") (var "assertPairs"),
          "hassert">: Logic.ifElse (Equality.equal (Lists.length $ var "encoded") (int32 1))
            (Lists.head $ var "encoded")
            (inject H._Assertion H._Assertion_tuple $ var "encoded")] $
          right $ inject H._Type H._Type_ctx $ record H._ContextType [
            H._ContextType_ctx>>: var "hassert",
            H._ContextType_type>>: var "htyp"])

findOrdVariables :: TBinding (Type -> S.Set Name)
findOrdVariables = haskellCoderDefinition "findOrdVariables" $
  doc "Find type variables that require an Ord constraint (used in maps or sets)" $
  "typ" ~> lets [
    "fold">: "names" ~> "typ'" ~>
      cases _Type (var "typ'")
        (Just $ var "names") [
        _Type_map>>: "mapType" ~> lets [
          "kt">: Core.mapTypeKeys $ var "mapType"] $
          var "tryType" @@ var "names" @@ var "kt",
        _Type_set>>: "et" ~>
          var "tryType" @@ var "names" @@ var "et"],
    "isTypeVariable">: "v" ~>
      Maybes.isNothing $ Names.namespaceOf @@ var "v",
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
  doc "Get implicit typeclass constraints for type variables that need Ord" $
  "typ" ~> lets [
    "toPair">: "name" ~>
      pair (var "name") (Sets.fromList $ list [Graph.typeClassOrdering])] $
    Maps.fromList $ Lists.map (var "toPair") (Sets.toList $ findOrdVariables @@ var "typ")

moduleToHaskellModule :: TBinding (Module -> [Definition] -> Context -> Graph -> Prelude.Either (InContext Error) H.Module)
moduleToHaskellModule = haskellCoderDefinition "moduleToHaskellModule" $
  doc "Convert a Hydra module and definitions to a Haskell module AST" $
  "mod" ~> "defs" ~> "cx" ~> "g" ~>
    "namespaces" <<~ HaskellUtils.namespacesForModule @@ var "mod" @@ var "cx" @@ var "g" $
      constructModule @@ var "namespaces" @@ var "mod" @@ var "defs" @@ var "cx" @@ var "g"

moduleToHaskell :: TBinding (Module -> [Definition] -> Context -> Graph -> Prelude.Either (InContext Error) (M.Map String String))
moduleToHaskell = haskellCoderDefinition "moduleToHaskell" $
  doc "Convert a Hydra module to Haskell source code as a filepath-to-content map" $
  "mod" ~> "defs" ~> "cx" ~> "g" ~>
  "hsmod" <<~ moduleToHaskellModule @@ var "mod" @@ var "defs" @@ var "cx" @@ var "g" $ lets [
  "s">: Serialization.printExpr @@ (Serialization.parenthesize @@ (HaskellSerde.moduleToExpr @@ var "hsmod")),
  "filepath">: Names.namespaceToFilePath @@ Util.caseConventionPascal @@ (wrap _FileExtension $ string "hs") @@ (Module.moduleNamespace $ var "mod")] $
  right $ Maps.singleton (var "filepath") (var "s")

nameDecls :: TBinding (HaskellNamespaces -> Name -> Type -> [H.DeclarationWithComments])
nameDecls = haskellCoderDefinition "nameDecls" $
  doc "Generate Haskell declarations for type and field name constants" $
  "namespaces" ~> "name" ~> "typ" ~> lets [
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

toDataDeclaration :: TBinding (HaskellNamespaces -> TermDefinition -> Context -> Graph -> Either (InContext Error) H.DeclarationWithComments)
toDataDeclaration = haskellCoderDefinition "toDataDeclaration" $
  doc "Convert a Hydra term definition to a Haskell declaration with comments" $
  "namespaces" ~> "def" ~> "cx" ~> "g" ~> lets [
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
          "hterm" <<~ encodeTerm @@ int32 0 @@ var "namespaces" @@ var "term'" @@ var "cx" @@ var "g" $ lets [
         "vb">: HaskellUtils.simpleValueBinding @@ var "hname'" @@ var "hterm" @@ var "bindings",
         -- Extract constraints from the TypeScheme and convert to class assertions
         "schemeConstraints">: optCases (var "typ") Phantoms.nothing ("ts" ~> Core.typeSchemeConstraints (var "ts")),
         "schemeClasses">: typeSchemeConstraintsToClassMap @@ var "schemeConstraints"] $
         "explicitClasses" <<~ Annotations.getTypeClasses @@ var "cx" @@ var "g" @@ (Rewriting.removeTypesFromTerm @@ var "term") $
         -- Combine constraints from TypeScheme with any explicit annotations
         "combinedClasses" <~ Maps.union (var "schemeClasses") (var "explicitClasses") $
         "schemeType" <~ optCases (var "typ") Core.typeUnit ("ts" ~> Core.typeSchemeType (var "ts")) $
         "htype" <<~ encodeTypeWithClassAssertions @@ var "namespaces" @@ var "combinedClasses" @@ var "schemeType" @@ var "cx" @@ var "g" $ lets [
         "decl">: inject H._Declaration H._Declaration_typedBinding $ record H._TypedBinding [
           H._TypedBinding_typeSignature>>: record H._TypeSignature [
             H._TypeSignature_name>>: var "hname'",
             H._TypeSignature_type>>: var "htype"],
           H._TypedBinding_valueBinding>>: var "rewriteValueBinding" @@ var "vb"]] $
          right $ record H._DeclarationWithComments [
            H._DeclarationWithComments_body>>: var "decl",
            H._DeclarationWithComments_comments>>: var "comments"]) [
        _Term_let>>: "letTerm" ~> lets [
          -- For let terms, encode each binding's term directly
          "lbindings">: Core.letBindings $ var "letTerm",
          "env">: Core.letBody $ var "letTerm",
          "toTermDefinition">: "hname''" ~> "hterm'" ~>
            inject H._LocalBinding H._LocalBinding_value $ HaskellUtils.simpleValueBinding @@ var "hname''" @@ var "hterm'" @@ nothing,
          "hnames">: Lists.map ("binding" ~> HaskellUtils.simpleName @@ (Core.unName $ Core.bindingName $ var "binding")) (var "lbindings"),
          "terms">: Lists.map (unaryFunction $ Core.bindingTerm) (var "lbindings")] $
          "hterms" <<~ Eithers.mapList ("t" ~> encodeTerm @@ int32 0 @@ var "namespaces" @@ var "t" @@ var "cx" @@ var "g") (var "terms") $ lets [
          "hbindings">: Lists.zipWith (var "toTermDefinition") (var "hnames") (var "hterms"),
          -- Merge new bindings with any previously accumulated bindings from outer lets
          "prevBindings">: Maybes.maybe (list ([] :: [TTerm H.LocalBinding])) ("lb" ~> unwrap H._LocalBindings @@ var "lb") (var "bindings"),
          "allBindings">: Lists.concat2 (var "prevBindings") (var "hbindings")] $
          var "toDecl" @@ var "comments" @@ var "hname'" @@ var "env" @@ (just $ wrap H._LocalBindings $ var "allBindings")]] $
    "comments" <<~ Annotations.getTermDescription @@ var "cx" @@ var "g" @@ var "term" $
    var "toDecl" @@ var "comments" @@ var "hname" @@ var "term" @@ nothing

-- | Simplified version of toTypeDeclarations that works with Name and Type directly
-- This is used with the new Definition-based API
toTypeDeclarationsFrom :: TBinding (HaskellNamespaces -> Name -> Type -> Context -> Graph -> Either (InContext Error) [H.DeclarationWithComments])
toTypeDeclarationsFrom = haskellCoderDefinition "toTypeDeclarationsFrom" $
  doc "Convert a Hydra type definition to Haskell declarations" $
  "namespaces" ~> "elementName" ~> "typ" ~> "cx" ~> "g" ~> lets [
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
      "hname0">: HaskellUtils.simpleName @@ (HaskellUtils.newtypeAccessorName @@ var "tname")] $
      "htype" <<~ adaptTypeToHaskellAndEncode @@ var "namespaces" @@ var "typ'" @@ var "cx" @@ var "g" $ lets [
      "hfield">: record H._FieldWithComments [
        H._FieldWithComments_field>>: record H._Field [
          H._Field_name>>: var "hname0",
          H._Field_type>>: var "htype"],
        H._FieldWithComments_comments>>: nothing],
      "constructorName">: HaskellUtils.simpleName @@ (Names.localNameOf @@ var "tname")] $
      right $ record H._ConstructorWithComments [
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
        "htype" <<~ adaptTypeToHaskellAndEncode @@ var "namespaces" @@ var "ftype" @@ var "cx" @@ var "g" $
        "comments" <<~ Annotations.getTypeDescription @@ var "cx" @@ var "g" @@ var "ftype" $
        right $ record H._FieldWithComments [
          H._FieldWithComments_field>>: record H._Field [
            H._Field_name>>: var "hname'",
            H._Field_type>>: var "htype"],
          H._FieldWithComments_comments>>: var "comments"]] $
      "hFields" <<~ Eithers.mapList (var "toField") (var "fields") $
      right $ record H._ConstructorWithComments [
        H._ConstructorWithComments_body>>: inject H._Constructor H._Constructor_record $ record H._RecordConstructor [
          H._RecordConstructor_name>>: HaskellUtils.simpleName @@ var "lname'",
          H._RecordConstructor_fields>>: var "hFields"],
        H._ConstructorWithComments_comments>>: nothing],
    "unionCons">: "boundNames'" ~> "lname'" ~> "fieldType" ~> lets [
      "fname">: Core.fieldTypeName $ var "fieldType",
      "ftype">: Core.fieldTypeType $ var "fieldType",
      "deconflict">: "name" ~> lets [
        "tname">: Names.unqualifyName @@ record _QualifiedName [
          _QualifiedName_namespace>>: just $ Pairs.first $ Module.namespacesFocus $ var "namespaces",
          _QualifiedName_local>>: var "name"]] $
        Logic.ifElse (Sets.member (var "tname") (var "boundNames'"))
          (var "deconflict" @@ Strings.cat2 (var "name") (string "_"))
          (var "name")] $
      "comments" <<~ Annotations.getTypeDescription @@ var "cx" @@ var "g" @@ var "ftype" $ lets [
      "nm">: var "deconflict" @@ Strings.cat2 (Formatting.capitalize @@ var "lname'") (Formatting.capitalize @@ (Core.unName $ var "fname"))] $
      "typeList" <<~ (Logic.ifElse (Equality.equal (Rewriting.deannotateType @@ var "ftype") MetaTypes.unit)
        (right $ list ([] :: [TTerm H.CaseRhs])) $
        "htype" <<~ adaptTypeToHaskellAndEncode @@ var "namespaces" @@ var "ftype" @@ var "cx" @@ var "g" $
          right $ list [var "htype"]) $
      right $ record H._ConstructorWithComments [
        H._ConstructorWithComments_body>>: inject H._Constructor H._Constructor_ordinary $ record H._OrdinaryConstructor [
          H._OrdinaryConstructor_name>>: HaskellUtils.simpleName @@ var "nm",
          H._OrdinaryConstructor_fields>>: var "typeList"],
        H._ConstructorWithComments_comments>>: var "comments"]] $
      "isSer" <<~ Schemas.isSerializableByName @@ var "cx" @@ var "g" @@ var "elementName" $ lets [
      "deriv">: wrap H._Deriving $ Logic.ifElse (var "isSer")
        (Lists.map (HaskellUtils.rawName) (list [string "Eq", string "Ord", string "Read", string "Show"]))
        (list ([] :: [TTerm H.Name])),
      "unpackResult">: HaskellUtils.unpackForallType @@ var "typ",
      "vars">: Pairs.first $ var "unpackResult",
      "t'">: Pairs.second $ var "unpackResult",
      "hd">: var "declHead" @@ var "hname" @@ (Lists.reverse $ var "vars")] $
      "decl" <<~ (cases _Type (Rewriting.deannotateType @@ var "t'")
        (Just $ "htype" <<~ (adaptTypeToHaskellAndEncode @@ var "namespaces" @@ var "typ" @@ var "cx" @@ var "g") $
          right $ inject H._Declaration H._Declaration_type $ record H._TypeDeclaration [
            H._TypeDeclaration_name>>: var "hd",
            H._TypeDeclaration_type>>: var "htype"]) [
        _Type_record>>: "rt" ~>
          "cons" <<~ (var "recordCons" @@ var "lname" @@ var "rt") $
          right $ inject H._Declaration H._Declaration_data $ record H._DataDeclaration [
            H._DataDeclaration_keyword>>: injectUnit H._DataOrNewtype H._DataOrNewtype_data,
            H._DataDeclaration_context>>: list ([] :: [TTerm H.Assertion]),
            H._DataDeclaration_head>>: var "hd",
            H._DataDeclaration_constructors>>: list [var "cons"],
            H._DataDeclaration_deriving>>: list [var "deriv"]],
        _Type_union>>: "rt" ~>
          "cons" <<~ Eithers.mapList (var "unionCons" @@ (Sets.fromList (Maps.keys (Graph.graphBoundTerms $ var "g"))) @@ var "lname") (var "rt") $
          right $ inject H._Declaration H._Declaration_data $ record H._DataDeclaration [
            H._DataDeclaration_keyword>>: injectUnit H._DataOrNewtype H._DataOrNewtype_data,
            H._DataDeclaration_context>>: list ([] :: [TTerm H.Assertion]),
            H._DataDeclaration_head>>: var "hd",
            H._DataDeclaration_constructors>>: var "cons",
            H._DataDeclaration_deriving>>: list [var "deriv"]],
        _Type_wrap>>: "wrapped" ~>
          "cons" <<~ var "newtypeCons" @@ var "elementName" @@ var "wrapped" $
            right $ inject H._Declaration H._Declaration_data $ record H._DataDeclaration [
              H._DataDeclaration_keyword>>: injectUnit H._DataOrNewtype H._DataOrNewtype_newtype,
              H._DataDeclaration_context>>: list ([] :: [TTerm H.Assertion]),
              H._DataDeclaration_head>>: var "hd",
              H._DataDeclaration_constructors>>: list [var "cons"],
              H._DataDeclaration_deriving>>: list [var "deriv"]]]) $
      "comments" <<~ Annotations.getTypeDescription @@ var "cx" @@ var "g" @@ var "typ" $
      "tdecls" <<~ (Logic.ifElse (includeTypeDefinitions)
        ("decl'" <<~ typeDecl @@ var "namespaces" @@ var "elementName" @@ var "typ" @@ var "cx" @@ var "g" $
          right $ list [var "decl'"])
        (right $ list ([] :: [TTerm H.DeclarationWithComments]))) $ lets [
      "mainDecl">: record H._DeclarationWithComments [
        H._DeclarationWithComments_body>>: var "decl",
        H._DeclarationWithComments_comments>>: var "comments"],
      "nameDecls'">: nameDecls @@ var "namespaces" @@ var "elementName" @@ var "typ"] $
      right $ Lists.concat $ list [list [var "mainDecl"], var "nameDecls'", var "tdecls"]

typeDecl :: TBinding (HaskellNamespaces -> Name -> Type -> Context -> Graph -> Either (InContext Error) H.DeclarationWithComments)
typeDecl = haskellCoderDefinition "typeDecl" $
  doc "Generate a Haskell declaration for a type definition constant" $
  "namespaces" ~> "name" ~> "typ" ~> "cx" ~> "g" ~> lets [
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
    "expr" <<~ encodeTerm @@ int32 0 @@ var "namespaces" @@ var "finalTerm" @@ var "cx" @@ var "g" $ lets [
    "rhs">: wrap H._RightHandSide $ var "expr",
    "hname">: HaskellUtils.simpleName @@ (var "typeNameLocal" @@ var "name"),
    "pat">: HaskellUtils.applicationPattern @@ var "hname" @@ list ([] :: [TTerm H.Pattern]),
    "decl">: inject H._Declaration H._Declaration_valueBinding $ inject H._ValueBinding H._ValueBinding_simple $ record H._SimpleValueBinding [
      H._SimpleValueBinding_pattern>>: var "pat",
      H._SimpleValueBinding_rhs>>: var "rhs",
      H._SimpleValueBinding_localBindings>>: nothing]] $
    right $ record H._DeclarationWithComments [
      H._DeclarationWithComments_body>>: var "decl",
      H._DeclarationWithComments_comments>>: nothing]

-- | Convert TypeScheme constraints to the Map format used by encodeTypeWithClassAssertions.
-- TypeScheme constraints are Maybe (Map Name TypeVariableMetadata), where TypeVariableMetadata
-- has a 'classes' field of type Set Name. We convert this to Map Name (Set TypeClass).
typeSchemeConstraintsToClassMap :: TBinding (Maybe (M.Map Name TypeVariableMetadata) -> M.Map Name (S.Set TypeClass))
typeSchemeConstraintsToClassMap = haskellCoderDefinition "typeSchemeConstraintsToClassMap" $
  doc "Convert type scheme constraints to a map of type variables to typeclasses" $
  "maybeConstraints" ~> lets [
    -- Convert a class name to a TypeClass, returning Nothing for unknown classes
    "nameToTypeClass">: "className" ~> lets [
      "classNameStr">: Core.unName $ var "className",
      "isEq">: Equality.equal (var "classNameStr") (Core.unName $ Core.nameLift _TypeClass_equality),
      "isOrd">: Equality.equal (var "classNameStr") (Core.unName $ Core.nameLift _TypeClass_ordering)] $
      Logic.ifElse (var "isEq")
        (just $ inject _TypeClass _TypeClass_equality unit)
        (Logic.ifElse (var "isOrd")
          (just $ inject _TypeClass _TypeClass_ordering unit)
          nothing)] $
    Maybes.maybe
      Maps.empty
      ("constraints" ~>
        Maps.map
          ("meta" ~> Sets.fromList $
            Maybes.cat $ Lists.map (var "nameToTypeClass") $ Sets.toList $ Core.typeVariableMetadataClasses (var "meta"))
          (var "constraints"))
      (var "maybeConstraints")
