{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Inference where

-- Standard Tier-2 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Coders                 as Coders
import qualified Hydra.Dsl.Compute                as Compute
import qualified Hydra.Dsl.Core                   as Core
import qualified Hydra.Dsl.Graph                  as Graph
import qualified Hydra.Dsl.Lib.Chars              as Chars
import qualified Hydra.Dsl.Lib.Equality           as Equality
import qualified Hydra.Dsl.Lib.Flows              as Flows
import qualified Hydra.Dsl.Lib.Lists              as Lists
import qualified Hydra.Dsl.Lib.Literals           as Literals
import qualified Hydra.Dsl.Lib.Logic              as Logic
import qualified Hydra.Dsl.Lib.Maps               as Maps
import qualified Hydra.Dsl.Lib.Math               as Math
import qualified Hydra.Dsl.Lib.Optionals          as Optionals
import           Hydra.Dsl.Phantoms               as Phantoms
import qualified Hydra.Dsl.Lib.Sets               as Sets
import           Hydra.Dsl.Lib.Strings            as Strings
import qualified Hydra.Dsl.Mantle                 as Mantle
import qualified Hydra.Dsl.Module                 as Module
import qualified Hydra.Dsl.TTerms                 as TTerms
import qualified Hydra.Dsl.TTypes                 as TTypes
import qualified Hydra.Dsl.Terms                  as Terms
import qualified Hydra.Dsl.Topology               as Topology
import qualified Hydra.Dsl.Types                  as Types
import qualified Hydra.Dsl.Typing                 as Typing
import qualified Hydra.Sources.Tier1.All          as Tier1
import qualified Hydra.Sources.Tier1.Constants    as Constants
import qualified Hydra.Sources.Tier1.Encode.Core as EncodeCore
import qualified Hydra.Sources.Tier1.Decode       as Decode
import qualified Hydra.Sources.Tier1.Formatting   as Formatting
import qualified Hydra.Sources.Tier1.Functions    as Functions
import qualified Hydra.Sources.Tier1.Literals     as Literals
import qualified Hydra.Sources.Tier1.Messages     as Messages
import qualified Hydra.Sources.Tier1.Strip        as Strip
import           Prelude hiding ((++))
import qualified Data.Int                  as I
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y

-- Uncomment tier-2 sources as needed
--import qualified Hydra.Sources.Tier2.Accessors as Accessors
--import qualified Hydra.Sources.Tier2.AdapterUtils as AdapterUtils
--import qualified Hydra.Sources.Tier2.Adapters as Adapters
import qualified Hydra.Sources.Tier2.Annotations as Annotations
--import qualified Hydra.Sources.Tier2.Arity as Arity
--import qualified Hydra.Sources.Tier2.Decode.Core as DecodeCore
--import qualified Hydra.Sources.Tier2.CoreLanguage as CoreLanguage
import qualified Hydra.Sources.Tier2.Errors as Errors
import qualified Hydra.Sources.Tier2.Extract.Core as ExtractCore
import qualified Hydra.Sources.Tier2.Monads as Monads
--import qualified Hydra.Sources.Tier2.GrammarToModule as GrammarToModule
--import qualified Hydra.Sources.Tier2.Inference as Inference
--import qualified Hydra.Sources.Tier2.Lexical as Lexical
--import qualified Hydra.Sources.Tier2.LiteralAdapters as LiteralAdapters
--import qualified Hydra.Sources.Tier2.Describe.Core as DescribeCore
--import qualified Hydra.Sources.Tier2.Qnames as Qnames
--import qualified Hydra.Sources.Tier2.Reduction as Reduction
import qualified Hydra.Sources.Tier2.Rewriting as Rewriting
import qualified Hydra.Sources.Tier2.Schemas as Schemas
--import qualified Hydra.Sources.Tier2.Serialization as Serialization
import qualified Hydra.Sources.Tier2.Show.Core as ShowCore
import qualified Hydra.Sources.Tier2.Sorting as Sorting
import qualified Hydra.Sources.Tier2.Substitution as Substitution
--import qualified Hydra.Sources.Tier2.Tarjan as Tarjan
--import qualified Hydra.Sources.Tier2.Templating as Templating
--import qualified Hydra.Sources.Tier2.TermAdapters as TermAdapters
--import qualified Hydra.Sources.Tier2.TermEncoding as TermEncoding
import qualified Hydra.Sources.Tier2.Unification as Unification
import qualified Hydra.Sources.Tier2.Variants as Variants


hydraInferenceModule :: Module
hydraInferenceModule = Module (Namespace "hydra.inference") elements
    [Annotations.hydraAnnotationsModule, Tier1.hydraFunctionsModule, Schemas.hydraSchemasModule, Unification.hydraUnificationModule]
    [Tier1.hydraCodersModule, Tier1.hydraComputeModule, Tier1.hydraMantleModule, Tier1.hydraModuleModule, Tier1.hydraTopologyModule, Tier1.hydraTypingModule] $
    Just "Type inference following Algorithm W, extended for nominal terms and types"
  where
    elements = [
      el debugInferenceDef,
      el key_vcountDef,
      el normalTypeVariableDef,
      el freshNameDef,
      el freshNamesDef,
      el freshVariableTypeDef,
      el typeOfDef,
      el typeOfCollectionDef,
      el typeOfNominalDef,
      el singleTypeDef,
      el checkTypeDef,
      el checkTypeVariablesDef,
      el gatherForallDef,
      el typeSchemeToFTypeDef,
      el toFContextDef,
      el showInferenceResultDef,
      el emptyInferenceContextDef,
      el freeVariablesInContextDef,
      el generalizeDef,
      el isUnboundDef,
      el graphToInferenceContextDef,
      el inferInGraphContextDef,
      el inferGraphTypesDef,
      el inferManyDef,
      el inferTypeOfDef,
      el inferTypeOfAnnotatedTermDef,
      el inferTypeOfApplicationDef,
      el inferTypeOfCaseStatementDef,
      el inferTypeOfCollectionDef,
      el inferTypeOfEliminationDef,
      el inferTypeOfFunctionDef,
      el inferTypeOfInjectionDef,
      el inferTypeOfLambdaDef,
      el inferTypeOfLetDef,
      el inferTypeOfLetAfterNormalizationDef,
      el inferTypeOfListDef,
      el inferTypeOfLiteralDef,
      el inferTypeOfMapDef,
      el inferTypeOfOptionalDef,
      el inferTypeOfPrimitiveDef,
      el inferTypeOfProductDef,
      el inferTypeOfProjectionDef,
      el inferTypeOfRecordDef,
      el inferTypeOfSetDef,
      el inferTypeOfSumDef,
      el inferTypeOfTermDef,
      el inferTypeOfTupleProjectionDef,
      el inferTypeOfTypeAbstractionDef,
      el inferTypeOfTypeApplicationDef,
      el inferTypeOfTypedTermDef,
      el inferTypeOfUnwrapDef,
      el inferTypeOfVariableDef,
      el inferTypeOfWrappedTermDef,
      el inferTypesOfTemporaryLetBindingsDef,
      el bindConstraintsDef,
      el forInferredTermDef,
      el forVarDef,
      el forVarsDef,
      el fTypeToTypeSchemeDef,
      el instantiateFTypeDef,
      el instantiateTypeSchemeDef,
      el mapConstraintsDef,
      el nominalApplicationDef,
      el requireSchemaTypeDef,
      el extendContextDef,
      el yieldDef,
      el yieldCheckedDef,
      el yieldDebugDef]

inferenceDefinition :: String -> TTerm a -> TElement a
inferenceDefinition = definitionInModule hydraInferenceModule

debugInferenceDef :: TElement Bool
debugInferenceDef = inferenceDefinition "debugInference" $
  doc "Disable type checking by default, for better performance" $
  true

key_vcountDef :: TElement Name
key_vcountDef = inferenceDefinition "key_vcount" $
  doc "Key for inference type variable count" $
  Core.name $ string "inferenceTypeVariableCount"

normalTypeVariableDef :: TElement (Int -> Name)
normalTypeVariableDef = inferenceDefinition "normalTypeVariable" $
  doc "Type variable naming convention follows Haskell: t0, t1, etc." $
  lambda "i" $ Core.name (Strings.cat2 (string "t") (Literals.showInt32 $ var "i"))

freshNameDef :: TElement (Flow s Name)
freshNameDef = inferenceDefinition "freshName" $
  doc "Generate a fresh type variable name" $
  Flows.map (ref normalTypeVariableDef) (ref Annotations.nextCountDef @@ ref key_vcountDef)

freshNamesDef :: TElement (Int -> Flow s [Name])
freshNamesDef = inferenceDefinition "freshNames" $
  doc "Generate multiple fresh type variable names" $
  lambda "n" $ Flows.sequence $ Lists.replicate (var "n") (ref freshNameDef)

freshVariableTypeDef :: TElement (Flow s Type)
freshVariableTypeDef = inferenceDefinition "freshVariableType" $
  doc "Generate a fresh type variable" $
  Flows.map (unaryFunction Core.typeVariable) (ref freshNameDef)

typeOfDef :: TElement (InferenceContext -> S.Set Name -> M.Map Name Type -> Term -> Flow s Type)
typeOfDef = inferenceDefinition "typeOf" $
  doc "Infer the type of a term given context and type environment" $
  lambdas ["cx", "vars", "types", "term"] $
    ref Monads.withTraceDef @@
      (Strings.cat $ list [
        string "checking type of: ",
        ref ShowCore.termDef @@ var "term",
        string " (vars: ",
        ref Formatting.showListDef @@ unaryFunction Core.unName @@ (Sets.toList $ var "vars"),
        string ", types: ",
        ref Formatting.showListDef @@ unaryFunction Core.unName @@ (Maps.keys $ var "types"),
        string ")"]) @@
      (cases _Term (var "term")
        (Just $ Flows.fail $ Strings.cat $ list [
          string "unsupported term variant in typeOf: ",
          ref ShowCore.termVariantDef @@ (ref Variants.termVariantDef @@ var "term")]) [
        _Term_annotated>>: lambda "at" $ lets [
          "term1">: Core.annotatedTermSubject $ var "at"] $
          ref typeOfDef @@ var "cx" @@ var "vars" @@ var "types" @@ var "term1",

        _Term_application>>: lambda "app" $ lets [
          "a">: Core.applicationFunction $ var "app",
          "b">: Core.applicationArgument $ var "app"] $
          withVar "t1" (ref typeOfDef @@ var "cx" @@ var "vars" @@ var "types" @@ var "a") $
          withVar "t2" (ref typeOfDef @@ var "cx" @@ var "vars" @@ var "types" @@ var "b") $
          withVar "_" (ref checkTypeVariablesDef @@ var "vars" @@ var "t1") $
          withVar "_" (ref checkTypeVariablesDef @@ var "vars" @@ var "t2") $
          cases _Type (var "t1") Nothing [
            _Type_function>>: lambda "ft" $ lets [
              "p">: Core.functionTypeDomain $ var "ft",
              "q">: Core.functionTypeCodomain $ var "ft"] $
              Logic.ifElse (Equality.equal (var "p") (var "t2"))
                (Flows.pure $ var "q")
                (Flows.fail $ Strings.cat $ list [
                  string "expected ",
                  ref ShowCore.typeDef @@ var "p",
                  string " in ",
                  ref ShowCore.termDef @@ var "term",
                  string " but found ",
                  ref ShowCore.typeDef @@ var "t2"]),
            _Type_variable>>: constant $ Flows.fail $ Strings.cat $ list [
              string "left hand side of application ",
              ref ShowCore.termDef @@ var "term",
              string " is not a function type: ",
              ref ShowCore.typeDef @@ var "t1"]],

        _Term_function>>: lambda "f" $
          cases _Function (var "f") Nothing [
            _Function_elimination>>: lambda "elm" $
              cases _Elimination (var "elm") Nothing [
                _Elimination_product>>: lambda "tp" $ lets [
                  "index">: Core.tupleProjectionIndex $ var "tp",
                  "arity">: Core.tupleProjectionArity $ var "tp",
                  "mtypes">: Core.tupleProjectionDomain $ var "tp"] $
                  Optionals.maybe
                    (Flows.fail $ Strings.cat $ list [
                      string "untyped tuple projection: ",
                      ref ShowCore.termDef @@ var "term"])
                    (lambda "types" $
                      withVar "_" (Flows.sequence $ Lists.map (ref checkTypeVariablesDef @@ var "vars") (var "types")) $
                      Flows.pure $ Core.typeFunction $ Core.functionType
                        (Core.typeProduct $ var "types")
                        (Lists.at (var "index") (var "types")))
                    (var "mtypes")
--        EliminationRecord (Projection tname (Field fname fterm)) -> ...
--        EliminationUnion (CaseStatement tname def cases) -> ...
--        EliminationWrap tname -> ...
              ],
            _Function_lambda>>: lambda "l" $ lets [
              "x">: Core.lambdaParameter $ var "l",
              "mt">: Core.lambdaDomain $ var "l",
              "e">: Core.lambdaBody $ var "l"] $
              Optionals.maybe
                (Flows.fail $ Strings.cat $ list [
                  string "untyped lambda: ",
                  ref ShowCore.termDef @@ var "term"])
                (lambda "t" $
                  withVar "_" (ref checkTypeVariablesDef @@ var "vars" @@ var "t") $
                  withVar "t1" (ref typeOfDef @@ var "cx" @@ var "vars" @@
                    (Maps.insert (var "x") (var "t") (var "types")) @@ var "e") $
                  withVar "_" (ref checkTypeVariablesDef @@ var "vars" @@ var "t1") $
                  Flows.pure $ Core.typeFunction $ Core.functionType (var "t") (var "t1"))
                (var "mt"),
            _Function_primitive>>: lambda "name" $ lets [
              -- Note: no instantiation
              "ts">: Optionals.maybe
                (Flows.fail $ Strings.cat $ list [
                  string "no such primitive: ",
                  Core.unName $ var "name"])
                (unaryFunction Flows.pure)
                (Maps.lookup (var "name") (Typing.inferenceContextPrimitiveTypes $ var "cx"))] $
              Flows.map (ref typeSchemeToFTypeDef) (var "ts")],

        _Term_let>>: lambda "letTerm" $ lets [
          "es">: Core.letBindings $ var "letTerm",
          "e">: Core.letEnvironment $ var "letTerm",
          "bnames">: Lists.map (unaryFunction Core.letBindingName) (var "es"),
          "bterms">: Lists.map (unaryFunction Core.letBindingTerm) (var "es"),
          "binType">: lambda "b" $
            Optionals.maybe
              (Flows.fail $ Strings.cat $ list [
                string "untyped let binding in ",
                ref ShowCore.termDef @@ var "term"])
              (lambda "ts" $ Flows.pure $ ref typeSchemeToFTypeDef @@ var "ts")
              (Core.letBindingType $ var "b")] $
          withVar "btypes" (Flows.mapList (var "binType") (var "es")) $ lets [
            "types2">: Maps.union (Maps.fromList $ Lists.zip (var "bnames") (var "btypes")) (var "types")] $
          withVar "est" (Flows.mapList (lambda "v" $ ref typeOfDef @@ var "cx" @@ var "vars" @@ var "types2" @@ var "v") (var "bterms")) $
          withVar "_" (Flows.sequence $ Lists.map (ref checkTypeVariablesDef @@ var "vars") (var "est")) $
          withVar "_" (Flows.sequence $ Lists.map (ref checkTypeVariablesDef @@ var "vars") (var "btypes")) $
          Logic.ifElse (Equality.equal (var "est") (var "btypes"))
            (ref typeOfDef @@ var "cx" @@ var "vars" @@ var "types2" @@ var "e")
            (Flows.fail $ Strings.cat $ list [
              string "binding types disagree: ",
              ref Formatting.showListDef @@ ref ShowCore.typeDef @@ var "est",
              string " and ",
              ref Formatting.showListDef @@ ref ShowCore.typeDef @@ var "btypes"]),

        _Term_list>>: lambda "els" $
          Logic.ifElse (Lists.null $ var "els")
            (withVar "t" (ref freshNameDef) $ lets [
              "var">: Core.typeVariable $ var "t"] $
              Flows.pure $ Core.typeForall $ Core.forallType (var "t") (Core.typeList $ var "var"))
            (lets [
              "x">: Lists.head $ var "els",
              "xs">: Lists.tail $ var "els"] $
              withVar "tx" (Flows.bind (ref typeOfDef @@ var "cx" @@ var "vars" @@ var "types" @@ var "x") (ref instantiateFTypeDef)) $
              withVar "_" (Flows.sequence $ Lists.map (lambda "e" $
                withVar "t" (Flows.bind (ref typeOfDef @@ var "cx" @@ var "vars" @@ var "types" @@ var "e") (ref instantiateFTypeDef)) $
                ref Unification.unifyTypesDef @@ (Typing.inferenceContextSchemaTypes $ var "cx") @@ var "t" @@ var "tx" @@ string "type check over collection") (var "xs")) $
              withVar "_" (ref checkTypeVariablesDef @@ var "vars" @@ var "tx") $
              Flows.pure $ Core.typeList $ var "tx"),

        _Term_literal>>: lambda "lit" $
          Flows.pure $ Core.typeLiteral $ ref Variants.literalTypeDef @@ var "lit",

        _Term_map>>: lambda "m" $
          Logic.ifElse (Maps.null $ var "m")
            (Flows.pure $ ref typeSchemeToFTypeDef @@ (
              Core.typeScheme (list [Core.name $ string "k", Core.name $ string "v"]) $
                Core.typeMap $ Core.mapType
                  (Core.typeVariable $ Core.name $ string "k")
                  (Core.typeVariable $ Core.name $ string "v")))
            (lets [
              "pairs">: Maps.toList $ var "m"] $
              withVar "kt" (Flows.bind (Flows.mapList (ref typeOfDef @@ var "cx" @@ var "vars" @@ var "types") $
                Lists.map (unaryFunction first) (var "pairs")) (ref singleTypeDef @@ string "map keys")) $
              withVar "vt" (Flows.bind (Flows.mapList (ref typeOfDef @@ var "cx" @@ var "vars" @@ var "types") $
                Lists.map (unaryFunction second) (var "pairs")) (ref singleTypeDef @@ string "map values")) $
              withVar "_" (ref checkTypeVariablesDef @@ var "vars" @@ var "kt") $
              withVar "_" (ref checkTypeVariablesDef @@ var "vars" @@ var "vt") $
              Flows.pure $ Core.typeMap $ Core.mapType (var "kt") (var "vt")),

        _Term_optional>>: lambda "mt" $
          ref typeOfCollectionDef @@ var "cx" @@ string "optional" @@ (unaryFunction Core.typeOptional) @@ var "vars" @@ var "types" @@
            (Optionals.maybe (list []) (unaryFunction Lists.singleton) $ var "mt"),

        _Term_product>>: lambda "tuple" $
          withVar "etypes" (Flows.mapList (ref typeOfDef @@ var "cx" @@ var "vars" @@ var "types") (var "tuple")) $
          withVar "_" (Flows.sequence $ Lists.map (ref checkTypeVariablesDef @@ var "vars") (var "etypes")) $
          Flows.pure $ Core.typeProduct $ var "etypes",

        _Term_record>>: lambda "record" $ lets [
          "tname">: Core.recordTypeName $ var "record",
          "fields">: Core.recordFields $ var "record"] $
          withVar "ftypes" (Flows.mapList (ref typeOfDef @@ var "cx" @@ var "vars" @@ var "types") $
            Lists.map (unaryFunction Core.fieldTerm) (var "fields")) $
          withVar "_" (Flows.sequence $ Lists.map (ref checkTypeVariablesDef @@ var "vars") (var "ftypes")) $
          ref typeOfNominalDef @@ string "record typeOf" @@ var "cx" @@ var "tname" @@
            (Core.typeRecord $ Core.rowType (var "tname") $
              Lists.zipWith (lambdas ["n", "t"] $ Core.fieldType (var "n") (var "t"))
                (Lists.map (unaryFunction Core.fieldName) (var "fields"))
                (var "ftypes")),

        _Term_set>>: lambda "els" $
          ref typeOfCollectionDef @@ var "cx" @@ string "set" @@ (unaryFunction Core.typeSet) @@ var "vars" @@ var "types" @@
            (Sets.toList $ var "els"),

--    TermSum (Sum idx size term1) -> ...

        _Term_typeAbstraction>>: lambda "ta" $ lets [
          "v">: Core.typeAbstractionParameter $ var "ta",
          "e">: Core.typeAbstractionBody $ var "ta"] $
          withVar "t1" (ref typeOfDef @@ var "cx" @@ (Sets.insert (var "v") (var "vars")) @@ var "types" @@ var "e") $
          withVar "_" (ref checkTypeVariablesDef @@ (Sets.insert (var "v") (var "vars")) @@ var "t1") $
          Flows.pure $ Core.typeForall $ Core.forallType (var "v") (var "t1"),

        _Term_typeApplication>>: lambda "tt" $ lets [
          "e">: Core.typedTermTerm $ var "tt",
          "t">: Core.typedTermType $ var "tt"] $
          withVar "t1" (ref typeOfDef @@ var "cx" @@ var "vars" @@ var "types" @@ var "e") $
          withVar "_" (ref checkTypeVariablesDef @@ var "vars" @@ var "t1") $
--      Flows.fail $ "type-checking type application"
--        ++ "\n\tterm: " ++ ShowCore.term e
--        ++ "\n\ttype: " ++ ShowCore.type_ t
--        ++ "\n\tvars: " ++ show (fmap unName $ S.toList vars)
--        ++ "\n\tt1: " ++ ShowCore.type_ t1
          cases _Type (var "t1") Nothing [
            _Type_forall>>: lambda "ft" $ lets [
              "v">: Core.forallTypeParameter $ var "ft",
              "t2">: Core.forallTypeBody $ var "ft"] $
              Flows.pure $ ref Substitution.substInTypeDef @@
                (Typing.typeSubst $ Maps.fromList $ list [pair (var "v") (var "t")]) @@
                (var "t2"),
            _Type_variable>>: constant $ Flows.fail $ Strings.cat $ list [
              string "not a forall type: ",
              ref ShowCore.typeDef @@ var "t1",
              string " in ",
              ref ShowCore.termDef @@ var "term"]],

        _Term_union>>: lambda "injection" $ lets [
          "tname">: Core.injectionTypeName $ var "injection",
          "field">: Core.injectionField $ var "injection",
          "fname">: Core.fieldName $ var "field",
          "term1">: Core.fieldTerm $ var "field",
          "fieldTypeOf">: lambdas ["ftype", "fname1"] $
            Logic.ifElse (Equality.equal (var "fname1") (var "fname"))
              (Flows.pure $ var "ftype")
              (Flows.map (unaryFunction Core.typeVariable) $ ref freshNameDef),
          "resolveType">: lambdas ["subst", "v"] $
            Optionals.fromMaybe (Core.typeVariable $ var "v") (Maps.lookup (var "v") (var "subst"))] $
          withVar "ftype" (ref typeOfDef @@ var "cx" @@ var "vars" @@ var "types" @@ var "term1") $
          withVar "_" (ref checkTypeVariablesDef @@ var "vars" @@ var "ftype") $
          withVar "schemaType" (ref requireSchemaTypeDef @@ var "cx" @@ var "tname") $ lets [
            "svars">: Core.typeSchemeVariables $ var "schemaType",
            "styp">: Core.typeSchemeType $ var "schemaType"] $
          withVar "sfields" (ref ExtractCore.unionTypeDef @@ var "tname" @@ var "styp") $ lets [
            "fnames">: Lists.map (unaryFunction Core.fieldTypeName) (var "sfields")] $
          withVar "ftypes" (Flows.mapList (var "fieldTypeOf" @@ var "ftype") (var "fnames")) $ lets [
            "expected">: Core.typeUnion $ Core.rowType (var "tname") $
              Lists.zipWith (lambdas ["n", "t"] $ Core.fieldType (var "n") (var "t")) (var "fnames") (var "ftypes")] $
          withVar "substWrapper" (ref Unification.unifyTypesDef @@
            (Typing.inferenceContextSchemaTypes $ var "cx") @@
            (var "styp") @@
            (var "expected") @@
            (string "union typeOf")) $ lets [
            "subst">: Typing.unTypeSubst $ var "substWrapper",
            "tparams">: Lists.map (var "resolveType" @@ var "subst") (var "svars")] $
          Flows.pure $ ref nominalApplicationDef @@ var "tname" @@ var "tparams",

        _Term_variable>>: lambda "name" $
          Optionals.maybe
            (Flows.fail $ Strings.cat $ list [
              string "unbound variable: ",
              Core.unName $ var "name"])
            (unaryFunction Flows.pure)
            (Maps.lookup (var "name") (var "types")),

        _Term_wrap>>: lambda "wt" $ lets [
          "tname">: Core.wrappedTermTypeName $ var "wt",
          "innerTerm">: Core.wrappedTermObject $ var "wt"] $
          withVar "innerType" (ref typeOfDef @@ var "cx" @@ var "vars" @@ var "types" @@ var "innerTerm") $
          withVar "_" (ref checkTypeVariablesDef @@ var "vars" @@ var "innerType") $
          ref typeOfNominalDef @@ string "wrapper typeOf" @@ var "cx" @@ var "tname" @@
            (Core.typeWrap $ Core.wrappedType (var "tname") (var "innerType"))])

typeOfCollectionDef :: TElement (InferenceContext -> String -> (Type -> Type) -> S.Set Name -> M.Map Name Type -> [Term] -> Flow s Type)
typeOfCollectionDef = inferenceDefinition "typeOfCollection" $
  doc "Infer the type of a collection of terms" $
  lambdas ["cx", "desc", "cons", "vars", "types", "els"] $
    Logic.ifElse (Lists.null $ var "els")
      (Flows.pure $ ref typeSchemeToFTypeDef @@
        (Core.typeScheme (list [Core.name $ string "t"]) $
          var "cons" @@ Core.typeVariable (Core.name $ string "t")))
      (withVar "etypes" (Flows.mapList (lambda "el" $ ref typeOfDef @@ var "cx" @@ var "vars" @@ var "types" @@ var "el") (var "els")) $
        withVar "et" (ref singleTypeDef @@ var "desc" @@ var "etypes") $
        withVar "result" (ref checkTypeVariablesDef @@ var "vars" @@ var "et") $
        Flows.pure $ var "cons" @@ var "et")

typeOfNominalDef :: TElement (String -> InferenceContext -> Name -> Type -> Flow s Type)
typeOfNominalDef = inferenceDefinition "typeOfNominal" $
  doc "Infer the type of a nominal type" $
  lambdas ["desc", "cx", "tname", "expected"] $ lets [
    "resolveType">: lambdas ["subst", "v"] $
      Optionals.fromMaybe (Core.typeVariable $ var "v") (Maps.lookup (var "v") (var "subst"))] $
    withVar "schemaType" (ref requireSchemaTypeDef @@ var "cx" @@ var "tname") $ lets [
      "svars">: Core.typeSchemeVariables $ var "schemaType",
      "styp">: Core.typeSchemeType $ var "schemaType"] $
    withVar "substWrapper" (ref Unification.unifyTypesDef @@
      (Typing.inferenceContextSchemaTypes $ var "cx") @@
      var "styp" @@
      var "expected" @@
      var "desc") $ lets [
        "subst">: Typing.unTypeSubst $ var "substWrapper",
        "tparams">: Lists.map (var "resolveType" @@ var "subst") (var "svars")] $
      Flows.pure $ ref nominalApplicationDef @@ var "tname" @@ var "tparams"

singleTypeDef :: TElement (String -> [Type] -> Flow s Type)
singleTypeDef = inferenceDefinition "singleType" $
  doc "Ensure all types in a list are equal and return the common type" $
  lambdas ["desc", "types"] $ lets [
    "h">: Lists.head $ var "types",
    "allEqual">: Lists.foldl
      (lambda "b" $ lambda "t" $ Logic.and (var "b") (Equality.equal (var "t") (var "h")))
      true
      (var "types")] $
    Logic.ifElse (var "allEqual")
      (Flows.pure $ var "h")
      (Flows.fail $ Strings.cat $ list [
        string "unequal types ",
        (ref Formatting.showListDef @@ ref ShowCore.typeDef @@ var "types"),
        string " in ",
        var "desc"])

checkTypeDef :: TElement (S.Set Name -> InferenceContext -> Type -> Term -> Flow s ())
checkTypeDef = inferenceDefinition "checkType" $
  doc "Check that a term has the expected type" $
  lambdas ["k", "g", "t", "e"] $ Logic.ifElse (ref debugInferenceDef)
    (withVar "t0" (ref typeOfDef @@ var "g" @@ var "k" @@ (ref toFContextDef @@ var "g") @@ var "e") $
      Logic.ifElse (Equality.equal (var "t0") (var "t"))
        (Flows.pure unit)
        (Flows.fail $ Strings.cat $ list [
          string "type checking failed: expected ",
          ref ShowCore.typeDef @@ var "t",
          string " but found ",
          ref ShowCore.typeDef @@ var "t0"]))
    (Flows.pure unit)

checkTypeVariablesDef :: TElement (S.Set Name -> Type -> Flow s ())
checkTypeVariablesDef = inferenceDefinition "checkTypeVariables" $
  doc "Check that all type variables in a type are bound" $
  lambdas ["vars", "typ"] $ cases _Type (var "typ")
    (Just $
      withVar "result" (Flows.sequence $ Lists.map (ref checkTypeVariablesDef @@ var "vars") $
        (ref Rewriting.subtypesDef @@ var "typ")) $
      Flows.pure unit) [
        _Type_forall>>: lambda "ft" $ ref checkTypeVariablesDef
          @@ (Sets.insert (Core.forallTypeParameter $ var "ft") (var "vars"))
          @@ (Core.forallTypeBody $ var "ft"),
        _Type_variable>>: lambda "v" $ Logic.ifElse (Sets.member (var "v") (var "vars"))
          (Flows.pure unit)
          (Flows.fail $ Strings.cat $ list [
            string "unbound type variable \"",
            Core.unName $ var "v",
            string "\" in ",
            ref ShowCore.typeDef @@ var "typ"])]

typeSchemeToFTypeDef :: TElement (TypeScheme -> Type)
typeSchemeToFTypeDef = inferenceDefinition "typeSchemeToFType" $
  doc "Convert a type scheme to a forall type" $
  lambda "ts" $ lets [
    "vars">:  Core.typeSchemeVariables $ var "ts",
    "body">:  Core.typeSchemeType $ var "ts"] $
    Lists.foldl
      (lambda "t" $ lambda "v" $ Core.typeForall $ Core.forallType (var "v") (var "t"))
      (var "body")
      (Lists.reverse $ var "vars")

toFContextDef :: TElement (InferenceContext -> M.Map Name Type)
toFContextDef = inferenceDefinition "toFContext" $
  doc "Convert inference context to type context" $
  lambda "cx" $
    Maps.map (ref typeSchemeToFTypeDef) $ Typing.inferenceContextDataTypes $ var "cx"

showInferenceResultDef :: TElement (InferenceResult -> String)
showInferenceResultDef = inferenceDefinition "showInferenceResult" $
  doc "Show an inference result for debugging" $
  lambda "result" $ lets [
    "term">: Typing.inferenceResultTerm $ var "result",
    "typ">: Typing.inferenceResultType $ var "result",
    "subst">: Typing.inferenceResultSubst $ var "result"] $
    Strings.cat $ list [
      string "{term=",
      ref ShowCore.termDef @@ var "term",
      string ", type=",
      ref ShowCore.typeDef @@ var "typ",
      string ", subst=",
      ref ShowCore.typeSubstDef @@ var "subst",
      string "}"]

emptyInferenceContextDef :: TElement InferenceContext
emptyInferenceContextDef = inferenceDefinition "emptyInferenceContext" $
  doc "An empty inference context" $
  Typing.inferenceContext
    (Phantoms.map M.empty)
    (Phantoms.map M.empty)
    (Phantoms.map M.empty)
    false

freeVariablesInContextDef :: TElement (InferenceContext -> S.Set Name)
freeVariablesInContextDef = inferenceDefinition "freeVariablesInContext" $
  doc "Get all free variables in an inference context" $
  lambda "cx" $
    Lists.foldl (binaryFunction Sets.union) Sets.empty $
      Lists.map (ref Rewriting.freeVariablesInTypeSchemeSimpleDef) $
        Maps.elems $ Typing.inferenceContextDataTypes $ var "cx"

generalizeDef :: TElement (InferenceContext -> Type -> TypeScheme)
generalizeDef = inferenceDefinition "generalize" $
  doc "Generalize a type to a type scheme" $
  lambdas ["cx", "typ"] $ lets [
    "vars">: Lists.nub $ Lists.filter (ref isUnboundDef @@ var "cx") $
              Sets.toList $ ref Rewriting.freeVariablesInTypeDef @@ var "typ"] $
     Core.typeScheme (var "vars") (var "typ")

isUnboundDef :: TElement (InferenceContext -> Name -> Bool)
isUnboundDef = inferenceDefinition "isUnbound" $
  doc "Check if a variable is unbound in context" $
  lambdas ["cx", "v"] $
    Logic.and
      (Logic.not $ Sets.member (var "v") $ ref freeVariablesInContextDef @@ var "cx")
      (Logic.not $ Maps.member (var "v") $ Typing.inferenceContextSchemaTypes $ var "cx")

graphToInferenceContextDef :: TElement (Graph -> Flow s InferenceContext)
graphToInferenceContextDef = inferenceDefinition "graphToInferenceContext" $
  doc "Convert a graph to an inference context" $
  lambda "g0" $ lets [
    "schema">: Optionals.fromMaybe (var "g0") (Graph.graphSchema $ var "g0"),
    "primTypes">: Maps.fromList $ Lists.map
      (lambda "p" $ pair (Graph.primitiveName $ var "p") (Graph.primitiveType $ var "p"))
      (Maps.elems $ Graph.graphPrimitives $ var "g0"),
    "varTypes">: Maps.empty] $
    withVar "schemaTypes" (ref Schemas.schemaGraphToTypingEnvironmentDef @@ var "schema") $
    Flows.pure $ Typing.inferenceContext (var "schemaTypes") (var "primTypes") (var "varTypes") false

-- Note: this operation is expensive, as it creates a new typing environment for each individual term
inferInGraphContextDef :: TElement (Term -> Flow Graph InferenceResult)
inferInGraphContextDef = inferenceDefinition "inferInGraphContext" $
  doc "Infer the type of a term in graph context" $
  lambda "term" $
    withVar "g" (ref Errors.getStateDef) $
    withVar "cx" (ref graphToInferenceContextDef @@ var "g") $
    ref inferTypeOfTermDef @@ var "cx" @@ var "term" @@ string "single term"

inferGraphTypesDef :: TElement (Graph -> Flow s Graph)
inferGraphTypesDef = inferenceDefinition "inferGraphTypes" $
  doc "Infer types for all elements in a graph" $
  lambda "g0" $ lets [
    "fromLetTerm">: lambda "l" $ lets [
      "bindings">: Core.letBindings $ var "l",
      "env">: Core.letEnvironment $ var "l",
      "fromBinding">: lambda "b" $ pair
        (Core.letBindingName $ var "b")
        (Graph.element
          (Core.letBindingName $ var "b")
          (Core.letBindingTerm $ var "b")
          (Core.letBindingType $ var "b"))] $
      Graph.graph
        (Maps.fromList $ Lists.map (var "fromBinding") (var "bindings"))
        (Maps.empty)
        (Maps.empty)
        (var "env")
        (Graph.graphPrimitives $ var "g0")
        (Graph.graphSchema $ var "g0"),
    "toLetTerm">: lambda "g" $ lets [
      "toBinding">: lambda "el" $ Core.letBinding
        (Graph.elementName $ var "el")
        (Graph.elementTerm $ var "el")
        nothing] $
      Core.termLet $ Core.let_
        (Lists.map (var "toBinding") $ Maps.elems $ Graph.graphElements $ var "g")
        (Graph.graphBody $ var "g"),
    "withResult">: lambda "result" $ lets [
      "term">: Typing.inferenceResultTerm $ var "result",
      "ts">: Typing.inferenceResultType $ var "result"] $
      cases _Term (ref Rewriting.normalizeTypeVariablesInTermDef @@ var "term") Nothing [
        _Term_let>>: lambda "l" $ Flows.pure $ var "fromLetTerm" @@ var "l",
        _Term_variable>>: constant $ Flows.fail $ string "Expected inferred graph as let term"]] $
    ref Monads.withTraceDef @@ string "graph inference" @@
      (withVar "cx" (ref graphToInferenceContextDef @@ var "g0") $
        Flows.bind
          (ref inferTypeOfTermDef @@ var "cx" @@ (var "toLetTerm" @@ var "g0") @@ string "graph term")
          (var "withResult"))

inferManyDef :: TElement (InferenceContext -> [(Term, String)] -> Flow s ([Term], [Type], TypeSubst))
inferManyDef = inferenceDefinition "inferMany" $
  doc "Infer types for multiple terms" $
  lambdas ["cx", "pairs"] $
    Logic.ifElse (Lists.null $ var "pairs")
      (Flows.pure $ pair (list []) $ pair (list []) (ref Substitution.idTypeSubstDef))
      (lets [
        "e">: first $ Lists.head $ var "pairs",
        "desc">: second $ Lists.head $ var "pairs",
        "tl">: Lists.tail $ var "pairs"] $
        withVar "result1" (ref inferTypeOfTermDef @@ var "cx" @@ var "e" @@ var "desc") $ lets [
          "e1">: Typing.inferenceResultTerm $ var "result1",
          "t1">: Typing.inferenceResultType $ var "result1",
          "s1">: Typing.inferenceResultSubst $ var "result1"] $
          withVar "result2" (ref inferManyDef @@ (ref Substitution.substInContextDef @@ var "s1" @@ var "cx") @@ var "tl") $ lets [
            "e2">: first $ var "result2",
            "t2">: first $ second $ var "result2",
            "s2">: second $ second $ var "result2"] $
            Flows.pure $ pair
              (Lists.cons (ref Substitution.substTypesInTermDef @@ var "s2" @@ var "e1") (var "e2"))
              (pair
                (Lists.cons (ref Substitution.substInTypeDef @@ var "s2" @@ var "t1") (var "t2"))
                (ref Substitution.composeTypeSubstDef @@ var "s1" @@ var "s2")))

inferTypeOfDef :: TElement (InferenceContext -> Term -> Flow s (Term, TypeScheme))
inferTypeOfDef = inferenceDefinition "inferTypeOf" $
  doc "Infer the type of a term and return a type scheme" $
  lambdas ["cx", "term"] $ lets [
    "letTerm">: Core.termLet $ Core.let_
      (list [Core.letBinding (Core.name $ string "ignoredVariableName") (var "term") nothing])
      (TTerms.string "ignoredEnvironment"),
    "unifyAndSubst">: lambda "result" $ lets [
      "subst">: Typing.inferenceResultSubst $ var "result"] $
      withVar "letResult" (ref Annotations.withEmptyGraphDef @@
        (ref ExtractCore.letTermDef @@
          (ref Rewriting.normalizeTypeVariablesInTermDef @@
            Typing.inferenceResultTerm (var "result")))) $ lets [
        "bindings">: Core.letBindings $ var "letResult"] $
        Logic.ifElse (Equality.equal (int32 1) (Lists.length $ var "bindings"))
          (lets [
            "binding">: Lists.head $ var "bindings",
            "term1">: Core.letBindingTerm $ var "binding",
            "mts">: Core.letBindingType $ var "binding"] $
            Optionals.maybe
              (Flows.fail $ string "Expected a type scheme")
              (lambda "ts" $ Flows.pure $ pair (var "term1") (var "ts"))
              (var "mts"))
          (Flows.fail $ Strings.cat $ list [
            string "Expected a single binding with a type scheme, but got: ",
            Literals.showInt32 $ Lists.length $ var "bindings",
            string " bindings"])] $
    withVar "result" (ref inferTypeOfTermDef @@ var "cx" @@ var "letTerm" @@ string "infer type of term") $
    var "unifyAndSubst" @@ var "result"

inferTypeOfAnnotatedTermDef :: TElement (InferenceContext -> AnnotatedTerm -> Flow s InferenceResult)
inferTypeOfAnnotatedTermDef = inferenceDefinition "inferTypeOfAnnotatedTerm" $
  doc "Infer the type of an annotated term" $
  lambdas ["cx", "at"] $ lets [
    "term">: Core.annotatedTermSubject $ var "at",
    "ann">: Core.annotatedTermAnnotation $ var "at"] $
    Flows.map
      (lambda "result" $ lets [
        "iterm">: Typing.inferenceResultTerm $ var "result",
        "itype">: Typing.inferenceResultType $ var "result",
        "isubst">: Typing.inferenceResultSubst $ var "result"] $
        Typing.inferenceResult
          (Core.termAnnotated $ Core.annotatedTerm (var "iterm") (var "ann"))
          (var "itype")
          (var "isubst"))
      (ref inferTypeOfTermDef @@ var "cx" @@ var "term" @@ string "annotated term")

inferTypeOfApplicationDef :: TElement (InferenceContext -> Application -> Flow s InferenceResult)
inferTypeOfApplicationDef = inferenceDefinition "inferTypeOfApplication" $
  doc "Infer the type of a function application" $
  lambdas ["cx", "app"] $ lets [
    "e0">: Core.applicationFunction $ var "app",
    "e1">: Core.applicationArgument $ var "app"] $
    withVar "lhsResult" (ref inferTypeOfTermDef @@ var "cx" @@ var "e0" @@ string "lhs") $ lets [
    "a">: Typing.inferenceResultTerm $ var "lhsResult",
    "t0">: Typing.inferenceResultType $ var "lhsResult",
    "s0">: Typing.inferenceResultSubst $ var "lhsResult"] $
    withVar "rhsResult" (ref inferTypeOfTermDef @@ (ref Substitution.substInContextDef @@ var "s0" @@ var "cx") @@ var "e1" @@ string "rhs") $ lets [
    "b">: Typing.inferenceResultTerm $ var "rhsResult",
    "t1">: Typing.inferenceResultType $ var "rhsResult",
    "s1">: Typing.inferenceResultSubst $ var "rhsResult"] $
    withVar "v" (ref freshNameDef) $
    withVar "s2" (ref Unification.unifyTypesDef
      @@ (Typing.inferenceContextSchemaTypes $ var "cx")
      @@ (ref Substitution.substInTypeDef @@ var "s1" @@ var "t0")
      @@ (Core.typeFunction $ Core.functionType (var "t1") (Core.typeVariable $ var "v"))
      @@ string "application lhs") $ lets [
    "rExpr">: Core.termApplication $ Core.application
      (ref Substitution.substTypesInTermDef @@ (ref Substitution.composeTypeSubstDef @@ var "s1" @@ var "s2") @@ var "a")
      (ref Substitution.substTypesInTermDef @@ var "s2" @@ var "b"),
    "rType">: ref Substitution.substInTypeDef @@ var "s2" @@ Core.typeVariable (var "v"),
    "rSubst">: ref Substitution.composeTypeSubstListDef @@ list [var "s0", var "s1", var "s2"]] $
    Flows.pure $ Typing.inferenceResult (var "rExpr") (var "rType") (var "rSubst")

inferTypeOfCaseStatementDef :: TElement (InferenceContext -> CaseStatement -> Flow s InferenceResult)
inferTypeOfCaseStatementDef = inferenceDefinition "inferTypeOfCaseStatement" $
  doc "Infer the type of a case statement" $
  lambdas ["cx", "caseStmt"] $ lets [
    "tname">: Core.caseStatementTypeName $ var "caseStmt",
    "dflt">: Core.caseStatementDefault $ var "caseStmt",
    "cases">: Core.caseStatementCases $ var "caseStmt",
    "fnames">: Lists.map (unaryFunction Core.fieldName) (var "cases")] $
    withVar "schemaType" (ref requireSchemaTypeDef @@ var "cx" @@ var "tname") $ lets [
      "svars">: Core.typeSchemeVariables $ var "schemaType",
      "styp">: Core.typeSchemeType $ var "schemaType"] $
      withVar "sfields" (ref ExtractCore.unionTypeDef @@ var "tname" @@ var "styp") $
      withVar "dfltResult" (Flows.traverseOptional (lambda "t" $ ref inferTypeOfTermDef @@ var "cx" @@ var "t" @@
        (Strings.cat $ list [string "case ", Core.unName $ var "tname", string ".<default>"])) (var "dflt")) $
      withVar "caseResults" (ref inferManyDef @@ var "cx" @@
        Lists.map (lambda "f" $ pair (Core.fieldTerm $ var "f")
          (Strings.cat $ list [string "case ", Core.unName $ var "tname", string ".", Core.unName $ Core.fieldName $ var "f"])) (var "cases")) $ lets [
      "iterms">: first $ var "caseResults",
      "itypes">: first $ second $ var "caseResults",
      "isubst">: second $ second $ var "caseResults"] $
      withVar "codv" (ref freshNameDef) $ lets [
      "cod">: Core.typeVariable $ var "codv",
      "caseMap">: Maps.fromList $ Lists.map (lambda "ft" $ pair (Core.fieldTypeName $ var "ft") (Core.fieldTypeType $ var "ft")) (var "sfields"),
      "dfltConstraints">: ref Functions.optionalToListDef @@ (Optionals.map (lambda "r" $ Typing.typeConstraint (var "cod") (Typing.inferenceResultType $ var "r") (string "match default")) (var "dfltResult")),
      "caseConstraints">: Optionals.cat $ Lists.zipWith
        (lambdas ["fname", "itype"] $ Optionals.map (lambda "ftype" $ Typing.typeConstraint (var "itype") (Core.typeFunction $ Core.functionType (var "ftype") (var "cod")) (string "case type"))
          (Maps.lookup (var "fname") (var "caseMap")))
        (var "fnames") (var "itypes")] $
      ref mapConstraintsDef @@ var "cx" @@
        (lambda "subst" $ ref yieldDef
          @@ (Core.termFunction $ Core.functionElimination $ Core.eliminationUnion $
              Core.caseStatement (var "tname") (Optionals.map (unaryFunction Typing.inferenceResultTerm) $ var "dfltResult") $
              Lists.zipWith (lambdas ["n", "t"] $ Core.field (var "n") (var "t")) (var "fnames") (var "iterms"))
          @@ (Core.typeFunction $ Core.functionType
              (ref nominalApplicationDef @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
              (var "cod"))
          @@ (ref Substitution.composeTypeSubstListDef @@ (Lists.concat $ list [
              ref Functions.optionalToListDef @@ (Optionals.map (unaryFunction Typing.inferenceResultSubst) (var "dfltResult")),
              list [var "isubst", var "subst"]]))) @@
        (Lists.concat $ list [var "dfltConstraints", var "caseConstraints"])

inferTypeOfCollectionDef :: TElement (InferenceContext -> (Type -> Type) -> ([Term] -> Term) -> String -> [Term] -> Flow s InferenceResult)
inferTypeOfCollectionDef = inferenceDefinition "inferTypeOfCollection" $
  doc "Infer the type of a collection" $
  lambdas ["cx", "typCons", "trmCons", "desc", "els"] $
    withVar "var" (ref freshNameDef) $
    withVar "results" (ref inferManyDef @@ var "cx" @@
      (Lists.zip (var "els") $ Lists.map (lambda "i" $ Strings.cat $ list [string "#", Literals.showInt32 $ var "i"]) $
        Math.rangeInt32 (int32 1) (Math.add (Lists.length $ var "els") (int32 1)))) $ lets [
    "terms">: first $ var "results",
    "types">: first $ second $ var "results",
    "subst1">: second $ second $ var "results",
    "constraints">: Lists.map (lambda "t" $ Typing.typeConstraint (Core.typeVariable $ var "var") (var "t") (var "desc")) (var "types")] $
    ref mapConstraintsDef @@ var "cx" @@
      (lambda "subst2" $ lets [
        "iterm">: var "trmCons" @@ var "terms",
        "itype">: var "typCons" @@ (Core.typeVariable $ var "var"),
        "isubst">: ref Substitution.composeTypeSubstDef @@ var "subst1" @@ var "subst2"] $
        ref yieldDef @@ var "iterm" @@ var "itype" @@ var "isubst") @@
      var "constraints"

inferTypeOfEliminationDef :: TElement (InferenceContext -> Elimination -> Flow s InferenceResult)
inferTypeOfEliminationDef = inferenceDefinition "inferTypeOfElimination" $
  doc "Infer the type of an elimination" $
  lambdas ["cx", "elm"] $
    cases _Elimination (var "elm") Nothing [
      _Elimination_product>>: lambda "tp" $ ref inferTypeOfTupleProjectionDef @@ var "cx" @@ var "tp",
      _Elimination_record>>: lambda "p" $ ref inferTypeOfProjectionDef @@ var "cx" @@ var "p",
      _Elimination_union>>: lambda "c" $ ref inferTypeOfCaseStatementDef @@ var "cx" @@ var "c",
      _Elimination_wrap>>: lambda "tname" $ ref inferTypeOfUnwrapDef @@ var "cx" @@ var "tname"]

inferTypeOfFunctionDef :: TElement (InferenceContext -> Function -> Flow s InferenceResult)
inferTypeOfFunctionDef = inferenceDefinition "inferTypeOfFunction" $
  doc "Infer the type of a function" $
  lambdas ["cx", "f"] $
    cases _Function (var "f") Nothing [
      _Function_elimination>>: lambda "elm" $ ref inferTypeOfEliminationDef @@ var "cx" @@ var "elm",
      _Function_lambda>>: lambda "l" $ ref inferTypeOfLambdaDef @@ var "cx" @@ var "l",
      _Function_primitive>>: lambda "name" $ ref inferTypeOfPrimitiveDef @@ var "cx" @@ var "name"]

inferTypeOfInjectionDef :: TElement (InferenceContext -> Injection -> Flow s InferenceResult)
inferTypeOfInjectionDef = inferenceDefinition "inferTypeOfInjection" $
  doc "Infer the type of a union injection" $
  lambdas ["cx", "injection"] $ lets [
    "tname">: Core.injectionTypeName $ var "injection",
    "field">: Core.injectionField $ var "injection",
    "fname">: Core.fieldName $ var "field",
    "term">: Core.fieldTerm $ var "field"] $
    withVar "schemaType" (ref requireSchemaTypeDef @@ var "cx" @@ var "tname") $
    withVar "result" (ref inferTypeOfTermDef @@ var "cx" @@ var "term" @@ string "injected term") $ lets [
      "svars">: Core.typeSchemeVariables $ var "schemaType",
      "styp">: Core.typeSchemeType $ var "schemaType",
      "iterm">: Typing.inferenceResultTerm $ var "result",
      "ityp">: Typing.inferenceResultType $ var "result",
      "isubst">: Typing.inferenceResultSubst $ var "result"] $
    withVar "sfields" (ref ExtractCore.unionTypeDef @@ var "tname" @@ var "styp") $
    withVar "ftyp" (ref Schemas.findFieldTypeDef @@ var "fname" @@ var "sfields") $
    ref mapConstraintsDef @@ var "cx" @@
      (lambda "subst" $ ref yieldDef
        @@ (Core.termUnion $ Core.injection (var "tname") $ Core.field (var "fname") (var "iterm"))
        @@ (ref nominalApplicationDef @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
        @@ (ref Substitution.composeTypeSubstDef @@ var "isubst" @@ var "subst")) @@
      list [Typing.typeConstraint (var "ftyp") (var "ityp") (string "schema type of injected field")]

inferTypeOfLambdaDef :: TElement (InferenceContext -> Lambda -> Flow s InferenceResult)
inferTypeOfLambdaDef = inferenceDefinition "inferTypeOfLambda" $
  doc "Infer the type of a lambda function" $
  lambdas ["cx", "lambda"] $ lets [
    "var">: Core.lambdaParameter $ var "lambda",
    "body">: Core.lambdaBody $ var "lambda"] $
    withVar "vdom" (ref freshNameDef) $ lets [
    "dom">: Core.typeVariable $ var "vdom",
    "cx2">: ref extendContextDef @@ list [pair (var "var") (Core.typeScheme (list []) (var "dom"))] @@ var "cx"] $
    withVar "result" (ref inferTypeOfTermDef @@ var "cx2" @@ var "body" @@ string "lambda body") $ lets [
    "iterm">: Typing.inferenceResultTerm $ var "result",
    "icod">: Typing.inferenceResultType $ var "result",
    "isubst">: Typing.inferenceResultSubst $ var "result",
    "rdom">: ref Substitution.substInTypeDef @@ var "isubst" @@ var "dom",
    "rterm">: Core.termFunction $ Core.functionLambda $ Core.lambda (var "var") (just $ var "rdom") (var "iterm"),
    "rtype">: Core.typeFunction $ Core.functionType (var "rdom") (var "icod"),
    "vars">: Sets.unions $ list [
      ref Rewriting.freeVariablesInTypeDef @@ var "rdom",
      ref Rewriting.freeVariablesInTypeDef @@ var "icod",
      ref freeVariablesInContextDef @@ (ref Substitution.substInContextDef @@ var "isubst" @@ var "cx2")],
    "cx3">: ref Substitution.substInContextDef @@ var "isubst" @@ var "cx"] $
--            checkType vars cx3 rtype rterm
    Flows.pure $ Typing.inferenceResult (var "rterm") (var "rtype") (var "isubst")

-- | Normalize a let term before inferring its type.
inferTypeOfLetDef :: TElement (InferenceContext -> Let -> Flow s InferenceResult)
inferTypeOfLetDef = inferenceDefinition "inferTypeOfLet" $
  doc "Normalize a let term before inferring its type" $
  lambdas ["cx", "let0"] $ lets [
    "bindings0">: Core.letBindings $ var "let0",
    "env0">: Core.letEnvironment $ var "let0",
    "names">: Lists.map (unaryFunction Core.letBindingName) (var "bindings0"),
    "nameSet">: Sets.fromList $ var "names",
    "toPair">: lambda "binding" $ lets [
      "name">: Core.letBindingName $ var "binding",
      "term">: Core.letBindingTerm $ var "binding"] $
      pair (var "name") $ Lists.filter (lambda "n" $ Sets.member (var "n") (var "nameSet")) $
        Sets.toList $ ref Rewriting.freeVariablesInTermDef @@ var "term",
    "adjList">: Lists.map (var "toPair") (var "bindings0"),
    "groups">: ref Sorting.topologicalSortComponentsDef @@ var "adjList",
    "bindingMap">: Maps.fromList $ Lists.zip (var "names") (var "bindings0"),
    "createLet">: lambdas ["e", "group"] $ Core.termLet $ Core.let_
      (Optionals.cat $ Lists.map (lambda "n" $ Maps.lookup (var "n") (var "bindingMap")) (var "group"))
      (var "e"),
    -- Note: this rewritten let term will yield success in all cases of dependencies among letrec bindings *except*
    --       in cases of polymorphic recursion. In those cases, type hints will be needed (#162).
    "rewrittenLet">: Lists.foldl (var "createLet") (var "env0") $ Lists.reverse (var "groups"),
    "restoreLet">: lambda "iterm" $ lets [
      "helper">: lambdas ["level", "bins", "term"] $
        Logic.ifElse (Equality.equal (var "level") (int32 0))
          (pair (var "bins") (var "term"))
          (cases _Term (var "term") Nothing [
            _Term_let>>: lambda "l" $ lets [
              "bs">: Core.letBindings $ var "l",
              "e">: Core.letEnvironment $ var "l"] $
              var "helper" @@
                (Math.sub (var "level") (int32 1)) @@
                (Lists.concat $ list [var "bs", var "bins"]) @@
                (var "e")]),
      "result">: var "helper" @@ (Lists.length $ var "groups") @@ list [] @@ var "iterm",
      "bindingList">: first $ var "result",
      "e">: second $ var "result",
      "bindingMap2">: Maps.fromList $ Lists.map (lambda "b" $ pair (Core.letBindingName $ var "b") (var "b")) (var "bindingList")] $
      Core.termLet $ Core.let_
        (Optionals.cat $ Lists.map (lambda "n" $ Maps.lookup (var "n") (var "bindingMap2")) (var "names"))
        (var "e"),
    "rewriteResult">: lambda "result" $ lets [
      "iterm">: Typing.inferenceResultTerm $ var "result",
      "itype">: Typing.inferenceResultType $ var "result",
      "isubst">: Typing.inferenceResultSubst $ var "result"] $
      Typing.inferenceResult (var "restoreLet" @@ var "iterm") (var "itype") (var "isubst")] $
    Flows.map (var "rewriteResult") $
      cases _Term (var "rewrittenLet")
        (Just $ ref inferTypeOfTermDef @@ var "cx" @@ var "rewrittenLet" @@ string "empty let term") [
        _Term_let>>: lambda "l" $ ref inferTypeOfLetAfterNormalizationDef @@ var "cx" @@ var "l"]

inferTypeOfLetAfterNormalizationDef :: TElement (InferenceContext -> Let -> Flow s InferenceResult)
inferTypeOfLetAfterNormalizationDef = inferenceDefinition "inferTypeOfLetAfterNormalization" $
  doc "Infer the type of a let (letrec) term which is already in a normal form" $
  lambdas ["cx0", "letTerm"] $ lets [
    "bins0">: Core.letBindings $ var "letTerm",
    "env0">: Core.letEnvironment $ var "letTerm",
    "bnames">: Lists.map (unaryFunction Core.letBindingName) (var "bins0")] $
    withVar "bvars" (ref freshNamesDef @@ (Lists.length $ var "bins0")) $ lets [
    "tbins0">: Lists.map (unaryFunction Core.typeVariable) (var "bvars"),
    "cx1">: ref extendContextDef
      @@ (Lists.zip (var "bnames") $ Lists.map (lambda "t" $ Core.typeScheme (list []) (var "t")) (var "tbins0"))
      @@ (var "cx0")] $
    withVar "inferredResult" (ref inferTypesOfTemporaryLetBindingsDef @@ var "cx1" @@ var "bins0") $ lets [
    "bterms1">: first $ var "inferredResult",
    "tbins1">: first $ second $ var "inferredResult",
    "s1">: second $ second $ var "inferredResult"] $
    withVar "s2" (ref Unification.unifyTypeListsDef @@
      (Typing.inferenceContextSchemaTypes $ var "cx0") @@
      (Lists.map (ref Substitution.substInTypeDef @@ var "s1") (var "tbins0")) @@
      (var "tbins1") @@
      (string "temporary type bindings")) $ lets [
    "g2">: ref Substitution.substInContextDef @@
      (ref Substitution.composeTypeSubstDef @@ var "s1" @@ var "s2") @@
      (var "cx0"),
    "tsbins1">: Lists.zip (var "bnames") $
      Lists.map (lambda "t" $ ref generalizeDef @@ var "g2" @@
        (ref Substitution.substInTypeDef @@ var "s2" @@ var "t")) (var "tbins1")] $
    withVar "envResult" (ref inferTypeOfTermDef @@
      (ref extendContextDef @@ var "tsbins1" @@ var "g2") @@
      (var "env0") @@
      (string "let environment")) $ lets [
    "env1">: Typing.inferenceResultTerm $ var "envResult",
    "tenv">: Typing.inferenceResultType $ var "envResult",
    "senv">: Typing.inferenceResultSubst $ var "envResult",
    "st1">: Typing.termSubst $ Maps.fromList $
      Lists.map (lambda "pair" $ lets [
        "name">: first $ var "pair",
        "ts">: second $ var "pair"] $
        pair (var "name") $
          Lists.foldl (lambdas ["t", "v"] $ Core.termTypeApplication $
            Core.typedTerm (var "t") (Core.typeVariable $ var "v"))
            (Core.termVariable $ var "name")
            (Core.typeSchemeVariables $ var "ts")) (var "tsbins1"),
    "createBinding">: lambda "bindingPair" $ lets [
      "nameTsPair">: first $ var "bindingPair",
      "term">: second $ var "bindingPair",
      "name">: first $ var "nameTsPair",
      "ts">: second $ var "nameTsPair",
      "typeAbstractedTerm">: Lists.foldl
        (lambdas ["b", "v"] $ Core.termTypeAbstraction $ Core.typeAbstraction (var "v") (var "b"))
        (ref Substitution.substituteInTermDef @@ var "st1" @@ var "term")
        (Core.typeSchemeVariables $ var "ts")] $
      Core.letBinding (var "name")
        (ref Substitution.substTypesInTermDef @@
          (ref Substitution.composeTypeSubstDef @@ var "senv" @@ var "s2") @@
          (var "typeAbstractedTerm"))
        (just $ ref Substitution.substInTypeSchemeDef @@ var "senv" @@ var "ts"),
    "bins1">: Lists.map (var "createBinding") $
      Lists.zip (var "tsbins1") (var "bterms1"),
    "ret">: Typing.inferenceResult
      (Core.termLet $ Core.let_ (var "bins1") (var "env1"))
      (var "tenv")
      (ref Substitution.composeTypeSubstListDef @@ list [var "s1", var "s2", var "senv"])] $
    Flows.pure $ var "ret"

inferTypeOfListDef :: TElement (InferenceContext -> [Term] -> Flow s InferenceResult)
inferTypeOfListDef = inferenceDefinition "inferTypeOfList" $
  doc "Infer the type of a list" $
  lambda "cx" $
    ref inferTypeOfCollectionDef @@ var "cx" @@ (unaryFunction Core.typeList) @@ (unaryFunction Core.termList) @@ string "list element"

inferTypeOfLiteralDef :: TElement (InferenceContext -> Literal -> Flow s InferenceResult)
inferTypeOfLiteralDef = inferenceDefinition "inferTypeOfLiteral" $
  doc "Infer the type of a literal" $
  lambdas ["_", "lit"] $
    Flows.pure $ Typing.inferenceResult
      (Core.termLiteral $ var "lit")
      (Core.typeLiteral $ ref Variants.literalTypeDef @@ var "lit")
      (ref Substitution.idTypeSubstDef)

inferTypeOfMapDef :: TElement (InferenceContext -> M.Map Term Term -> Flow s InferenceResult)
inferTypeOfMapDef = inferenceDefinition "inferTypeOfMap" $
  doc "Infer the type of a map" $
  lambdas ["cx", "m"] $
    withVar "kvar" (ref freshNameDef) $
    withVar "vvar" (ref freshNameDef) $
    Logic.ifElse (Maps.null $ var "m")
      -- TODO: get rid of this special case; it should follow from the general case
      (Flows.pure $ ref yieldDef
        @@ (Core.termMap Maps.empty)
        @@ (Core.typeMap $ Core.mapType (Core.typeVariable $ var "kvar") (Core.typeVariable $ var "vvar"))
        @@ ref Substitution.idTypeSubstDef)
      (withVar "kresults" (ref inferManyDef @@ var "cx" @@
        (Lists.map (lambda "k" $ pair (var "k") (string "map key")) $ Maps.keys $ var "m")) $ lets [
          "kterms">: first $ var "kresults",
          "ktypes">: first $ second $ var "kresults",
          "ksubst">: second $ second $ var "kresults"] $
        withVar "vresults" (ref inferManyDef @@ var "cx" @@
          (Lists.map (lambda "v" $ pair (var "v") (string "map value")) $ Maps.elems $ var "m")) $ lets [
            "vterms">: first $ var "vresults",
            "vtypes">: first $ second $ var "vresults",
            "vsubst">: second $ second $ var "vresults",
            "kcons">: Lists.map (lambda "t" $ Typing.typeConstraint (Core.typeVariable $ var "kvar") (var "t") (string "map key")) (var "ktypes"),
            "vcons">: Lists.map (lambda "t" $ Typing.typeConstraint (Core.typeVariable $ var "vvar") (var "t") (string "map value")) (var "vtypes")] $
          ref mapConstraintsDef @@ var "cx" @@
            (lambda "subst" $ ref yieldDef
              @@ (Core.termMap $ Maps.fromList $ Lists.zip (var "kterms") (var "vterms"))
              @@ (Core.typeMap $ Core.mapType (Core.typeVariable $ var "kvar") (Core.typeVariable $ var "vvar"))
              @@ (ref Substitution.composeTypeSubstListDef @@ list [var "ksubst", var "vsubst", var "subst"])) @@
            (Lists.concat $ list [var "kcons", var "vcons"]))

inferTypeOfOptionalDef :: TElement (InferenceContext -> Maybe Term -> Flow s InferenceResult)
inferTypeOfOptionalDef = inferenceDefinition "inferTypeOfOptional" $
  doc "Infer the type of an optional" $
  lambdas ["cx", "m"] $ lets [
    "trmCons">: lambda "terms" $
      Logic.ifElse (Lists.null $ var "terms")
        (Core.termOptional nothing)
        (Core.termOptional $ just $ Lists.head $ var "terms")] $
    ref inferTypeOfCollectionDef
      @@ var "cx"
      @@ (unaryFunction Core.typeOptional)
      @@ var "trmCons"
      @@ string "optional element"
      @@ (Optionals.maybe (list []) (unaryFunction Lists.singleton) $ var "m")

inferTypeOfPrimitiveDef :: TElement (InferenceContext -> Name -> Flow s InferenceResult)
inferTypeOfPrimitiveDef = inferenceDefinition "inferTypeOfPrimitive" $
  doc "Infer the type of a primitive function" $
  lambdas ["cx", "name"] $
    Optionals.maybe
      (Flows.fail $ Strings.cat2 (string "No such primitive: ") (Core.unName $ var "name"))
      (lambda "scheme" $
        withVar "ts" (ref instantiateTypeSchemeDef @@ var "scheme") $ lets [
          "vars">: Core.typeSchemeVariables $ var "ts",
          "itype">: Core.typeSchemeType $ var "ts",
          "iterm">: Lists.foldl (lambdas ["t", "v"] $ Core.termTypeApplication $ Core.typedTerm (var "t") (Core.typeVariable $ var "v"))
            (Core.termFunction $ Core.functionPrimitive $ var "name") (var "vars")] $
          ref yieldCheckedDef @@ var "cx" @@ var "vars" @@ var "iterm" @@ var "itype" @@ ref Substitution.idTypeSubstDef)
      (Maps.lookup (var "name") (Typing.inferenceContextPrimitiveTypes $ var "cx"))

inferTypeOfProductDef :: TElement (InferenceContext -> [Term] -> Flow s InferenceResult)
inferTypeOfProductDef = inferenceDefinition "inferTypeOfProduct" $
  doc "Infer the type of a product (tuple)" $
  lambdas ["cx", "els"] $
    Flows.map
      (lambda "results" $ lets [
        "iterms">: first $ var "results",
        "itypes">: first $ second $ var "results",
        "isubst">: second $ second $ var "results"] $
        ref yieldDef @@ (Core.termProduct $ var "iterms") @@ (Core.typeProduct $ var "itypes") @@ var "isubst")
      (ref inferManyDef @@ var "cx" @@ (Lists.map (lambda "e" $ pair (var "e") (string "tuple element")) $ var "els"))

inferTypeOfProjectionDef :: TElement (InferenceContext -> Projection -> Flow s InferenceResult)
inferTypeOfProjectionDef = inferenceDefinition "inferTypeOfProjection" $
  doc "Infer the type of a record projection" $
  lambdas ["cx", "proj"] $ lets [
    "tname">: Core.projectionTypeName $ var "proj",
    "fname">: Core.projectionField $ var "proj"] $
    withVar "schemaType" (ref requireSchemaTypeDef @@ var "cx" @@ var "tname") $ lets [
    "svars">: Core.typeSchemeVariables $ var "schemaType",
    "styp">: Core.typeSchemeType $ var "schemaType"] $
    withVar "sfields" (ref ExtractCore.recordTypeDef @@ var "tname" @@ var "styp") $
    withVar "ftyp" (ref Schemas.findFieldTypeDef @@ var "fname" @@ var "sfields") $
    Flows.pure $ ref yieldDef
      @@ (Core.termFunction $ Core.functionElimination $ Core.eliminationRecord $
          Core.projection (var "tname") (var "fname"))
      @@ (Core.typeFunction $ Core.functionType
          (ref nominalApplicationDef @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
          (var "ftyp"))
      @@ ref Substitution.idTypeSubstDef

inferTypeOfRecordDef :: TElement (InferenceContext -> Record -> Flow s InferenceResult)
inferTypeOfRecordDef = inferenceDefinition "inferTypeOfRecord" $
  doc "Infer the type of a record" $
  lambdas ["cx", "record"] $ lets [
    "tname">: Core.recordTypeName $ var "record",
    "fields">: Core.recordFields $ var "record",
    "fnames">: Lists.map (unaryFunction Core.fieldName) (var "fields")] $
    withVar "schemaType" (ref requireSchemaTypeDef @@ var "cx" @@ var "tname") $
    withVar "results" (ref inferManyDef @@ var "cx" @@
      Lists.map (lambda "f" $ pair (Core.fieldTerm $ var "f")
        (Strings.cat2 (string "field ") (Core.unName $ Core.fieldName $ var "f"))) (var "fields")) $ lets [
      "svars">: Core.typeSchemeVariables $ var "schemaType",
      "styp">: Core.typeSchemeType $ var "schemaType",
      "iterms">: first $ var "results",
      "itypes">: first $ second $ var "results",
      "isubst">: second $ second $ var "results",
      "ityp">: Core.typeRecord $ Core.rowType (var "tname") $
        Lists.zipWith (lambdas ["n", "t"] $ Core.fieldType (var "n") (var "t")) (var "fnames") (var "itypes")] $
    ref mapConstraintsDef @@ var "cx" @@
      (lambda "subst" $ ref yieldDef
        @@ (Core.termRecord $ Core.record (var "tname") $
            Lists.zipWith (lambdas ["n", "t"] $ Core.field (var "n") (var "t")) (var "fnames") (var "iterms"))
        @@ (ref nominalApplicationDef @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
        @@ (ref Substitution.composeTypeSubstDef @@ var "isubst" @@ var "subst")) @@
      list [Typing.typeConstraint (var "styp") (var "ityp") (string "schema type of record")]

inferTypeOfSetDef :: TElement (InferenceContext -> S.Set Term -> Flow s InferenceResult)
inferTypeOfSetDef = inferenceDefinition "inferTypeOfSet" $
  doc "Infer the type of a set" $
  lambdas ["cx", "s"] $
    ref inferTypeOfCollectionDef
      @@ var "cx"
      @@ (unaryFunction Core.typeSet)
      @@ (lambda "terms" $ Core.termSet $ Sets.fromList $ var "terms")
      @@ string "set element"
      @@ (Sets.toList $ var "s")

inferTypeOfSumDef :: TElement (InferenceContext -> Sum -> Flow s InferenceResult)
inferTypeOfSumDef = inferenceDefinition "inferTypeOfSum" $
  doc "Infer the type of a sum type" $
  lambdas ["cx", "sum"] $ lets [
    "i">: Core.sumIndex $ var "sum",
    "s">: Core.sumSize $ var "sum",
    "term">: Core.sumTerm $ var "sum"] $
    withVar "result" (ref inferTypeOfTermDef @@ var "cx" @@ var "term" @@ string "sum term") $ lets [
    "iterm">: Typing.inferenceResultTerm $ var "result",
    "ityp">: Typing.inferenceResultType $ var "result",
    "isubst">: Typing.inferenceResultSubst $ var "result",
    "varOrTerm">: lambda "t" $ lambda "j" $
      Logic.ifElse (Equality.equalInt32 (var "i") (var "j"))
        (Flows.pure $ Mantle.eitherLeft $ var "t")
        (Flows.map (unaryFunction Mantle.eitherRight) $ ref freshNameDef)] $
    withVar "vars" (Flows.sequence $ Lists.map (var "varOrTerm" @@ var "ityp") $
      Math.rangeInt32 (int32 0) (Math.sub (var "s") (int32 1))) $ lets [
        "toType">: lambda "e" $
          cases _Either (var "e") Nothing [
            _Either_left>>: lambda "t" $ var "t",
            _Either_right>>: lambda "v" $ Core.typeVariable $ var "v"]] $
      Flows.pure $ ref yieldDef
        @@ (Core.termSum $ Core.sum (var "i") (var "s") (var "iterm"))
        @@ (Core.typeSum $ Lists.map (var "toType") (var "vars"))
        @@ var "isubst"

inferTypeOfTermDef :: TElement (InferenceContext -> Term -> String -> Flow s InferenceResult)
inferTypeOfTermDef = inferenceDefinition "inferTypeOfTerm" $
  doc "Infer the type of a term with description" $
  lambdas ["cx", "term", "desc"] $
    ref Monads.withTraceDef @@ var "desc" @@ (
      cases _Term (var "term") Nothing [
        _Term_annotated>>: lambda "a" $ ref inferTypeOfAnnotatedTermDef @@ var "cx" @@ var "a",
        _Term_application>>: lambda "a" $ ref inferTypeOfApplicationDef @@ var "cx" @@ var "a",
        _Term_function>>: lambda "f" $ ref inferTypeOfFunctionDef @@ var "cx" @@ var "f",
        _Term_let>>: lambda "l" $ ref inferTypeOfLetDef @@ var "cx" @@ var "l",
        _Term_list>>: lambda "els" $ ref inferTypeOfListDef @@ var "cx" @@ var "els",
        _Term_literal>>: lambda "l" $ ref inferTypeOfLiteralDef @@ var "cx" @@ var "l",
        _Term_map>>: lambda "m" $ ref inferTypeOfMapDef @@ var "cx" @@ var "m",
        _Term_optional>>: lambda "m" $ ref inferTypeOfOptionalDef @@ var "cx" @@ var "m",
        _Term_product>>: lambda "els" $ ref inferTypeOfProductDef @@ var "cx" @@ var "els",
        _Term_record>>: lambda "r" $ ref inferTypeOfRecordDef @@ var "cx" @@ var "r",
        _Term_set>>: lambda "s" $ ref inferTypeOfSetDef @@ var "cx" @@ var "s",
        _Term_sum>>: lambda "s" $ ref inferTypeOfSumDef @@ var "cx" @@ var "s",
        _Term_typeAbstraction>>: lambda "ta" $ ref inferTypeOfTypeAbstractionDef @@ var "cx" @@ var "ta",
        _Term_typeApplication>>: lambda "tt" $ ref inferTypeOfTypeApplicationDef @@ var "cx" @@ var "tt",
        _Term_union>>: lambda "i" $ ref inferTypeOfInjectionDef @@ var "cx" @@ var "i",
        _Term_variable>>: lambda "name" $ ref inferTypeOfVariableDef @@ var "cx" @@ var "name",
        _Term_wrap>>: lambda "w" $ ref inferTypeOfWrappedTermDef @@ var "cx" @@ var "w"])

inferTypeOfTupleProjectionDef :: TElement (InferenceContext -> TupleProjection -> Flow s InferenceResult)
inferTypeOfTupleProjectionDef = inferenceDefinition "inferTypeOfTupleProjection" $
  doc "Infer the type of a tuple projection" $
  lambdas ["_", "tp"] $ lets [
    "arity">: Core.tupleProjectionArity $ var "tp",
    "idx">: Core.tupleProjectionIndex $ var "tp"] $
    ref forVarsDef @@ var "arity" @@ (lambda "vars" $ lets [
      "types">: Lists.map (unaryFunction Core.typeVariable) (var "vars"),
      "cod">: Lists.at (var "idx") (var "types")] $
      ref yieldDef
        @@ (Core.termFunction $ Core.functionElimination $ Core.eliminationProduct $
            Core.tupleProjection (var "arity") (var "idx") (just $ var "types"))
        @@ (Core.typeFunction $ Core.functionType (Core.typeProduct $ var "types") (var "cod"))
        @@ ref Substitution.idTypeSubstDef)

inferTypeOfTypeAbstractionDef :: TElement (InferenceContext -> TypeAbstraction -> Flow s InferenceResult)
inferTypeOfTypeAbstractionDef = inferenceDefinition "inferTypeOfTypeAbstraction" $
  doc "Infer the type of a type abstraction" $
  lambdas ["cx", "ta"] $
    ref inferTypeOfTermDef @@ var "cx" @@ (Core.typeAbstractionBody $ var "ta") @@ string "type abstraction"

inferTypeOfTypeApplicationDef :: TElement (InferenceContext -> TypedTerm -> Flow s InferenceResult)
inferTypeOfTypeApplicationDef = inferenceDefinition "inferTypeOfTypeApplication" $
  doc "Infer the type of a type application" $
  lambdas ["cx", "tt"] $
    ref inferTypeOfTermDef @@ var "cx" @@ (Core.typedTermTerm $ var "tt") @@ string "type application term"

-- For now, type annotations are simply ignored during inference.
inferTypeOfTypedTermDef :: TElement (InferenceContext -> TypedTerm -> Flow s InferenceResult)
inferTypeOfTypedTermDef = inferenceDefinition "inferTypeOfTypedTerm" $
  doc "Infer the type of a typed term" $
  lambdas ["cx", "tt"] $
    ref inferTypeOfTermDef @@ var "cx" @@ (Core.typedTermTerm $ var "tt") @@ string "typed term"

inferTypeOfUnwrapDef :: TElement (InferenceContext -> Name -> Flow s InferenceResult)
inferTypeOfUnwrapDef = inferenceDefinition "inferTypeOfUnwrap" $
  doc "Infer the type of an unwrap operation" $
  lambdas ["cx", "tname"] $
    withVar "schemaType" (ref requireSchemaTypeDef @@ var "cx" @@ var "tname") $ lets [
    "svars">: Core.typeSchemeVariables $ var "schemaType",
    "styp">: Core.typeSchemeType $ var "schemaType"] $
    withVar "wtyp" (ref ExtractCore.wrappedTypeDef @@ var "tname" @@ var "styp") $
    Flows.pure $ ref yieldDef
      @@ (Core.termFunction $ Core.functionElimination $ Core.eliminationWrap $ var "tname")
      @@ (Core.typeFunction $ Core.functionType
          (ref nominalApplicationDef @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "svars"))
          (var "wtyp"))
      @@ ref Substitution.idTypeSubstDef

inferTypeOfVariableDef :: TElement (InferenceContext -> Name -> Flow s InferenceResult)
inferTypeOfVariableDef = inferenceDefinition "inferTypeOfVariable" $
  doc "Infer the type of a variable" $
  lambdas ["cx", "name"] $
    Optionals.maybe
      (Flows.fail $ Strings.cat2 (string "Variable not bound to type: ") (Core.unName $ var "name"))
      (lambda "scheme" $
        withVar "ts" (ref instantiateTypeSchemeDef @@ var "scheme") $ lets [
          "vars">: Core.typeSchemeVariables $ var "ts",
          "itype">: Core.typeSchemeType $ var "ts",
          "iterm">: Lists.foldl (lambdas ["t", "ty"] $ Core.termTypeApplication $ Core.typedTerm (var "t") (var "ty"))
            (Core.termVariable $ var "name") (Lists.map (unaryFunction Core.typeVariable) $ var "vars")] $
          Flows.pure $ Typing.inferenceResult (var "iterm") (var "itype") (ref Substitution.idTypeSubstDef))
      (Maps.lookup (var "name") (Typing.inferenceContextDataTypes $ var "cx"))

inferTypeOfWrappedTermDef :: TElement (InferenceContext -> WrappedTerm -> Flow s InferenceResult)
inferTypeOfWrappedTermDef = inferenceDefinition "inferTypeOfWrappedTerm" $
  doc "Infer the type of a wrapped term" $
  lambdas ["cx", "wt"] $ lets [
    "tname">: Core.wrappedTermTypeName $ var "wt",
    "term">: Core.wrappedTermObject $ var "wt"] $
    withVar "schemaType" (ref requireSchemaTypeDef @@ var "cx" @@ var "tname") $
    withVar "result" (ref inferTypeOfTermDef @@ var "cx" @@ var "term" @@ string "wrapped term") $ lets [
      "svars">: Core.typeSchemeVariables $ var "schemaType",
      "styp">: Core.typeSchemeType $ var "schemaType",
      "iterm">: Typing.inferenceResultTerm $ var "result",
      "ityp">: Typing.inferenceResultType $ var "result",
      "isubst">: Typing.inferenceResultSubst $ var "result"] $
    withVar "freshVars" (ref freshNamesDef @@ Lists.length (var "svars")) $ lets [
      "subst">: Typing.typeSubst $ Maps.fromList $ Lists.zip (var "svars") (Lists.map (unaryFunction Core.typeVariable) $ var "freshVars"),
      "stypInst">: ref Substitution.substInTypeDef @@ var "subst" @@ var "styp",
      "nominalInst">: ref nominalApplicationDef @@ var "tname" @@ Lists.map (unaryFunction Core.typeVariable) (var "freshVars"),
      "expected">: Core.typeWrap $ Core.wrappedType (var "tname") (var "ityp"),
      "freeVars">: Sets.toList $ Sets.unions $ list [
        ref Rewriting.freeVariablesInTypeDef @@ var "ityp",
        ref Rewriting.freeVariablesInTermDef @@ var "iterm",
        Sets.fromList $ var "freshVars"]] $
    ref bindConstraintsDef @@ var "cx" @@
      (lambda "subst2" $ ref yieldCheckedDef @@ var "cx" @@ var "freeVars" @@
        (Core.termWrap $ Core.wrappedTerm (var "tname") (var "iterm")) @@
        var "nominalInst" @@
        (ref Substitution.composeTypeSubstDef @@ var "isubst" @@ var "subst2")) @@
      list [Typing.typeConstraint (var "stypInst") (var "expected") (string "schema type of wrapper")]

inferTypesOfTemporaryLetBindingsDef :: TElement (InferenceContext -> [LetBinding] -> Flow s ([Term], ([Type], TypeSubst)))
inferTypesOfTemporaryLetBindingsDef = inferenceDefinition "inferTypesOfTemporaryLetBindings" $
  doc "Infer types for temporary let bindings" $
  lambdas ["cx", "bins"] $
    Logic.ifElse (Lists.null $ var "bins")
      (Flows.pure $ pair (list []) (pair (list []) (ref Substitution.idTypeSubstDef)))
      (lets [
        "binding">: Lists.head $ var "bins",
        "k">: Core.letBindingName $ var "binding",
        "v">: Core.letBindingTerm $ var "binding",
        "tl">: Lists.tail $ var "bins"] $
        withVar "result1" (ref inferTypeOfTermDef @@ var "cx" @@ var "v" @@
          (Strings.cat $ list [
            string "temporary let binding '",
            Core.unName $ var "k",
            string "'"])) $ lets [
        "j">: Typing.inferenceResultTerm $ var "result1",
        "u_prime">: Typing.inferenceResultType $ var "result1",
        "u">: Typing.inferenceResultSubst $ var "result1"] $
        withVar "result2" (ref inferTypesOfTemporaryLetBindingsDef @@
          (ref Substitution.substInContextDef @@ var "u" @@ var "cx") @@
          var "tl") $ lets [
        "h">: first $ var "result2",
        "r_prime">: first $ second $ var "result2",
        "r">: second $ second $ var "result2"] $
        Flows.pure $ pair
          (Lists.cons (ref Substitution.substTypesInTermDef @@ var "r" @@ var "j") (var "h"))
          (pair
            (Lists.cons (ref Substitution.substInTypeDef @@ var "r" @@ var "u_prime") (var "r_prime"))
            (ref Substitution.composeTypeSubstDef @@ var "u" @@ var "r")))

forVarsDef :: TElement (Int -> ([Name] -> a) -> Flow s a)
forVarsDef = inferenceDefinition "forVars" $
  doc "Generate fresh variables and map over them" $
  lambdas ["n", "f"] $
    Flows.map (var "f") $ ref freshNamesDef @@ var "n"

forVarDef :: TElement ((Name -> a) -> Flow s a)
forVarDef = inferenceDefinition "forVar" $
  doc "Generate a fresh variable and map over it" $
  lambda "f" $
    Flows.map (var "f") $ ref freshNameDef

forInferredTermDef :: TElement (InferenceContext -> Term -> String -> (InferenceResult -> a) -> Flow s a)
forInferredTermDef = inferenceDefinition "forInferredTerm" $
  doc "Infer a term's type and map over the result" $
  lambdas ["cx", "term", "desc", "f"] $
    Flows.map (var "f") $ ref inferTypeOfTermDef @@ var "cx" @@ var "term" @@ var "desc"

bindConstraintsDef :: TElement (InferenceContext -> (TypeSubst -> Flow s a) -> [TypeConstraint] -> Flow s a)
bindConstraintsDef = inferenceDefinition "bindConstraints" $
  doc "Bind type constraints and continue with substitution" $
  lambdas ["cx", "f", "constraints"] $
    Flows.bind (ref Unification.unifyTypeConstraintsDef @@ Typing.inferenceContextSchemaTypes (var "cx") @@ var "constraints") (var "f")

fTypeToTypeSchemeDef :: TElement (Type -> TypeScheme)
fTypeToTypeSchemeDef = inferenceDefinition "fTypeToTypeScheme" $
  doc "Convert a forall type to a type scheme" $
  lambda "typ" $ ref gatherForallDef @@ list [] @@ var "typ"

gatherForallDef :: TElement ([Name] -> Type -> TypeScheme)
gatherForallDef = inferenceDefinition "gatherForall" $
  doc "Helper to gather forall variables" $
  lambdas ["vars", "typ"] $
    cases _Type (ref Strip.stripTypeDef @@ var "typ") Nothing [
      _Type_forall>>: lambda "ft" $ ref gatherForallDef @@
        (Lists.cons (Core.forallTypeParameter $ var "ft") (var "vars")) @@
        (Core.forallTypeBody $ var "ft"),
      _Type_variable>>: constant $ Core.typeScheme (Lists.reverse $ var "vars") (var "typ")]

instantiateFTypeDef :: TElement (Type -> Flow s Type)
instantiateFTypeDef = inferenceDefinition "instantiateFType" $
  doc "Instantiate a forall type with fresh variables" $
  lambda "typ" $
    withVar "ts" (ref instantiateTypeSchemeDef @@ (ref fTypeToTypeSchemeDef @@ var "typ")) $
    Flows.pure $ Core.typeSchemeType $ var "ts"

instantiateTypeSchemeDef :: TElement (TypeScheme -> Flow s TypeScheme)
instantiateTypeSchemeDef = inferenceDefinition "instantiateTypeScheme" $
  doc "Instantiate a type scheme with fresh variables" $
  lambda "scheme" $ lets [
    "oldVars">: Core.typeSchemeVariables $ var "scheme"] $
    withVar "newVars" (ref freshNamesDef @@ Lists.length (var "oldVars")) $ lets [
      "subst">: Typing.typeSubst $ Maps.fromList $ Lists.zip (var "oldVars") (Lists.map (unaryFunction Core.typeVariable) $ var "newVars")] $
      Flows.pure $ Core.typeScheme (var "newVars") $
        ref Substitution.substInTypeDef @@ var "subst" @@ Core.typeSchemeType (var "scheme")

mapConstraintsDef :: TElement (InferenceContext -> (TypeSubst -> a) -> [TypeConstraint] -> Flow s a)
mapConstraintsDef = inferenceDefinition "mapConstraints" $
  doc "Map over type constraints after unification" $
  lambdas ["cx", "f", "constraints"] $
    Flows.map (var "f") $
      ref Unification.unifyTypeConstraintsDef @@ (Typing.inferenceContextSchemaTypes $ var "cx") @@ var "constraints"

nominalApplicationDef :: TElement (Name -> [Type] -> Type)
nominalApplicationDef = inferenceDefinition "nominalApplication" $
  doc "Apply type arguments to a nominal type" $
  lambdas ["tname", "args"] $
    Lists.foldl
      (lambdas ["t", "a"] $ Core.typeApplication $ Core.applicationType (var "t") (var "a"))
      (Core.typeVariable $ var "tname")
      (var "args")

requireSchemaTypeDef :: TElement (InferenceContext -> Name -> Flow s TypeScheme)
requireSchemaTypeDef = inferenceDefinition "requireSchemaType" $
  doc "Require a schema type from the context" $
  lambdas ["cx", "tname"] $
    Optionals.maybe
      (Flows.fail $ Strings.cat2 (string "No such schema type: ") (Core.unName $ var "tname"))
      (lambda "ts" $ ref instantiateTypeSchemeDef @@ (ref Rewriting.stripTypeSchemeRecursiveDef @@ var "ts"))
      (Maps.lookup (var "tname") (Typing.inferenceContextSchemaTypes $ var "cx"))

extendContextDef :: TElement ([(Name, TypeScheme)] -> InferenceContext -> InferenceContext)
extendContextDef = inferenceDefinition "extendContext" $
  doc "Add (term variable, type scheme) pairs to the typing environment" $
  lambdas ["pairs", "cx"] $
    Typing.inferenceContextWithDataTypes (var "cx") $
      Maps.union
        (Maps.fromList $ var "pairs")
        (Typing.inferenceContextDataTypes $ var "cx")

yieldDef :: TElement (Term -> Type -> TypeSubst -> InferenceResult)
yieldDef = inferenceDefinition "yield" $
  doc "Create an inference result" $
  lambdas ["term", "typ", "subst"] $
    Typing.inferenceResult
      (ref Substitution.substTypesInTermDef @@ var "subst" @@ var "term")
      (ref Substitution.substInTypeDef @@ var "subst" @@ var "typ")
      (var "subst")

yieldCheckedDef :: TElement (InferenceContext -> [Name] -> Term -> Type -> TypeSubst -> Flow s InferenceResult)
yieldCheckedDef = inferenceDefinition "yieldChecked" $
  doc "Create a checked inference result" $
  lambdas ["cx", "vars", "term", "typ", "subst"] $ lets [
    "iterm">: ref Substitution.substTypesInTermDef @@ var "subst" @@ var "term",
    "itype">: ref Substitution.substInTypeDef @@ var "subst" @@ var "typ"] $
    Flows.pure $ Typing.inferenceResult (var "iterm") (var "itype") (var "subst")

yieldDebugDef :: TElement (InferenceContext -> String -> Term -> Type -> TypeSubst -> Flow s InferenceResult)
yieldDebugDef = inferenceDefinition "yieldDebug" $
  doc "Create an inference result with debug output" $
  lambdas ["cx", "debugId", "term", "typ", "subst"] $ lets [
    "rterm">: ref Substitution.substTypesInTermDef @@ var "subst" @@ var "term",
    "rtyp">: ref Substitution.substInTypeDef @@ var "subst" @@ var "typ"] $
    withVar "result" (ref Annotations.debugIfDef @@ var "debugId" @@
      (Strings.cat $ list [
        string "\n\tterm: ",  ref ShowCore.termDef @@ var "term",
        string "\n\ttyp: ",   ref ShowCore.typeDef @@ var "typ",
        string "\n\tsubst: ", ref ShowCore.typeSubstDef @@ var "subst",
        string "\n\trterm: ", ref ShowCore.termDef @@ var "rterm",
        string "\n\trtyp: ",  ref ShowCore.typeDef @@ var "rtyp"])) $
    Flows.pure $ Typing.inferenceResult (var "rterm") (var "rtyp") (var "subst")
