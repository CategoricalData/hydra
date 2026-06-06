-- | Java code generator in Hydra DSL.
-- This module provides DSL versions of Java code generation functions.

module Hydra.Sources.Java.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Optionals                 as Optionals
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Errors                      as Error
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Dsl.Typing                      as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Strip          as Strip
import qualified Hydra.Sources.Kernel.Terms.Variables      as Variables
import qualified Hydra.Sources.Kernel.Terms.Dependencies   as Dependencies
import qualified Hydra.Sources.Kernel.Terms.Scoping        as Scoping
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Analysis       as Analysis
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Predicates    as Predicates
import qualified Hydra.Sources.Kernel.Terms.Resolution    as Resolution
import qualified Hydra.Sources.Kernel.Terms.Environment   as Environment
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Kernel.Terms.Arity           as Arity
import qualified Hydra.Dsl.Meta.Graph                       as Graph
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import           Prelude hiding ((++))
import qualified Data.List                  as L

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Coerce (coerce)

-- Additional imports
import qualified Hydra.Java.Syntax as Java
import qualified Hydra.Java.Environment as JavaHelpers
import qualified Hydra.Dsl.Java.Helpers as JavaDsl
import qualified Hydra.Sources.Java.Syntax as JavaSyntax
import qualified Hydra.Sources.Java.Environment as JavaEnvironmentSource
import qualified Hydra.Sources.Java.Language as JavaLanguageSource
import qualified Hydra.Sources.Java.Names as JavaNamesSource
import qualified Hydra.Sources.Java.Serde as JavaSerdeSource
import qualified Hydra.Sources.Java.Utils as JavaUtilsSource
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Serialization  as SerializationSource

ns :: ModuleName
ns = ModuleName "hydra.java.coder"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([JavaUtilsSource.ns, JavaNamesSource.ns, JavaSerdeSource.ns, moduleName JavaLanguageSource.module_, Analysis.ns, Checking.ns, Formatting.ns, Names.ns, Rewriting.ns, Dependencies.ns, Scoping.ns, Strip.ns, Variables.ns, Lexical.ns, Environment.ns, Predicates.ns, Resolution.ns, ShowCore.ns, Annotations.ns, Constants.ns,
      Inference.ns, Sorting.ns, Arity.ns, moduleName DecodeCore.module_, moduleName EncodeCore.module_, SerializationSource.ns] L.++ (JavaEnvironmentSource.ns:JavaSyntax.ns:KernelTypes.kernelTypesModuleNames)),
            moduleMetadata = descriptionMetadata (Just "Java code generator: converts Hydra modules to Java source code")}
  where
    definitions = [
      toDefinition addComment,
      toDefinition analyzeJavaFunction,
      toDefinition annotateBodyWithCod,
      toDefinition annotateLambdaArgs,
      toDefinition applyCastIfSafe,
      toDefinition applyJavaArg,
      toDefinition applyOvergenSubstToTermAnnotations,
      toDefinition applyOvergenSubstToTermAnnotations_go,
      toDefinition applySubstFull,
      toDefinition applySubstSimple,
      toDefinition arraysCompareExpr,
      toDefinition arraysEqualsClause,
      toDefinition augmentVariantClass,
      toDefinition bindingIsFunctionType,
      toDefinition bindingNameToFilePath,
      toDefinition bindingsToStatements,
      toDefinition boundTypeVariables,
      toDefinition buildArgSubst,
      toDefinition buildCurriedLambda,
      toDefinition buildSubstFromAnnotations,
      toDefinition buildSubstFromAnnotations_go,
      toDefinition buildTypeSubst,
      toDefinition buildTypeSubst_go,
      toDefinition buildTypeVarSubst,
      toDefinition buildTypeVarSubst_go,
      toDefinition classModsPublic,
      toDefinition classifyDataReference,
      toDefinition classifyDataTerm,
      toDefinition classifyDataTerm_countLambdaParams,
      toDefinition classifyDataTerm_stripTypeLambdas,
      toDefinition cmpDeclStatement,
      toDefinition cmpNotZeroExpr,
      toDefinition collectForallParams,
      toDefinition collectLambdaDomains,
      toDefinition collectTypeApps,
      toDefinition collectTypeApps0,
      toDefinition collectTypeVars,
      toDefinition collectTypeVars_go,
      toDefinition comparableCompareExpr,
      toDefinition compareAndReturnStmts,
      toDefinition compareFieldExpr,
      toDefinition compareToBody,
      toDefinition compareToZeroClause,
      toDefinition constantDecl,
      toDefinition constantDeclForFieldType,
      toDefinition constantDeclForTypeName,
      toDefinition constructElementsInterface,
      toDefinition correctCastType,
      toDefinition correctTypeApps,
      toDefinition correctTypeAppsWithArgs,
      toDefinition countFunctionParams,
      toDefinition declarationForRecordType,
      toDefinition declarationForRecordType',
      toDefinition declarationForUnionType,
      toDefinition decodeTypeFromTerm,
      toDefinition dedupBindings,
      toDefinition detectAccumulatorUnification,
      toDefinition directRefSubstitution,
      toDefinition directRefSubstitution_processGroup,
      toDefinition domTypeArgs,
      toDefinition elementJavaIdentifier,
      toDefinition elementJavaIdentifier_qualify,
      toDefinition elementsClassName,
      toDefinition elementsQualifiedName,
      toDefinition encodeApplication,
      toDefinition encodeApplication_fallback,
      toDefinition encodeDefinitions,
      toDefinition encodeElimination,
      toDefinition encodeFunction,
      toDefinition encodeFunctionFormTerm,
      toDefinition encodeFunctionPrimitiveByName,
      toDefinition encodeLiteral,
      toDefinition encodeLiteralType,
      toDefinition encodeLiteralType_simple,
      toDefinition encodeLiteral_encodeFloat,
      toDefinition encodeLiteral_encodeFloat32,
      toDefinition encodeLiteral_encodeFloat64,
      toDefinition encodeLiteral_encodeInteger,
      toDefinition encodeLiteral_javaParseDouble,
      toDefinition encodeLiteral_javaSpecialFloatExpr,
      toDefinition encodeLiteral_litExp,
      toDefinition encodeLiteral_primCast,
      toDefinition encodeNullaryConstant,
      toDefinition encodeNullaryConstant_typeArgsFromReturnType,
      toDefinition encodeNullaryPrimitiveByName,
      toDefinition encodeTerm,
      toDefinition encodeTermDefinition,
      toDefinition encodeTermInternal,
      toDefinition encodeTermTCO,
      toDefinition encodeType,
      toDefinition encodeTypeDefinition,
      toDefinition encodeType_resolveIfTypedef,
      toDefinition encodeVariable,
      toDefinition encodeVariable_buildCurried,
      toDefinition encodeVariable_hoistedLambdaCase,
      toDefinition eqClause,
      toDefinition equalsClause,
      toDefinition extractArgType,
      toDefinition extractDirectReturn,
      toDefinition extractDirectReturn_go,
      toDefinition extractInOutPair,
      toDefinition extractTypeApplicationArgs,
      toDefinition extractTypeApplicationArgs_go,
      toDefinition fieldTypeToFormalParam,
      toDefinition filterByFlags,
      toDefinition filterPhantomTypeArgs,
      toDefinition filterPhantomTypeArgs_filterAndApply,
      toDefinition findMatchingLambdaVar,
      toDefinition findPairFirst,
      toDefinition findSelfRefVar,
      toDefinition first20Primes,
      toDefinition flattenApps,
      toDefinition flattenBindings,
      toDefinition freshJavaName,
      toDefinition freshJavaName_go,
      toDefinition functionCall,
      toDefinition getCodomain,
      toDefinition getFunctionType,
      toDefinition groupPairsByFirst,
      toDefinition hashCodeCompareExpr,
      toDefinition hashCodeMultPair,
      toDefinition innerClassRef,
      toDefinition insertBranchVar,
      toDefinition interfaceTypes,
      toDefinition isBigNumericType,
      toDefinition isBinaryType,
      toDefinition isFieldUnitType,
      toDefinition isLambdaBoundIn,
      toDefinition isLambdaBoundIn_isQualified,
      toDefinition isLambdaBoundVariable,
      toDefinition isLocalVariable,
      toDefinition isNonComparableType,
      toDefinition isRecursiveVariable,
      toDefinition isSerializableJavaType,
      toDefinition isSimpleName,
      toDefinition isUnresolvedInferenceVar,
      toDefinition isUnresolvedInferenceVar_isDigit,
      toDefinition java11Features,
      toDefinition java8Features,
      toDefinition javaComparableRefType,
      toDefinition javaEnvGetGraph,
      toDefinition javaEnvSetGraph,
      toDefinition javaFeatures,
      toDefinition javaIdentifierToString,
      toDefinition javaTypeArgumentsForNamedType,
      toDefinition javaTypeArgumentsForType,
      toDefinition javaTypeParametersForType,
      toDefinition javaTypeParametersForType_bvars,
      toDefinition lazyFlagsForPrimitive,
      toDefinition moduleToJava,
      toDefinition nameMapToTypeMap,
      toDefinition namespaceParent,
      toDefinition noComment,
      toDefinition noInterfaceComment,
      toDefinition otherwiseBranch,
      toDefinition peelDomainTypes,
      toDefinition peelDomainsAndCod,
      toDefinition peelExpectedTypes,
      toDefinition propagateType,
      toDefinition propagateType_propagateIntoLambda,
      toDefinition propagateType_rebuildLet,
      toDefinition propagateTypesInAppChain,
      toDefinition rebuildApps,
      toDefinition recordCompareToMethod,
      toDefinition recordConstructor,
      toDefinition recordEqualsMethod,
      toDefinition recordHashCodeMethod,
      toDefinition recordMemberVar,
      toDefinition recordWithMethod,
      toDefinition resolveTypeApps,
      toDefinition selfRefSubstitution,
      toDefinition selfRefSubstitution_processGroup,
      toDefinition serializableTypes,
      toDefinition splitConstantInitializer,
      toDefinition splitConstantInitializer_splitVar,
      toDefinition stripForalls,
      toDefinition substituteTypeVarsWithTypes,
      toDefinition substituteTypeVarsWithTypes_go,
      toDefinition tagCmpNotZeroExpr,
      toDefinition tagCompareExpr,
      toDefinition takeTypeArgs,
      toDefinition toClassDecl,
      toDefinition toDeclInit,
      toDefinition toDeclStatement,
      toDefinition tryInferFunctionType,
      toDefinition typeAppFallbackCast,
      toDefinition typeAppNullaryOrHoisted,
      toDefinition typeArgsOrDiamond,
      toDefinition typesMatch,
      toDefinition unwrapReturnType,
      toDefinition variantCompareToMethod,
      toDefinition visitBranch,
      toDefinition withCommentString,
      toDefinition withInterfaceCommentString,
      toDefinition withLambda,
      toDefinition withTypeLambda,
      toDefinition wrapInSupplierLambda,
      toDefinition wrapLazyArguments]

-- | Add a comment from a FieldType to a class body declaration
addComment :: TypedTermDefinition (Java.ClassBodyDeclaration -> FieldType -> InferenceContext -> Graph -> Either Error Java.ClassBodyDeclarationWithComments)
addComment = def "addComment" $
  lambda "decl" $ lambda "field" $
        "cx" ~> "g" ~>
        Eithers.map
          (lambda "c" $ JavaDsl.classBodyDeclarationWithComments (var "decl") (var "c"))
          (Annotations.commentsFromFieldType @@ var "cx" @@ var "g" @@ var "field")

-- | Analyze a Java function term, collecting lambdas, type lambdas, lets, and type applications
analyzeJavaFunction :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> Term -> InferenceContext -> Graph -> Either Error (FunctionStructure JavaHelpers.JavaEnvironment))
analyzeJavaFunction = def "analyzeJavaFunction" $
  lambda "env" $ lambda "term" $
    "cx" ~> "g" ~>
    Analysis.analyzeFunctionTerm @@ var "cx" @@ javaEnvGetGraph @@ javaEnvSetGraph @@ var "env" @@ var "term"

-- | Annotate a term body with the expected codomain type, propagating through
-- applications so that inner type-applied subterms also get correct annotations.
annotateBodyWithCod :: TypedTermDefinition (Type -> Term -> Term)
annotateBodyWithCod = def "annotateBodyWithCod" $
  lambda "typ" $ lambda "term" $
    "setAnn" <~ (lambda "t" $
      Annotations.setTermAnnotation @@ asTerm Constants.keyType
        @@ just (encodeTypeAsTerm @@ var "typ")
        @@ var "t") $
    cases _Term (Strip.deannotateTerm @@ var "term")
      (Just $ var "setAnn" @@ var "term") [
      -- For type applications, annotate the whole thing with the expected type
      _Term_typeApplication>>: lambda "_ta" $
        var "setAnn" @@ var "term",
      -- For applications, annotate the application with the overall type,
      -- and also annotate arguments that have type applications
      _Term_application>>: lambda "app" $
        "lhs" <~ Core.applicationFunction (var "app") $
        "rhs" <~ Core.applicationArgument (var "app") $
        "annotatedRhs" <~ cases _Term (Strip.deannotateTerm @@ var "rhs")
          (Just $ var "rhs") [
          _Term_typeApplication>>: lambda "_ta2" $
            annotateBodyWithCod @@ (extractArgType @@ var "lhs" @@ var "typ") @@ var "rhs"] $
        var "setAnn" @@ (inject _Term _Term_application (record _Application [
          _Application_function>>: var "lhs",
          _Application_argument>>: var "annotatedRhs"]))]

-- | Annotate lambda arguments with expected types computed from the callee's type scheme
-- and type applications. This corrects type annotations that normalizeTypeVariablesInTerm
-- may have made inconsistent with the outer scope.
annotateLambdaArgs :: TypedTermDefinition (Name -> [Type] -> [Term] -> InferenceContext -> Graph -> Either Error [Term])
annotateLambdaArgs = def "annotateLambdaArgs" $
  lambda "cname" $ lambda "tApps" $ lambda "argTerms" $
    "cx" ~> "g" ~>
    Logic.ifElse (Lists.null (var "tApps"))
      (right (var "argTerms"))
      -- Look up the type scheme from either elements or primitives
      ("mts" <<~ (
        "mel" <<~ right (Lexical.lookupBinding @@ var "g" @@ var "cname") $
        Optionals.cases (var "mel")
          (right (Optionals.map
              (lambda "prim" $ Scoping.termSignatureToTypeScheme @@ (Packaging.primitiveDefinitionSignature $ Graph.primitiveDefinition (var "prim")))
              (Maps.lookup (var "cname") (Graph.graphPrimitives (var "g")))))
          (lambda "el" $ right (Core.bindingTypeScheme (var "el")))) $
      Optionals.cases (var "mts")
        (right (var "argTerms"))
        (lambda "ts" $
          "schemeType" <~ Core.typeSchemeBody (var "ts") $
          "schemeTypeVars" <~ (collectTypeVars @@ var "schemeType") $
          "schemeVars" <~ Lists.filter
            (lambda "v" $ Sets.member (var "v") (var "schemeTypeVars"))
            (Core.typeSchemeVariables (var "ts")) $
          Logic.ifElse (Logic.or (Lists.null (var "schemeVars"))
              (Logic.not (Equality.equal (Lists.length (var "schemeVars")) (Lists.length (var "tApps")))))
            (right (var "argTerms"))
            ("subst" <~ Maps.fromList (Lists.zip (var "schemeVars") (var "tApps")) $
              "expectedTypes" <~ (peelExpectedTypes @@ var "subst" @@ Lists.length (var "argTerms") @@ var "schemeType") $
              right (Lists.zipWith
                (lambda "arg" $ lambda "mExpected" $ propagateType @@ var "mExpected" @@ var "arg")
                (var "argTerms")
                (Lists.concat2 (var "expectedTypes")
                  (Lists.replicate (Lists.length (var "argTerms")) (inject _Type _Type_variable (wrap _Name (string "unused")))))))))

-- | Apply a lambda cast if the type is safe (doesn't contain potentially wrong type variables).
applyCastIfSafe :: TypedTermDefinition (JavaHelpers.Aliases -> Type -> Java.Expression -> InferenceContext -> Graph -> Either Error Java.Expression)
applyCastIfSafe = def "applyCastIfSafe" $
  lambda "aliases" $ lambda "castType" $ lambda "expr" $
    "cx" ~> "g" ~>
    "trusted" <~ (project JavaHelpers._Aliases JavaHelpers._Aliases_trustedTypeVars @@ var "aliases") $
    "inScope" <~ (project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeTypeParams @@ var "aliases") $
    "castVars" <~ (collectTypeVars @@ var "castType") $
    "javaTypeVars" <~ Sets.fromList (Lists.filter
      (lambda "v" $ Logic.or
        (Sets.member (var "v") (var "inScope"))
        (isLambdaBoundVariable @@ var "v"))
      (Sets.toList (var "castVars"))) $
    "isSafe" <~ Logic.or (Sets.null (var "trusted"))
      (Logic.or (Sets.null (var "javaTypeVars"))
        (Sets.null (Sets.difference (var "javaTypeVars") (var "trusted")))) $
    Logic.ifElse (var "isSafe")
      ("jtype" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "castType" @@ var "cx" @@ var "g") $
       "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jtype" @@ var "cx") $
       right (JavaUtilsSource.javaCastExpressionToJavaExpression @@
         (JavaUtilsSource.javaCastExpression @@ var "rt" @@
           (JavaUtilsSource.javaExpressionToJavaUnaryExpression @@ var "expr"))))
      (right (var "expr"))

-- | Apply a Java argument to a Java expression using .apply() method invocation.
applyJavaArg :: TypedTermDefinition (Java.Expression -> Java.Expression -> Java.Expression)
applyJavaArg = def "applyJavaArg" $
  lambda "expr" $ lambda "jarg" $
    JavaUtilsSource.javaMethodInvocationToJavaExpression @@
      (JavaUtilsSource.methodInvocation @@ just (right
        (JavaUtilsSource.javaExpressionToJavaPrimary @@ var "expr"))
        @@ JavaDsl.identifier (asTerm JavaNamesSource.applyMethodName)
        @@ list [var "jarg"])

-- | Apply a type substitution to all type annotations in a term.
-- Monadic wrapper that gets the graph state and delegates to the pure helper.
applyOvergenSubstToTermAnnotations :: TypedTermDefinition (M.Map Name Type -> Term -> InferenceContext -> Graph -> Either Error Term)
applyOvergenSubstToTermAnnotations = def "applyOvergenSubstToTermAnnotations" $
  lambda "subst" $ lambda "term0" $
    "cx" ~> "g" ~>
    right (applyOvergenSubstToTermAnnotations_go @@ var "subst" @@ var "g" @@ var "term0")

-- | Recursive helper for applyOvergenSubstToTermAnnotations. Walks a term and applies
-- a type substitution to all type annotations. Also updates lambda domains and type applications.
-- Pure function: takes the substitution, graph context, and term directly.
applyOvergenSubstToTermAnnotations_go :: TypedTermDefinition (M.Map Name Type -> Graph -> Term -> Term)
applyOvergenSubstToTermAnnotations_go = def "applyOvergenSubstToTermAnnotations_go" $
  lambda "subst" $ lambda "cx" $ lambda "term" $
    cases _Term (var "term")
      (Just $ var "term") [
      _Term_annotated>>: lambda "at" $
        "inner" <~ Core.annotatedTermBody (var "at") $
        "ann" <~ (Annotations.getAnnotationMap @@ Core.annotatedTermAnnotation (var "at")) $
        "ann'" <~ Optionals.cases (Maps.lookup Constants.keyType (var "ann"))
          (var "ann")
          (lambda "typeTerm" $
            Eithers.either_
              (lambda "_" $ var "ann")
              (lambda "t" $
                "t'" <~ (substituteTypeVarsWithTypes @@ var "subst" @@ var "t") $
                Maps.insert (asTerm Constants.keyType) (Phantoms.encoderFor _Type @@ var "t'") (var "ann"))
              (Phantoms.decoderFor _Type @@ var "cx" @@ var "typeTerm")) $
        Core.termAnnotated (Core.annotatedTerm
          (applyOvergenSubstToTermAnnotations_go @@ var "subst" @@ var "cx" @@ var "inner")
          (Annotations.wrapAnnotationMap @@ var "ann'")),
      _Term_application>>: lambda "app" $
        Core.termApplication (Core.application
          (applyOvergenSubstToTermAnnotations_go @@ var "subst" @@ var "cx" @@ Core.applicationFunction (var "app"))
          (applyOvergenSubstToTermAnnotations_go @@ var "subst" @@ var "cx" @@ Core.applicationArgument (var "app"))),
      _Term_lambda>>: lambda "lam" $
        Core.termLambda (Core.lambda
          (Core.lambdaParameter (var "lam"))
          (Optionals.map (lambda "d" $ substituteTypeVarsWithTypes @@ var "subst" @@ var "d") (Core.lambdaDomain (var "lam")))
          (applyOvergenSubstToTermAnnotations_go @@ var "subst" @@ var "cx" @@ Core.lambdaBody (var "lam"))),
      _Term_cases>>: lambda "cs" $
        Core.termCases (Core.caseStatement
          (Core.caseStatementTypeName (var "cs"))
          (Optionals.map (lambda "d" $ applyOvergenSubstToTermAnnotations_go @@ var "subst" @@ var "cx" @@ var "d") (Core.caseStatementDefault (var "cs")))
          (Lists.map (lambda "fld" $ Core.caseAlternative (Core.caseAlternativeName (var "fld")) (applyOvergenSubstToTermAnnotations_go @@ var "subst" @@ var "cx" @@ Core.caseAlternativeHandler (var "fld"))) (Core.caseStatementCases (var "cs")))),
      _Term_let>>: lambda "lt" $
        Core.termLet (Core.let_
          (Lists.map (lambda "b" $ Core.binding (Core.bindingName (var "b")) (applyOvergenSubstToTermAnnotations_go @@ var "subst" @@ var "cx" @@ Core.bindingTerm (var "b")) (Core.bindingTypeScheme (var "b"))) (Core.letBindings (var "lt")))
          (applyOvergenSubstToTermAnnotations_go @@ var "subst" @@ var "cx" @@ Core.letBody (var "lt"))),
      _Term_typeApplication>>: lambda "ta" $
        Core.termTypeApplication (Core.typeApplicationTerm
          (applyOvergenSubstToTermAnnotations_go @@ var "subst" @@ var "cx"
            @@ Core.typeApplicationTermBody (var "ta"))
          (substituteTypeVarsWithTypes @@ var "subst"
            @@ Core.typeApplicationTermType (var "ta"))),
      _Term_typeLambda>>: lambda "tl" $
        Core.termTypeLambda (Core.typeLambda
          (Core.typeLambdaParameter (var "tl"))
          (applyOvergenSubstToTermAnnotations_go @@ var "subst" @@ var "cx"
            @@ Core.typeLambdaBody (var "tl")))]

-- | Recursively apply a type substitution
applySubstFull :: TypedTermDefinition (M.Map Name Type -> Type -> Type)
applySubstFull = def "applySubstFull" $
  lambda "s" $ lambda "t" $ cases _Type (Strip.deannotateType @@ var "t")
    (Just $ var "t") [
    _Type_variable>>: lambda "v" $
      Maps.findWithDefault (var "t") (var "v") (var "s"),
    _Type_function>>: lambda "ft" $
      Core.typeFunction (Core.functionType
        (applySubstFull @@ var "s" @@ Core.functionTypeDomain (var "ft"))
        (applySubstFull @@ var "s" @@ Core.functionTypeCodomain (var "ft"))),
    _Type_application>>: lambda "at" $
      Core.typeApplication (Core.applicationType
        (applySubstFull @@ var "s" @@ Core.applicationTypeFunction (var "at"))
        (applySubstFull @@ var "s" @@ (project _ApplicationType _ApplicationType_argument @@ var "at"))),
    _Type_list>>: lambda "inner" $
      Core.typeList (applySubstFull @@ var "s" @@ var "inner"),
    _Type_set>>: lambda "inner" $
      Core.typeSet (applySubstFull @@ var "s" @@ var "inner"),
    _Type_optional>>: lambda "inner" $
      Core.typeOptional (applySubstFull @@ var "s" @@ var "inner"),
    _Type_map>>: lambda "mt" $
      Core.typeMap (Core.mapType
        (applySubstFull @@ var "s" @@ Core.mapTypeKeys (var "mt"))
        (applySubstFull @@ var "s" @@ Core.mapTypeValues (var "mt"))),
    _Type_pair>>: lambda "pt" $
      Core.typePair (Core.pairType
        (applySubstFull @@ var "s" @@ Core.pairTypeFirst (var "pt"))
        (applySubstFull @@ var "s" @@ Core.pairTypeSecond (var "pt"))),
    _Type_either>>: lambda "et" $
      Core.typeEither (Core.eitherType
        (applySubstFull @@ var "s" @@ Core.eitherTypeLeft (var "et"))
        (applySubstFull @@ var "s" @@ Core.eitherTypeRight (var "et"))),
    _Type_forall>>: lambda "ft" $
      Core.typeForall (Core.forallType
        (Core.forallTypeParameter (var "ft"))
        (applySubstFull @@ (Maps.delete (Core.forallTypeParameter (var "ft")) (var "s"))
          @@ Core.forallTypeBody (var "ft")))]

-- | Simple top-level-only type variable substitution.
applySubstSimple :: TypedTermDefinition (M.Map Name Type -> Type -> Type)
applySubstSimple = def "applySubstSimple" $
  lambda "subst" $ lambda "t" $
    cases _Type (Strip.deannotateType @@ var "t")
      (Just $ var "t") [
      _Type_variable>>: lambda "v" $
        Maps.findWithDefault (var "t") (var "v") (var "subst")]

-- | Shared helper: java.util.Arrays.compare(this.field, otherVar.field)
arraysCompareExpr :: TypedTermDefinition (String -> String -> Java.Expression)
arraysCompareExpr = def "arraysCompareExpr" $
  lambda "otherVar" $ lambda "fname" $ lets [
    "header">: JavaDsl.methodInvocationHeaderComplex
      (JavaDsl.methodInvocationComplex
        (JavaDsl.methodInvocationVariantType (JavaUtilsSource.javaTypeName @@ wrap Java._Identifier (string "java.util.Arrays")))
        (list ([] :: [TypedTerm Java.TypeArgument]))
        (wrap Java._Identifier (string "compare"))),
    "arg1">: JavaUtilsSource.javaExpressionNameToJavaExpression @@
      (JavaDsl.expressionName nothing (wrap Java._Identifier (JavaUtilsSource.sanitizeJavaName @@ var "fname"))),
    "arg2">: JavaUtilsSource.javaExpressionNameToJavaExpression @@
      (JavaUtilsSource.fieldExpression @@ (JavaUtilsSource.javaIdentifier @@ var "otherVar") @@ (JavaUtilsSource.javaIdentifier @@ var "fname"))] $
    JavaUtilsSource.javaMethodInvocationToJavaExpression @@ (JavaDsl.methodInvocation_ (var "header") (list [var "arg1", var "arg2"]))

-- | java.util.Arrays.equals(this.field, other.field) for byte[] fields
arraysEqualsClause :: TypedTermDefinition (String -> String -> Java.InclusiveOrExpression)
arraysEqualsClause = def "arraysEqualsClause" $
  lambda "tmpName" $ lambda "fname" $ lets [
    "thisArg">: JavaUtilsSource.javaExpressionNameToJavaExpression
      @@ (JavaUtilsSource.fieldExpression @@ wrap Java._Identifier (string "this")
        @@ (JavaUtilsSource.javaIdentifier @@ var "fname")),
    "otherArg">: JavaUtilsSource.javaExpressionNameToJavaExpression
      @@ (JavaUtilsSource.fieldExpression @@ (JavaUtilsSource.javaIdentifier @@ var "tmpName")
        @@ (JavaUtilsSource.javaIdentifier @@ var "fname")),
    "header">: JavaDsl.methodInvocationHeaderComplex
      (JavaDsl.methodInvocationComplex
        (JavaDsl.methodInvocationVariantType (JavaUtilsSource.javaTypeName @@ wrap Java._Identifier (string "java.util.Arrays")))
        (list ([] :: [TypedTerm Java.TypeArgument]))
        (wrap Java._Identifier (asTerm JavaNamesSource.equalsMethodName)))] $
    JavaUtilsSource.javaPostfixExpressionToJavaInclusiveOrExpression
      @@ (JavaUtilsSource.javaMethodInvocationToJavaPostfixExpression
        @@ (JavaDsl.methodInvocation_ (var "header") (list [var "thisArg", var "otherArg"])))

-- | Augment a variant class declaration for union types.
-- Adds public static final modifiers, sets parent class extends, and adds accept method.
augmentVariantClass :: TypedTermDefinition (JavaHelpers.Aliases -> [Java.TypeParameter] -> Name -> Java.ClassDeclaration -> Java.ClassDeclaration)
augmentVariantClass = def "augmentVariantClass" $
  lambda "aliases" $ lambda "tparams" $ lambda "elName" $ lambda "cd" $
    cases Java._ClassDeclaration (var "cd")
      (Just $ var "cd") [
      Java._ClassDeclaration_normal>>: lambda "ncd" $
        "args" <~ Lists.map (lambda "tp" $ JavaUtilsSource.typeParameterToTypeArgument @@ var "tp") (var "tparams") $
        "extendsPart" <~ (JavaUtilsSource.nameToJavaClassType @@ var "aliases" @@ true @@ var "args" @@ var "elName" @@ nothing) $
        "newMods" <~ list [
          inject Java._ClassModifier Java._ClassModifier_public unit,
          inject Java._ClassModifier Java._ClassModifier_static unit,
          inject Java._ClassModifier Java._ClassModifier_final unit] $
        "oldBody" <~ project Java._NormalClassDeclaration Java._NormalClassDeclaration_body @@ var "ncd" $
        "oldDecls" <~ (unwrap Java._ClassBody @@ var "oldBody") $
        "acceptDecl" <~ (withCommentString @@ string "Dispatch to {@code visitor}." @@ (JavaUtilsSource.toAcceptMethod @@ false @@ var "tparams")) $
        "newBody" <~ wrap Java._ClassBody (Lists.concat2 (var "oldDecls") (list [var "acceptDecl"])) $
        inject Java._ClassDeclaration Java._ClassDeclaration_normal (record Java._NormalClassDeclaration [
          Java._NormalClassDeclaration_modifiers>>: var "newMods",
          Java._NormalClassDeclaration_identifier>>:
            project Java._NormalClassDeclaration Java._NormalClassDeclaration_identifier @@ var "ncd",
          Java._NormalClassDeclaration_parameters>>: var "tparams",
          Java._NormalClassDeclaration_extends>>: just (var "extendsPart"),
          Java._NormalClassDeclaration_implements>>:
            project Java._NormalClassDeclaration Java._NormalClassDeclaration_implements @@ var "ncd",
          Java._NormalClassDeclaration_permits>>:
            project Java._NormalClassDeclaration Java._NormalClassDeclaration_permits @@ var "ncd",
          Java._NormalClassDeclaration_body>>: var "newBody"])]

-- | Helper: coerce bigint to int for javaInt/javaIntExpression TypedTermDefinition (phantom type mismatch workaround)
bigintAsInt :: TypedTerm Integer -> TypedTerm Int
bigintAsInt = coerce

-- | Check if a Binding has function type.
bindingIsFunctionType :: TypedTermDefinition (Binding -> Bool)
bindingIsFunctionType = def "bindingIsFunctionType" $
  lambda "b" $
    Optionals.cases
      (Core.bindingTypeScheme (var "b"))
      -- No type scheme: check term structure
      (cases _Term (Strip.deannotateTerm @@ Core.bindingTerm (var "b"))
        (Just $ boolean False) [
        _Term_lambda>>: lambda "_f" $ boolean True,
        _Term_project>>: lambda "_f" $ boolean True,
        _Term_cases>>: lambda "_f" $ boolean True,
        _Term_unwrap>>: lambda "_f" $ boolean True])
      -- Has type scheme: check type
      (lambda "ts" $
        cases _Type (Strip.deannotateType @@ Core.typeSchemeBody (var "ts"))
          (Just $ boolean False) [
          _Type_function>>: lambda "_ft" $ boolean True,
          _Type_forall>>: lambda "fa" $
            cases _Type (Strip.deannotateType @@ Core.forallTypeBody (var "fa"))
              (Just $ boolean False) [
              _Type_function>>: lambda "_ft2" $ boolean True]])

-- | Decode a Type from its term encoding (limited subset).

bindingNameToFilePath :: TypedTermDefinition (Name -> String)
bindingNameToFilePath = def "bindingNameToFilePath" $
  lambda "name" $ lets [
    "qn">: Names.qualifyName @@ var "name",
    "ns_">: Util.qualifiedNameModuleName (var "qn"),
    "local">: Util.qualifiedNameLocal (var "qn"),
    "sanitized">: Formatting.sanitizeWithUnderscores @@ JavaLanguageSource.reservedWords @@ var "local",
    "unq">: Names.unqualifyName @@ Util.qualifiedName (var "ns_") (var "sanitized")] $
    Names.nameToFilePath @@ Util.caseConventionCamel @@ Util.caseConventionPascal
      @@ wrap _FileExtension (string "java") @@ var "unq"

-- | Convert let-bindings to Java block statements.
bindingsToStatements :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> [Binding] -> InferenceContext -> Graph -> Either Error ([Java.BlockStatement], JavaHelpers.JavaEnvironment))
bindingsToStatements = def "bindingsToStatements" $
  lambda "env" $ lambda "bindings" $
    "cx" ~> "g0" ~>
    "aliases" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env") $
    "g" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_graph @@ var "env") $
    -- Flatten nested lets then deduplicate names
    "flatBindings" <~ (dedupBindings @@ (project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeJavaVars @@ var "aliases")
      @@ (flattenBindings @@ var "bindings")) $
    -- Extend Graph with flattened bindings
    "gExtended" <~ (Scoping.extendGraphForLet @@ ("g" ~> "b" ~> Logic.ifElse (Predicates.isComplexBinding @@ var "g" @@ var "b") (just MetaTerms.true) nothing) @@ var "g"
      @@ record _Let [
        _Let_bindings>>: var "flatBindings",
        _Let_body>>: inject _Term _Term_variable (wrap _Name (string "dummy"))]) $
    -- Compute binding vars
    "bindingVars" <~ Sets.fromList (Lists.map (lambda "b" $ Core.bindingName (var "b")) (var "flatBindings")) $
    -- Build dependency graph
    "allDeps" <~ Maps.fromList (Lists.map
      (lambda "b" $
        "key" <~ Core.bindingName (var "b") $
        "deps" <~ Sets.intersection (var "bindingVars") (Variables.freeVariablesInTerm @@ Core.bindingTerm (var "b")) $
        pair (var "key") (var "deps"))
      (var "flatBindings")) $
    -- Topological sort for correct declaration order
    "sorted" <~ (Sorting.topologicalSortComponents @@
      (Lists.map (lambda "entry" $
        "key" <~ Pairs.first (var "entry") $
        "deps" <~ Pairs.second (var "entry") $
        pair (var "key") (Sets.toList (var "deps")))
        (Maps.toList (var "allDeps")))) $
    -- Identify recursive bindings
    "recursiveVars" <~ Sets.fromList (Lists.concat (Lists.map
      (lambda "names" $
        Logic.ifElse (Equality.equal (Lists.length (var "names")) (int32 1))
          (Optionals.cases (Lists.maybeHead (var "names")) (list ([] :: [TypedTerm Name])) (lambda "singleName" $
              Optionals.cases (Maps.lookup (var "singleName") (var "allDeps"))
                (list ([] :: [TypedTerm Name]))
                (lambda "deps" $
                  Logic.ifElse (Sets.member (var "singleName") (var "deps"))
                    (list [var "singleName"])
                    (list ([] :: [TypedTerm Name])))))
          (var "names"))
      (var "sorted"))) $
    -- Identify thunked vars. Mirror the Python coder's rule
    -- (shouldThunkBinding = isComplexBinding && !isTrivialTerm), with the
    -- additional Java-specific filter that recursive bindings go through the
    -- AtomicReference path instead, and function-typed bindings are emitted
    -- as methods rather than thunked values.
    --
    -- This replaces an earlier `needsThunking` heuristic that only thunked
    -- bindings whose RHS textually contained a let/typeApp/typeLambda. That
    -- rule missed cases like `dflt = recurse term` (lambda-applied), where
    -- the value is captured in only one branch of a downstream case-dispatch
    -- and eagerly evaluating it walks the entire subtree on every visit —
    -- producing the O(N^2) blowup observed in hydra.java.coder self-host (#344).
    "thunkedVars" <~ Sets.fromList (Lists.concat (Lists.map
      (lambda "b" $
        "bname" <~ Core.bindingName (var "b") $
        Logic.ifElse (Logic.and
          (Logic.not (Sets.member (var "bname") (var "recursiveVars")))
          (Logic.and
            (Predicates.isComplexBinding @@ var "gExtended" @@ var "b")
            (Logic.and
              (Logic.not (Predicates.isTrivialTerm @@ Core.bindingTerm (var "b")))
              (Logic.not (bindingIsFunctionType @@ var "b")))))
          (list [var "bname"])
          (list ([] :: [TypedTerm Name])))
      (var "flatBindings"))) $
    -- Build extended aliases
    "aliasesExtended" <~ (record JavaHelpers._Aliases [
      JavaHelpers._Aliases_currentNamespace>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_currentNamespace @@ var "aliases",
      JavaHelpers._Aliases_packages>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_packages @@ var "aliases",
      JavaHelpers._Aliases_branchVars>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_branchVars @@ var "aliases",
      JavaHelpers._Aliases_recursiveVars>>:
        Sets.union (project JavaHelpers._Aliases JavaHelpers._Aliases_recursiveVars @@ var "aliases") (var "recursiveVars"),
      JavaHelpers._Aliases_inScopeTypeParams>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeTypeParams @@ var "aliases",
      JavaHelpers._Aliases_polymorphicLocals>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_polymorphicLocals @@ var "aliases",
      JavaHelpers._Aliases_inScopeJavaVars>>:
        Sets.union (project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeJavaVars @@ var "aliases") (var "bindingVars"),
      JavaHelpers._Aliases_varRenames>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_varRenames @@ var "aliases",
      JavaHelpers._Aliases_lambdaVars>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_lambdaVars @@ var "aliases",
      JavaHelpers._Aliases_typeVarSubst>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_typeVarSubst @@ var "aliases",
      JavaHelpers._Aliases_trustedTypeVars>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_trustedTypeVars @@ var "aliases",
      JavaHelpers._Aliases_methodCodomain>>:
        project JavaHelpers._Aliases JavaHelpers._Aliases_methodCodomain @@ var "aliases",
      JavaHelpers._Aliases_thunkedVars>>:
        Sets.union (project JavaHelpers._Aliases JavaHelpers._Aliases_thunkedVars @@ var "aliases") (var "thunkedVars")]) $
    -- Build extended environment
    "envExtended" <~ (record JavaHelpers._JavaEnvironment [
      JavaHelpers._JavaEnvironment_aliases>>: var "aliasesExtended",
      JavaHelpers._JavaEnvironment_graph>>: var "gExtended"]) $
    -- Generate statements
    Logic.ifElse (Lists.null (var "bindings"))
      (right (pair (list ([] :: [TypedTerm Java.BlockStatement])) (var "envExtended")))
      ("groups" <<~ (Eithers.mapList
        (lambda "names" $
          -- For each group: generate init statements (for recursive vars) and decl statements
          "inits" <<~ (Eithers.mapList (lambda "n" $ toDeclInit @@ var "aliasesExtended" @@ var "gExtended" @@ var "recursiveVars" @@ var "flatBindings" @@ var "n" @@ var "cx" @@ var "g") (var "names")) $
          "decls" <<~ (Eithers.mapList (lambda "n" $ toDeclStatement @@ var "envExtended" @@ var "aliasesExtended" @@ var "gExtended" @@ var "recursiveVars" @@ var "thunkedVars" @@ var "flatBindings" @@ var "n" @@ var "cx" @@ var "g") (var "names")) $
          right (Lists.concat2 (Optionals.cat (var "inits")) (var "decls")))
        (var "sorted")) $
        right (pair (Lists.concat (var "groups")) (var "envExtended")))

-- | Dispatch type to class declaration.

boundTypeVariables :: TypedTermDefinition (Type -> [Name])
boundTypeVariables = def "boundTypeVariables" $
  lambda "typ" $ cases _Type (var "typ")
    (Just $ list ([] :: [TypedTerm Name])) [
    _Type_annotated>>: lambda "at" $
      boundTypeVariables @@ (Core.annotatedTypeBody (var "at")),
    _Type_forall>>: lambda "ft" $
      Lists.cons
        (Core.forallTypeParameter (var "ft"))
        (boundTypeVariables @@ (Core.forallTypeBody (var "ft")))]

-- | Build a type substitution from scheme domain types and actual argument types.
buildArgSubst :: TypedTermDefinition (S.Set Name -> [Type] -> [Type] -> M.Map Name Type)
buildArgSubst = def "buildArgSubst" $
  lambda "schemeVarSet" $ lambda "schemeDoms" $ lambda "argTypes" $
    Maps.fromList (Lists.bind
      (Lists.zip (var "schemeDoms") (var "argTypes"))
      (lambda "p" $
        "sdom" <~ Pairs.first (var "p") $
        "argType" <~ Pairs.second (var "p") $
        cases _Type (Strip.deannotateType @@ var "sdom")
          (Just $ list ([] :: [TypedTerm (Name, Type)])) [
          _Type_variable>>: lambda "v" $
            Logic.ifElse
              (Sets.member (var "v") (var "schemeVarSet"))
              (list [pair (var "v") (var "argType")])
              (list ([] :: [TypedTerm (Name, Type)]))]))

-- | Build a curried lambda chain from a list of parameter names wrapping an inner expression.
-- E.g., buildCurriedLambda [p0, p1] inner = javaLambda p0 (javaLambda p1 inner)
buildCurriedLambda :: TypedTermDefinition ([Name] -> Java.Expression -> Java.Expression)
buildCurriedLambda = def "buildCurriedLambda" $
  lambda "params" $ lambda "inner" $
    Lists.foldl
      (lambda "acc" $ lambda "p" $ JavaUtilsSource.javaLambda @@ var "p" @@ var "acc")
      (var "inner")
      (Lists.reverse (var "params"))

-- | Build a type variable substitution by walking a term and comparing lambda domain types
-- against annotation map types. Returns a Map Name Name (fresh→canonical mapping).
buildSubstFromAnnotations :: TypedTermDefinition (S.Set Name -> Term -> InferenceContext -> Graph -> Either Error (M.Map Name Name))
buildSubstFromAnnotations = def "buildSubstFromAnnotations" $
  lambda "schemeVarSet" $ lambda "term" $
    "cx" ~> "g" ~>
    right (buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ var "term")

-- | Recursive helper for buildSubstFromAnnotations. Walks a term and compares lambda domain types
-- (normalized) against annotation map types (NOT normalized) to recover the fresh→canonical mapping.
-- Pure function: takes the graph and term directly.
buildSubstFromAnnotations_go :: TypedTermDefinition (S.Set Name -> Graph -> Term -> M.Map Name Name)
buildSubstFromAnnotations_go = def "buildSubstFromAnnotations_go" $
  lambda "schemeVarSet" $ lambda "g" $ lambda "term" $
    cases _Term (var "term")
      (Just Maps.empty) [
      _Term_annotated>>: lambda "at" $
        "body" <~ Core.annotatedTermBody (var "at") $
        "anns" <~ (Annotations.getAnnotationMap @@ Core.annotatedTermAnnotation (var "at")) $
        "bodySubst" <~ (buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ var "body") $
        "annSubst" <~ Optionals.cases (Maps.lookup Constants.keyType (var "anns"))
          Maps.empty
          (lambda "typeTerm" $
            Eithers.either_
              (lambda "_" Maps.empty)
              (lambda "annType" $
                cases _Term (Strip.deannotateTerm @@ var "body")
                  (Just Maps.empty) [
                  _Term_lambda>>: lambda "lam" $
                    Optionals.cases (Core.lambdaDomain (var "lam"))
                      Maps.empty
                      (lambda "dom" $
                        cases _Type (Strip.deannotateType @@ var "annType")
                          (Just Maps.empty) [
                          _Type_function>>: lambda "ft" $
                            buildTypeVarSubst @@ var "schemeVarSet"
                              @@ Core.functionTypeDomain (var "ft")
                              @@ var "dom"])])
              (Phantoms.decoderFor _Type @@ var "g" @@ var "typeTerm")) $
        Maps.union (var "annSubst") (var "bodySubst"),
      _Term_application>>: lambda "app" $
        Maps.union
          (buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ Core.applicationFunction (var "app"))
          (buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ Core.applicationArgument (var "app")),
      _Term_lambda>>: lambda "lam" $
        buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ Core.lambdaBody (var "lam"),
      _Term_cases>>: lambda "cs" $
        "defSubst" <~ Optionals.cases (Core.caseStatementDefault (var "cs"))
          Maps.empty
          (lambda "d" $ buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ var "d") $
        "caseSubsts" <~ Lists.foldl
          (lambda "acc" $ lambda "fld" $
            Maps.union (var "acc") (buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ Core.caseAlternativeHandler (var "fld")))
          Maps.empty
          (Core.caseStatementCases (var "cs")) $
        Maps.union (var "defSubst") (var "caseSubsts"),
      _Term_let>>: lambda "lt" $
        "bindingSubst" <~ Lists.foldl
          (lambda "acc" $ lambda "b" $
            Maps.union (var "acc") (buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ Core.bindingTerm (var "b")))
          Maps.empty
          (Core.letBindings (var "lt")) $
        Maps.union (var "bindingSubst") (buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ Core.letBody (var "lt")),
      _Term_list>>: lambda "terms" $
        Lists.foldl
          (lambda "acc" $ lambda "t" $
            Maps.union (var "acc") (buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ var "t"))
          Maps.empty
          (var "terms"),
      _Term_optional>>: lambda "mt" $
        Optionals.cases (var "mt")
          Maps.empty
          (lambda "t" $ buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ var "t"),
      _Term_pair>>: lambda "p" $
        Maps.union
          (buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ Pairs.first (var "p"))
          (buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ Pairs.second (var "p")),
      _Term_record>>: lambda "r" $
        Lists.foldl
          (lambda "acc" $ lambda "fld" $
            Maps.union (var "acc") (buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ Core.fieldTerm (var "fld")))
          Maps.empty
          (Core.recordFields (var "r")),
      _Term_set>>: lambda "terms" $
        Lists.foldl
          (lambda "acc" $ lambda "t" $
            Maps.union (var "acc") (buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ var "t"))
          Maps.empty
          (Sets.toList (var "terms")),
      _Term_typeApplication>>: lambda "ta" $
        buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g"
          @@ Core.typeApplicationTermBody (var "ta"),
      _Term_typeLambda>>: lambda "tl" $
        buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g"
          @@ Core.typeLambdaBody (var "tl"),
      _Term_either>>: lambda "e" $
        Eithers.either_
          (lambda "t" $ buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ var "t")
          (lambda "t" $ buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ var "t")
          (var "e")]

-- | Build a mapping from scheme type variables to actual types by structurally matching
-- a scheme type against an actual type. Only maps variables that are in the schemeVarSet.
buildTypeSubst :: TypedTermDefinition (S.Set Name -> Type -> Type -> M.Map Name Type)
buildTypeSubst = def "buildTypeSubst" $
  lambda "schemeVarSet" $ lambda "schemeType" $ lambda "actualType" $
    buildTypeSubst_go @@ var "schemeVarSet"
      @@ (Strip.deannotateType @@ var "schemeType")
      @@ (Strip.deannotateType @@ var "actualType")

-- | Recursive helper for buildTypeSubst. Takes deannotated types.
buildTypeSubst_go :: TypedTermDefinition (S.Set Name -> Type -> Type -> M.Map Name Type)
buildTypeSubst_go = def "buildTypeSubst_go" $
  lambda "svs" $ lambda "st" $ lambda "at" $
    "goSub" <~ (lambda "a" $ lambda "b" $
      buildTypeSubst_go @@ var "svs"
        @@ (Strip.deannotateType @@ var "a")
        @@ (Strip.deannotateType @@ var "b")) $
    cases _Type (var "st")
      (Just (Maps.empty :: TypedTerm (M.Map Name Type))) [
      _Type_variable>>: lambda "v" $
        Logic.ifElse (Sets.member (var "v") (var "svs"))
          (Maps.singleton (var "v") (var "at"))
          (Maps.empty :: TypedTerm (M.Map Name Type)),
      _Type_function>>: lambda "sft" $
        cases _Type (var "at")
          (Just (Maps.empty :: TypedTerm (M.Map Name Type))) [
          _Type_function>>: lambda "aft" $
            Maps.union
              (var "goSub" @@ Core.functionTypeDomain (var "sft") @@ Core.functionTypeDomain (var "aft"))
              (var "goSub" @@ Core.functionTypeCodomain (var "sft") @@ Core.functionTypeCodomain (var "aft"))],
      _Type_application>>: lambda "sat" $
        cases _Type (var "at")
          (Just (Maps.empty :: TypedTerm (M.Map Name Type))) [
          _Type_application>>: lambda "aat" $
            Maps.union
              (var "goSub" @@ Core.applicationTypeFunction (var "sat") @@ Core.applicationTypeFunction (var "aat"))
              (var "goSub" @@ (project _ApplicationType _ApplicationType_argument @@ var "sat")
                           @@ (project _ApplicationType _ApplicationType_argument @@ var "aat"))],
      _Type_list>>: lambda "sl" $
        cases _Type (var "at")
          (Just (Maps.empty :: TypedTerm (M.Map Name Type))) [
          _Type_list>>: lambda "al" $
            var "goSub" @@ var "sl" @@ var "al"],
      _Type_set>>: lambda "ss" $
        cases _Type (var "at")
          (Just (Maps.empty :: TypedTerm (M.Map Name Type))) [
          _Type_set>>: lambda "as'" $
            var "goSub" @@ var "ss" @@ var "as'"],
      _Type_optional>>: lambda "sm" $
        cases _Type (var "at")
          (Just (Maps.empty :: TypedTerm (M.Map Name Type))) [
          _Type_optional>>: lambda "am" $
            var "goSub" @@ var "sm" @@ var "am"],
      _Type_map>>: lambda "smt" $
        cases _Type (var "at")
          (Just (Maps.empty :: TypedTerm (M.Map Name Type))) [
          _Type_map>>: lambda "amt" $
            Maps.union
              (var "goSub" @@ Core.mapTypeKeys (var "smt") @@ Core.mapTypeKeys (var "amt"))
              (var "goSub" @@ Core.mapTypeValues (var "smt") @@ Core.mapTypeValues (var "amt"))],
      _Type_pair>>: lambda "spt" $
        cases _Type (var "at")
          (Just (Maps.empty :: TypedTerm (M.Map Name Type))) [
          _Type_pair>>: lambda "apt" $
            Maps.union
              (var "goSub" @@ Core.pairTypeFirst (var "spt") @@ Core.pairTypeFirst (var "apt"))
              (var "goSub" @@ Core.pairTypeSecond (var "spt") @@ Core.pairTypeSecond (var "apt"))],
      _Type_either>>: lambda "set'" $
        cases _Type (var "at")
          (Just (Maps.empty :: TypedTerm (M.Map Name Type))) [
          _Type_either>>: lambda "aet" $
            Maps.union
              (var "goSub" @@ Core.eitherTypeLeft (var "set'") @@ Core.eitherTypeLeft (var "aet"))
              (var "goSub" @@ Core.eitherTypeRight (var "set'") @@ Core.eitherTypeRight (var "aet"))],
      _Type_forall>>: lambda "sfa" $
        -- For forall, unwrap both sides if matching, or just the scheme side
        cases _Type (var "at")
          (Just $ var "goSub" @@ Core.forallTypeBody (var "sfa") @@ var "at") [
          _Type_forall>>: lambda "afa" $
            var "goSub" @@ Core.forallTypeBody (var "sfa") @@ Core.forallTypeBody (var "afa")]]

-- | Build a type variable substitution by structurally matching a "fresh" type (from inference
-- annotations) against a "canonical" type (from the type scheme). When both types have a
-- TypeVariable at the same structural position, maps the fresh name to the canonical name.
-- Only includes mappings where the names actually differ.
buildTypeVarSubst :: TypedTermDefinition (S.Set Name -> Type -> Type -> M.Map Name Name)
buildTypeVarSubst = def "buildTypeVarSubst" $
  lambda "schemeVarSet" $ lambda "freshTyp" $ lambda "canonTyp" $
    buildTypeVarSubst_go @@ var "schemeVarSet"
      @@ (Strip.deannotateType @@ var "freshTyp")
      @@ (Strip.deannotateType @@ var "canonTyp")

-- | Recursive helper for buildTypeVarSubst. Takes deannotated types.
buildTypeVarSubst_go :: TypedTermDefinition (S.Set Name -> Type -> Type -> M.Map Name Name)
buildTypeVarSubst_go = def "buildTypeVarSubst_go" $
  lambda "svs" $ lambda "ft" $ lambda "ct" $
    "goSub" <~ (lambda "a" $ lambda "b" $
      buildTypeVarSubst_go @@ var "svs"
        @@ (Strip.deannotateType @@ var "a")
        @@ (Strip.deannotateType @@ var "b")) $
    cases _Type (var "ft")
      (Just $ -- Default: check if ct is a forall, and if so unwrap it
        cases _Type (var "ct")
          (Just (Maps.empty :: TypedTerm (M.Map Name Name))) [
          _Type_forall>>: lambda "cfa" $
            buildTypeVarSubst_go @@ var "svs" @@ var "ft"
              @@ (Strip.deannotateType @@ Core.forallTypeBody (var "cfa"))]) [
      _Type_variable>>: lambda "fn" $
        cases _Type (var "ct")
          (Just (Maps.empty :: TypedTerm (M.Map Name Name))) [
          _Type_variable>>: lambda "cn" $
            Logic.ifElse
              (Logic.and (Logic.not (Equality.equal (var "fn") (var "cn"))) (Sets.member (var "cn") (var "svs")))
              (Maps.singleton (var "fn") (var "cn"))
              (Maps.empty :: TypedTerm (M.Map Name Name))],
      _Type_function>>: lambda "fft" $
        cases _Type (var "ct")
          (Just (Maps.empty :: TypedTerm (M.Map Name Name))) [
          _Type_function>>: lambda "cft" $
            Maps.union
              (var "goSub" @@ Core.functionTypeDomain (var "fft") @@ Core.functionTypeDomain (var "cft"))
              (var "goSub" @@ Core.functionTypeCodomain (var "fft") @@ Core.functionTypeCodomain (var "cft"))],
      _Type_application>>: lambda "fat" $
        cases _Type (var "ct")
          (Just (Maps.empty :: TypedTerm (M.Map Name Name))) [
          _Type_application>>: lambda "cat" $
            Maps.union
              (var "goSub" @@ Core.applicationTypeFunction (var "fat") @@ Core.applicationTypeFunction (var "cat"))
              (var "goSub" @@ (project _ApplicationType _ApplicationType_argument @@ var "fat")
                           @@ (project _ApplicationType _ApplicationType_argument @@ var "cat"))],
      _Type_list>>: lambda "fl" $
        cases _Type (var "ct")
          (Just (Maps.empty :: TypedTerm (M.Map Name Name))) [
          _Type_list>>: lambda "cl" $
            var "goSub" @@ var "fl" @@ var "cl"],
      _Type_set>>: lambda "fs" $
        cases _Type (var "ct")
          (Just (Maps.empty :: TypedTerm (M.Map Name Name))) [
          _Type_set>>: lambda "cs" $
            var "goSub" @@ var "fs" @@ var "cs"],
      _Type_optional>>: lambda "fm" $
        cases _Type (var "ct")
          (Just (Maps.empty :: TypedTerm (M.Map Name Name))) [
          _Type_optional>>: lambda "cm" $
            var "goSub" @@ var "fm" @@ var "cm"],
      _Type_map>>: lambda "fmt" $
        cases _Type (var "ct")
          (Just (Maps.empty :: TypedTerm (M.Map Name Name))) [
          _Type_map>>: lambda "cmt" $
            Maps.union
              (var "goSub" @@ Core.mapTypeKeys (var "fmt") @@ Core.mapTypeKeys (var "cmt"))
              (var "goSub" @@ Core.mapTypeValues (var "fmt") @@ Core.mapTypeValues (var "cmt"))],
      _Type_pair>>: lambda "fpt" $
        cases _Type (var "ct")
          (Just (Maps.empty :: TypedTerm (M.Map Name Name))) [
          _Type_pair>>: lambda "cpt" $
            Maps.union
              (var "goSub" @@ Core.pairTypeFirst (var "fpt") @@ Core.pairTypeFirst (var "cpt"))
              (var "goSub" @@ Core.pairTypeSecond (var "fpt") @@ Core.pairTypeSecond (var "cpt"))],
      _Type_either>>: lambda "fet" $
        cases _Type (var "ct")
          (Just (Maps.empty :: TypedTerm (M.Map Name Name))) [
          _Type_either>>: lambda "cet" $
            Maps.union
              (var "goSub" @@ Core.eitherTypeLeft (var "fet") @@ Core.eitherTypeLeft (var "cet"))
              (var "goSub" @@ Core.eitherTypeRight (var "fet") @@ Core.eitherTypeRight (var "cet"))],
      _Type_forall>>: lambda "ffa" $
        cases _Type (var "ct")
          (Just $ -- ct is not a forall, but ft is: unwrap ft and recurse
            buildTypeVarSubst_go @@ var "svs"
              @@ (Strip.deannotateType @@ Core.forallTypeBody (var "ffa"))
              @@ var "ct") [
          _Type_forall>>: lambda "cfa" $
            var "goSub" @@ Core.forallTypeBody (var "ffa") @@ Core.forallTypeBody (var "cfa")]]

classModsPublic :: TypedTermDefinition [Java.ClassModifier]
classModsPublic = def "classModsPublic" $
  list [inject Java._ClassModifier Java._ClassModifier_public unit]

-- | Classify a data reference by looking up its element and classifying its term
classifyDataReference :: TypedTermDefinition (Name -> InferenceContext -> Graph -> Either Error JavaHelpers.JavaSymbolClass)
classifyDataReference = def "classifyDataReference" $
  lambda "name" $
    "cx" ~> "g" ~>
    "mel" <<~ right (Lexical.lookupBinding @@ var "g" @@ var "name") $
    Optionals.cases (var "mel")
      -- Not found: treat as local variable
      (right $ inject JavaHelpers._JavaSymbolClass JavaHelpers._JavaSymbolClass_localVariable unit)
      (lambda "el" $
        Optionals.cases (Core.bindingTypeScheme (var "el"))
          (left (Error.errorOther $ Error.otherError $ Strings.cat2 (string "no type scheme for element ") ((unwrap _Name @@ Core.bindingName (var "el")))))
          (lambda "ts" $
            right $ classifyDataTerm @@ var "ts" @@ Core.bindingTerm (var "el")))

-- | Classify a data term by its symbol class (constant, nullary function, hoisted lambda, etc.)
classifyDataTerm :: TypedTermDefinition (TypeScheme -> Term -> JavaHelpers.JavaSymbolClass)
classifyDataTerm = def "classifyDataTerm" $
  lambda "ts" $ lambda "term" $
    Logic.ifElse (Dependencies.isLambda @@ var "term")
      -- Lambda terms
      ("n" <~ classifyDataTerm_countLambdaParams @@ var "term" $
        Logic.ifElse (Equality.gt (var "n") (int32 1))
          (inject JavaHelpers._JavaSymbolClass JavaHelpers._JavaSymbolClass_hoistedLambda (var "n"))
          (inject JavaHelpers._JavaSymbolClass JavaHelpers._JavaSymbolClass_unaryFunction unit))
      -- Non-lambda terms
      ("hasTypeParams" <~ Logic.not (Lists.null (Core.typeSchemeVariables (var "ts"))) $
        Logic.ifElse (var "hasTypeParams")
          -- Polymorphic: check stripped body
          ("n2" <~ classifyDataTerm_countLambdaParams @@ (classifyDataTerm_stripTypeLambdas @@ var "term") $
            Logic.ifElse (Equality.gt (var "n2") (int32 0))
              (inject JavaHelpers._JavaSymbolClass JavaHelpers._JavaSymbolClass_hoistedLambda (var "n2"))
              (inject JavaHelpers._JavaSymbolClass JavaHelpers._JavaSymbolClass_nullaryFunction unit))
          -- Non-lambda, non-polymorphic: nullary function
          (inject JavaHelpers._JavaSymbolClass JavaHelpers._JavaSymbolClass_nullaryFunction unit))

-- | Count the number of lambda parameters in a term (recursing through let bodies)
classifyDataTerm_countLambdaParams :: TypedTermDefinition (Term -> Int)
classifyDataTerm_countLambdaParams = def "classifyDataTerm_countLambdaParams" $
  lambda "t" $
    cases _Term (Strip.deannotateTerm @@ var "t")
      (Just $ int32 0) [
      _Term_lambda>>: lambda "lam" $
        Math.add (int32 1)
          (classifyDataTerm_countLambdaParams @@ (project _Lambda _Lambda_body @@ var "lam")),
      _Term_let>>: lambda "lt" $
        classifyDataTerm_countLambdaParams @@ (project _Let _Let_body @@ var "lt")]

-- | Strip type lambda wrappers from a term
classifyDataTerm_stripTypeLambdas :: TypedTermDefinition (Term -> Term)
classifyDataTerm_stripTypeLambdas = def "classifyDataTerm_stripTypeLambdas" $
  lambda "t" $
    cases _Term (Strip.deannotateTerm @@ var "t")
      (Just $ var "t") [
      _Term_typeLambda>>: lambda "tl" $
        classifyDataTerm_stripTypeLambdas @@ (project _TypeLambda _TypeLambda_body @@ var "tl")]

-- | Classify a data reference by looking up its element and classifying its term

-- | Shared helper: int cmp = 0; declaration
cmpDeclStatement :: TypedTermDefinition (JavaHelpers.Aliases -> Java.BlockStatement)
cmpDeclStatement = def "cmpDeclStatement" $
  lambda "aliases" $
    JavaUtilsSource.variableDeclarationStatement @@ var "aliases" @@ (asTerm JavaUtilsSource.javaIntType)
      @@ (JavaUtilsSource.javaIdentifier @@ string "cmp") @@ (JavaUtilsSource.javaIntExpression @@ bigintAsInt (bigint 0))

-- | Shared helper: cmp != 0 expression
cmpNotZeroExpr :: TypedTermDefinition Java.Expression
cmpNotZeroExpr = def "cmpNotZeroExpr" $ lets [
    "lhs">: JavaUtilsSource.javaRelationalExpressionToJavaEqualityExpression @@
      (JavaUtilsSource.javaPostfixExpressionToJavaRelationalExpression @@
        (JavaDsl.postfixExpressionName (JavaDsl.expressionName nothing (JavaUtilsSource.javaIdentifier @@ string "cmp")))),
    "rhs">: JavaUtilsSource.javaPostfixExpressionToJavaRelationalExpression @@
      (JavaDsl.postfixExpressionPrimary (JavaUtilsSource.javaLiteralToJavaPrimary @@ (JavaUtilsSource.javaInt @@ bigintAsInt (bigint 0))))] $
    JavaUtilsSource.javaEqualityExpressionToJavaExpression @@
      (JavaDsl.equalityExpressionNotEqual (JavaDsl.equalityExpressionBinary (var "lhs") (var "rhs")))

-- | Collect forall-bound type parameter names from a type
collectForallParams :: TypedTermDefinition (Type -> [Name])
collectForallParams = def "collectForallParams" $
  lambda "t" $ cases _Type (Strip.deannotateType @@ var "t")
    (Just $ list ([] :: [TypedTerm Name])) [
    _Type_forall>>: lambda "fa" $
      Lists.cons (Core.forallTypeParameter (var "fa"))
        (collectForallParams @@ Core.forallTypeBody (var "fa"))]

-- | Collect domain annotations from a chain of nested lambdas.
-- Returns (domains, innerBody).
collectLambdaDomains :: TypedTermDefinition (Term -> ([Type], Term))
collectLambdaDomains = def "collectLambdaDomains" $
  lambda "t" $
    cases _Term (Strip.deannotateTerm @@ var "t")
      (Just $ pair (list ([] :: [TypedTerm Type])) (var "t")) [
      _Term_lambda>>: lambda "lam" $
        Optionals.cases (Core.lambdaDomain (var "lam"))
          (pair (list ([] :: [TypedTerm Type])) (var "t"))
          (lambda "dom" $
            "rest" <~ (collectLambdaDomains @@ Core.lambdaBody (var "lam")) $
            pair (Lists.cons (var "dom") (Pairs.first (var "rest")))
              (Pairs.second (var "rest")))]

-- | Collect type arguments from nested TermTypeApplication chain, stripping annotations.
collectTypeApps :: TypedTermDefinition (Term -> [Type] -> (Term, [Type]))
collectTypeApps = def "collectTypeApps" $
  lambda "t" $ lambda "acc" $
    cases _Term (Strip.deannotateTerm @@ var "t")
      (Just $ pair (Strip.deannotateTerm @@ var "t") (var "acc")) [
      _Term_typeApplication>>: lambda "ta" $
        collectTypeApps
          @@ Core.typeApplicationTermBody (var "ta")
          @@ Lists.cons (Core.typeApplicationTermType (var "ta")) (var "acc")]

-- | Like collectTypeApps but preserves the original (annotated) term when no more type apps.
collectTypeApps0 :: TypedTermDefinition (Term -> [Type] -> (Term, [Type]))
collectTypeApps0 = def "collectTypeApps0" $
  lambda "t" $ lambda "acc" $
    cases _Term (Strip.deannotateTerm @@ var "t")
      (Just $ pair (var "t") (var "acc")) [
      _Term_typeApplication>>: lambda "ta" $
        collectTypeApps0
          @@ Core.typeApplicationTermBody (var "ta")
          @@ Lists.cons (Core.typeApplicationTermType (var "ta")) (var "acc")]

-- | Collect all type variable names from a type
collectTypeVars :: TypedTermDefinition (Type -> S.Set Name)
collectTypeVars = def "collectTypeVars" $
  lambda "typ" $ collectTypeVars_go @@ (Strip.deannotateType @@ var "typ")

-- | Helper for collectTypeVars
collectTypeVars_go :: TypedTermDefinition (Type -> S.Set Name)
collectTypeVars_go = def "collectTypeVars_go" $
  lambda "t" $ cases _Type (var "t")
    (Just $ (Sets.empty :: TypedTerm (S.Set Name))) [
    _Type_variable>>: lambda "name" $
      Sets.singleton (var "name"),
    _Type_function>>: lambda "ft" $
      Sets.union
        (collectTypeVars_go @@ (Strip.deannotateType @@ Core.functionTypeDomain (var "ft")))
        (collectTypeVars_go @@ (Strip.deannotateType @@ Core.functionTypeCodomain (var "ft"))),
    _Type_application>>: lambda "at" $
      Sets.union
        (collectTypeVars_go @@ (Strip.deannotateType @@ Core.applicationTypeFunction (var "at")))
        (collectTypeVars_go @@ (Strip.deannotateType @@ (project _ApplicationType _ApplicationType_argument @@ var "at"))),
    _Type_list>>: lambda "inner" $
      collectTypeVars_go @@ (Strip.deannotateType @@ var "inner"),
    _Type_set>>: lambda "inner" $
      collectTypeVars_go @@ (Strip.deannotateType @@ var "inner"),
    _Type_optional>>: lambda "inner" $
      collectTypeVars_go @@ (Strip.deannotateType @@ var "inner"),
    _Type_map>>: lambda "mt" $
      Sets.union
        (collectTypeVars_go @@ (Strip.deannotateType @@ Core.mapTypeKeys (var "mt")))
        (collectTypeVars_go @@ (Strip.deannotateType @@ Core.mapTypeValues (var "mt"))),
    _Type_pair>>: lambda "pt" $
      Sets.union
        (collectTypeVars_go @@ (Strip.deannotateType @@ Core.pairTypeFirst (var "pt")))
        (collectTypeVars_go @@ (Strip.deannotateType @@ Core.pairTypeSecond (var "pt"))),
    _Type_either>>: lambda "et" $
      Sets.union
        (collectTypeVars_go @@ (Strip.deannotateType @@ Core.eitherTypeLeft (var "et")))
        (collectTypeVars_go @@ (Strip.deannotateType @@ Core.eitherTypeRight (var "et"))),
    _Type_forall>>: lambda "ft" $
      collectTypeVars_go @@ (Strip.deannotateType @@ Core.forallTypeBody (var "ft"))]

-- | Shared helper: hydra.util.Comparing.compare(this.field, otherVar.field)
comparableCompareExpr :: TypedTermDefinition (String -> String -> Java.Expression)
comparableCompareExpr = def "comparableCompareExpr" $
  lambda "otherVar" $ lambda "fname" $ lets [
    "thisField">: JavaUtilsSource.javaIdentifierToJavaExpression @@ wrap Java._Identifier (JavaUtilsSource.sanitizeJavaName @@ var "fname"),
    "otherField">: JavaUtilsSource.javaExpressionNameToJavaExpression @@
      (JavaUtilsSource.fieldExpression @@ (JavaUtilsSource.javaIdentifier @@ var "otherVar") @@ (JavaUtilsSource.javaIdentifier @@ var "fname"))] $
    JavaUtilsSource.javaMethodInvocationToJavaExpression @@
      (JavaUtilsSource.methodInvocationStatic
        @@ JavaDsl.identifier (string "hydra.util.Comparing")
        @@ JavaDsl.identifier (string "compare")
        @@ list [var "thisField", var "otherField"])

-- | Shared helper: cmp = expr; if (cmp != 0) return cmp;
compareAndReturnStmts :: TypedTermDefinition (String -> FieldType -> [Java.BlockStatement])
compareAndReturnStmts = def "compareAndReturnStmts" $
  lambda "otherVar" $ lambda "f" $
    list [
      JavaDsl.blockStatementStatement
        (JavaUtilsSource.javaAssignmentStatement
          @@ (JavaDsl.leftHandSideExpressionName (JavaDsl.expressionName nothing (JavaUtilsSource.javaIdentifier @@ string "cmp")))
          @@ (compareFieldExpr @@ var "otherVar" @@ var "f")),
      JavaDsl.blockStatementStatement
        (JavaDsl.statementIfThen (JavaDsl.ifThenStatement (asTerm cmpNotZeroExpr)
          (JavaUtilsSource.javaReturnStatement @@ just
            (JavaUtilsSource.javaExpressionNameToJavaExpression @@ (JavaDsl.expressionName nothing (JavaUtilsSource.javaIdentifier @@ string "cmp"))))))]

-- | Shared helper: dispatch to appropriate comparison expression based on field type
compareFieldExpr :: TypedTermDefinition (String -> FieldType -> Java.Expression)
compareFieldExpr = def "compareFieldExpr" $
  lambda "otherVar" $ lambda "ft" $
    "fname" <~ Core.unName (Core.fieldTypeName (var "ft")) $
    "ftype" <~ Core.fieldTypeType (var "ft") $
    Logic.ifElse (isBinaryType @@ var "ftype")
      (arraysCompareExpr @@ var "otherVar" @@ var "fname")
      (Logic.ifElse (isNonComparableType @@ var "ftype")
        (hashCodeCompareExpr @@ var "otherVar" @@ var "fname")
        (comparableCompareExpr @@ var "otherVar" @@ var "fname"))

-- | Shared helper: build the compareTo method body for a list of fields
compareToBody :: TypedTermDefinition (JavaHelpers.Aliases -> String -> [FieldType] -> [Java.BlockStatement])
compareToBody = def "compareToBody" $
  lambda "aliases" $ lambda "otherVar" $ lambda "fields" $ lets [
    "zeroStmts">: list [JavaDsl.blockStatementStatement (JavaUtilsSource.javaReturnStatement @@ just (JavaUtilsSource.javaIntExpression @@ bigintAsInt (bigint 0)))]] $
    Optionals.fromOptional (var "zeroStmts") (Optionals.map
      (lambda "p" $ lets [
        "firstField">: Pairs.first (var "p"),
        "restFields">: Pairs.second (var "p")] $
        Logic.ifElse (Lists.null (var "restFields"))
          (list [JavaDsl.blockStatementStatement (JavaUtilsSource.javaReturnStatement @@ just (compareFieldExpr @@ var "otherVar" @@ var "firstField"))])
          (Lists.concat2
            (list [cmpDeclStatement @@ var "aliases"])
            (Lists.concat2
              (Lists.concat (Lists.map (lambda "f" $ compareAndReturnStmts @@ var "otherVar" @@ var "f") (Lists.cons (var "firstField") (Optionals.fromOptional (list ([] :: [TypedTerm FieldType])) (Lists.maybeInit (var "restFields"))))))
              (list [JavaDsl.blockStatementStatement (JavaUtilsSource.javaReturnStatement @@ just (compareFieldExpr @@ var "otherVar" @@ (Optionals.fromOptional (var "firstField") (Lists.maybeLast (var "restFields")))))]))))
      (Lists.uncons (var "fields")))

-- | this.field.compareTo(other.field) == 0 for BigDecimal/BigInteger fields
compareToZeroClause :: TypedTermDefinition (String -> String -> Java.InclusiveOrExpression)
compareToZeroClause = def "compareToZeroClause" $
  lambda "tmpName" $ lambda "fname" $ lets [
    "compareToArg">: JavaUtilsSource.javaExpressionNameToJavaExpression
      @@ (JavaUtilsSource.fieldExpression @@ (JavaUtilsSource.javaIdentifier @@ var "tmpName")
        @@ (JavaUtilsSource.javaIdentifier @@ var "fname")),
    "compareToVar">: JavaDsl.methodInvocationVariantExpression
      (JavaUtilsSource.fieldExpression @@ wrap Java._Identifier (string "this")
        @@ (JavaUtilsSource.javaIdentifier @@ var "fname")),
    "compareToHeader">: JavaDsl.methodInvocationHeaderComplex
      (JavaDsl.methodInvocationComplex (var "compareToVar")
        (list ([] :: [TypedTerm Java.TypeArgument]))
        (wrap Java._Identifier (asTerm JavaNamesSource.compareToMethodName))),
    "lhs">: JavaUtilsSource.javaRelationalExpressionToJavaEqualityExpression
      @@ (JavaUtilsSource.javaPostfixExpressionToJavaRelationalExpression
        @@ (JavaUtilsSource.javaMethodInvocationToJavaPostfixExpression
          @@ (JavaDsl.methodInvocation_ (var "compareToHeader") (list [var "compareToArg"])))),
    "rhs">: JavaUtilsSource.javaPostfixExpressionToJavaRelationalExpression
      @@ (JavaDsl.postfixExpressionPrimary
        (JavaUtilsSource.javaLiteralToJavaPrimary @@ (JavaUtilsSource.javaInt @@ bigintAsInt (bigint 0))))] $
    JavaUtilsSource.javaEqualityExpressionToJavaInclusiveOrExpression
      @@ (JavaDsl.equalityExpressionEqual (JavaDsl.equalityExpressionBinary (var "lhs") (var "rhs")))

-- | Create a constant field declaration (e.g., public static final Name TYPE_ = new Name("..."))
constantDecl :: TypedTermDefinition (String -> String -> JavaHelpers.Aliases -> Name -> InferenceContext -> Graph -> Either Error Java.ClassBodyDeclarationWithComments)
constantDecl = def "constantDecl" $
  lambda "comment" $ lambda "javaName" $ lambda "aliases" $ lambda "name" $
    "cx" ~> "g" ~>
    lets [
    "mods">: list [inject Java._FieldModifier Java._FieldModifier_public unit,
                   inject Java._FieldModifier Java._FieldModifier_static unit,
                   inject Java._FieldModifier Java._FieldModifier_final unit],
    "nameName">: JavaUtilsSource.nameToJavaName @@ var "aliases" @@ Core.name (string "hydra.core.Name")] $
    "env" <~ (record JavaHelpers._JavaEnvironment [
      JavaHelpers._JavaEnvironment_aliases>>: var "aliases",
      JavaHelpers._JavaEnvironment_graph>>: var "g"]) $
    "jt" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ (Core.typeVariable (Core.name (string "hydra.core.Name"))) @@ var "cx" @@ var "g") $
    "arg" <<~ (encodeTerm @@ var "env" @@ (Core.termLiteral (Core.literalString (unwrap _Name @@ var "name"))) @@ var "cx" @@ var "g") $
    "init" <~ (inject Java._VariableInitializer Java._VariableInitializer_expression
      (JavaUtilsSource.javaConstructorCall @@ (JavaUtilsSource.javaConstructorName @@ var "nameName" @@ nothing)
        @@ list [var "arg"] @@ nothing)) $
    "var" <~ (JavaUtilsSource.javaVariableDeclarator @@ wrap Java._Identifier (var "javaName") @@ just (var "init")) $
    right (withCommentString @@ var "comment" @@ (JavaUtilsSource.javaMemberField @@ var "mods" @@ var "jt" @@ var "var"))

-- | Create a constant field declaration for a field name. The parentName is the FQN of the enclosing type.
constantDeclForFieldType :: TypedTermDefinition (Name -> JavaHelpers.Aliases -> FieldType -> InferenceContext -> Graph -> Either Error Java.ClassBodyDeclarationWithComments)
constantDeclForFieldType = def "constantDeclForFieldType" $
  lambda "parentName" $ lambda "aliases" $ lambda "ftyp" $
    "cx" ~> "g" ~>
    lets [
    "name">: Core.fieldTypeName (var "ftyp"),
    "javaName">: Formatting.nonAlnumToUnderscores @@ (Formatting.convertCase @@ Util.caseConventionCamel @@ Util.caseConventionUpperSnake @@ (unwrap _Name @@ var "name")),
    "comment">: Strings.cat (list [
      string "Name of the {@code ",
      unwrap _Name @@ var "parentName",
      string ".",
      unwrap _Name @@ var "name",
      string "} field."])] $
    constantDecl @@ var "comment" @@ var "javaName" @@ var "aliases" @@ var "name" @@ var "cx" @@ var "g"

-- | Create a constant field declaration for a type name.
constantDeclForTypeName :: TypedTermDefinition (JavaHelpers.Aliases -> Name -> InferenceContext -> Graph -> Either Error Java.ClassBodyDeclarationWithComments)
constantDeclForTypeName = def "constantDeclForTypeName" $
  lambda "aliases" $ lambda "name" $
    "cx" ~> "g" ~>
    lets [
    "comment">: Strings.cat (list [
      string "Name of the {@code ",
      unwrap _Name @@ var "name",
      string "} type."])] $
    constantDecl @@ var "comment" @@ string "TYPE_" @@ var "aliases" @@ var "name" @@ var "cx" @@ var "g"

-- | Construct an elements interface for a module's data definitions
constructElementsInterface :: TypedTermDefinition (Module -> [Java.InterfaceMemberDeclarationWithComments] -> (Name, Java.CompilationUnit))
constructElementsInterface = def "constructElementsInterface" $
  lambda "mod" $ lambda "members" $ lets [
    "ns">: Packaging.moduleName (var "mod"),
    "parentNs">: namespaceParent @@ var "ns",
    "pkg">: Optionals.cases (var "parentNs")
      (JavaUtilsSource.javaPackageDeclaration @@ var "ns")
      (lambda "pns" $ JavaUtilsSource.javaPackageDeclaration @@ var "pns"),
    "mods">: list [inject Java._InterfaceModifier Java._InterfaceModifier_public unit],
    "className">: elementsClassName @@ var "ns",
    "elName">: elementsQualifiedName @@ var "ns",
    "body">: wrap Java._InterfaceBody (var "members"),
    "itf">: inject Java._TopLevelClassOrInterfaceDeclaration Java._TopLevelClassOrInterfaceDeclaration_interface
      (inject Java._InterfaceDeclaration Java._InterfaceDeclaration_normalInterface
        (record Java._NormalInterfaceDeclaration [
          Java._NormalInterfaceDeclaration_modifiers>>: var "mods",
          Java._NormalInterfaceDeclaration_identifier>>:
            JavaUtilsSource.javaTypeIdentifier @@ var "className",
          Java._NormalInterfaceDeclaration_parameters>>:
            list ([] :: [TypedTerm Java.TypeParameter]),
          Java._NormalInterfaceDeclaration_extends>>:
            list ([] :: [TypedTerm Java.InterfaceType]),
          Java._NormalInterfaceDeclaration_permits>>:
            list ([] :: [TypedTerm Java.TypeName]),
          Java._NormalInterfaceDeclaration_body>>: var "body"])),
    "decl">: record Java._TopLevelClassOrInterfaceDeclarationWithComments [
      Java._TopLevelClassOrInterfaceDeclarationWithComments_value>>: var "itf",
      Java._TopLevelClassOrInterfaceDeclarationWithComments_comments>>: (Optionals.bind (Packaging.moduleMetadata (var "mod")) ("em" ~> Packaging.entityMetadataDescription (var "em")))]] $
    pair (var "elName")
      (inject Java._CompilationUnit Java._CompilationUnit_ordinary
        (record Java._OrdinaryCompilationUnit [
          Java._OrdinaryCompilationUnit_package>>: just (var "pkg"),
          Java._OrdinaryCompilationUnit_imports>>:
            list ([] :: [TypedTerm Java.ImportDeclaration]),
          Java._OrdinaryCompilationUnit_types>>: list [var "decl"]]))

-- | Correct the cast type for pair terms. When we have a TermTypeApplication wrapping
-- a TermPair with exactly 2 type args, reconstruct the pair type from the type args
-- (which have been correctly renamed by normalizeTypeVariablesInTerm) instead of using
-- the annotation type (which may have stale variable names).
correctCastType :: TypedTermDefinition (Term -> [Type] -> Type -> InferenceContext -> Graph -> Either Error Type)
correctCastType = def "correctCastType" $
  lambda "innerBody" $ lambda "typeArgs" $ lambda "fallback" $
    "cx" ~> "g" ~>
    cases _Term (Strip.deannotateTerm @@ var "innerBody")
      (Just $ right (var "fallback")) [
      _Term_pair>>: lambda "_p" $
        Logic.ifElse (Equality.equal (Lists.length (var "typeArgs")) (int32 2))
          (right (inject _Type _Type_pair (Core.pairType
            (Optionals.fromOptional (var "fallback") (Lists.maybeAt (int32 0) (var "typeArgs")))
            (Optionals.fromOptional (var "fallback") (Lists.maybeAt (int32 1) (var "typeArgs"))))))
          (right (var "fallback"))]

-- | Compute corrected type applications for a function call.
correctTypeApps :: TypedTermDefinition (Graph -> Name -> [Term] -> [Type] -> InferenceContext -> Graph -> Either Error [Type])
correctTypeApps = def "correctTypeApps" $
  lambda "gr" $ lambda "name" $ lambda "args" $ lambda "fallbackTypeApps" $
    "cx" ~> "g" ~>
    "mel" <<~ right (Lexical.lookupBinding @@ var "g" @@ var "name") $
    Optionals.cases (var "mel")
      (right (var "fallbackTypeApps"))
      (lambda "el" $
        Optionals.cases (Core.bindingTypeScheme (var "el"))
          (right (var "fallbackTypeApps"))
          (lambda "ts" $
            "schemeType" <~ Core.typeSchemeBody (var "ts") $
            "allSchemeVars" <~ Lists.filter (lambda "v" $ isSimpleName @@ var "v") (Core.typeSchemeVariables (var "ts")) $
            "schemeTypeVars" <~ collectTypeVars @@ var "schemeType" $
            "usedFlags" <~ Lists.map (lambda "v" $ Sets.member (var "v") (var "schemeTypeVars")) (var "allSchemeVars") $
            "usedSchemeVars" <~ filterByFlags @@ var "allSchemeVars" @@ var "usedFlags" $
            "nParams" <~ countFunctionParams @@ var "schemeType" $
            "peeled" <~ peelDomainTypes @@ var "nParams" @@ var "schemeType" $
            "calleeDoms" <~ Pairs.first (var "peeled") $
            "calleeCod" <~ Pairs.second (var "peeled") $
            "overgenSubst" <~ detectAccumulatorUnification @@ var "calleeDoms" @@ var "calleeCod" @@ var "usedSchemeVars" $
            "keepFlags" <~ Lists.map
              (lambda "v" $ Logic.and
                (Sets.member (var "v") (var "schemeTypeVars"))
                (Logic.not (Maps.member (var "v") (var "overgenSubst"))))
              (var "allSchemeVars") $
            "schemeVars" <~ filterByFlags @@ var "allSchemeVars" @@ var "keepFlags" $
            "filteredFallback0" <~ Logic.ifElse
              (Equality.equal (Lists.length (var "allSchemeVars")) (Lists.length (var "fallbackTypeApps")))
              (filterByFlags @@ var "fallbackTypeApps" @@ var "keepFlags")
              (var "fallbackTypeApps") $
            "filteredFallback" <~ Logic.ifElse
              (Maps.null (var "overgenSubst"))
              (var "filteredFallback0")
              (Lists.map (lambda "t" $ substituteTypeVarsWithTypes @@ var "overgenSubst" @@ var "t") (var "filteredFallback0")) $
            Logic.ifElse
              (Logic.or (Lists.null (var "schemeVars"))
                (Logic.not (Equality.equal (Lists.length (var "schemeVars")) (Lists.length (var "filteredFallback")))))
              (right (var "filteredFallback"))
              (correctTypeAppsWithArgs @@ var "schemeVars" @@ var "filteredFallback" @@ var "schemeType" @@ var "args" @@ var "cx" @@ var "g")))

-- | Try to verify and correct IR type args using annotation-based arg types.
correctTypeAppsWithArgs :: TypedTermDefinition ([Name] -> [Type] -> Type -> [Term] -> InferenceContext -> Graph -> Either Error [Type])
correctTypeAppsWithArgs = def "correctTypeAppsWithArgs" $
  lambda "schemeVars" $ lambda "fallbackTypeApps" $ lambda "schemeType" $ lambda "args" $
    "cx" ~> "g" ~>
    "schemeVarSet" <~ Sets.fromList (var "schemeVars") $
    "irSubst" <~ Maps.fromList (Lists.zip (var "schemeVars") (var "fallbackTypeApps")) $
    "peeled" <~ peelDomainTypes @@ Lists.length (var "args") @@ var "schemeType" $
    "schemeDoms" <~ Pairs.first (var "peeled") $
    "mArgTypes" <<~ Eithers.mapList
      (lambda "arg" $
        getTypeE (var "cx") (var "g") ((Annotations.termAnnotationInternal @@ var "arg")))
      (var "args") $
    Logic.ifElse
      (Logic.not (Lists.null (Lists.filter (lambda "m" $ Optionals.isNone (var "m")) (var "mArgTypes"))))
      (right (var "fallbackTypeApps"))
      ("argTypes" <~ Lists.bind (var "mArgTypes")
        (lambda "m" $ Optionals.cases (var "m") (list ([] :: [TypedTerm Type])) (lambda "x" $ Lists.pure (var "x"))) $
      "irDoms" <~ Lists.map (lambda "d" $ applySubstSimple @@ var "irSubst" @@ var "d") (var "schemeDoms") $
      "domsMatch" <~ Lists.null (Lists.filter
        (lambda "p" $ Logic.not (typesMatch @@ (Strip.deannotateType @@ Pairs.first (var "p"))
                                              @@ (Strip.deannotateType @@ Pairs.second (var "p"))))
        (Lists.zip (var "irDoms") (var "argTypes"))) $
      Logic.ifElse (var "domsMatch")
        (right (var "fallbackTypeApps"))
        (right (resolveTypeApps @@ var "schemeVars" @@ var "fallbackTypeApps"
          @@ (buildArgSubst @@ var "schemeVarSet" @@ var "schemeDoms" @@ var "argTypes"))))

-- | Count the number of parameters in a function type by peeling domain types.
countFunctionParams :: TypedTermDefinition (Type -> Int)
countFunctionParams = def "countFunctionParams" $
  lambda "t" $
    cases _Type (Strip.deannotateType @@ var "t")
      (Just $ int32 0) [
      _Type_function>>: lambda "ft" $
        Math.add (int32 1) (countFunctionParams @@ Core.functionTypeCodomain (var "ft"))]

-- | Create a record type class declaration (without parent class).
declarationForRecordType :: TypedTermDefinition (Bool -> Bool -> JavaHelpers.Aliases -> [Java.TypeParameter] -> Name
  -> [FieldType] -> InferenceContext -> Graph -> Either Error Java.ClassDeclaration)
declarationForRecordType = def "declarationForRecordType" $
  lambda "isInner" $ lambda "isSer" $ lambda "aliases" $ lambda "tparams" $ lambda "elName" $ lambda "fields" $
    "cx" ~> "g" ~>
    declarationForRecordType' @@ var "isInner" @@ var "isSer" @@ var "aliases" @@ var "tparams" @@ var "elName" @@ nothing @@ var "fields" @@ var "cx" @@ var "g"

-- | Create a record type class declaration (with optional parent class).
declarationForRecordType' :: TypedTermDefinition (Bool -> Bool -> JavaHelpers.Aliases -> [Java.TypeParameter] -> Name -> Maybe Name -> [FieldType]
  -> InferenceContext -> Graph -> Either Error Java.ClassDeclaration)
declarationForRecordType' = def "declarationForRecordType'" $
  lambda "isInner" $ lambda "isSer" $ lambda "aliases" $ lambda "tparams" $ lambda "elName" $ lambda "parentName" $ lambda "fields" $
    "cx" ~> "g" ~>
    "memberVars" <<~ (Eithers.mapList (lambda "f" $ recordMemberVar @@ var "aliases" @@ var "f" @@ var "cx" @@ var "g") (var "fields")) $
    "memberVars'" <<~ (Eithers.mapList (lambda "p" $ addComment @@ (Pairs.first (var "p")) @@ (Pairs.second (var "p")) @@ var "cx" @@ var "g")
      (Lists.zip (var "memberVars") (var "fields"))) $
    "elNameStr" <~ (unwrap Java._Identifier @@ (JavaUtilsSource.nameToJavaName @@ var "aliases" @@ var "elName")) $
    "withMethods" <<~ (Logic.ifElse (Equality.gt (Lists.length (var "fields")) (int32 1))
      (Eithers.mapList (lambda "f" $
          "decl" <<~ (recordWithMethod @@ var "aliases" @@ var "elName" @@ var "fields" @@ var "f" @@ var "cx" @@ var "g") $
          "fname" <~ (unwrap _Name @@ Core.fieldTypeName (var "f")) $
          "comment" <~ Strings.cat (list [
            string "Returns a copy of this {@link ",
            var "elNameStr",
            string "} with {@code ",
            var "fname",
            string "} replaced."]) $
          right (withCommentString @@ var "comment" @@ var "decl"))
        (var "fields"))
      (right (list ([] :: [TypedTerm Java.ClassBodyDeclarationWithComments])))) $
    "cons" <<~ (recordConstructor @@ var "aliases" @@ var "elName" @@ var "fields" @@ var "cx" @@ var "g") $
    "paramLines" <<~ (Eithers.mapList (lambda "f" $
      "fname" <~ (unwrap _Name @@ Core.fieldTypeName (var "f")) $
      "mDoc" <<~ (Annotations.commentsFromFieldType @@ var "cx" @@ var "g" @@ var "f") $
      right (Optionals.cases (var "mDoc") (string "") (lambda "d" $ Strings.cat (list [
          string "@param ",
          var "fname",
          string " ",
          var "d"]))))
      (var "fields")) $
    "nonEmptyParamLines" <~ Lists.filter
      (lambda "l" $ Logic.not (Equality.equal (var "l") (string "")))
      (var "paramLines") $
    "consBaseComment" <~ Strings.cat (list [
      string "Constructs an immutable {@link ",
      var "elNameStr",
      string "}."]) $
    "consComment" <~ Logic.ifElse (Lists.null (var "nonEmptyParamLines"))
      (var "consBaseComment")
      (Strings.cat (list [
        var "consBaseComment",
        string "\n\n",
        Strings.intercalate (string "\n") (var "nonEmptyParamLines")])) $
    "consWithComment" <~ (withCommentString @@ var "consComment" @@ var "cons") $
    "tn" <<~ (Logic.ifElse (var "isInner")
      (right (list ([] :: [TypedTerm Java.ClassBodyDeclarationWithComments])))
      ("d" <<~ (constantDeclForTypeName @@ var "aliases" @@ var "elName" @@ var "cx" @@ var "g") $
        "dfields" <<~ (Eithers.mapList (lambda "f" $ constantDeclForFieldType @@ var "elName" @@ var "aliases" @@ var "f" @@ var "cx" @@ var "g") (var "fields")) $
        right (Lists.cons (var "d") (var "dfields")))) $
    "comparableMethods" <~ (Optionals.cases (var "parentName")
      (Logic.ifElse (Logic.and (Logic.not (var "isInner")) (var "isSer"))
        (list [recordCompareToMethod @@ var "aliases" @@ var "tparams" @@ var "elName" @@ var "fields"])
        (list ([] :: [TypedTerm Java.ClassBodyDeclaration])))
      (lambda "pn" $ Logic.ifElse (var "isSer")
        (list [variantCompareToMethod @@ var "aliases" @@ var "tparams" @@ var "pn" @@ var "elName" @@ var "fields"])
        (list ([] :: [TypedTerm Java.ClassBodyDeclaration])))) $
    "noCommentMethods" <~ Lists.map (lambda "x" $ noComment @@ var "x")
      (Lists.concat2
        (list [recordEqualsMethod @@ var "aliases" @@ var "elName" @@ var "fields",
               recordHashCodeMethod @@ var "fields"])
        (var "comparableMethods")) $
    "bodyDecls" <~ (Lists.concat (list [
      var "tn",
      var "memberVars'",
      list [var "consWithComment"],
      var "noCommentMethods",
      var "withMethods"])) $
    "ifaces" <~ (Logic.ifElse (var "isInner")
      (serializableTypes @@ var "isSer")
      (interfaceTypes @@ var "isSer" @@ var "aliases" @@ var "tparams" @@ var "elName")) $
    right (JavaUtilsSource.javaClassDeclaration @@ var "aliases" @@ var "tparams" @@ var "elName"
      @@ asTerm classModsPublic @@ nothing @@ var "ifaces" @@ var "bodyDecls")

-- | Generate class declaration for a union type.
declarationForUnionType :: TypedTermDefinition (Bool -> JavaHelpers.Aliases -> [Java.TypeParameter] -> Name -> [FieldType] -> InferenceContext -> Graph -> Either Error Java.ClassDeclaration)
declarationForUnionType = def "declarationForUnionType" $
  lambda "isSer" $ lambda "aliases" $ lambda "tparams" $ lambda "elName" $ lambda "fields" $
    "cx" ~> "g" ~>
    -- Generate variant subclasses
    "variantClasses" <<~ (Eithers.mapList (lambda "ft" $
      "fname" <~ (project _FieldType _FieldType_name @@ var "ft") $
      "ftype" <~ (project _FieldType _FieldType_type @@ var "ft") $
      "rfields" <~ Logic.ifElse (Predicates.isUnitType @@ (Strip.deannotateType @@ var "ftype"))
        (list ([] :: [TypedTerm FieldType]))
        (list [Core.fieldType (wrap _Name (string "value")) (Strip.deannotateType @@ var "ftype")]) $
      "varName" <~ (JavaUtilsSource.variantClassName @@ false @@ var "elName" @@ var "fname") $
      "innerDecl" <<~ (declarationForRecordType' @@ true @@ var "isSer" @@ var "aliases" @@ (list ([] :: [TypedTerm Java.TypeParameter]))
        @@ var "varName" @@ (Logic.ifElse (var "isSer") (just (var "elName")) nothing) @@ var "rfields" @@ var "cx" @@ var "g") $
      right (augmentVariantClass @@ var "aliases" @@ var "tparams" @@ var "elName" @@ var "innerDecl"))
      (var "fields")) $
    -- Wrap variant classes as class body declarations and add comments
    "variantDecls" <~ Lists.map
      (lambda "vc" $ inject Java._ClassBodyDeclaration Java._ClassBodyDeclaration_classMember
        (inject Java._ClassMemberDeclaration Java._ClassMemberDeclaration_class (var "vc")))
      (var "variantClasses") $
    "variantDecls'" <<~ (Eithers.mapList
      (lambda "pair" $ addComment @@ (Pairs.first (var "pair")) @@ (Pairs.second (var "pair")) @@ var "cx" @@ var "g")
      (Lists.zip (var "variantDecls") (var "fields"))) $
    -- Build other declarations
    "privateConst" <~ (JavaUtilsSource.makeConstructor @@ var "aliases" @@ var "elName" @@ true
      @@ list ([] :: [TypedTerm Java.FormalParameter]) @@ list ([] :: [TypedTerm Java.BlockStatement])) $
    "acceptDecl" <~ (JavaUtilsSource.toAcceptMethod @@ true @@ var "tparams") $
    -- Build visitor and partial visitor interfaces
    "vtparams" <~ Lists.concat2 (var "tparams") (list [JavaUtilsSource.javaTypeParameter @@ asTerm JavaNamesSource.visitorReturnParameter]) $
    "elNameStr" <~ (unwrap Java._Identifier @@ (JavaUtilsSource.nameToJavaName @@ var "aliases" @@ var "elName")) $
    "visitorMethods" <~ Lists.map
      (lambda "ft" $
        "fname" <~ (project _FieldType _FieldType_name @@ var "ft") $
        "fnameStr" <~ (unwrap _Name @@ var "fname") $
        "typeArgs" <~ Lists.map (lambda "tp" $ JavaUtilsSource.typeParameterToTypeArgument @@ var "tp") (var "tparams") $
        "varName" <~ (JavaUtilsSource.variantClassName @@ false @@ var "elName" @@ var "fname") $
        "varNameStr" <~ (unwrap Java._Identifier @@ (JavaUtilsSource.nameToJavaName @@ var "aliases" @@ var "varName")) $
        "varRef" <~ (JavaUtilsSource.javaClassTypeToJavaType @@
          (JavaUtilsSource.nameToJavaClassType @@ var "aliases" @@ false @@ var "typeArgs"
            @@ var "varName" @@ nothing)) $
        "param" <~ (JavaUtilsSource.javaTypeToJavaFormalParameter @@ var "varRef" @@ wrap _Name (string "instance")) $
        "resultR" <~ (JavaUtilsSource.javaTypeToJavaResult @@ (JavaDsl.typeReference (asTerm JavaUtilsSource.visitorTypeVariable))) $
        "comment" <~ Strings.cat (list [
          string "Visit the {@link ",
          var "varNameStr",
          string "} case."]) $
        pair (var "comment")
          (JavaUtilsSource.interfaceMethodDeclaration @@ list ([] :: [TypedTerm Java.InterfaceMethodModifier]) @@ list ([] :: [TypedTerm Java.TypeParameter])
            @@ asTerm JavaNamesSource.visitMethodName @@ list [var "param"] @@ var "resultR" @@ nothing))
      (var "fields") $
    "visitorBody" <~ wrap Java._InterfaceBody (Lists.map (lambda "p" $
      withInterfaceCommentString @@ (Pairs.first (var "p")) @@ (Pairs.second (var "p"))) (var "visitorMethods")) $
    "visitor" <~ (JavaUtilsSource.javaInterfaceDeclarationToJavaClassBodyDeclaration @@
      (record Java._NormalInterfaceDeclaration [
        Java._NormalInterfaceDeclaration_modifiers>>: list [inject Java._InterfaceModifier Java._InterfaceModifier_public unit],
        Java._NormalInterfaceDeclaration_identifier>>: wrap Java._TypeIdentifier (JavaDsl.identifier (asTerm JavaNamesSource.visitorName)),
        Java._NormalInterfaceDeclaration_parameters>>: var "vtparams",
        Java._NormalInterfaceDeclaration_extends>>: list ([] :: [TypedTerm Java.InterfaceType]),
        Java._NormalInterfaceDeclaration_permits>>: list ([] :: [TypedTerm Java.TypeName]),
        Java._NormalInterfaceDeclaration_body>>: var "visitorBody"])) $
    -- Partial visitor: extends Visitor, has default otherwise() and override visit methods
    "typeArgs" <~ Lists.map (lambda "tp" $ JavaUtilsSource.typeParameterToTypeArgument @@ var "tp") (var "tparams") $
    "visitorClassType" <~ (JavaUtilsSource.javaClassType @@
      (Lists.concat2 (Lists.map (lambda "tp" $ JavaUtilsSource.typeParameterToReferenceType @@ var "tp") (var "tparams"))
        (list [asTerm JavaUtilsSource.visitorTypeVariable]))
      @@ nothing @@ asTerm JavaNamesSource.visitorName) $
    -- otherwise method: throws IllegalStateException
    "mainInstanceParam" <~ (JavaUtilsSource.javaTypeToJavaFormalParameter @@
      (JavaUtilsSource.javaClassTypeToJavaType @@
        (JavaUtilsSource.nameToJavaClassType @@ var "aliases" @@ false @@ var "typeArgs" @@ var "elName" @@ nothing))
      @@ wrap _Name (string "instance")) $
    "resultR" <~ (JavaUtilsSource.javaTypeToJavaResult @@ (JavaDsl.typeReference (asTerm JavaUtilsSource.visitorTypeVariable))) $
    "throwStmt" <~ (JavaDsl.blockStatementStatement (JavaUtilsSource.javaThrowIllegalStateException @@
      list [JavaUtilsSource.javaAdditiveExpressionToJavaExpression @@ (JavaUtilsSource.addExpressions @@ list [
        JavaUtilsSource.javaStringMultiplicativeExpression @@ string "Non-exhaustive patterns when matching: ",
        inject Java._MultiplicativeExpression Java._MultiplicativeExpression_unary
          (JavaUtilsSource.javaIdentifierToJavaUnaryExpression @@ (JavaDsl.identifier (string "instance")))])])) $
    "defaultMod" <~ list [inject Java._InterfaceMethodModifier Java._InterfaceMethodModifier_default unit] $
    "otherwiseDecl" <~ (JavaUtilsSource.interfaceMethodDeclaration @@ var "defaultMod" @@ list ([] :: [TypedTerm Java.TypeParameter])
      @@ asTerm JavaNamesSource.otherwiseMethodName @@ list [var "mainInstanceParam"] @@ var "resultR"
      @@ just (list [var "throwStmt"])) $
    "otherwiseComment" <~ string "Default branch for unhandled cases." $
    -- Partial visitor visit methods: default to calling otherwise()
    "pvVisitMethods" <~ Lists.map
      (lambda "ft" $
        "fname" <~ (project _FieldType _FieldType_name @@ var "ft") $
        "varName" <~ (JavaUtilsSource.variantClassName @@ false @@ var "elName" @@ var "fname") $
        "varNameStr" <~ (unwrap Java._Identifier @@ (JavaUtilsSource.nameToJavaName @@ var "aliases" @@ var "varName")) $
        "varRef" <~ (JavaUtilsSource.javaClassTypeToJavaType @@
          (JavaUtilsSource.nameToJavaClassType @@ var "aliases" @@ false @@ var "typeArgs"
            @@ var "varName" @@ nothing)) $
        "param" <~ (JavaUtilsSource.javaTypeToJavaFormalParameter @@ var "varRef" @@ wrap _Name (string "instance")) $
        "mi" <~ (JavaUtilsSource.methodInvocation @@ nothing
              @@ JavaDsl.identifier (asTerm JavaNamesSource.otherwiseMethodName)
              @@ list [JavaUtilsSource.javaIdentifierToJavaExpression @@ (JavaDsl.identifier (string "instance"))]) $
        "returnOtherwise" <~ (JavaDsl.blockStatementStatement (JavaUtilsSource.javaReturnStatement @@ just
          (JavaUtilsSource.javaPrimaryToJavaExpression @@
            (JavaUtilsSource.javaMethodInvocationToJavaPrimary @@ var "mi")))) $
        "comment" <~ Strings.cat (list [
          string "Visit the {@link ",
          var "varNameStr",
          string "} case."]) $
        pair (var "comment")
          (JavaUtilsSource.interfaceMethodDeclaration @@ var "defaultMod" @@ list ([] :: [TypedTerm Java.TypeParameter])
            @@ asTerm JavaNamesSource.visitMethodName @@ list [var "param"] @@ var "resultR"
            @@ just (list [var "returnOtherwise"])))
      (var "fields") $
    "pvBody" <~ wrap Java._InterfaceBody (Lists.cons
      (withInterfaceCommentString @@ var "otherwiseComment" @@ var "otherwiseDecl")
      (Lists.map (lambda "p" $
        withInterfaceCommentString @@ (Pairs.first (var "p")) @@ (Pairs.second (var "p"))) (var "pvVisitMethods"))) $
    "partialVisitor" <~ (JavaUtilsSource.javaInterfaceDeclarationToJavaClassBodyDeclaration @@
      (record Java._NormalInterfaceDeclaration [
        Java._NormalInterfaceDeclaration_modifiers>>: list [inject Java._InterfaceModifier Java._InterfaceModifier_public unit],
        Java._NormalInterfaceDeclaration_identifier>>: wrap Java._TypeIdentifier (JavaDsl.identifier (asTerm JavaNamesSource.partialVisitorName)),
        Java._NormalInterfaceDeclaration_parameters>>: var "vtparams",
        Java._NormalInterfaceDeclaration_extends>>: list [wrap Java._InterfaceType (var "visitorClassType")],
        Java._NormalInterfaceDeclaration_permits>>: list ([] :: [TypedTerm Java.TypeName]),
        Java._NormalInterfaceDeclaration_body>>: var "pvBody"])) $
    -- Build constant declarations
    "tn0" <<~ (constantDeclForTypeName @@ var "aliases" @@ var "elName" @@ var "cx" @@ var "g") $
    "tn1" <<~ (Eithers.mapList (lambda "ft" $ constantDeclForFieldType @@ var "elName" @@ var "aliases" @@ var "ft" @@ var "cx" @@ var "g") (var "fields")) $
    "tn" <~ list [var "tn0"] `Lists.concat2` var "tn1" $
    "privateConstComment" <~ Strings.cat (list [
      string "Constructs an immutable {@link ",
      var "elNameStr",
      string "}."]) $
    "acceptComment" <~ string "Dispatch to {@code visitor}." $
    "visitorIfaceComment" <~ Strings.cat (list [
      string "Visitor over {@link ",
      var "elNameStr",
      string "}."]) $
    "partialVisitorIfaceComment" <~ Strings.cat (list [
      string "Partial visitor over {@link ",
      var "elNameStr",
      string "} with a default {@link #otherwise} branch."]) $
    "otherDecls" <~ list [
      withCommentString @@ var "privateConstComment" @@ var "privateConst",
      withCommentString @@ var "acceptComment" @@ var "acceptDecl",
      withCommentString @@ var "visitorIfaceComment" @@ var "visitor",
      withCommentString @@ var "partialVisitorIfaceComment" @@ var "partialVisitor"] $
    "bodyDecls" <~ Lists.concat (list [var "tn", var "otherDecls", var "variantDecls'"]) $
    "mods" <~ Lists.concat2 (asTerm classModsPublic) (list [inject Java._ClassModifier Java._ClassModifier_abstract unit]) $
    right (JavaUtilsSource.javaClassDeclaration @@ var "aliases" @@ var "tparams" @@ var "elName" @@ var "mods"
      @@ nothing @@ (interfaceTypes @@ var "isSer" @@ var "aliases" @@ var "tparams" @@ var "elName") @@ var "bodyDecls")

-- | Decode a Type from its term encoding (limited subset).
decodeTypeFromTerm :: TypedTermDefinition (Term -> Maybe Type)
decodeTypeFromTerm = def "decodeTypeFromTerm" $
  lambda "term" $
    cases _Term (Strip.deannotateTerm @@ var "term")
      (Just nothing) [
      _Term_inject>>: lambda "inj" $
        Logic.ifElse
          (Equality.equal (Core.injectionTypeName (var "inj")) (Core.name (string "hydra.core.Type")))
          ("fname" <~ Core.unName (Core.fieldName (Core.injectionField (var "inj"))) $
           "fterm" <~ Core.fieldTerm (Core.injectionField (var "inj")) $
           Logic.ifElse
             (Equality.equal (var "fname") (string "variable"))
             (cases _Term (var "fterm")
               (Just nothing) [
               _Term_wrap>>: lambda "wt" $
                 cases _Term (Core.wrappedTermBody (var "wt"))
                   (Just nothing) [
                   _Term_literal>>: lambda "lit" $
                     cases _Literal (var "lit")
                       (Just nothing) [
                       _Literal_string>>: lambda "s" $
                         just (Core.typeVariable (Core.name (var "s")))]]])
             (Logic.ifElse
               (Equality.equal (var "fname") (string "annotated"))
               (cases _Term (var "fterm")
                 (Just nothing) [
                 _Term_record>>: lambda "rec" $
                   Optionals.bind
                     (Lists.maybeHead (Lists.filter
                       (lambda "f" $ Equality.equal (Core.fieldName (var "f")) (Core.name (string "body")))
                       (Core.recordFields (var "rec"))))
                     (lambda "bodyField" $
                       decodeTypeFromTerm @@ Core.fieldTerm (var "bodyField"))])
               (Logic.ifElse
                 (Equality.equal (var "fname") (string "application"))
                 (cases _Term (var "fterm")
                   (Just nothing) [
                   _Term_record>>: lambda "rec" $
                     Optionals.bind
                       (Lists.maybeHead (Lists.filter
                         (lambda "f" $ Equality.equal (Core.fieldName (var "f")) (Core.name (string "function")))
                         (Core.recordFields (var "rec"))))
                       (lambda "funcField" $
                         Optionals.bind (decodeTypeFromTerm @@ Core.fieldTerm (var "funcField")) (lambda "func" $
                           Optionals.bind
                             (Lists.maybeHead (Lists.filter
                               (lambda "f" $ Equality.equal (Core.fieldName (var "f")) (Core.name (string "argument")))
                               (Core.recordFields (var "rec"))))
                             (lambda "argField" $
                               Optionals.map
                                 (lambda "arg" $ Core.typeApplication (Core.applicationType (var "func") (var "arg")))
                                 (decodeTypeFromTerm @@ Core.fieldTerm (var "argField")))))])
                 (Logic.ifElse
                   (Equality.equal (var "fname") (string "function"))
                   (cases _Term (var "fterm")
                     (Just nothing) [
                     _Term_record>>: lambda "rec" $
                       Optionals.bind
                         (Lists.maybeHead (Lists.filter
                           (lambda "f" $ Equality.equal (Core.fieldName (var "f")) (Core.name (string "domain")))
                           (Core.recordFields (var "rec"))))
                         (lambda "domField" $
                           Optionals.bind (decodeTypeFromTerm @@ Core.fieldTerm (var "domField")) (lambda "dom" $
                             Optionals.bind
                               (Lists.maybeHead (Lists.filter
                                 (lambda "f" $ Equality.equal (Core.fieldName (var "f")) (Core.name (string "codomain")))
                                 (Core.recordFields (var "rec"))))
                               (lambda "codField" $
                                 Optionals.map
                                   (lambda "cod" $ Core.typeFunction (Core.functionType (var "dom") (var "cod")))
                                   (decodeTypeFromTerm @@ Core.fieldTerm (var "codField")))))])
                   (Logic.ifElse
                     (Equality.equal (var "fname") (string "literal"))
                     (cases _Term (var "fterm")
                       (Just nothing) [
                       _Term_inject>>: lambda "litInj" $
                         Logic.ifElse
                           (Equality.equal (Core.unName (Core.fieldName (Core.injectionField (var "litInj")))) (string "string"))
                           (just (Core.typeLiteral (inject _LiteralType _LiteralType_string unit)))
                           nothing])
                     nothing)))))
          nothing]

-- | Deduplicate binding names that collide with in-scope variables.
dedupBindings :: TypedTermDefinition (S.Set Name -> [Binding] -> [Binding])
dedupBindings = def "dedupBindings" $
  lambda "inScope" $ lambda "bs" $
    Optionals.fromOptional (list ([] :: [TypedTerm Binding])) (Optionals.map
      (lambda "p" $
        "b" <~ Pairs.first (var "p") $
        "rest" <~ Pairs.second (var "p") $
        "name" <~ Core.bindingName (var "b") $
       Logic.ifElse
         (Sets.member (var "name") (var "inScope"))
         ("newName" <~ (freshJavaName @@ var "name" @@ var "inScope") $
          "subst" <~ Maps.singleton (var "name") (var "newName") $
          "rest2" <~ Lists.map
            (lambda "b2" $ Core.binding
              (Core.bindingName (var "b2"))
              (Variables.substituteVariables @@ var "subst" @@ Core.bindingTerm (var "b2"))
              (Core.bindingTypeScheme (var "b2")))
            (var "rest") $
          Lists.cons
            (Core.binding (var "newName") (Core.bindingTerm (var "b")) (Core.bindingTypeScheme (var "b")))
            (dedupBindings @@ Sets.insert (var "newName") (var "inScope") @@ var "rest2"))
         (Lists.cons (var "b")
           (dedupBindings @@ Sets.insert (var "name") (var "inScope") @@ var "rest")))
      (Lists.uncons (var "bs")))

def :: String -> TypedTerm a -> TypedTermDefinition a
def = definitionInModule module_

-- | Detect over-generalized type variables in a scheme type.
detectAccumulatorUnification :: TypedTermDefinition ([Type] -> Type -> [Name] -> M.Map Name Type)
detectAccumulatorUnification = def "detectAccumulatorUnification" $
  lambda "doms" $ lambda "cod" $ lambda "tparams" $
    "tparamSet" <~ Sets.fromList (var "tparams") $
    "allPairs" <~ Lists.bind (var "doms") (lambda "d" $ extractInOutPair @@ var "d") $
    "groupedByInput" <~ (groupPairsByFirst @@ var "allPairs") $
    "selfRefSubst" <~ (selfRefSubstitution @@ var "groupedByInput") $
    "directPairs" <~ Lists.bind (var "doms") (lambda "d" $ extractDirectReturn @@ var "tparamSet" @@ var "d") $
    "groupedDirect" <~ (groupPairsByFirst @@ var "directPairs") $
    "directInputVars" <~ Sets.fromList (Lists.map (lambda "p" $ Pairs.first (var "p")) (var "directPairs")) $
    "codVar" <~ (cases _Type (Strip.deannotateType @@ var "cod")
      (Just nothing) [
      _Type_variable>>: lambda "v" $ just (var "v")]) $
    "directRefSubst" <~ (directRefSubstitution @@ var "directInputVars" @@ var "codVar" @@ var "groupedDirect") $
    "codSubst" <~ (Optionals.cases (findPairFirst @@ var "cod") (Maps.empty) (lambda "cv" $
        Logic.ifElse
          (Maps.member (var "cv") (var "selfRefSubst"))
          (Maps.empty)
          (Optionals.cases (findSelfRefVar @@ var "groupedByInput") (Maps.empty) (lambda "refVar" $
              Logic.ifElse
                (Equality.equal (var "cv") (var "refVar"))
                (Maps.empty)
                (Maps.singleton (var "cv") (var "refVar")))))) $
    "domVars" <~ Sets.fromList (Lists.bind (var "doms") (lambda "d" $ Sets.toList (collectTypeVars @@ var "d"))) $
    "danglingSubst" <~ (Optionals.cases (findPairFirst @@ var "cod") (Maps.empty) (lambda "cv" $
        Logic.ifElse
          (Sets.member (var "cv") (var "domVars"))
          (Maps.empty)
          (Optionals.cases (findSelfRefVar @@ var "groupedByInput") (Maps.empty) (lambda "refVar" $ Maps.singleton (var "cv") (Core.typeVariable (var "refVar")))))) $
    Maps.union (Maps.union (Maps.union
      (nameMapToTypeMap @@ var "selfRefSubst")
      (nameMapToTypeMap @@ var "codSubst"))
      (var "danglingSubst"))
      (nameMapToTypeMap @@ var "directRefSubst")

-- | Direct-return substitution: for each input var with >=2 self-refs and
-- safe non-self vars, substitute those vars to the input var.
directRefSubstitution :: TypedTermDefinition (S.Set Name -> Maybe Name -> M.Map Name [Name] -> M.Map Name Name)
directRefSubstitution = def "directRefSubstitution" $
  lambda "directInputVars" $ lambda "codVar" $ lambda "grouped" $
    Lists.foldl
      (lambda "subst" $ lambda "entry" $
        directRefSubstitution_processGroup
          @@ var "directInputVars" @@ var "codVar"
          @@ var "subst" @@ Pairs.first (var "entry") @@ Pairs.second (var "entry"))
      (Maps.empty)
      (Maps.toList (var "grouped"))

directRefSubstitution_processGroup :: TypedTermDefinition (S.Set Name -> Maybe Name -> M.Map Name Name -> Name -> [Name] -> M.Map Name Name)
directRefSubstitution_processGroup = def "directRefSubstitution_processGroup" $
  lambda "directInputVars" $ lambda "codVar" $ lambda "subst" $ lambda "inVar" $ lambda "outVars" $
    "selfRefCount" <~ Lists.length (Lists.filter (lambda "v" $ Equality.equal (var "v") (var "inVar")) (var "outVars")) $
    "nonSelfVars" <~ Lists.filter (lambda "v" $ Logic.not (Equality.equal (var "v") (var "inVar"))) (var "outVars") $
    "safeNonSelfVars" <~ Lists.filter
      (lambda "v" $ Logic.and
        (Logic.not (Sets.member (var "v") (var "directInputVars")))
        (Logic.not (Equality.equal (just (var "v")) (var "codVar"))))
      (var "nonSelfVars") $
    Logic.ifElse
      (Logic.and
        (Equality.gte (var "selfRefCount") (int32 2))
        (Logic.not (Lists.null (var "safeNonSelfVars"))))
      (Lists.foldl
        (lambda "s" $ lambda "v" $ Maps.insert (var "v") (var "inVar") (var "s"))
        (var "subst")
        (var "safeNonSelfVars"))
      (var "subst")

-- | Extract Java type arguments from a domain type.
-- Uses actual type application args when available, falling back to javaTypeArgumentsForType.
domTypeArgs :: TypedTermDefinition (JavaHelpers.Aliases -> Type -> InferenceContext -> Graph -> Either Error [Java.TypeArgument])
domTypeArgs = def "domTypeArgs" $
  lambda "aliases" $ lambda "d" $
    "cx" ~> "g" ~>
    "args" <~ (extractTypeApplicationArgs @@ (Strip.deannotateType @@ var "d")) $
    Logic.ifElse (Logic.not (Lists.null (var "args")))
      (Eithers.mapList (lambda "t" $
        "jt" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "t" @@ var "cx" @@ var "g") $
        "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt" @@ var "cx") $
        right (JavaDsl.typeArgumentReference (var "rt")))
        (var "args"))
      (right (javaTypeArgumentsForType @@ var "d"))

-- | Generate a Java identifier for a data element (variable, constant, function, etc.)
elementJavaIdentifier :: TypedTermDefinition (Bool -> Bool -> JavaHelpers.Aliases -> Name -> Java.Identifier)
elementJavaIdentifier = def "elementJavaIdentifier" $
  lambda "isPrim" $ lambda "isMethod" $ lambda "aliases" $ lambda "name" $ lets [
    "qn">: Names.qualifyName @@ var "name",
    "ns_">: Util.qualifiedNameModuleName (var "qn"),
    "local">: Util.qualifiedNameLocal (var "qn"),
    "sep">: Logic.ifElse (var "isMethod") (string "::") (string ".")] $
    Logic.ifElse (var "isPrim")
      (wrap Java._Identifier (Strings.cat2
        (Strings.cat2
          (elementJavaIdentifier_qualify @@ var "aliases" @@ var "ns_"
            @@ (Formatting.capitalize @@ var "local"))
          (string "."))
        (asTerm JavaNamesSource.applyMethodName)))
      (Optionals.cases (var "ns_")
        (wrap Java._Identifier (JavaUtilsSource.sanitizeJavaName @@ var "local"))
        (lambda "n" $ wrap Java._Identifier (Strings.cat2
          (Strings.cat2
            (elementJavaIdentifier_qualify @@ var "aliases" @@ (namespaceParent @@ var "n")
              @@ (elementsClassName @@ var "n"))
            (var "sep"))
          (JavaUtilsSource.sanitizeJavaName @@ var "local"))))

-- | Helper for elementJavaIdentifier: qualify a name through the aliases
elementJavaIdentifier_qualify :: TypedTermDefinition (JavaHelpers.Aliases -> Maybe ModuleName -> String -> String)
elementJavaIdentifier_qualify = def "elementJavaIdentifier_qualify" $
  lambda "aliases" $ lambda "mns" $ lambda "s" $
    unwrap Java._Identifier @@ (JavaUtilsSource.nameToJavaName @@ var "aliases"
      @@ (Names.unqualifyName @@ Util.qualifiedName (var "mns") (var "s")))

-- | Convert a namespace to an elements class name (e.g., "hydra.java.syntax" -> "Syntax")
elementsClassName :: TypedTermDefinition (ModuleName -> String)
elementsClassName = def "elementsClassName" $
  lambda "ns" $ lets [
    "nsStr">: unwrap _ModuleName @@ var "ns",
    "parts">: Strings.splitOn (string ".") (var "nsStr")] $
    Formatting.sanitizeWithUnderscores @@ JavaLanguageSource.reservedWords
      @@ (Formatting.capitalize @@ (Optionals.fromOptional (var "nsStr") (Lists.maybeLast (var "parts"))))

-- | Produce the qualified name for a term module's elements interface.
-- Uses the parent namespace so that e.g. "hydra.formatting" -> "hydra.Formatting" (not "hydra.formatting.Formatting").
elementsQualifiedName :: TypedTermDefinition (ModuleName -> Name)
elementsQualifiedName = def "elementsQualifiedName" $
  lambda "ns" $
    Names.unqualifyName @@ Util.qualifiedName (namespaceParent @@ var "ns") (elementsClassName @@ var "ns")

-- | Encode a function application.
encodeApplication :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> Application -> InferenceContext -> Graph -> Either Error Java.Expression)
encodeApplication = def "encodeApplication" $
  lambda "env" $ lambda "app" $
    "cx" ~> "g0" ~>
    "aliases" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env") $
    "g" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_graph @@ var "env") $
    -- Gather function, args, and type applications
    "gathered" <~ (Analysis.gatherArgsWithTypeApps
      @@ (inject _Term _Term_application (var "app"))
      @@ list ([] :: [TypedTerm Term])
      @@ list ([] :: [TypedTerm Type])) $
    "fun" <~ Pairs.first (var "gathered") $
    "args" <~ Pairs.first (Pairs.second (var "gathered")) $
    "typeApps" <~ Pairs.second (Pairs.second (var "gathered")) $
    -- Get the function's arity from its type
    "mfunTyp" <<~ (getTypeE (var "cx") (var "g") (Annotations.termAnnotationInternal @@ var "fun")) $
    "funTyp" <<~ (Optionals.cases (var "mfunTyp")
      (Checking.typeOfTerm @@ var "cx" @@ var "g" @@ var "fun")
      (lambda "t" $ right (var "t"))) $
    "arity" <~ (Arity.typeArity @@ var "funTyp") $
    -- Determine callee name for type annotation correction
    "deannotatedFun" <~ (Strip.deannotateTerm @@ var "fun") $
    "calleeName" <~ (cases _Term (var "deannotatedFun")
      (Just nothing) [
      _Term_variable>>: lambda "n" $ just (var "n")]) $
    -- Annotate lambda args if we have a callee name
    "annotatedArgs" <<~ (Optionals.cases (var "calleeName")
      (right (var "args"))
      (lambda "cname" $ annotateLambdaArgs @@ var "cname" @@ var "typeApps" @@ var "args" @@ var "cx" @@ var "g")) $
    -- Dispatch based on the deannotated function form
    cases _Term (var "deannotatedFun")
      (Just $ encodeApplication_fallback @@ var "env" @@ var "aliases" @@ var "g" @@ var "typeApps"
        @@ (Core.applicationFunction (var "app")) @@ (Core.applicationArgument (var "app")) @@ var "cx" @@ var "g") [
      _Term_variable>>: lambda "name" $
        -- If the variable resolves to a primitive, handle it like FunctionPrimitive
        Logic.ifElse (Optionals.isGiven (Maps.lookup (var "name") (Graph.graphPrimitives (var "g"))))
          ("hargs" <~ Lists.take (var "arity") (var "annotatedArgs") $
           "rargs" <~ Lists.drop (var "arity") (var "annotatedArgs") $
           "initialCall" <<~ (functionCall @@ var "env" @@ true @@ var "name" @@ var "hargs" @@ (list ([] :: [TypedTerm Type])) @@ var "cx" @@ var "g") $
           Eithers.foldl (lambda "acc" $ lambda "h" $
             "jarg" <<~ (encodeTerm @@ var "env" @@ var "h" @@ var "cx" @@ var "g") $
             right (applyJavaArg @@ var "acc" @@ var "jarg"))
             (var "initialCall") (var "rargs"))
        -- Check if this is a recursive let-bound variable (not shadowed by lambda parameter)
        (Logic.ifElse (Logic.and (isRecursiveVariable @@ var "aliases" @@ var "name")
            (Logic.not (isLambdaBoundIn @@ var "name"
              @@ (project JavaHelpers._Aliases JavaHelpers._Aliases_lambdaVars @@ var "aliases"))))
          -- Use curried construction for recursive bindings
          (encodeApplication_fallback @@ var "env" @@ var "aliases" @@ var "g" @@ var "typeApps"
            @@ (Core.applicationFunction (var "app")) @@ (Core.applicationArgument (var "app")) @@ var "cx" @@ var "g")
          -- Normal variable application
          ("symClass" <<~ (classifyDataReference @@ var "name" @@ var "cx" @@ var "g") $
            "methodArity" <~ (cases JavaHelpers._JavaSymbolClass (var "symClass")
              (Just $ var "arity") [
              JavaHelpers._JavaSymbolClass_hoistedLambda>>: lambda "n" $ var "n"]) $
            "hargs" <~ Lists.take (var "methodArity") (var "annotatedArgs") $
            "rargs" <~ Lists.drop (var "methodArity") (var "annotatedArgs") $
            -- Filter type applications: drop all type args if any references a type variable not in scope
            "trusted" <~ (project JavaHelpers._Aliases JavaHelpers._Aliases_trustedTypeVars @@ var "aliases") $
            "inScope" <~ (project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeTypeParams @@ var "aliases") $
            "filteredTypeApps" <~ (Logic.ifElse (Logic.or (Sets.null (var "trusted")) (Sets.null (var "inScope")))
              (list ([] :: [TypedTerm Type]))
              ("allVars" <~ Sets.unions (Lists.map (lambda "t" $ collectTypeVars @@ var "t") (var "typeApps")) $
                Logic.ifElse (Logic.not (Sets.null (Sets.difference (var "allVars") (var "inScope"))))
                  (list ([] :: [TypedTerm Type]))
                  (Logic.ifElse (Sets.null (Sets.difference (var "allVars") (var "trusted")))
                    (var "typeApps")
                    (list ([] :: [TypedTerm Type]))))) $
            -- Correct the type application ordering
            "safeTypeApps" <<~ (Logic.ifElse (Lists.null (var "filteredTypeApps"))
              (right (list ([] :: [TypedTerm Type])))
              (correctTypeApps @@ var "g" @@ var "name" @@ var "hargs" @@ var "filteredTypeApps" @@ var "cx" @@ var "g")) $
            -- Filter phantom type args
            "finalTypeApps" <<~ (filterPhantomTypeArgs @@ var "name" @@ var "safeTypeApps" @@ var "cx" @@ var "g") $
            "initialCall" <<~ (functionCall @@ var "env" @@ false @@ var "name" @@ var "hargs" @@ var "finalTypeApps" @@ var "cx" @@ var "g") $
            Eithers.foldl (lambda "acc" $ lambda "h" $
              "jarg" <<~ (encodeTerm @@ var "env" @@ var "h" @@ var "cx" @@ var "g") $
              right (applyJavaArg @@ var "acc" @@ var "jarg"))
              (var "initialCall") (var "rargs")))]

-- | Fallback path for encodeApplication — used for eliminations and default expressions.
encodeApplication_fallback :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> JavaHelpers.Aliases -> Graph -> [Type] -> Term -> Term -> InferenceContext -> Graph -> Either Error Java.Expression)
encodeApplication_fallback = def "encodeApplication_fallback" $
  lambda "env" $ lambda "aliases" $ lambda "gr" $ lambda "typeApps" $ lambda "lhs" $ lambda "rhs" $
    "cx" ~> "g" ~>
    ("mt" <<~ (getTypeE (var "cx") (var "g") (Annotations.termAnnotationInternal @@ var "lhs")) $
    "t" <<~ (Optionals.cases (var "mt")
      (Checking.typeOfTerm @@ var "cx" @@ var "g" @@ var "lhs")
      (lambda "typ" $ right (var "typ"))) $
    cases _Type (Strip.deannotateTypeParameters @@ (Strip.deannotateType @@ var "t"))
      (Just $
        -- Non-function type: encode as generic .apply() call
        "jfun" <<~ (encodeTerm @@ var "env" @@ var "lhs" @@ var "cx" @@ var "g") $
        "jarg" <<~ (encodeTerm @@ var "env" @@ var "rhs" @@ var "cx" @@ var "g") $
        right (applyJavaArg @@ var "jfun" @@ var "jarg")) [
      _Type_function>>: lambda "ft" $
        "dom" <~ Core.functionTypeDomain (var "ft") $
        "cod" <~ Core.functionTypeCodomain (var "ft") $
        "defaultExpr" <~ (
            "jfun" <<~ (encodeTerm @@ var "env" @@ var "lhs" @@ var "cx" @@ var "g") $
            "jarg" <<~ (encodeTerm @@ var "env" @@ var "rhs" @@ var "cx" @@ var "g") $
            right (applyJavaArg @@ var "jfun" @@ var "jarg")) $
        "elimBranch" <~ (
            "jarg" <<~ (encodeTerm @@ var "env" @@ var "rhs" @@ var "cx" @@ var "g") $
            -- If dom has no type args, try to get a richer type from the argument
            "enrichedDom" <<~ (Logic.ifElse
              (Logic.not (Lists.null (javaTypeArgumentsForType @@ var "dom")))
              (right (var "dom"))
              ("mrt" <<~ (getTypeE (var "cx") (var "g") (Annotations.termAnnotationInternal @@ var "rhs")) $
                Optionals.cases (var "mrt")
                  ("rt" <<~ (Checking.typeOfTerm @@ var "cx" @@ var "g" @@ var "rhs") $
                    right (Logic.ifElse (Logic.not (Lists.null (javaTypeArgumentsForType @@ var "rt")))
                      (var "rt")
                      (var "dom")))
                  (lambda "rt" $
                    right (Logic.ifElse (Logic.not (Lists.null (javaTypeArgumentsForType @@ var "rt")))
                      (var "rt")
                      (var "dom"))))) $
            encodeElimination @@ var "env" @@ just (var "jarg") @@ var "enrichedDom" @@ var "cod" @@ (Strip.deannotateTerm @@ var "lhs") @@ var "cx" @@ var "g") $
        -- Peel TypeApp wrappers in the dispatch so `TypeApp (Cases ...) Value`
        -- routes to elimBranch instead of falling through to defaultExpr (which
        -- would re-encode the wrapped form and recurse infinitely through the
        -- `_Term_typeApplication` handler in encodeTermInternal).
        cases _Term (Strip.deannotateAndDetypeTerm @@ var "lhs")
          (Just $ var "defaultExpr") [
          _Term_project>>: lambda "_p" $ var "elimBranch",
          _Term_cases>>: lambda "_c" $ var "elimBranch",
          _Term_unwrap>>: lambda "_w" $ var "elimBranch"]])

-- | Encode all definitions in a module to Java compilation units.
encodeDefinitions :: TypedTermDefinition (Module -> [Definition] -> InferenceContext -> Graph -> Either Error (M.Map Name Java.CompilationUnit))
encodeDefinitions = def "encodeDefinitions" $
  lambda "mod" $ lambda "defs" $
    "cx" ~> "g" ~>
    "aliases" <~ (JavaUtilsSource.importAliasesForModule @@ var "mod") $
    "env" <~ (record JavaHelpers._JavaEnvironment [
      JavaHelpers._JavaEnvironment_aliases>>: var "aliases",
      JavaHelpers._JavaEnvironment_graph>>: var "g"]) $
    "pkg" <~ (JavaUtilsSource.javaPackageDeclaration @@ (Packaging.moduleName (var "mod"))) $
    "partitioned" <~ (Environment.partitionDefinitions @@ var "defs") $
    "typeDefs" <~ Pairs.first (var "partitioned") $
    "termDefs" <~ Pairs.second (var "partitioned") $
    -- Filter out typedefs (non-record/union/wrap types)
    "nonTypedefDefs" <~ Lists.filter (lambda "td" $
      "typ" <~ (Core.typeSchemeBody $ project _TypeDefinition _TypeDefinition_body @@ var "td") $
      isSerializableJavaType @@ (var "typ"))
      (var "typeDefs") $
    "typeUnits" <<~ (Eithers.mapList (lambda "td" $ encodeTypeDefinition @@ var "pkg" @@ var "aliases" @@ var "td" @@ var "cx" @@ var "g") (var "nonTypedefDefs")) $
    "termUnits" <<~ Logic.ifElse (Lists.null (var "termDefs"))
      (right (list ([] :: [TypedTerm (Name, Java.CompilationUnit)])))
      ("dataMembers" <<~ (Eithers.mapList (lambda "td" $ encodeTermDefinition @@ var "env" @@ var "td" @@ var "cx" @@ var "g") (var "termDefs")) $
        right (list [constructElementsInterface @@ var "mod" @@ var "dataMembers"])) $
    right (Maps.fromList (Lists.concat2 (var "typeUnits") (var "termUnits")))

-- | Encode an elimination expression. The "elimTerm" argument must be one of
-- the elimination-form Term variants: project, cases, or unwrap.
encodeElimination :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> Maybe Java.Expression -> Type -> Type -> Term -> InferenceContext -> Graph -> Either Error Java.Expression)
encodeElimination = def "encodeElimination" $
  lambda "env" $ lambda "marg" $ lambda "dom" $ lambda "cod" $ lambda "elimTerm" $
    "cx" ~> "g" ~>
    "aliases" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env") $
    -- Peel TypeApp wrappers (in addition to annotations) so callers can pass
    -- in `TypeApp (Cases ...) Value` and we still dispatch to the right branch.
    cases _Term (Strip.deannotateAndDetypeTerm @@ var "elimTerm")
      (Just $ left (Error.errorOther $ Error.otherError $ Strings.cat2 (string "unexpected ") (Strings.cat2 (string "elimination case") (Strings.cat2 (string " in ") (string "encodeElimination"))))) [

      -- Projection: field projection
      _Term_project>>: lambda "proj" $
        "fname" <~ (Core.projectionFieldName (var "proj")) $
        "jdom0" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "dom" @@ var "cx" @@ var "g") $
        "jdomr" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jdom0" @@ var "cx") $
        Optionals.cases (var "marg")
          -- No arg: generate lambda for projection
          ("projVar" <~ wrap _Name (string "projected") $
            "jbody" <~ (JavaUtilsSource.javaExpressionNameToJavaExpression @@
              (JavaUtilsSource.fieldExpression @@ (JavaUtilsSource.variableToJavaIdentifier @@ var "projVar")
                @@ (JavaUtilsSource.javaIdentifier @@ (Core.unName (var "fname"))))) $
            right (JavaUtilsSource.javaLambda @@ var "projVar" @@ var "jbody"))
          -- With arg: field access on expression
          (lambda "jarg" $
            "qual" <~ (inject Java._FieldAccess_Qualifier Java._FieldAccess_Qualifier_primary
              (JavaUtilsSource.javaExpressionToJavaPrimary @@ var "jarg")) $
            right (JavaUtilsSource.javaFieldAccessToJavaExpression @@ (record Java._FieldAccess [
              Java._FieldAccess_qualifier>>: var "qual",
              Java._FieldAccess_identifier>>: JavaUtilsSource.javaIdentifier @@ (Core.unName (var "fname"))]))),

      -- Case statement
      _Term_cases>>: lambda "cs" $
        "tname" <~ (project _CaseStatement _CaseStatement_typeName @@ var "cs") $
        "def_" <~ (project _CaseStatement _CaseStatement_default @@ var "cs") $
        "fields" <~ (project _CaseStatement _CaseStatement_cases @@ var "cs") $
        Optionals.cases (var "marg")
          -- No arg: wrap elimination in a lambda. We need the inner application
          -- `App elimTerm u` to typecheck: elimTerm's case-statement domain (the
          -- bare nominal `tname`) must match the wrapper lambda's parameter `u`,
          -- whose type is `dom`. When `dom` is a polymorphic instantiation like
          -- `ParseResult @ Value`, we wrap `elimTerm` with matching TypeApps so
          -- the case-statement is type-applied to `[Value]`, making its inferred
          -- function-type domain match `dom`.
          ("uVar" <~ wrap _Name (string "u") $
            "domTypeArgs0" <~ (lambda "ty" $ lambda "acc" $
              cases _Type (Strip.deannotateType @@ var "ty")
                (Just $ var "acc") [
                _Type_application>>: lambda "atyp" $
                  var "domTypeArgs0"
                    @@ Core.applicationTypeFunction (var "atyp")
                    @@ Lists.cons (Core.applicationTypeArgument (var "atyp")) (var "acc")]) $
            "domTypeArgs" <~ (var "domTypeArgs0" @@ var "dom" @@ list ([] :: [TypedTerm Type])) $
            -- Use the deannotated-and-detyped form of elimTerm as the base, then wrap with
            -- typeApps derived from dom. This handles both bare-Cases and already-wrapped
            -- TypeApp(Cases) inputs uniformly.
            "bareElim" <~ Strip.deannotateAndDetypeTerm @@ var "elimTerm" $
            "wrappedElimTerm" <~ Lists.foldl
              (lambda "trm" $ lambda "t" $
                inject _Term _Term_typeApplication (record _TypeApplicationTerm [
                  _TypeApplicationTerm_body>>: var "trm",
                  _TypeApplicationTerm_type>>: var "t"]))
              (var "bareElim")
              (var "domTypeArgs") $
            "typedLambda" <~ (inject _Term _Term_lambda (record _Lambda [
              _Lambda_parameter>>: var "uVar",
              _Lambda_domain>>: just (var "dom"),
              _Lambda_body>>: inject _Term _Term_application (record _Application [
                _Application_function>>: var "wrappedElimTerm",
                _Application_argument>>: inject _Term _Term_variable (var "uVar")])])) $
            encodeTerm @@ var "env" @@ var "typedLambda" @@ var "cx" @@ var "g")
          -- With arg: apply elimination to visitor
          (lambda "jarg" $
            "prim" <~ (JavaUtilsSource.javaExpressionToJavaPrimary @@ var "jarg") $
            "consId" <~ (innerClassRef @@ var "aliases" @@ var "tname" @@ asTerm JavaNamesSource.partialVisitorName) $
            "effectiveCod" <~ (var "cod") $
            "jcod" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "effectiveCod" @@ var "cx" @@ var "g") $
            "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jcod" @@ var "cx") $
            "domArgs" <<~ (domTypeArgs @@ var "aliases" @@ var "dom" @@ var "cx" @@ var "g") $
            "targs" <~ (typeArgsOrDiamond @@ (Lists.concat2 (var "domArgs") (list [JavaDsl.typeArgumentReference (var "rt")]))) $
            "otherwiseBranches" <<~ (Optionals.cases (var "def_")
              (right (list ([] :: [TypedTerm Java.ClassBodyDeclarationWithComments])))
              (lambda "d" $
                "b" <<~ (otherwiseBranch @@ var "env" @@ var "aliases" @@ var "dom" @@ var "cod" @@ var "tname" @@ var "jcod" @@ var "domArgs" @@ var "d" @@ var "cx" @@ var "g") $
                right (list [var "b"]))) $
            "visitBranches" <<~ (Eithers.mapList (lambda "f" $ visitBranch @@ var "env" @@ var "aliases" @@ var "dom" @@ var "tname" @@ var "jcod" @@ var "domArgs" @@ var "f" @@ var "cx" @@ var "g") (var "fields")) $
            "body" <~ wrap Java._ClassBody (Lists.concat2 (var "otherwiseBranches") (var "visitBranches")) $
            "visitor" <~ (JavaUtilsSource.javaConstructorCall @@ (JavaUtilsSource.javaConstructorName @@ var "consId" @@ just (var "targs")) @@ list ([] :: [TypedTerm Java.Expression]) @@ just (var "body")) $
            right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocation @@ just (right (var "prim"))
                @@ JavaDsl.identifier (asTerm JavaNamesSource.acceptMethodName) @@ list [var "visitor"]))),

      -- Unwrap: unwrap a newtype
      _Term_unwrap>>: lambda "wrapName" $
        "withArg" <~ (lambda "ja" $
          JavaUtilsSource.javaFieldAccessToJavaExpression @@ (record Java._FieldAccess [
            Java._FieldAccess_qualifier>>: inject Java._FieldAccess_Qualifier Java._FieldAccess_Qualifier_primary
              (JavaUtilsSource.javaExpressionToJavaPrimary @@ var "ja"),
            Java._FieldAccess_identifier>>: JavaUtilsSource.javaIdentifier @@ asTerm JavaNamesSource.valueFieldName])) $
        right (Optionals.cases (var "marg")
          -- No arg: generate lambda for unwrapping
          ("wVar" <~ wrap _Name (string "wrapped") $
            "wArg" <~ (JavaUtilsSource.javaIdentifierToJavaExpression @@ (JavaUtilsSource.variableToJavaIdentifier @@ var "wVar")) $
            JavaUtilsSource.javaLambda @@ var "wVar" @@ (var "withArg" @@ var "wArg"))
          -- With arg: field access
          (lambda "jarg" $ var "withArg" @@ var "jarg"))]

-- | Encode a function-form term. The "funTerm" argument must be one of
-- the function-form Term variants: lambda, project, cases, or unwrap.
encodeFunction :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> Type -> Type -> Term -> InferenceContext -> Graph -> Either Error Java.Expression)
encodeFunction = def "encodeFunction" $
  lambda "env" $ lambda "dom" $ lambda "cod" $ lambda "funTerm" $
    "cx" ~> "g" ~>
    "aliases" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env") $
    "encodeLambdaFallback" <~ (lambda "env2" $ lambda "lam" $
      "lambdaVar" <~ Core.lambdaParameter (var "lam") $
      "body" <~ Core.lambdaBody (var "lam") $
      "fs" <<~ (analyzeJavaFunction @@ var "env2" @@ var "body" @@ var "cx" @@ var "g") $
      "bindings" <~ (project _FunctionStructure _FunctionStructure_bindings @@ var "fs") $
      "innerBody" <~ (project _FunctionStructure _FunctionStructure_body @@ var "fs") $
      "env3" <~ (project _FunctionStructure _FunctionStructure_environment @@ var "fs") $
      "bindResult" <<~ (bindingsToStatements @@ var "env3" @@ var "bindings" @@ var "cx" @@ var "g") $
      "bindingStmts" <~ Pairs.first (var "bindResult") $
      "env4" <~ Pairs.second (var "bindResult") $
      "jbody" <<~ (encodeTerm @@ var "env4" @@ var "innerBody" @@ var "cx" @@ var "g") $
      "lam1" <~ (Logic.ifElse (Lists.null (var "bindings"))
        (JavaUtilsSource.javaLambda @@ var "lambdaVar" @@ var "jbody")
        ("returnSt" <~ (JavaDsl.blockStatementStatement (JavaUtilsSource.javaReturnStatement @@ just (var "jbody"))) $
          JavaUtilsSource.javaLambdaFromBlock @@ var "lambdaVar" @@
            (wrap Java._Block (Lists.concat2 (var "bindingStmts") (list [var "returnSt"]))))) $
      applyCastIfSafe @@ var "aliases" @@ (inject _Type _Type_function (record _FunctionType [
        _FunctionType_domain>>: var "dom",
        _FunctionType_codomain>>: var "cod"])) @@ var "lam1" @@ var "cx" @@ var "g") $
    cases _Term (Strip.deannotateTerm @@ var "funTerm")
      (Just $ right (encodeLiteral @@ (inject _Literal _Literal_string
        (Strings.cat2 (string "Unimplemented function variant: ") (ShowCore.term @@ var "funTerm"))))) [

      -- Projection: delegate to encodeElimination
      _Term_project>>: lambda "_p" $
        (encodeElimination @@ var "env" @@ nothing @@ var "dom" @@ var "cod" @@ (Strip.deannotateTerm @@ var "funTerm") @@ var "cx" @@ var "g"),

      -- Case statement: delegate to encodeElimination
      _Term_cases>>: lambda "_c" $
        (encodeElimination @@ var "env" @@ nothing @@ var "dom" @@ var "cod" @@ (Strip.deannotateTerm @@ var "funTerm") @@ var "cx" @@ var "g"),

      -- Unwrap: delegate to encodeElimination
      _Term_unwrap>>: lambda "_w" $
        (encodeElimination @@ var "env" @@ nothing @@ var "dom" @@ var "cod" @@ (Strip.deannotateTerm @@ var "funTerm") @@ var "cx" @@ var "g"),

      -- Lambda: encode as Java lambda
      _Term_lambda>>: lambda "lam" $
        (withLambda @@ var "env" @@ var "lam" @@ (lambda "env2" $
          "lambdaVar" <~ Core.lambdaParameter (var "lam") $
          "body" <~ Core.lambdaBody (var "lam") $
          cases _Term (Strip.deannotateTerm @@ var "body")
            (Just $ var "encodeLambdaFallback" @@ var "env2" @@ var "lam") [

            -- Body is another lambda: recursively encode it
            _Term_lambda>>: lambda "innerLam" $
              cases _Type (Strip.deannotateType @@ var "cod")
                (Just $ left (Error.errorOther $ Error.otherError $ Strings.cat2 (string "expected function type for lambda body, but got: ")
                  (ShowCore.type_ @@ var "cod"))) [
                _Type_function>>: lambda "ft" $
                  "dom2" <~ Core.functionTypeDomain (var "ft") $
                  "cod2" <~ Core.functionTypeCodomain (var "ft") $
                  "innerJavaLambda" <<~ (encodeFunction @@ var "env2" @@ var "dom2" @@ var "cod2"
                    @@ (inject _Term _Term_lambda (var "innerLam")) @@ var "cx" @@ var "g") $
                  "lam1" <~ (JavaUtilsSource.javaLambda @@ var "lambdaVar" @@ var "innerJavaLambda") $
                  applyCastIfSafe @@ var "aliases" @@ (inject _Type _Type_function (record _FunctionType [
                    _FunctionType_domain>>: var "dom",
                    _FunctionType_codomain>>: var "cod"])) @@ var "lam1" @@ var "cx" @@ var "g"]]))]

-- | Shared handler for function-form Term variants (lambda, project, cases, unwrap)
-- at the encodeTermInternal level. Resolves the term's type and delegates to encodeFunction.
encodeFunctionFormTerm :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> [M.Map Name Term] -> Term -> InferenceContext -> Graph -> Either Error Java.Expression)
encodeFunctionFormTerm = def "encodeFunctionFormTerm" $
  lambda "env" $ lambda "anns" $ lambda "term" $
    "cx" ~> "g" ~>
    "combinedAnns" <~ Lists.foldl (lambda "acc" $ lambda "m" $ Maps.union (var "acc") (var "m")) Maps.empty (var "anns") $
    "mt" <<~ (getTypeE (var "cx") (var "g") (var "combinedAnns")) $
    "typ" <<~ (Optionals.cases (var "mt")
      (Optionals.cases (tryInferFunctionType @@ var "term")
        (Checking.typeOfTerm @@ var "cx" @@ var "g" @@ var "term")
        (lambda "inferredType" $ right (var "inferredType")))
      (lambda "t" $ right (var "t"))) $
    cases _Type (Strip.deannotateType @@ var "typ")
      (Just $ encodeNullaryConstant @@ var "env" @@ var "typ" @@ var "term" @@ var "cx" @@ var "g") [
      _Type_function>>: lambda "ft" $
        encodeFunction @@ var "env" @@ (Core.functionTypeDomain (var "ft")) @@ (Core.functionTypeCodomain (var "ft")) @@ var "term" @@ var "cx" @@ var "g"]

-- | Encode a primitive reference (by name) with function-type arity, as a method reference or curried wrapper.
encodeFunctionPrimitiveByName :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> Type -> Type -> Name -> InferenceContext -> Graph -> Either Error Java.Expression)
encodeFunctionPrimitiveByName = def "encodeFunctionPrimitiveByName" $
  lambda "env" $ lambda "dom" $ lambda "cod" $ lambda "name" $
    "cx" ~> "g" ~>
    "aliases" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env") $
    "classWithApply" <~ (JavaDsl.unIdentifier (elementJavaIdentifier @@ true @@ false @@ var "aliases" @@ var "name")) $
    "suffix" <~ Strings.cat2 (string ".") (asTerm JavaNamesSource.applyMethodName) $
    "className" <~ Strings.fromList (Lists.take
      (Math.sub (Strings.length (var "classWithApply")) (Strings.length (var "suffix")))
      (Strings.toList (var "classWithApply"))) $
    "arity" <~ (Arity.typeArity @@ (inject _Type _Type_function (record _FunctionType [
      _FunctionType_domain>>: var "dom",
      _FunctionType_codomain>>: var "cod"]))) $
    Logic.ifElse (Equality.lte (var "arity") (int32 1))
      (right (JavaUtilsSource.javaIdentifierToJavaExpression @@
        (JavaDsl.identifier (Strings.cat (list [var "className", string "::", asTerm JavaNamesSource.applyMethodName])))))
      ("paramNames" <~ Lists.map
        (lambda "i" $ wrap _Name (Strings.cat2 (string "p") (Literals.showInt32 (var "i"))))
        (Math.range (int32 0) (Math.sub (var "arity") (int32 1))) $
        "paramExprs" <~ Lists.map
          (lambda "p" $ JavaUtilsSource.javaIdentifierToJavaExpression @@ (JavaUtilsSource.variableToJavaIdentifier @@ var "p"))
          (var "paramNames") $
        "classId" <~ JavaDsl.identifier (var "className") $
        "call" <~ (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
          (JavaUtilsSource.methodInvocationStatic @@ var "classId" @@ JavaDsl.identifier (asTerm JavaNamesSource.applyMethodName) @@ var "paramExprs")) $
        "curried" <~ (buildCurriedLambda @@ var "paramNames" @@ var "call") $
        "jtype" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ (inject _Type _Type_function (record _FunctionType [
          _FunctionType_domain>>: var "dom",
          _FunctionType_codomain>>: var "cod"])) @@ var "cx" @@ var "g") $
        "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jtype" @@ var "cx") $
        right (JavaUtilsSource.javaCastExpressionToJavaExpression @@
          (JavaUtilsSource.javaCastExpression @@ var "rt" @@ (JavaUtilsSource.javaExpressionToJavaUnaryExpression @@ var "curried"))))

-- | Encode a literal value to a Java expression
encodeLiteral :: TypedTermDefinition (Literal -> Java.Expression)
encodeLiteral = def "encodeLiteral" $
  lambda "lit" $
    cases _Literal (var "lit") Nothing [
      _Literal_binary>>: "bs" ~>
        "byteValues" <~ Literals.binaryToBytes (var "bs") $
        JavaUtilsSource.javaArrayCreation @@
          (asTerm JavaUtilsSource.javaBytePrimitiveType) @@
          (just $ JavaUtilsSource.javaArrayInitializer @@
            (Lists.map
              (lambda "w" $
                JavaUtilsSource.javaLiteralToJavaExpression @@
                  (JavaDsl.literalInteger $ JavaDsl.integerLiteral $ Literals.int32ToBigint (var "w")))
              (var "byteValues"))),
      _Literal_boolean>>: "b" ~>
        encodeLiteral_litExp @@ (JavaUtilsSource.javaBoolean @@ var "b"),
      _Literal_decimal>>: "v" ~>
        JavaUtilsSource.javaConstructorCall @@
          (JavaUtilsSource.javaConstructorName @@
            (JavaDsl.identifier $ string "java.math.BigDecimal") @@ nothing) @@
          list [encodeLiteral @@ inject _Literal _Literal_string (Literals.showDecimal $ var "v")] @@
          nothing,
      _Literal_float>>: "f" ~> encodeLiteral_encodeFloat @@ var "f",
      _Literal_integer>>: "i" ~> encodeLiteral_encodeInteger @@ var "i",
      _Literal_string>>: "s" ~>
        encodeLiteral_litExp @@ (JavaUtilsSource.javaString @@ var "s")]

-- | Encode a Hydra literal type to a Java type
encodeLiteralType :: TypedTermDefinition (LiteralType -> InferenceContext -> Graph -> Either Error Java.Type)
encodeLiteralType = def "encodeLiteralType" $
  lambda "lt" $ 
    "cx" ~> "g" ~>
    cases _LiteralType (var "lt") Nothing [
    _LiteralType_binary>>: constant $
      right (JavaDsl.typeReference
        (JavaDsl.referenceTypeArray
          (JavaDsl.arrayType
            (JavaDsl.dims (list [list ([] :: [TypedTerm Java.Annotation])]))
            (JavaDsl.arrayTypeVariantPrimitive
              (JavaDsl.primitiveTypeWithAnnotations
                (JavaDsl.primitiveTypeNumeric (JavaDsl.numericTypeIntegral JavaDsl.integralTypeByte))
                (list ([] :: [TypedTerm Java.Annotation]))))))),
    _LiteralType_boolean>>: constant $
      encodeLiteralType_simple @@ string "Boolean" @@ var "cx" @@ var "g",
    _LiteralType_decimal>>: constant $
      right (JavaUtilsSource.javaRefType
        @@ list ([] :: [TypedTerm Java.ReferenceType])
        @@ just (JavaNamesSource.javaPackageName @@ list [string "java", string "math"])
        @@ string "BigDecimal"),
    _LiteralType_float>>: lambda "ft" $
      cases _FloatType (var "ft") Nothing [
        _FloatType_float32>>: constant $
          encodeLiteralType_simple @@ string "Float" @@ var "cx" @@ var "g",
        _FloatType_float64>>: constant $
          encodeLiteralType_simple @@ string "Double" @@ var "cx" @@ var "g"],
    _LiteralType_integer>>: lambda "it" $
      cases _IntegerType (var "it") Nothing [
        _IntegerType_bigint>>: constant $
          right (JavaUtilsSource.javaRefType
            @@ list ([] :: [TypedTerm Java.ReferenceType])
            @@ just (JavaNamesSource.javaPackageName @@ list [string "java", string "math"])
            @@ string "BigInteger"),
        _IntegerType_int8>>: constant $
          encodeLiteralType_simple @@ string "Byte" @@ var "cx" @@ var "g",
        _IntegerType_int16>>: constant $
          encodeLiteralType_simple @@ string "Short" @@ var "cx" @@ var "g",
        _IntegerType_int32>>: constant $
          encodeLiteralType_simple @@ string "Integer" @@ var "cx" @@ var "g",
        _IntegerType_int64>>: constant $
          encodeLiteralType_simple @@ string "Long" @@ var "cx" @@ var "g",
        _IntegerType_uint8>>: constant $
          encodeLiteralType_simple @@ string "Short" @@ var "cx" @@ var "g",
        _IntegerType_uint16>>: constant $
          encodeLiteralType_simple @@ string "Character" @@ var "cx" @@ var "g",
        _IntegerType_uint32>>: constant $
          encodeLiteralType_simple @@ string "Long" @@ var "cx" @@ var "g",
        _IntegerType_uint64>>: constant $
          right (JavaUtilsSource.javaRefType
            @@ list ([] :: [TypedTerm Java.ReferenceType])
            @@ just (JavaNamesSource.javaPackageName @@ list [string "java", string "math"])
            @@ string "BigInteger")],
    _LiteralType_string>>: constant $
      encodeLiteralType_simple @@ string "String" @@ var "cx" @@ var "g"]

-- | Helper: encode a simple Java reference type by class name (no package, no type arguments)
encodeLiteralType_simple :: TypedTermDefinition (String -> InferenceContext -> Graph -> Either Error Java.Type)
encodeLiteralType_simple = def "encodeLiteralType_simple" $
  lambda "n" $ 
    "cx" ~> "g" ~>
    right (JavaUtilsSource.javaRefType
    @@ list ([] :: [TypedTerm Java.ReferenceType])
    @@ nothing
    @@ var "n")

-- | Encode a float value to a Java expression
encodeLiteral_encodeFloat :: TypedTermDefinition (FloatValue -> Java.Expression)
encodeLiteral_encodeFloat = def "encodeLiteral_encodeFloat" $
  lambda "f" $
    cases _FloatValue (var "f") Nothing [
      _FloatValue_float32>>: "v" ~>
        encodeLiteral_encodeFloat32 @@ var "v",
      _FloatValue_float64>>: "v" ~>
        encodeLiteral_encodeFloat64 @@ var "v"]

-- | Encode a float32 value, handling NaN and Infinity specially since BigDecimal cannot represent them.
encodeLiteral_encodeFloat32 :: TypedTermDefinition (Float -> Java.Expression)
encodeLiteral_encodeFloat32 = def "encodeLiteral_encodeFloat32" $
  lambda "v" $ lets [
    "s">: Literals.showFloat32 (var "v")] $
    Logic.ifElse (Equality.equal (var "s") (string "NaN"))
      (encodeLiteral_javaSpecialFloatExpr @@ string "Float" @@ string "NaN") $
    Logic.ifElse (Equality.equal (var "s") (string "Infinity"))
      (encodeLiteral_javaSpecialFloatExpr @@ string "Float" @@ string "POSITIVE_INFINITY") $
    Logic.ifElse (Equality.equal (var "s") (string "-Infinity"))
      (encodeLiteral_javaSpecialFloatExpr @@ string "Float" @@ string "NEGATIVE_INFINITY") $
    encodeLiteral_primCast @@
      (JavaDsl.primitiveTypeNumeric $ JavaDsl.numericTypeFloatingPoint JavaDsl.floatingPointTypeFloat) @@
      (encodeLiteral_litExp @@
        (JavaDsl.literalFloatingPoint $ JavaDsl.floatingPointLiteral $
          Literals.float32ToFloat64 (var "v")))

-- | Encode a float64 value, handling NaN, Infinity, and negative zero specially.
-- The Java FloatingPointLiteral wrapper cannot represent NaN, Infinity, or signed zero.
encodeLiteral_encodeFloat64 :: TypedTermDefinition (Double -> Java.Expression)
encodeLiteral_encodeFloat64 = def "encodeLiteral_encodeFloat64" $
  lambda "v" $ lets [
    "s">: Literals.showFloat64 (var "v")] $
    Logic.ifElse (Equality.equal (var "s") (string "NaN"))
      (encodeLiteral_javaSpecialFloatExpr @@ string "Double" @@ string "NaN") $
    Logic.ifElse (Equality.equal (var "s") (string "Infinity"))
      (encodeLiteral_javaSpecialFloatExpr @@ string "Double" @@ string "POSITIVE_INFINITY") $
    Logic.ifElse (Equality.equal (var "s") (string "-Infinity"))
      (encodeLiteral_javaSpecialFloatExpr @@ string "Double" @@ string "NEGATIVE_INFINITY") $
    -- Negative zero must be emitted via Double.parseDouble("-0.0") to preserve the sign.
    Logic.ifElse (Equality.equal (var "s") (string "-0.0"))
      (encodeLiteral_javaParseDouble @@ string "-0.0") $
    encodeLiteral_litExp @@
      (JavaDsl.literalFloatingPoint $ JavaDsl.floatingPointLiteral $
        var "v")

-- | Encode an integer value to a Java expression
encodeLiteral_encodeInteger :: TypedTermDefinition (IntegerValue -> Java.Expression)
encodeLiteral_encodeInteger = def "encodeLiteral_encodeInteger" $
  lambda "i" $
    cases _IntegerValue (var "i") Nothing [
      _IntegerValue_bigint>>: "v" ~>
        JavaUtilsSource.javaConstructorCall @@
          (JavaUtilsSource.javaConstructorName @@
            (JavaDsl.identifier $ string "java.math.BigInteger") @@ nothing) @@
          list [encodeLiteral @@ inject _Literal _Literal_string (Literals.showBigint $ var "v")] @@
          nothing,
      _IntegerValue_int8>>: "v" ~>
        encodeLiteral_primCast @@
          (JavaDsl.primitiveTypeNumeric $ JavaDsl.numericTypeIntegral JavaDsl.integralTypeByte) @@
          (encodeLiteral_litExp @@
            (JavaDsl.literalInteger $ JavaDsl.integerLiteral $ Literals.int8ToBigint (var "v"))),
      _IntegerValue_int16>>: "v" ~>
        encodeLiteral_primCast @@
          (JavaDsl.primitiveTypeNumeric $ JavaDsl.numericTypeIntegral JavaDsl.integralTypeShort) @@
          (encodeLiteral_litExp @@
            (JavaDsl.literalInteger $ JavaDsl.integerLiteral $ Literals.int16ToBigint (var "v"))),
      _IntegerValue_int32>>: "v" ~>
        encodeLiteral_litExp @@
          (JavaDsl.literalInteger $ JavaDsl.integerLiteral $ Literals.int32ToBigint (var "v")),
      _IntegerValue_int64>>: "v" ~>
        encodeLiteral_primCast @@
          (JavaDsl.primitiveTypeNumeric $ JavaDsl.numericTypeIntegral JavaDsl.integralTypeLong) @@
          (encodeLiteral_litExp @@
            (JavaDsl.literalInteger $ JavaDsl.integerLiteral $ Literals.int64ToBigint (var "v"))),
      _IntegerValue_uint8>>: "v" ~>
        encodeLiteral_primCast @@
          (JavaDsl.primitiveTypeNumeric $ JavaDsl.numericTypeIntegral JavaDsl.integralTypeShort) @@
          (encodeLiteral_litExp @@
            (JavaDsl.literalInteger $ JavaDsl.integerLiteral $ Literals.uint8ToBigint (var "v"))),
      _IntegerValue_uint16>>: "v" ~>
        encodeLiteral_litExp @@
          (JavaDsl.literalCharacter $ var "v"),
      _IntegerValue_uint32>>: "v" ~>
        encodeLiteral_primCast @@
          (JavaDsl.primitiveTypeNumeric $ JavaDsl.numericTypeIntegral JavaDsl.integralTypeLong) @@
          (encodeLiteral_litExp @@
            (JavaDsl.literalInteger $ JavaDsl.integerLiteral $ Literals.uint32ToBigint (var "v"))),
      _IntegerValue_uint64>>: "v" ~>
        JavaUtilsSource.javaConstructorCall @@
          (JavaUtilsSource.javaConstructorName @@
            (JavaDsl.identifier $ string "java.math.BigInteger") @@ nothing) @@
          list [encodeLiteral @@ inject _Literal _Literal_string
            (Literals.showBigint $ Literals.uint64ToBigint (var "v"))] @@
          nothing]

-- | Emit a Java method call expression Double.parseDouble("<value>"). Used for
-- float64 values that cannot be represented as a FloatingPointLiteral (e.g., negative zero).
encodeLiteral_javaParseDouble :: TypedTermDefinition (String -> Java.Expression)
encodeLiteral_javaParseDouble = def "encodeLiteral_javaParseDouble" $
  lambda "value" $
    JavaUtilsSource.javaMethodInvocationToJavaExpression @@
      (JavaUtilsSource.methodInvocationStatic
        @@ JavaDsl.identifier (string "Double")
        @@ JavaDsl.identifier (string "parseDouble")
        @@ list [encodeLiteral @@ inject _Literal _Literal_string (var "value")])

-- | Emit a Java field access expression like Float.NaN or Double.POSITIVE_INFINITY.
encodeLiteral_javaSpecialFloatExpr :: TypedTermDefinition (String -> String -> Java.Expression)
encodeLiteral_javaSpecialFloatExpr = def "encodeLiteral_javaSpecialFloatExpr" $
  lambda "className" $ lambda "fieldName" $
    JavaUtilsSource.javaExpressionNameToJavaExpression @@
      (JavaDsl.expressionName
        (just (JavaDsl.ambiguousName (list [JavaDsl.identifier $ var "className"])))
        (JavaDsl.identifier $ var "fieldName"))

-- | Helper: convert a Java literal to a Java expression
encodeLiteral_litExp :: TypedTermDefinition (Java.Literal -> Java.Expression)
encodeLiteral_litExp = def "encodeLiteral_litExp" $
  lambda "l" $ JavaUtilsSource.javaLiteralToJavaExpression @@ var "l"

-- | Helper: cast an expression to a primitive type
encodeLiteral_primCast :: TypedTermDefinition (Java.PrimitiveType -> Java.Expression -> Java.Expression)
encodeLiteral_primCast = def "encodeLiteral_primCast" $
  lambda "pt" $ lambda "expr" $
    JavaUtilsSource.javaCastExpressionToJavaExpression @@
      (JavaUtilsSource.javaCastPrimitive @@ var "pt" @@
        (JavaUtilsSource.javaExpressionToJavaUnaryExpression @@ var "expr"))

-- | Encode a nullary constant function as a Java expression
encodeNullaryConstant :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> Type -> Term -> InferenceContext -> Graph -> Either Error Java.Expression)
encodeNullaryConstant = def "encodeNullaryConstant" $
  lambda "env" $ lambda "typ" $ lambda "funTerm" $
    "cx" ~> "g" ~>
    left (Error.errorOther $ Error.otherError $ Strings.cat2 (string "unexpected ") (Strings.cat2 (string "nullary function") (Strings.cat2 (string " in ") (ShowCore.term @@ var "funTerm"))))

-- | Extract type arguments from the return type for generic method calls
encodeNullaryConstant_typeArgsFromReturnType :: TypedTermDefinition (JavaHelpers.Aliases -> Type -> InferenceContext -> Graph -> Either Error [Java.TypeArgument])
encodeNullaryConstant_typeArgsFromReturnType = def "encodeNullaryConstant_typeArgsFromReturnType" $
  lambda "aliases" $ lambda "t" $
    "cx" ~> "g" ~>
    cases _Type (Strip.deannotateType @@ var "t")
      (Just $ right (list ([] :: [TypedTerm Java.TypeArgument]))) [
      _Type_set>>: "st" ~>
        "jst" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "st" @@ var "cx" @@ var "g") $
        "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jst" @@ var "cx") $
        right (list [JavaDsl.typeArgumentReference (var "rt")]),
      _Type_list>>: "lt_" ~>
        "jlt" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "lt_" @@ var "cx" @@ var "g") $
        "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jlt" @@ var "cx") $
        right (list [JavaDsl.typeArgumentReference (var "rt")]),
      _Type_optional>>: "mt" ~>
        "jmt" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "mt" @@ var "cx" @@ var "g") $
        "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jmt" @@ var "cx") $
        right (list [JavaDsl.typeArgumentReference (var "rt")]),
      _Type_map>>: "mp" ~>
        "jkt" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ (project _MapType _MapType_keys @@ var "mp") @@ var "cx" @@ var "g") $
        "rk" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jkt" @@ var "cx") $
        "jvt" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ (project _MapType _MapType_values @@ var "mp") @@ var "cx" @@ var "g") $
        "rv" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jvt" @@ var "cx") $
        right (list [JavaDsl.typeArgumentReference (var "rk"), JavaDsl.typeArgumentReference (var "rv")])]

-- | Encode a nullary primitive reference (by name) as a Java expression.
encodeNullaryPrimitiveByName :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> Type -> Name -> InferenceContext -> Graph -> Either Error Java.Expression)
encodeNullaryPrimitiveByName = def "encodeNullaryPrimitiveByName" $
  lambda "env" $ lambda "typ" $ lambda "name" $
    "cx" ~> "g" ~>
    "aliases" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env") $
    "targs" <<~ (encodeNullaryConstant_typeArgsFromReturnType @@ var "aliases" @@ var "typ" @@ var "cx" @@ var "g") $
    Logic.ifElse (Lists.null (var "targs"))
      ("header" <~ (inject Java._MethodInvocation_Header Java._MethodInvocation_Header_simple
        (wrap Java._MethodName
          (elementJavaIdentifier @@ boolean True @@ boolean False @@ var "aliases" @@ var "name"))) $
       right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
         (record Java._MethodInvocation [
           Java._MethodInvocation_header>>: var "header",
           Java._MethodInvocation_arguments>>: list ([] :: [TypedTerm Java.Expression])])))
      ("fullName" <~ (unwrap Java._Identifier @@ (elementJavaIdentifier @@ boolean True @@ boolean False @@ var "aliases" @@ var "name")) $
       "parts" <~ Strings.splitOn (string ".") (var "fullName") $
       "className" <~ JavaDsl.identifier (Strings.intercalate (string ".") (Optionals.fromOptional (list ([] :: [TypedTerm String])) (Lists.maybeInit (var "parts")))) $
       "methodName" <~ JavaDsl.identifier (Optionals.fromOptional (var "fullName") (Lists.maybeLast (var "parts"))) $
       right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
         (JavaUtilsSource.methodInvocationStaticWithTypeArgs @@ var "className" @@ var "methodName" @@ var "targs" @@ (list ([] :: [TypedTerm Java.Expression])))))

-- | Encode a Hydra term as a Java expression.
-- Wrapper that calls encodeTermInternal with empty accumulators.
encodeTerm :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> Term -> InferenceContext -> Graph -> Either Error Java.Expression)
encodeTerm = def "encodeTerm" $
  lambda "env" $ lambda "term" $
    "cx" ~> "g" ~>
    encodeTermInternal @@ var "env" @@ list ([] :: [TypedTerm (M.Map Name Term)]) @@ list ([] :: [TypedTerm Java.Type]) @@ var "term" @@ var "cx" @@ var "g"

-- | Encode a term definition as a Java interface method declaration.
-- This is the most complex function — it handles type parameters, lambda analysis,
-- type variable substitution, accumulator unification, and body annotation.
encodeTermDefinition :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> TermDefinition -> InferenceContext -> Graph -> Either Error Java.InterfaceMemberDeclarationWithComments)
encodeTermDefinition = def "encodeTermDefinition" $
  lambda "env" $ lambda "tdef" $
    "cx" ~> "g" ~>
    "name" <~ (project _TermDefinition _TermDefinition_name @@ var "tdef") $
    "term0" <~ (project _TermDefinition _TermDefinition_body @@ var "tdef") $
    "mDoc" <<~ (Annotations.getTermDescription @@ var "cx" @@ var "g" @@ var "term0") $
    "ts" <~ Optionals.cases (Optionals.map Scoping.termSignatureToTypeScheme (project _TermDefinition _TermDefinition_signature @@ var "tdef")) (Core.typeScheme (list ([] :: [TypedTerm Name])) (Core.typeVariable (wrap _Name (string "hydra.core.Unit"))) nothing) ("x" ~> var "x") $
    -- Unshadow variables
    ("term" <~ (Variables.unshadowVariables @@ var "term0") $
      "fs" <<~ (analyzeJavaFunction @@ var "env" @@ var "term" @@ var "cx" @@ var "g") $
      -- Get type parameters from scheme
      "schemeVars" <~ Lists.filter (lambda "v" $ isSimpleName @@ var "v") (Core.typeSchemeVariables (var "ts")) $
      "termVars" <~ (project _FunctionStructure _FunctionStructure_typeParams @@ var "fs") $
      "schemeTypeVars" <~ (collectTypeVars @@ Core.typeSchemeBody (var "ts")) $
      "usedSchemeVars" <~ Lists.filter (lambda "v" $ Sets.member (var "v") (var "schemeTypeVars")) (var "schemeVars") $
      "tparams" <~ Logic.ifElse (Lists.null (var "usedSchemeVars")) (var "termVars") (var "usedSchemeVars") $
      "params" <~ (project _FunctionStructure _FunctionStructure_params @@ var "fs") $
      "bindings" <~ (project _FunctionStructure _FunctionStructure_bindings @@ var "fs") $
      "body" <~ (project _FunctionStructure _FunctionStructure_body @@ var "fs") $
      "doms" <~ (project _FunctionStructure _FunctionStructure_domains @@ var "fs") $
      "env2" <~ (project _FunctionStructure _FunctionStructure_environment @@ var "fs") $
      -- Derive codomain from TypeScheme
      "schemeType" <~ Core.typeSchemeBody (var "ts") $
      "numParams" <~ Lists.length (var "params") $
      "peelResult" <~ (peelDomainsAndCod @@ var "numParams" @@ var "schemeType") $
      "schemeDoms" <~ Pairs.first (var "peelResult") $
      "cod" <~ Pairs.second (var "peelResult") $
      "schemeVarSet" <~ Sets.fromList (var "tparams") $
      -- Build type variable substitution from annotations
      "typeVarSubst" <<~ Logic.ifElse (Lists.null (var "tparams"))
        (right (Maps.empty))
        (buildSubstFromAnnotations @@ var "schemeVarSet" @@ var "term" @@ var "cx" @@ var "g") $
      -- Fix over-generalized type variables
      "overgenSubst" <~ (detectAccumulatorUnification @@ var "schemeDoms" @@ var "cod" @@ var "tparams") $
      "overgenVarSubst" <~ Maps.fromList (Optionals.cat (Lists.map
        (lambda "entry" $
          "k" <~ Pairs.first (var "entry") $
          "v" <~ Pairs.second (var "entry") $
          cases _Type (var "v")
            (Just nothing) [
            _Type_variable>>: lambda "n" $ just (pair (var "k") (var "n"))])
        (Maps.toList (var "overgenSubst")))) $
      "fixedCod" <~ Logic.ifElse (Maps.null (var "overgenSubst")) (var "cod")
        (substituteTypeVarsWithTypes @@ var "overgenSubst" @@ var "cod") $
      "fixedDoms" <~ Logic.ifElse (Maps.null (var "overgenSubst")) (var "schemeDoms")
        (Lists.map (lambda "d" $ substituteTypeVarsWithTypes @@ var "overgenSubst" @@ var "d") (var "schemeDoms")) $
      "fixedTparams" <~ Logic.ifElse (Maps.null (var "overgenSubst")) (var "tparams")
        (Lists.filter (lambda "v" $ Logic.not (Maps.member (var "v") (var "overgenSubst"))) (var "tparams")) $
      "constraints" <~ Optionals.fromOptional (Maps.empty) (Core.typeSchemeConstraints (var "ts")) $
      "jparams" <~ Lists.map (lambda "v" $
        JavaUtilsSource.javaTypeParameter @@ (Formatting.capitalize @@ (Core.unName (var "v"))))
        (var "fixedTparams") $
      -- Update aliases
      "aliases2base" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env2") $
      "trustedVars" <~ Sets.unions (Lists.map (lambda "d" $ collectTypeVars @@ var "d")
        (Lists.concat2 (var "fixedDoms") (list [var "fixedCod"]))) $
      "fixedSchemeVarSet" <~ Sets.fromList (var "fixedTparams") $
      "aliases2" <~ (record JavaHelpers._Aliases [
        JavaHelpers._Aliases_currentNamespace>>:
          project JavaHelpers._Aliases JavaHelpers._Aliases_currentNamespace @@ var "aliases2base",
        JavaHelpers._Aliases_packages>>:
          project JavaHelpers._Aliases JavaHelpers._Aliases_packages @@ var "aliases2base",
        JavaHelpers._Aliases_branchVars>>:
          project JavaHelpers._Aliases JavaHelpers._Aliases_branchVars @@ var "aliases2base",
        JavaHelpers._Aliases_recursiveVars>>:
          project JavaHelpers._Aliases JavaHelpers._Aliases_recursiveVars @@ var "aliases2base",
        JavaHelpers._Aliases_inScopeTypeParams>>: var "fixedSchemeVarSet",
        JavaHelpers._Aliases_polymorphicLocals>>:
          project JavaHelpers._Aliases JavaHelpers._Aliases_polymorphicLocals @@ var "aliases2base",
        JavaHelpers._Aliases_inScopeJavaVars>>:
          project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeJavaVars @@ var "aliases2base",
        JavaHelpers._Aliases_varRenames>>:
          project JavaHelpers._Aliases JavaHelpers._Aliases_varRenames @@ var "aliases2base",
        JavaHelpers._Aliases_lambdaVars>>:
          Sets.union (project JavaHelpers._Aliases JavaHelpers._Aliases_lambdaVars @@ var "aliases2base")
            (Sets.fromList (var "params")),
        JavaHelpers._Aliases_typeVarSubst>>:
          Maps.union (var "overgenVarSubst") (var "typeVarSubst"),
        JavaHelpers._Aliases_trustedTypeVars>>:
          Sets.intersection (var "trustedVars") (var "fixedSchemeVarSet"),
        JavaHelpers._Aliases_methodCodomain>>: just (var "fixedCod"),
        JavaHelpers._Aliases_thunkedVars>>:
          project JavaHelpers._Aliases JavaHelpers._Aliases_thunkedVars @@ var "aliases2base"]) $
      "env2WithTypeParams" <~ (record JavaHelpers._JavaEnvironment [
        JavaHelpers._JavaEnvironment_aliases>>: var "aliases2",
        JavaHelpers._JavaEnvironment_graph>>:
          project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_graph @@ var "env2"]) $
      -- Convert bindings to statements
      "bindResult" <<~ (bindingsToStatements @@ var "env2WithTypeParams" @@ var "bindings" @@ var "cx" @@ var "g") $
      "bindingStmts" <~ Pairs.first (var "bindResult") $
      "env3" <~ Pairs.second (var "bindResult") $
      -- Apply overgen subst to body annotations
      "body'" <<~ Logic.ifElse (Maps.null (var "overgenSubst"))
        (right (var "body"))
        (applyOvergenSubstToTermAnnotations @@ var "overgenSubst" @@ var "body" @@ var "cx" @@ var "g") $
      -- Annotate the body with its expected type using propagateTypesInAppChain
      "annotatedBody" <~ (propagateTypesInAppChain @@ var "fixedCod" @@ var "fixedCod" @@ var "body'") $
      -- Generate method declaration
      "jformalParams" <<~ (Eithers.mapList
        (lambda "pair" $
          "jdom" <<~ (encodeType @@ var "aliases2" @@ Sets.empty @@ (Pairs.first (var "pair")) @@ var "cx" @@ var "g") $
          right (JavaUtilsSource.javaTypeToJavaFormalParameter @@ var "jdom" @@ Pairs.second (var "pair")))
        (Lists.zip (var "fixedDoms") (var "params"))) $
      "jcod" <<~ (encodeType @@ var "aliases2" @@ Sets.empty @@ var "fixedCod" @@ var "cx" @@ var "g") $
      "result" <~ (JavaUtilsSource.javaTypeToJavaResult @@ var "jcod") $
      "mods" <~ list [inject Java._InterfaceMethodModifier Java._InterfaceMethodModifier_static unit] $
      "jname" <~ (JavaUtilsSource.sanitizeJavaName @@ (Formatting.decapitalize @@ (Names.localNameOf @@ var "name"))) $
      -- TCO disabled for now: instanceof chains cause significant regression vs visitor pattern.
      -- To re-enable, restore the following:
      --   "isTCO" <~ (Logic.and
      --     (Logic.not $ Lists.null (var "params"))
      --     (Analysis.isSelfTailRecursive @@ var "name" @@ var "body")) $
      "isTCO" <~ boolean False $
      "methodBody" <<~ (Logic.ifElse (var "isTCO")
        -- TCO path: wrap body in while(true) loop
        -- Note: bindingStmts (let-binding prefixes) go INSIDE the while loop so they are
        -- re-evaluated each iteration when parameters change via reassignment + continue.
        -- Create snapshot names for each parameter (e.g. term -> term_tco) so that
        -- non-continue branches can capture effectively-final variables in lambdas.
        ("tcoSuffix" <~ string "_tco" $
          "snapshotNames" <~ Lists.map ("p" ~> wrap _Name (Strings.cat2 (unwrap _Name @@ var "p") (var "tcoSuffix"))) (var "params") $
          "tcoVarRenames" <~ (Maps.fromList (Lists.zip (var "params") (var "snapshotNames"))) $
          -- Generate: final var param_tco = param;
          "snapshotDecls" <~ Lists.map ("pair" ~>
            JavaUtilsSource.finalVarDeclarationStatement
              @@ (JavaUtilsSource.variableToJavaIdentifier @@ Pairs.second (var "pair"))
              @@ (JavaUtilsSource.javaIdentifierToJavaExpression
                    @@ (JavaUtilsSource.variableToJavaIdentifier @@ Pairs.first (var "pair"))))
            (Lists.zip (var "params") (var "snapshotNames")) $
          -- For TCO, re-wrap the body with any let-bindings so encodeTermTCO handles them
          -- with the renamed environment (tcoVarRenames). This ensures let-bound lambdas
          -- capture the effectively-final snapshot variables instead of the reassigned parameters.
          "tcoBody" <~ Logic.ifElse (Lists.null (var "bindings"))
            (var "annotatedBody")
            (Core.termLet (Core.let_ (var "bindings") (var "annotatedBody"))) $
          "tcoStmts" <<~ (encodeTermTCO @@ var "env2WithTypeParams" @@ var "name" @@ var "params" @@ var "tcoVarRenames" @@ int32 0 @@ var "tcoBody" @@ var "cx" @@ var "g") $
          "whileBodyStmts" <~ Lists.concat2 (var "snapshotDecls") (var "tcoStmts") $
          "whileBodyBlock" <~ (JavaDsl.statementWithoutTrailing (JavaDsl.stmtBlock (JavaDsl.block (var "whileBodyStmts")))) $
          "noCond" <~ (nothing :: TypedTerm (Maybe Java.Expression)) $
          "whileStmt" <~ (JavaDsl.blockStatementStatement
            (JavaDsl.statementWhile (record Java._WhileStatement [
              Java._WhileStatement_cond>>: var "noCond",
              Java._WhileStatement_body>>: var "whileBodyBlock"]))) $
          right $ list [var "whileStmt"])
        -- Normal path: encode body as expression with return
        ("jbody" <<~ (encodeTerm @@ var "env3" @@ var "annotatedBody" @@ var "cx" @@ var "g") $
          "returnSt" <~ (JavaDsl.blockStatementStatement (JavaUtilsSource.javaReturnStatement @@ just (var "jbody"))) $
          right $ Lists.concat2 (var "bindingStmts") (list [var "returnSt"]))) $
      "imdMember" <~ (JavaUtilsSource.interfaceMethodDeclaration @@ var "mods" @@ var "jparams"
        @@ var "jname" @@ var "jformalParams" @@ var "result"
        @@ just (var "methodBody")) $
      right (Optionals.cases (var "mDoc") (noInterfaceComment @@ var "imdMember") (lambda "doc" $ withInterfaceCommentString @@ var "doc" @@ var "imdMember")))

-- | Internal term encoder with annotation and type-application accumulators.
encodeTermInternal :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> [M.Map Name Term] -> [Java.Type] -> Term -> InferenceContext -> Graph -> Either Error Java.Expression)
encodeTermInternal = def "encodeTermInternal" $
  lambda "env" $ lambda "anns" $ lambda "tyapps" $ lambda "term" $
    "cx" ~> "g0" ~>
    lets [
    "aliases">: project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env",
    "g">: project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_graph @@ var "env",
    "encode">: lambda "t" $ encodeTerm @@ var "env" @@ var "t" @@ var "cx" @@ var "g"] $
    cases _Term (var "term") (Just $
      right (encodeLiteral @@ inject _Literal _Literal_string (string "Unimplemented term variant")))
      [

      -- TermAnnotated: accumulate annotation, recurse
      _Term_annotated>>: lambda "at" $
        encodeTermInternal @@ var "env"
          @@ Lists.cons (Annotations.getAnnotationMap @@ Core.annotatedTermAnnotation (var "at")) (var "anns")
          @@ var "tyapps"
          @@ (Core.annotatedTermBody (var "at")) @@ var "cx" @@ var "g",

      -- TermApplication: delegate to encodeApplication
      _Term_application>>: lambda "app" $
        encodeApplication @@ var "env" @@ var "app" @@ var "cx" @@ var "g",

      -- TermEither: left or right
      -- Extract the Either type from annotations so we can pass component types to inner terms.
      -- Without this, inner lambdas lose their type context and type variables get concretized.
      _Term_either>>: lambda "et" $
        "mtargs" <<~ (Logic.ifElse (Lists.null (var "tyapps"))
          (right nothing)
          ("ta" <<~ (takeTypeArgs @@ string "either" @@ int32 2 @@ var "tyapps" @@ var "cx" @@ var "g") $
            right (just (var "ta")))) $
        "combinedAnns" <~ Lists.foldl (lambda "acc" $ lambda "m" $ Maps.union (var "acc") (var "m")) Maps.empty (var "anns") $
        "mEitherType" <<~ (getTypeE (var "cx") (var "g") (var "combinedAnns")) $
        "branchTypes" <~ (Optionals.bind (var "mEitherType") (lambda "etyp" $
          cases _Type (Strip.deannotateType @@ var "etyp")
            (Just nothing) [
            _Type_either>>: lambda "et2" $
              just (pair (Core.eitherTypeLeft (var "et2")) (Core.eitherTypeRight (var "et2")))])) $
        "encodeWithType" <~ (lambda "branchType" $ lambda "t1" $
          "annotated" <~ (Annotations.setTermAnnotation @@ asTerm Constants.keyType
            @@ just (encodeTypeAsTerm @@ var "branchType") @@ var "t1") $
          encodeTermInternal @@ var "env" @@ var "anns" @@ list ([] :: [TypedTerm Java.Type]) @@ var "annotated" @@ var "cx" @@ var "g") $
        "eitherCall" <~ (lambda "methodName" $ lambda "expr" $
          Optionals.cases (var "mtargs")
            (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocationStatic
                @@ JavaDsl.identifier (string "hydra.util.Either")
                @@ JavaDsl.identifier (var "methodName")
                @@ list [var "expr"]))
            (lambda "targs" $ JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocationStaticWithTypeArgs
                @@ JavaDsl.identifier (string "hydra.util.Either")
                @@ JavaDsl.identifier (var "methodName")
                @@ var "targs" @@ list [var "expr"]))) $
        Eithers.either_
          (lambda "term1" $
            "expr" <<~ (Optionals.cases (var "branchTypes")
              (var "encode" @@ var "term1")
              (lambda "bt" $ var "encodeWithType" @@ Pairs.first (var "bt") @@ var "term1")) $
            right (var "eitherCall" @@ string "left" @@ var "expr"))
          (lambda "term1" $
            "expr" <<~ (Optionals.cases (var "branchTypes")
              (var "encode" @@ var "term1")
              (lambda "bt" $ var "encodeWithType" @@ Pairs.second (var "bt") @@ var "term1")) $
            right (var "eitherCall" @@ string "right" @@ var "expr"))
          (var "et"),

      -- Function-form terms: encode with type from annotations.
      _Term_lambda>>: lambda "_lam" $ encodeFunctionFormTerm @@ var "env" @@ var "anns" @@ var "term" @@ var "cx" @@ var "g",
      _Term_project>>: lambda "_p" $ encodeFunctionFormTerm @@ var "env" @@ var "anns" @@ var "term" @@ var "cx" @@ var "g",
      _Term_cases>>: lambda "_c" $ encodeFunctionFormTerm @@ var "env" @@ var "anns" @@ var "term" @@ var "cx" @@ var "g",
      _Term_unwrap>>: lambda "_w" $ encodeFunctionFormTerm @@ var "env" @@ var "anns" @@ var "term" @@ var "cx" @@ var "g",

      -- TermLet: convert let bindings to block-bodied nullary lambda with .get()
      _Term_let>>: lambda "lt" $
        ("bindings" <~ Core.letBindings (var "lt") $
        "body" <~ Core.letBody (var "lt") $
        Logic.ifElse (Lists.null (var "bindings"))
          (encodeTermInternal @@ var "env" @@ var "anns" @@ list ([] :: [TypedTerm Java.Type]) @@ var "body" @@ var "cx" @@ var "g")
          ("bindResult" <<~ (bindingsToStatements @@ var "env" @@ var "bindings" @@ var "cx" @@ var "g") $
            "bindingStmts" <~ Pairs.first (var "bindResult") $
            "env2" <~ Pairs.second (var "bindResult") $
            "jbody" <<~ (encodeTermInternal @@ var "env2" @@ var "anns" @@ list ([] :: [TypedTerm Java.Type]) @@ var "body" @@ var "cx" @@ var "g") $
            "returnSt" <~ JavaDsl.blockStatementStatement (JavaUtilsSource.javaReturnStatement @@ just (var "jbody")) $
            "block" <~ (wrap Java._Block (Lists.concat2 (var "bindingStmts") (list [var "returnSt"]))) $
            "nullaryLambda" <~ JavaDsl.expressionLambda
              (JavaDsl.lambdaExpression
                (JavaDsl.lambdaParametersTuple (list ([] :: [TypedTerm Java.FormalParameter])))
                (JavaDsl.lambdaBodyBlock (var "block"))) $
            "combinedAnns" <~ Lists.foldl (lambda "acc" $ lambda "m" $ Maps.union (var "acc") (var "m")) Maps.empty (var "anns") $
            "g2" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_graph @@ var "env2") $
            "aliases2" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env2") $
            "mt" <<~ (getTypeE (var "cx") (var "g") (var "combinedAnns")) $
            "letType" <<~ (Optionals.cases (var "mt")
              (Checking.typeOfTerm @@ var "cx" @@ var "g2" @@ var "body")
              (lambda "t" $ right (var "t"))) $
            "jLetType" <<~ (encodeType @@ var "aliases2" @@ Sets.empty @@ var "letType" @@ var "cx" @@ var "g") $
            "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jLetType" @@ var "cx") $
            "supplierRt" <~ JavaDsl.referenceTypeClassOrInterface
              (JavaDsl.classOrInterfaceTypeClass
                (JavaUtilsSource.javaClassType @@ list [var "rt"] @@ asTerm JavaNamesSource.javaUtilFunctionPackageName @@ string "Supplier")) $
            "castExpr" <~ (JavaUtilsSource.javaCastExpressionToJavaExpression @@
              (JavaUtilsSource.javaCastExpression @@ var "supplierRt" @@ (JavaUtilsSource.javaExpressionToJavaUnaryExpression @@ var "nullaryLambda"))) $
            right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocation @@ just (right
                (JavaUtilsSource.javaExpressionToJavaPrimary @@ var "castExpr")) @@ JavaDsl.identifier (string "get") @@ list ([] :: [TypedTerm Java.Expression]))))),

      -- TermList: hydra.util.ConsList.of(elements) or hydra.util.ConsList.empty()
      _Term_list>>: lambda "els" $
        Logic.ifElse (Lists.null (var "els"))
          (Logic.ifElse (Lists.null (var "tyapps"))
            (right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocationStatic
                @@ JavaDsl.identifier (string "hydra.util.ConsList")
                @@ JavaDsl.identifier (string "empty")
                @@ list ([] :: [TypedTerm Java.Expression]))))
            ("targs" <<~ (takeTypeArgs @@ string "list" @@ int32 1 @@ var "tyapps" @@ var "cx" @@ var "g") $
              right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
                (JavaUtilsSource.methodInvocationStaticWithTypeArgs
                  @@ JavaDsl.identifier (string "hydra.util.ConsList")
                  @@ JavaDsl.identifier (string "empty")
                  @@ var "targs" @@ list ([] :: [TypedTerm Java.Expression])))))
          ("jels" <<~ (Eithers.mapList (var "encode") (var "els")) $
            right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocationStatic
                @@ JavaDsl.identifier (string "hydra.util.ConsList")
                @@ JavaDsl.identifier (string "of")
                @@ var "jels"))),

      -- TermLiteral: direct encoding
      _Term_literal>>: lambda "l" $
        right (encodeLiteral @@ var "l"),

      -- TermMap: hydra.util.PersistentMap.ofEntries(java.util.Map.entry(k,v), ...) or hydra.util.PersistentMap.empty()
      _Term_map>>: lambda "m" $
        Logic.ifElse (Maps.null (var "m"))
          (Logic.ifElse (Lists.null (var "tyapps"))
            (right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocationStatic
                @@ JavaDsl.identifier (string "hydra.util.PersistentMap")
                @@ JavaDsl.identifier (string "empty")
                @@ list ([] :: [TypedTerm Java.Expression]))))
            ("targs" <<~ (takeTypeArgs @@ string "map" @@ int32 2 @@ var "tyapps" @@ var "cx" @@ var "g") $
              right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
                (JavaUtilsSource.methodInvocationStaticWithTypeArgs
                  @@ JavaDsl.identifier (string "hydra.util.PersistentMap")
                  @@ JavaDsl.identifier (string "empty")
                  @@ var "targs" @@ list ([] :: [TypedTerm Java.Expression])))))
          ("jkeys" <<~ (Eithers.mapList (var "encode") (Maps.keys (var "m"))) $
            "jvals" <<~ (Eithers.mapList (var "encode") (Maps.elems (var "m"))) $
            "pairExprs" <~ Lists.map
              (lambda "kv" $ JavaUtilsSource.javaMethodInvocationToJavaExpression @@
                (JavaUtilsSource.methodInvocationStatic
                  @@ JavaDsl.identifier (string "java.util.Map")
                  @@ JavaDsl.identifier (string "entry")
                  @@ list [Pairs.first (var "kv"), Pairs.second (var "kv")]))
              (Lists.zip (var "jkeys") (var "jvals")) $
            right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocationStatic
                @@ JavaDsl.identifier (string "hydra.util.PersistentMap")
                @@ JavaDsl.identifier (string "ofEntries")
                @@ var "pairExprs"))),

      -- TermOptional: Maybe.nothing() or Maybe.just(x)
      _Term_optional>>: lambda "mt" $
        Optionals.cases (var "mt")
          (Logic.ifElse (Lists.null (var "tyapps"))
            (right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocationStatic
                @@ JavaDsl.identifier (string "hydra.util.Maybe")
                @@ JavaDsl.identifier (string "nothing")
                @@ list ([] :: [TypedTerm Java.Expression]))))
            ("targs" <<~ (takeTypeArgs @@ string "maybe" @@ int32 1 @@ var "tyapps" @@ var "cx" @@ var "g") $
              right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
                (JavaUtilsSource.methodInvocationStaticWithTypeArgs
                  @@ JavaDsl.identifier (string "hydra.util.Maybe")
                  @@ JavaDsl.identifier (string "nothing")
                  @@ var "targs" @@ list ([] :: [TypedTerm Java.Expression])))))
          (lambda "term1" $
            "expr" <<~ (var "encode" @@ var "term1") $
            right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocationStatic
                @@ JavaDsl.identifier (string "hydra.util.Maybe")
                @@ JavaDsl.identifier (string "just")
                @@ list [var "expr"]))),

      -- TermPair: new Pair(t1, t2)
      _Term_pair>>: lambda "p" $
        "jterm1" <<~ (var "encode" @@ Pairs.first (var "p")) $
        "jterm2" <<~ (var "encode" @@ Pairs.second (var "p")) $
        "mtargs" <<~ (Logic.ifElse (Lists.null (var "tyapps"))
          (right nothing)
          ("rts" <<~ (Eithers.mapList (lambda "jt" $ JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt" @@ var "cx") (var "tyapps")) $
            right (just (JavaDsl.typeArgumentsOrDiamondArguments
              (Lists.map (lambda "rt" $ JavaDsl.typeArgumentReference (var "rt")) (var "rts")))))) $
        right (JavaUtilsSource.javaConstructorCall
          @@ (JavaUtilsSource.javaConstructorName @@ JavaDsl.identifier (string "hydra.util.Pair") @@ var "mtargs")
          @@ list [var "jterm1", var "jterm2"] @@ nothing),

      -- TermRecord: new RecordType(field1, field2, ...)
      -- When tyapps is non-empty (from TermTypeApplication wrappers), use those directly.
      -- When tyapps is empty, fall back to extracting type args from the annotation type.
      -- This handles cases like unitCoder where the record has concrete type parameters
      -- (e.g. Coder<Graph, Graph, Term, Value>) but no TermTypeApplication wrappers in the term.
      _Term_record>>: lambda "rec" $
        "recName" <~ Core.recordTypeName (var "rec") $
        -- Try to resolve the record type from the graph to get expected field types.
        -- This allows us to annotate field terms with their expected types, preventing
        -- type variable concretization in polymorphic lambdas (e.g. Coder encode/decode fields).
        "mRecordType" <~ Eithers.either_ (constant nothing) ("t" ~> just (var "t"))
          (Resolution.requireType @@ var "cx" @@ var "g" @@ var "recName") $
        "strippedRecTyp" <~ Optionals.map (lambda "recTyp" $ stripForalls @@ (Strip.deannotateType @@ var "recTyp"))
          (var "mRecordType") $
        "mFieldTypeMap" <~ (Optionals.bind (var "strippedRecTyp") (lambda "bodyTyp" $
          cases _Type (var "bodyTyp")
            (Just nothing) [
            _Type_record>>: lambda "rt" $
              just (Maps.fromList (Lists.map
                (lambda "ft" $ pair (Core.fieldTypeName (var "ft")) (Core.fieldTypeType (var "ft")))
                (var "rt")))])) $
        -- Build type variable substitution from the annotation type's type arguments
        "combinedAnnsRec" <~ Lists.foldl (lambda "acc" $ lambda "m" $ Maps.union (var "acc") (var "m")) Maps.empty (var "anns") $
        "mAnnotType" <<~ (getTypeE (var "cx") (var "g") (var "combinedAnnsRec")) $
        "mTypeSubst" <~ (Optionals.bind (var "mAnnotType") (lambda "annTyp" $
          Optionals.bind (var "mRecordType") (lambda "recTyp" $
            -- Extract type args from annotation and type params from definition
            "args" <~ (extractTypeApplicationArgs @@ (Strip.deannotateType @@ var "annTyp")) $
            "params" <~ (collectForallParams @@ (Strip.deannotateType @@ var "recTyp")) $
            Logic.ifElse (Logic.or (Lists.null (var "args")) (Logic.not (Equality.equal (Lists.length (var "args")) (Lists.length (var "params")))))
              nothing
              (just (Maps.fromList (Lists.zip (var "params") (var "args"))))))) $
        "encodeField" <~ (lambda "fld" $
          Optionals.cases (var "mFieldTypeMap")
            (var "encode" @@ Core.fieldTerm (var "fld"))
            (lambda "ftmap" $
              "mftyp" <~ Maps.lookup (Core.fieldName (var "fld")) (var "ftmap") $
              Optionals.cases (var "mftyp")
                (var "encode" @@ Core.fieldTerm (var "fld"))
                (lambda "ftyp" $
                  -- Apply type substitution to the field type if available
                  "resolvedType" <~ Optionals.cases (var "mTypeSubst")
                    (var "ftyp")
                    (lambda "subst" $ applySubstFull @@ var "subst" @@ var "ftyp") $
                  -- Annotate the field term with the resolved type before encoding
                  "annotatedFieldTerm" <~ (Annotations.setTermAnnotation @@ asTerm Constants.keyType
                    @@ just (encodeTypeAsTerm @@ var "resolvedType") @@ Core.fieldTerm (var "fld")) $
                  encodeTermInternal @@ var "env" @@ var "anns" @@ list ([] :: [TypedTerm Java.Type]) @@ var "annotatedFieldTerm" @@ var "cx" @@ var "g"))) $
        "fieldExprs" <<~ (Eithers.mapList (var "encodeField") (Core.recordFields (var "rec"))) $
        "consId" <~ (JavaUtilsSource.nameToJavaName @@ var "aliases" @@ var "recName") $
        "mtargs" <<~ (Logic.ifElse (Logic.not (Lists.null (var "tyapps")))
          ("rts" <<~ (Eithers.mapList (lambda "jt" $ JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt" @@ var "cx") (var "tyapps")) $
            right (just (JavaDsl.typeArgumentsOrDiamondArguments
              (Lists.map (lambda "rt" $ JavaDsl.typeArgumentReference (var "rt")) (var "rts")))))
          -- tyapps is empty: try to extract type args from annotation
          ("combinedAnns" <~ Lists.foldl (lambda "acc" $ lambda "m" $ Maps.union (var "acc") (var "m")) Maps.empty (var "anns") $
           "mtyp" <<~ (getTypeE (var "cx") (var "g") (var "combinedAnns")) $
           Optionals.cases (var "mtyp")
             (right nothing)
             (lambda "annTyp" $
               "typeArgs" <~ (extractTypeApplicationArgs @@ (Strip.deannotateType @@ var "annTyp")) $
               Logic.ifElse (Lists.null (var "typeArgs"))
                 (right nothing)
                 ("jTypeArgs" <<~ (Eithers.mapList (lambda "t" $
                   "jt" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "t" @@ var "cx" @@ var "g") $
                   JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt" @@ var "cx") (var "typeArgs")) $
                  right (just (JavaDsl.typeArgumentsOrDiamondArguments
                    (Lists.map (lambda "rt" $ JavaDsl.typeArgumentReference (var "rt")) (var "jTypeArgs")))))))) $
        right (JavaUtilsSource.javaConstructorCall
          @@ (JavaUtilsSource.javaConstructorName @@ var "consId" @@ var "mtargs")
          @@ var "fieldExprs" @@ nothing),

      -- TermSet: hydra.util.PersistentSet.of(elements) or hydra.util.PersistentSet.empty()
      _Term_set>>: lambda "s" $
        Logic.ifElse (Sets.null (var "s"))
          (Logic.ifElse (Lists.null (var "tyapps"))
            (right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocationStatic
                @@ JavaDsl.identifier (string "hydra.util.PersistentSet")
                @@ JavaDsl.identifier (string "empty")
                @@ list ([] :: [TypedTerm Java.Expression]))))
            ("targs" <<~ (takeTypeArgs @@ string "set" @@ int32 1 @@ var "tyapps" @@ var "cx" @@ var "g") $
              right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
                (JavaUtilsSource.methodInvocationStaticWithTypeArgs
                  @@ JavaDsl.identifier (string "hydra.util.PersistentSet")
                  @@ JavaDsl.identifier (string "empty")
                  @@ var "targs" @@ list ([] :: [TypedTerm Java.Expression])))))
          ("slist" <~ Sets.toList (var "s") $
            "jels" <<~ (Eithers.mapList (var "encode") (var "slist")) $
            right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocationStatic
                @@ JavaDsl.identifier (string "hydra.util.PersistentSet")
                @@ JavaDsl.identifier (string "of")
                @@ var "jels"))),

      -- TermTypeLambda: enter type lambda scope
      _Term_typeLambda>>: lambda "tl" $
        withTypeLambda @@ var "env" @@ var "tl" @@ (lambda "env2" $
          "combinedAnns" <~ Lists.foldl (lambda "acc" $ lambda "m" $ Maps.union (var "acc") (var "m")) Maps.empty (var "anns") $
          "mtyp" <<~ (getTypeE (var "cx") (var "g") (var "combinedAnns")) $
          "annotatedBody" <~ (Optionals.cases (var "mtyp")
            (Core.typeLambdaBody (var "tl"))
            (lambda "t" $ cases _Type (var "t") (Just $ Core.typeLambdaBody (var "tl")) [
              _Type_forall>>: lambda "fa" $
                Annotations.setTermAnnotation @@ asTerm Constants.keyType
                  @@ just (encodeTypeAsTerm @@ Core.forallTypeBody (var "fa"))
                  @@ Core.typeLambdaBody (var "tl")])) $
          encodeTerm @@ var "env2" @@ var "annotatedBody" @@ var "cx" @@ var "g"),

      -- TermInject: new Variant(args)
      _Term_inject>>: lambda "inj" $
        "injTypeName" <~ Core.injectionTypeName (var "inj") $
        "injField" <~ Core.injectionField (var "inj") $
        "injFieldName" <~ Core.fieldName (var "injField") $
        "injFieldTerm" <~ Core.fieldTerm (var "injField") $
        "typeId" <~ JavaDsl.unIdentifier (JavaUtilsSource.nameToJavaName @@ var "aliases" @@ var "injTypeName") $
        "consId" <~ JavaDsl.identifier (Strings.cat (list [var "typeId", string ".", JavaUtilsSource.sanitizeJavaName @@ (Formatting.capitalize @@ (unwrap _Name @@ var "injFieldName"))])) $
        "fieldIsUnit" <<~ (isFieldUnitType @@ var "injTypeName" @@ var "injFieldName" @@ var "cx" @@ var "g") $
        "args" <<~ (Logic.ifElse (Logic.or (Predicates.isUnitTerm @@ (Strip.deannotateTerm @@ var "injFieldTerm")) (var "fieldIsUnit"))
          (right (list ([] :: [TypedTerm Java.Expression])))
          ("ex" <<~ (var "encode" @@ var "injFieldTerm") $
            right (list [var "ex"]))) $
        right (JavaUtilsSource.javaConstructorCall
          @@ (JavaUtilsSource.javaConstructorName @@ var "consId" @@ nothing)
          @@ var "args" @@ nothing),

      -- TermVariable: encode variable reference, or handle as primitive if it resolves to one
      _Term_variable>>: lambda "name" $
        Optionals.cases (Maps.lookup (var "name") (Graph.graphPrimitives (var "g")))
          (encodeVariable @@ var "env" @@ var "name" @@ var "cx" @@ var "g")
          (lambda "_prim" $
            "combinedAnns" <~ Lists.foldl (lambda "acc" $ lambda "m" $ Maps.union (var "acc") (var "m")) Maps.empty (var "anns") $
            "mt" <<~ (getTypeE (var "cx") (var "g") (var "combinedAnns")) $
            "typ" <<~ (Optionals.cases (var "mt")
              (Checking.typeOfTerm @@ var "cx" @@ var "g" @@ var "term")
              (lambda "t" $ right (var "t"))) $
            cases _Type (Strip.deannotateType @@ var "typ")
              (Just $ encodeNullaryPrimitiveByName @@ var "env" @@ var "typ" @@ var "name" @@ var "cx" @@ var "g") [
              _Type_function>>: lambda "ft" $
                encodeFunctionPrimitiveByName @@ var "env" @@ (Core.functionTypeDomain (var "ft")) @@ (Core.functionTypeCodomain (var "ft")) @@ var "name" @@ var "cx" @@ var "g"]),

      -- TermUnit: emit null
      _Term_unit>>: lambda "_" $
        right (JavaUtilsSource.javaLiteralToJavaExpression @@ JavaDsl.literalNull),

      -- TermWrap: new WrapperType(arg)
      _Term_wrap>>: lambda "wt" $
        "jarg" <<~ (var "encode" @@ Core.wrappedTermBody (var "wt")) $
        right (JavaUtilsSource.javaConstructorCall
          @@ (JavaUtilsSource.javaConstructorName @@ (JavaUtilsSource.nameToJavaName @@ var "aliases" @@ Core.wrappedTermTypeName (var "wt")) @@ nothing)
          @@ list [var "jarg"] @@ nothing),

      -- TermTypeApplication: handle type casts with correctCastType for pair types
      _Term_typeApplication>>: lambda "ta" $
        "atyp" <~ Core.typeApplicationTermType (var "ta") $
        "body" <~ Core.typeApplicationTermBody (var "ta") $
        "jatyp" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "atyp" @@ var "cx" @@ var "g") $
        "combinedAnns" <~ Lists.foldl (lambda "acc" $ lambda "m" $ Maps.union (var "acc") (var "m")) Maps.empty (var "anns") $
        "mtyp" <<~ (getTypeE (var "cx") (var "g") (var "combinedAnns")) $
        "typ" <<~ (Optionals.cases (var "mtyp")
          (Checking.typeOfTerm @@ var "cx" @@ var "g" @@ var "term")
          (lambda "t" $ right (var "t"))) $
        -- Collect all nested type applications (preserving annotations)
        "collected0" <~ (collectTypeApps0 @@ var "body" @@ list [var "atyp"]) $
        "innermostBody0" <~ Pairs.first (var "collected0") $
        "allTypeArgs0" <~ Pairs.second (var "collected0") $
        -- Correct the type for pair terms
        "correctedTyp" <<~ (correctCastType @@ var "innermostBody0" @@ var "allTypeArgs0" @@ var "typ" @@ var "cx" @@ var "g") $
        -- Collect all nested type applications (stripping annotations)
        "collected" <~ (collectTypeApps @@ var "body" @@ list [var "atyp"]) $
        "innermostBody" <~ Pairs.first (var "collected") $
        "allTypeArgs" <~ Pairs.second (var "collected") $
        -- Classify the innermost body if it's a variable or an Either injection
        cases _Term (var "innermostBody")
          (Just $ typeAppFallbackCast @@ var "env" @@ var "aliases" @@ var "anns" @@ var "tyapps"
            @@ var "jatyp" @@ var "body" @@ var "correctedTyp" @@ var "cx" @@ var "g") [
          _Term_variable>>: lambda "varName" $
            "cls" <<~ (classifyDataReference @@ var "varName" @@ var "cx" @@ var "g") $
            typeAppNullaryOrHoisted @@ var "env" @@ var "aliases" @@ var "anns" @@ var "tyapps"
              @@ var "jatyp" @@ var "body" @@ var "correctedTyp" @@ var "varName"
              @@ var "cls" @@ var "allTypeArgs" @@ var "cx" @@ var "g",
          -- Either injections: pass branch types from the TypeApp type args
          -- so inner lambdas don't lose their type variables
          _Term_either>>: lambda "eitherTerm" $
            Logic.ifElse (Equality.equal (Lists.length (var "allTypeArgs")) (int32 2))
              -- We have exactly 2 type args: the Either type parameters
              -- allTypeArgs order depends on collectTypeApps, which prepends: [outerType, innerType]
              -- For Either<L,R>, TypeApp(typeR, TypeApp(typeL, Either(...))):
              --   outer TypeApp has typeR (right branch type), inner has typeL (left branch type)
              --   collectTypeApps starts with [atyp] and prepends, so allTypeArgs = [typeL, typeR]
              ("eitherBranchTypes" <~ pair
                  (Optionals.fromOptional (var "correctedTyp") (Lists.maybeAt (int32 0) (var "allTypeArgs")))
                  (Optionals.fromOptional (var "correctedTyp") (Lists.maybeAt (int32 1) (var "allTypeArgs"))) $
                "jTypeArgs" <<~ (Eithers.mapList (lambda "t" $
                  "jt" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "t" @@ var "cx" @@ var "g") $
                  JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt" @@ var "cx") (var "allTypeArgs")) $
                "eitherTargs" <~ Lists.map (lambda "rt" $ JavaDsl.typeArgumentReference (var "rt")) (var "jTypeArgs") $
                "encodeEitherBranch" <~ (lambda "branchType" $ lambda "t1" $
                  "annotated" <~ (Annotations.setTermAnnotation @@ asTerm Constants.keyType
                    @@ just (encodeTypeAsTerm @@ var "branchType") @@ var "t1") $
                  encodeTermInternal @@ var "env" @@ var "anns" @@ list ([] :: [TypedTerm Java.Type]) @@ var "annotated" @@ var "cx" @@ var "g") $
                Eithers.either_
                  (lambda "term1" $
                    "expr" <<~ (var "encodeEitherBranch" @@ Pairs.first (var "eitherBranchTypes") @@ var "term1") $
                    right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
                      (JavaUtilsSource.methodInvocationStaticWithTypeArgs
                        @@ JavaDsl.identifier (string "hydra.util.Either")
                        @@ JavaDsl.identifier (string "left")
                        @@ var "eitherTargs" @@ list [var "expr"])))
                  (lambda "term1" $
                    "expr" <<~ (var "encodeEitherBranch" @@ Pairs.second (var "eitherBranchTypes") @@ var "term1") $
                    right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
                      (JavaUtilsSource.methodInvocationStaticWithTypeArgs
                        @@ JavaDsl.identifier (string "hydra.util.Either")
                        @@ JavaDsl.identifier (string "right")
                        @@ var "eitherTargs" @@ list [var "expr"])))
                  (var "eitherTerm"))
              -- Fallback if we don't have exactly 2 type args
              (typeAppFallbackCast @@ var "env" @@ var "aliases" @@ var "anns" @@ var "tyapps"
                @@ var "jatyp" @@ var "body" @@ var "correctedTyp" @@ var "cx" @@ var "g")
          ]
        ]

-- | Encode a term for TCO: self-tail-calls become param reassignment + continue.
--   Returns a list of Java BlockStatements (if/instanceof checks + return/continue).
--   tcoVarRenames maps original parameter names to snapshot names (e.g. term -> term_tco).
--   Non-continue paths use the snapshot names so lambdas capture effectively-final variables.
encodeTermTCO :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> Name -> [Name] -> M.Map Name Name -> Int -> Term -> InferenceContext -> Graph -> Either Error [Java.BlockStatement])
encodeTermTCO = def "encodeTermTCO" $
  "env0" ~> "funcName" ~> "paramNames" ~> "tcoVarRenames" ~> "tcoDepth" ~> "term" ~>
    "cx" ~> "g" ~>
    -- Apply varRenames to the environment so encodeTerm uses snapshot variable names
    "aliases0" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env0") $
    "env" <~ (record JavaHelpers._JavaEnvironment [
      JavaHelpers._JavaEnvironment_aliases>>: record JavaHelpers._Aliases [
        JavaHelpers._Aliases_currentNamespace>>:
          project JavaHelpers._Aliases JavaHelpers._Aliases_currentNamespace @@ var "aliases0",
        JavaHelpers._Aliases_packages>>:
          project JavaHelpers._Aliases JavaHelpers._Aliases_packages @@ var "aliases0",
        JavaHelpers._Aliases_branchVars>>:
          project JavaHelpers._Aliases JavaHelpers._Aliases_branchVars @@ var "aliases0",
        JavaHelpers._Aliases_recursiveVars>>:
          project JavaHelpers._Aliases JavaHelpers._Aliases_recursiveVars @@ var "aliases0",
        JavaHelpers._Aliases_inScopeTypeParams>>:
          project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeTypeParams @@ var "aliases0",
        JavaHelpers._Aliases_polymorphicLocals>>:
          project JavaHelpers._Aliases JavaHelpers._Aliases_polymorphicLocals @@ var "aliases0",
        JavaHelpers._Aliases_inScopeJavaVars>>:
          project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeJavaVars @@ var "aliases0",
        JavaHelpers._Aliases_varRenames>>:
          Maps.union (var "tcoVarRenames") (project JavaHelpers._Aliases JavaHelpers._Aliases_varRenames @@ var "aliases0"),
        JavaHelpers._Aliases_lambdaVars>>:
          project JavaHelpers._Aliases JavaHelpers._Aliases_lambdaVars @@ var "aliases0",
        JavaHelpers._Aliases_typeVarSubst>>:
          project JavaHelpers._Aliases JavaHelpers._Aliases_typeVarSubst @@ var "aliases0",
        JavaHelpers._Aliases_trustedTypeVars>>:
          project JavaHelpers._Aliases JavaHelpers._Aliases_trustedTypeVars @@ var "aliases0",
        JavaHelpers._Aliases_methodCodomain>>:
          project JavaHelpers._Aliases JavaHelpers._Aliases_methodCodomain @@ var "aliases0",
        JavaHelpers._Aliases_thunkedVars>>:
          project JavaHelpers._Aliases JavaHelpers._Aliases_thunkedVars @@ var "aliases0"],
      JavaHelpers._JavaEnvironment_graph>>:
        project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_graph @@ var "env0"]) $
    "stripped" <~ (Strip.deannotateAndDetypeTerm @@ var "term") $
    -- Check if this term is a direct self-tail-call: funcName(args...)
    "gathered" <~ (Analysis.gatherApplications @@ var "stripped") $
    "gatherArgs" <~ (Pairs.first $ var "gathered") $
    "gatherFun" <~ (Pairs.second $ var "gathered") $
    "strippedFun" <~ (Strip.deannotateAndDetypeTerm @@ var "gatherFun") $
    -- Check for self-call pattern: Variable(funcName)
    "isSelfCall" <~ (cases _Term (var "strippedFun")
      (Just false) [
        _Term_variable>>: "n" ~> Equality.equal (var "n") (var "funcName")]) $
    Logic.ifElse (Logic.and (var "isSelfCall")
                            (Equality.equal (Lists.length $ var "gatherArgs") (Lists.length $ var "paramNames")))
      -- TAIL CALL: emit param reassignment + continue
      (-- Filter out self-assignments (e.g. x = x) so params remain effectively final for lambdas
        "changePairs" <~ Lists.filter ("pair" ~>
          Logic.not (cases _Term (Strip.deannotateAndDetypeTerm @@ Pairs.second (var "pair"))
            (Just false) [
            _Term_variable>>: "n" ~> Equality.equal (var "n") (Pairs.first (var "pair"))]))
          (Lists.zip (var "paramNames") (var "gatherArgs")) $
        "changedParams" <~ Lists.map (reify Pairs.first) (var "changePairs") $
        "jChangedArgs" <<~ Eithers.mapList ("pair" ~> encodeTerm @@ var "env" @@ (Pairs.second (var "pair")) @@ var "cx" @@ var "g")
          (var "changePairs") $
        "assignments" <~ (Lists.map ("pair" ~>
          "paramName" <~ (Pairs.first $ var "pair") $
          "jArg" <~ (Pairs.second $ var "pair") $
          JavaDsl.blockStatementStatement
            (JavaUtilsSource.javaAssignmentStatement
              @@ (JavaDsl.leftHandSideExpressionName
                    (JavaUtilsSource.javaIdentifierToJavaExpressionName
                      @@ (JavaUtilsSource.variableToJavaIdentifier @@ var "paramName")))
              @@ var "jArg"))
          (Lists.zip (var "changedParams") (var "jChangedArgs"))) $
        "continueStmt" <~ (JavaDsl.blockStatementStatement
          (JavaDsl.statementWithoutTrailing
            (inject Java._StatementWithoutTrailingSubstatement Java._StatementWithoutTrailingSubstatement_continue
              (wrap Java._ContinueStatement (nothing :: TypedTerm (Maybe Java.Identifier)))))) $
        right $ Lists.concat2 (var "assignments") (list [var "continueStmt"]))
      -- NOT a self-call: check for let-expression or case statement application
      (cases _Term (var "stripped")
        (Just $
          -- Default: check for case statement application
          "gathered2" <~ (Analysis.gatherApplications @@ var "term") $
        "args2" <~ (Pairs.first $ var "gathered2") $
        "body2" <~ (Pairs.second $ var "gathered2") $
        Logic.ifElse (Equality.equal (Lists.length $ var "args2") (int32 1))
          -- Single argument: try to match as case statement
          ("arg" <~ (Optionals.fromOptional Core.termUnit (Lists.maybeHead $ var "args2")) $
            cases _Term (Strip.deannotateAndDetypeTerm @@ var "body2") (Just $
              -- Default: not a case statement, encode as return
              "expr" <<~ (encodeTerm @@ var "env" @@ var "term" @@ var "cx" @@ var "g") $
              right $ list [JavaDsl.blockStatementStatement (JavaUtilsSource.javaReturnStatement @@ just (var "expr"))]) [
              _Term_cases>>: "cs" ~>
                        -- Case statement: generate if/instanceof chain
                        "aliases" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env") $
                        "tname" <~ (Core.caseStatementTypeName $ var "cs") $
                        "dflt" <~ (Core.caseStatementDefault $ var "cs") $
                        "cases_" <~ (Core.caseStatementCases $ var "cs") $
                        "domArgs" <<~ (domTypeArgs @@ var "aliases" @@ (Resolution.nominalApplication @@ var "tname" @@ list ([] :: [TypedTerm Type])) @@ var "cx" @@ var "g") $
                        -- Encode the argument (the value being matched) and cache in a local variable
                        -- so that complex expressions (e.g. deannotateType(t_tco)) are computed once,
                        -- not duplicated across every instanceof check and cast.
                        "jArgRaw" <<~ (encodeTerm @@ var "env" @@ var "arg" @@ var "cx" @@ var "g") $
                        "depthSuffix" <~ Logic.ifElse (Equality.equal (var "tcoDepth") (int32 0))
                          (string "")
                          (Literals.showInt32 (var "tcoDepth")) $
                        "matchVarId" <~ (JavaUtilsSource.javaIdentifier @@
                          Strings.cat (list [string "_tco_match_", Formatting.decapitalize @@ (Names.localNameOf @@ var "tname"), var "depthSuffix"])) $
                        "matchDecl" <~ (JavaUtilsSource.varDeclarationStatement @@ var "matchVarId" @@ var "jArgRaw") $
                        "jArg" <~ (JavaUtilsSource.javaIdentifierToJavaExpression @@ var "matchVarId") $
                        -- Generate if/instanceof blocks for each case branch
                        "ifBlocks" <<~ (Eithers.mapList ("field" ~>
                          "fieldName" <~ (Core.caseAlternativeName (var "field")) $
                          -- Build the variant reference type for instanceof
                          "variantRefType" <~ (JavaUtilsSource.nameToJavaReferenceType @@ var "aliases" @@ true @@ var "domArgs"
                            @@ var "tname" @@ just (Formatting.capitalize @@ (Core.unName (var "fieldName")))) $
                          -- Extract the lambda body from this case branch
                          cases _Term (Strip.deannotateTerm @@ Core.caseAlternativeHandler (var "field"))
                            (Just $ left (Error.errorOther $ Error.otherError $ string "TCO: case branch is not a lambda")) [
                            _Term_lambda>>: "lam" ~>
                                  -- Use withLambda to properly set up the lambda context
                                  withLambda @@ var "env" @@ var "lam" @@ (lambda "env2" $
                                  "lambdaParam" <~ Core.lambdaParameter (var "lam") $
                                  "branchBody" <~ Core.lambdaBody (var "lam") $
                                  "env3" <~ (insertBranchVar @@ var "lambdaParam" @@ var "env2") $
                                  -- Build local var: VariantType varName = (VariantType) arg;
                                  "varId" <~ (JavaUtilsSource.variableToJavaIdentifier @@ var "lambdaParam") $
                                  "castExpr" <~ (JavaUtilsSource.javaCastExpressionToJavaExpression
                                    @@ (JavaUtilsSource.javaCastExpression @@ var "variantRefType"
                                          @@ (JavaUtilsSource.javaExpressionToJavaUnaryExpression @@ var "jArg"))) $
                                  "localDecl" <~ (JavaUtilsSource.varDeclarationStatement @@ var "varId" @@ var "castExpr") $
                                  -- Check if this branch body is a self-tail-call
                                  "isBranchTailCall" <~ (Analysis.isTailRecursiveInTailPosition @@ var "funcName" @@ var "branchBody") $
                                  "bodyStmts" <<~ (Logic.ifElse (var "isBranchTailCall")
                                    -- Self-call: emit assignment + continue via encodeTermTCO
                                    (encodeTermTCO @@ var "env3" @@ var "funcName" @@ var "paramNames" @@ var "tcoVarRenames" @@ (Math.add (var "tcoDepth") (int32 1)) @@ var "branchBody" @@ var "cx" @@ var "g")
                                    -- Not a self-call: use normal encoding with analyzeJavaFunction
                                    ("fs" <<~ (analyzeJavaFunction @@ var "env3" @@ var "branchBody" @@ var "cx" @@ var "g") $
                                      "bindings" <~ (project _FunctionStructure _FunctionStructure_bindings @@ var "fs") $
                                      "innerBody" <~ (project _FunctionStructure _FunctionStructure_body @@ var "fs") $
                                      "env4" <~ (project _FunctionStructure _FunctionStructure_environment @@ var "fs") $
                                      "bindResult" <<~ (bindingsToStatements @@ var "env4" @@ var "bindings" @@ var "cx" @@ var "g") $
                                      "bindingStmts" <~ Pairs.first (var "bindResult") $
                                      "env5" <~ Pairs.second (var "bindResult") $
                                      "jret" <<~ (encodeTerm @@ var "env5" @@ var "innerBody" @@ var "cx" @@ var "g") $
                                      "returnStmt" <~ (JavaDsl.blockStatementStatement (JavaUtilsSource.javaReturnStatement @@ just (var "jret"))) $
                                      right $ Lists.concat2 (var "bindingStmts") (list [var "returnStmt"]))) $
                                  -- Build: if (arg instanceof VariantType) { VariantType v = (VariantType) arg; stmts... }
                                  -- Use jArg (the encoded case argument) for the instanceof check,
                                  -- not paramNames[0], which may differ from the actual matched variable
                                  "relExpr" <~ (JavaUtilsSource.javaInstanceOf
                                    @@ (JavaUtilsSource.javaUnaryExpressionToJavaRelationalExpression
                                          @@ (JavaUtilsSource.javaExpressionToJavaUnaryExpression @@ var "jArg"))
                                    @@ var "variantRefType") $
                                  "condExpr" <~ (JavaUtilsSource.javaRelationalExpressionToJavaExpression @@ var "relExpr") $
                                  "blockStmts" <~ Lists.cons (var "localDecl") (var "bodyStmts") $
                                  "ifBody" <~ (JavaDsl.statementWithoutTrailing
                                    (JavaDsl.stmtBlock (JavaDsl.block (var "blockStmts")))) $
                                  right $ JavaDsl.blockStatementStatement
                                    (JavaDsl.statementIfThen (JavaDsl.ifThenStatement (var "condExpr") (var "ifBody"))))])
                          (var "cases_")) $
                        -- Default: return the expression (or the arg for otherwise)
                        "defaultStmt" <<~ (Optionals.cases (var "dflt")
                          -- No default: return the argument unchanged
                          (right $ list [JavaDsl.blockStatementStatement
                            (JavaUtilsSource.javaReturnStatement @@ just (var "jArg"))])
                          ("d" ~>
                            -- Default is a value to return, not a function to apply to the argument
                            "dExpr" <<~ (encodeTerm @@ var "env" @@ var "d" @@ var "cx" @@ var "g") $
                            right $ list [JavaDsl.blockStatementStatement
                              (JavaUtilsSource.javaReturnStatement @@ just (var "dExpr"))])) $
                        right $ Lists.concat (list [list [var "matchDecl"], var "ifBlocks", var "defaultStmt"])])
          -- Not a single-arg application: fall back to normal return
          ("expr" <<~ (encodeTerm @@ var "env" @@ var "term" @@ var "cx" @@ var "g") $
            right $ list [JavaDsl.blockStatementStatement (JavaUtilsSource.javaReturnStatement @@ just (var "expr"))])) [
        -- Let-expression: encode bindings as statements, then recurse on body
        _Term_let>>: "lt" ~>
          "letBindings" <~ Core.letBindings (var "lt") $
          "letBody" <~ Core.letBody (var "lt") $
          "bindResult" <<~ (bindingsToStatements @@ var "env" @@ var "letBindings" @@ var "cx" @@ var "g") $
          "letStmts" <~ Pairs.first (var "bindResult") $
          "envAfterLet" <~ Pairs.second (var "bindResult") $
          "tcoBodyStmts" <<~ (encodeTermTCO @@ var "envAfterLet" @@ var "funcName" @@ var "paramNames" @@ var "tcoVarRenames" @@ var "tcoDepth" @@ var "letBody" @@ var "cx" @@ var "g") $
          right $ Lists.concat2 (var "letStmts") (var "tcoBodyStmts")])

-- | Encode a Hydra type as a Java type
encodeType :: TypedTermDefinition (JavaHelpers.Aliases -> S.Set Name -> Type -> InferenceContext -> Graph -> Either Error Java.Type)
encodeType = def "encodeType" $
  lambda "aliases" $ lambda "boundVars" $ lambda "t" $
    "cx" ~> "g" ~>
    "inScopeTypeParams" <~ project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeTypeParams @@ var "aliases" $
    "typeVarSubst" <~ project JavaHelpers._Aliases JavaHelpers._Aliases_typeVarSubst @@ var "aliases" $
    cases _Type (Strip.deannotateType @@ var "t")
      (Just $ left (Error.errorOther $ Error.otherError $ Strings.cat2 (string "can't encode unsupported type in Java: ") (ShowCore.type_ @@ var "t"))) [
      _Type_application>>: lambda "at" $
        "jlhs" <<~ (encodeType @@ var "aliases" @@ var "boundVars" @@ (Core.applicationTypeFunction (var "at")) @@ var "cx" @@ var "g") $
        "jrhs" <<~ (Eithers.bind
          (encodeType @@ var "aliases" @@ var "boundVars" @@ (Core.applicationTypeArgument (var "at")) @@ var "cx" @@ var "g")
          (lambda "jt_" $ JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt_" @@ var "cx")) $
        JavaUtilsSource.addJavaTypeParameter @@ var "jrhs" @@ var "jlhs" @@ var "cx",
      _Type_function>>: lambda "ft" $
        "jdom" <<~ (Eithers.bind
          (encodeType @@ var "aliases" @@ var "boundVars" @@ (Core.functionTypeDomain (var "ft")) @@ var "cx" @@ var "g")
          (lambda "jt_" $ JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt_" @@ var "cx")) $
        "jcod" <<~ (Eithers.bind
          (encodeType @@ var "aliases" @@ var "boundVars" @@ (Core.functionTypeCodomain (var "ft")) @@ var "cx" @@ var "g")
          (lambda "jt_" $ JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt_" @@ var "cx")) $
        right (JavaUtilsSource.javaRefType
          @@ list [var "jdom", var "jcod"]
          @@ asTerm JavaNamesSource.javaUtilFunctionPackageName
          @@ string "Function"),
      _Type_forall>>: lambda "fa" $
        "jbody" <<~ (encodeType @@ var "aliases"
          @@ (Sets.insert (Core.forallTypeParameter (var "fa")) (var "boundVars"))
          @@ (Core.forallTypeBody (var "fa")) @@ var "cx" @@ var "g") $
        JavaUtilsSource.addJavaTypeParameter
          @@ (JavaUtilsSource.javaTypeVariable @@ (unwrap _Name @@ Core.forallTypeParameter (var "fa")))
          @@ var "jbody" @@ var "cx",
      _Type_list>>: lambda "et" $
        "jet" <<~ (encodeType @@ var "aliases" @@ var "boundVars" @@ var "et" @@ var "cx" @@ var "g") $
        "rt" <<~ (Eithers.bind (right (var "jet")) (lambda "jt_" $ JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt_" @@ var "cx")) $
        right (JavaUtilsSource.javaRefType
          @@ list [var "rt"]
          @@ asTerm JavaNamesSource.javaUtilPackageName
          @@ string "List"),
      _Type_literal>>: lambda "lt" $
        encodeLiteralType @@ var "lt" @@ var "cx" @@ var "g",
      _Type_either>>: lambda "et" $
        "jlt" <<~ (Eithers.bind
          (encodeType @@ var "aliases" @@ var "boundVars" @@ (project _EitherType _EitherType_left @@ var "et") @@ var "cx" @@ var "g")
          (lambda "jt_" $ JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt_" @@ var "cx")) $
        "jrt" <<~ (Eithers.bind
          (encodeType @@ var "aliases" @@ var "boundVars" @@ (project _EitherType _EitherType_right @@ var "et") @@ var "cx" @@ var "g")
          (lambda "jt_" $ JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt_" @@ var "cx")) $
        right (JavaUtilsSource.javaRefType
          @@ list [var "jlt", var "jrt"]
          @@ asTerm JavaNamesSource.hydraUtilPackageName
          @@ string "Either"),
      _Type_map>>: lambda "mt" $
        "jkt" <<~ (Eithers.bind
          (encodeType @@ var "aliases" @@ var "boundVars" @@ (Core.mapTypeKeys (var "mt")) @@ var "cx" @@ var "g")
          (lambda "jt_" $ JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt_" @@ var "cx")) $
        "jvt" <<~ (Eithers.bind
          (encodeType @@ var "aliases" @@ var "boundVars" @@ (Core.mapTypeValues (var "mt")) @@ var "cx" @@ var "g")
          (lambda "jt_" $ JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt_" @@ var "cx")) $
        right (JavaUtilsSource.javaRefType
          @@ list [var "jkt", var "jvt"]
          @@ asTerm JavaNamesSource.javaUtilPackageName
          @@ string "Map"),
      _Type_pair>>: lambda "pt" $
        "jfirst" <<~ (Eithers.bind
          (encodeType @@ var "aliases" @@ var "boundVars" @@ (project _PairType _PairType_first @@ var "pt") @@ var "cx" @@ var "g")
          (lambda "jt_" $ JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt_" @@ var "cx")) $
        "jsecond" <<~ (Eithers.bind
          (encodeType @@ var "aliases" @@ var "boundVars" @@ (project _PairType _PairType_second @@ var "pt") @@ var "cx" @@ var "g")
          (lambda "jt_" $ JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt_" @@ var "cx")) $
        right (JavaUtilsSource.javaRefType
          @@ list [var "jfirst", var "jsecond"]
          @@ asTerm JavaNamesSource.hydraUtilPackageName
          @@ string "Pair"),
      _Type_unit>>: lambda "_" $
        right (JavaUtilsSource.javaRefType
          @@ list ([] :: [TypedTerm Java.ReferenceType])
          @@ asTerm JavaNamesSource.javaLangPackageName
          @@ string "Void"),
      _Type_record>>: lambda "rt" $
        Logic.ifElse
          (Lists.null (var "rt"))
          (right (JavaUtilsSource.javaRefType
            @@ list ([] :: [TypedTerm Java.ReferenceType])
            @@ asTerm JavaNamesSource.javaLangPackageName
            @@ string "Void"))
          (left (Error.errorOther $ Error.otherError (string "unexpected anonymous record type"))),
      _Type_optional>>: lambda "ot" $
        "jot" <<~ (Eithers.bind
          (encodeType @@ var "aliases" @@ var "boundVars" @@ var "ot" @@ var "cx" @@ var "g")
          (lambda "jt_" $ JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt_" @@ var "cx")) $
        right (JavaUtilsSource.javaRefType
          @@ list [var "jot"]
          @@ asTerm JavaNamesSource.hydraUtilPackageName
          @@ string "Maybe"),
      _Type_set>>: lambda "st" $
        "jst" <<~ (Eithers.bind
          (encodeType @@ var "aliases" @@ var "boundVars" @@ var "st" @@ var "cx" @@ var "g")
          (lambda "jt_" $ JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt_" @@ var "cx")) $
        right (JavaUtilsSource.javaRefType
          @@ list [var "jst"]
          @@ asTerm JavaNamesSource.javaUtilPackageName
          @@ string "Set"),
      _Type_union>>: lambda "_" $
        left (Error.errorOther $ Error.otherError (string "unexpected anonymous union type")),
      _Type_variable>>: lambda "name0" $
        -- Apply type variable substitution
        "name" <~ Optionals.fromOptional (var "name0") (Maps.lookup (var "name0") (var "typeVarSubst")) $
        -- Check if it's a typedef that should be resolved
        "resolved" <<~ (encodeType_resolveIfTypedef @@ var "aliases" @@ var "boundVars" @@ var "inScopeTypeParams" @@ var "name" @@ var "cx" @@ var "g") $
        Optionals.cases (var "resolved")
          -- Not a typedef: determine reference kind
          (right $
            Logic.ifElse (Logic.or (Sets.member (var "name") (var "boundVars")) (Sets.member (var "name") (var "inScopeTypeParams")))
              -- In scope: type variable reference
              (inject Java._Type Java._Type_reference (JavaUtilsSource.javaTypeVariable @@ (unwrap _Name @@ var "name")))
              (Logic.ifElse (isLambdaBoundVariable @@ var "name")
                (inject Java._Type Java._Type_reference (JavaUtilsSource.javaTypeVariable @@ (unwrap _Name @@ var "name")))
                (Logic.ifElse (isUnresolvedInferenceVar @@ var "name")
                  -- Unresolved inference var: Object
                  (inject Java._Type Java._Type_reference
                    (inject Java._ReferenceType Java._ReferenceType_classOrInterface
                      (inject Java._ClassOrInterfaceType Java._ClassOrInterfaceType_class
                        (JavaUtilsSource.javaClassType
                          @@ list ([] :: [TypedTerm Java.ReferenceType])
                          @@ asTerm JavaNamesSource.javaLangPackageName
                          @@ string "Object"))))
                  -- Named reference
                  (inject Java._Type Java._Type_reference
                    (JavaUtilsSource.nameToJavaReferenceType @@ var "aliases" @@ boolean True
                      @@ list ([] :: [TypedTerm Java.TypeArgument])
                      @@ var "name"
                      @@ nothing)))))
          -- Typedef resolved: encode the resolved type
          (lambda "resolvedType" $
            encodeType @@ var "aliases" @@ var "boundVars" @@ var "resolvedType" @@ var "cx" @@ var "g"),
      _Type_wrap>>: lambda "_" $
        left (Error.errorOther $ Error.otherError (string "unexpected anonymous wrap type"))]

-- | Reference to the hydra.encode.core.type function (Type -> Term encoder)
encodeTypeAsTerm :: TypedTerm (Type -> Term)
encodeTypeAsTerm = TypedTerm $ TermVariable $ Name "hydra.encode.core.type"

-- | Encode a type definition as a Java compilation unit.
encodeTypeDefinition :: TypedTermDefinition (Java.PackageDeclaration -> JavaHelpers.Aliases -> TypeDefinition -> InferenceContext -> Graph -> Either Error (Name, Java.CompilationUnit))
encodeTypeDefinition = def "encodeTypeDefinition" $
  lambda "pkg" $ lambda "aliases" $ lambda "tdef" $
    "cx" ~> "g" ~>
    "name" <~ (project _TypeDefinition _TypeDefinition_name @@ var "tdef") $
    "typ" <~ (Core.typeSchemeBody $ project _TypeDefinition _TypeDefinition_body @@ var "tdef") $
    -- Check if serializable
    "serializable" <~ (isSerializableJavaType @@ var "typ") $
    "imports" <~ Logic.ifElse (var "serializable")
      (list [inject Java._ImportDeclaration Java._ImportDeclaration_singleType
        (wrap Java._SingleTypeImportDeclaration
          (JavaUtilsSource.javaTypeName @@ (JavaDsl.identifier (string "java.io.Serializable"))))])
      (list ([] :: [TypedTerm Java.ImportDeclaration])) $
    "decl" <<~ (toClassDecl @@ false @@ var "serializable" @@ var "aliases"
      @@ (list ([] :: [TypedTerm Java.TypeParameter])) @@ var "name" @@ var "typ" @@ var "cx" @@ var "g") $
    "comment" <<~ (Annotations.getTypeDescription @@ var "cx" @@ var "g" @@ var "typ") $
    "tdecl" <~ record Java._TopLevelClassOrInterfaceDeclarationWithComments [
      Java._TopLevelClassOrInterfaceDeclarationWithComments_value>>: inject Java._TopLevelClassOrInterfaceDeclaration Java._TopLevelClassOrInterfaceDeclaration_class (var "decl"),
      Java._TopLevelClassOrInterfaceDeclarationWithComments_comments>>: var "comment"] $
    right (pair (var "name")
      (inject Java._CompilationUnit Java._CompilationUnit_ordinary (record Java._OrdinaryCompilationUnit [
        Java._OrdinaryCompilationUnit_package>>: just (var "pkg"),
        Java._OrdinaryCompilationUnit_imports>>: var "imports",
        Java._OrdinaryCompilationUnit_types>>: list [var "tdecl"]])))

-- | Resolve a TypeVariable name if it refers to a typedef (simple type alias)
encodeType_resolveIfTypedef :: TypedTermDefinition (JavaHelpers.Aliases -> S.Set Name -> S.Set Name -> Name -> InferenceContext -> Graph -> Either Error (Maybe Type))
encodeType_resolveIfTypedef = def "encodeType_resolveIfTypedef" $
  lambda "aliases" $ lambda "boundVars" $ lambda "inScopeTypeParams" $ lambda "name" $
    "cx" ~> "g" ~>
    Logic.ifElse (Logic.or (Sets.member (var "name") (var "boundVars")) (Sets.member (var "name") (var "inScopeTypeParams")))
      (right nothing)
      (Logic.ifElse (isLambdaBoundVariable @@ var "name")
        (right nothing)
        ("schemaTypes" <~ Graph.graphSchemaTypes (var "g") $
          Optionals.cases (Maps.lookup (var "name") (var "schemaTypes"))
            (right nothing)
            (lambda "ts" $
              Logic.ifElse (Logic.not (Lists.null (Core.typeSchemeVariables (var "ts"))))
                (right nothing)
                (cases _Type (Strip.deannotateType @@ Core.typeSchemeBody (var "ts"))
                  (Just $ right (just (Core.typeSchemeBody (var "ts")))) [
                  _Type_record>>: lambda "_" $ right nothing,
                  _Type_union>>: lambda "_" $ right nothing,
                  _Type_wrap>>: lambda "_" $ right nothing]))))

-- | Encode a variable reference as a Java expression
encodeVariable :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> Name -> InferenceContext -> Graph -> Either Error Java.Expression)
encodeVariable = def "encodeVariable" $
  lambda "env" $ lambda "name" $
    "cx" ~> "g" ~>
    "aliases" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env") $
    -- Apply varRenames to get the Java-level variable name (for TCO snapshot support)
    "resolvedName" <~ (JavaUtilsSource.lookupJavaVarName @@ var "aliases" @@ var "name") $
    "jid" <~ (JavaUtilsSource.javaIdentifier @@ (unwrap _Name @@ var "resolvedName")) $
    -- Branch vars: access .value field
    Logic.ifElse (Sets.member (var "name") (project JavaHelpers._Aliases JavaHelpers._Aliases_branchVars @@ var "aliases"))
      (right (JavaUtilsSource.javaFieldAccessToJavaExpression @@
        (JavaDsl.fieldAccess
          (inject Java._FieldAccess_Qualifier Java._FieldAccess_Qualifier_primary
            (JavaUtilsSource.javaExpressionToJavaPrimary @@ (JavaUtilsSource.javaIdentifierToJavaExpression @@ var "jid")))
          (JavaUtilsSource.javaIdentifier @@ asTerm JavaNamesSource.valueFieldName))))
      -- instance_value special case
      (Logic.ifElse (Logic.and
          (Equality.equal (var "name") (wrap _Name (Strings.cat (list [asTerm JavaNamesSource.instanceName, string "_", asTerm JavaNamesSource.valueFieldName]))))
          (isRecursiveVariable @@ var "aliases" @@ var "name"))
        ("instanceExpr" <~ (JavaUtilsSource.javaIdentifierToJavaExpression @@ (JavaUtilsSource.javaIdentifier @@ asTerm JavaNamesSource.instanceName)) $
         right (JavaUtilsSource.javaFieldAccessToJavaExpression @@
          (JavaDsl.fieldAccess
            (inject Java._FieldAccess_Qualifier Java._FieldAccess_Qualifier_primary
              (JavaUtilsSource.javaExpressionToJavaPrimary @@ var "instanceExpr"))
            (JavaUtilsSource.javaIdentifier @@ asTerm JavaNamesSource.valueFieldName))))
        -- Recursive variables use .get()
        (Logic.ifElse (Logic.and
            (isRecursiveVariable @@ var "aliases" @@ var "name")
            (Logic.not (isLambdaBoundIn @@ var "name" @@ (project JavaHelpers._Aliases JavaHelpers._Aliases_lambdaVars @@ var "aliases"))))
          (right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
            (JavaUtilsSource.methodInvocation @@
              (just (Phantoms.left (JavaDsl.expressionName nothing (var "jid"))))
              @@ (JavaDsl.identifier (asTerm JavaNamesSource.getMethodName)) @@ (list ([] :: [TypedTerm Java.Expression])))))
          -- Thunked variables use .get()
          (Logic.ifElse (Logic.and
              (Sets.member (var "name") (project JavaHelpers._Aliases JavaHelpers._Aliases_thunkedVars @@ var "aliases"))
              (Logic.not (isLambdaBoundIn @@ var "name" @@ (project JavaHelpers._Aliases JavaHelpers._Aliases_lambdaVars @@ var "aliases"))))
            (right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocation @@
                (just (Phantoms.left (JavaDsl.expressionName nothing (var "jid"))))
                @@ (JavaDsl.identifier (asTerm JavaNamesSource.getMethodName)) @@ (list ([] :: [TypedTerm Java.Expression])))))
            -- Lambda-bound variables
            (Logic.ifElse (isLambdaBoundIn @@ var "name" @@ (project JavaHelpers._Aliases JavaHelpers._Aliases_lambdaVars @@ var "aliases"))
              ("actualName" <~ (findMatchingLambdaVar @@ var "name" @@ (project JavaHelpers._Aliases JavaHelpers._Aliases_lambdaVars @@ var "aliases")) $
               "resolvedActual" <~ (JavaUtilsSource.lookupJavaVarName @@ var "aliases" @@ var "actualName") $
               right (JavaUtilsSource.javaIdentifierToJavaExpression @@ (JavaUtilsSource.variableToJavaIdentifier @@ var "resolvedActual")))
              -- In-scope let-bound variables: treat as local (skip classifyDataReference which may find them in the extended graph)
              (Logic.ifElse (Sets.member (var "name") (project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeJavaVars @@ var "aliases"))
                (right (JavaUtilsSource.javaIdentifierToJavaExpression @@ (elementJavaIdentifier @@ boolean False @@ boolean False @@ var "aliases" @@ var "resolvedName")))
              -- Classify and encode
              ("cls" <<~ (classifyDataReference @@ var "name" @@ var "cx" @@ var "g") $
               cases JavaHelpers._JavaSymbolClass (var "cls") Nothing [
                 JavaHelpers._JavaSymbolClass_hoistedLambda>>: "arity" ~>
                   encodeVariable_hoistedLambdaCase @@ var "aliases" @@ var "name" @@ var "arity" @@ var "cx" @@ var "g",
                 JavaHelpers._JavaSymbolClass_localVariable>>: lambda "_" $
                   right (JavaUtilsSource.javaIdentifierToJavaExpression @@ (elementJavaIdentifier @@ boolean False @@ boolean False @@ var "aliases" @@ var "resolvedName")),
                 JavaHelpers._JavaSymbolClass_constant>>: lambda "_" $
                   right (JavaUtilsSource.javaIdentifierToJavaExpression @@ (elementJavaIdentifier @@ boolean False @@ boolean False @@ var "aliases" @@ var "name")),
                 JavaHelpers._JavaSymbolClass_nullaryFunction>>: lambda "_" $
                   right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
                     (JavaUtilsSource.methodInvocation @@ nothing @@
                       (elementJavaIdentifier @@ boolean False @@ boolean False @@ var "aliases" @@ var "name") @@
                       (list ([] :: [TypedTerm Java.Expression])))),
                 JavaHelpers._JavaSymbolClass_unaryFunction>>: lambda "_" $
                   right (JavaUtilsSource.javaIdentifierToJavaExpression @@ (elementJavaIdentifier @@ boolean False @@ boolean True @@ var "aliases" @@ var "name"))]))))))

-- | Build a curried lambda from a list of parameter names wrapping a call expression
encodeVariable_buildCurried :: TypedTermDefinition ([Name] -> Java.Expression -> Java.Expression)
encodeVariable_buildCurried = def "encodeVariable_buildCurried" $
  lambda "params" $ lambda "inner" $
    Optionals.fromOptional (var "inner") (Optionals.map
      (lambda "p" $
        JavaUtilsSource.javaLambda @@ Pairs.first (var "p") @@
          (encodeVariable_buildCurried @@
            Pairs.second (var "p") @@ var "inner"))
      (Lists.uncons (var "params")))

-- | Handle the HoistedLambda case of encodeVariable
encodeVariable_hoistedLambdaCase :: TypedTermDefinition (JavaHelpers.Aliases -> Name -> Int -> InferenceContext -> Graph -> Either Error Java.Expression)
encodeVariable_hoistedLambdaCase = def "encodeVariable_hoistedLambdaCase" $
  lambda "aliases" $ lambda "name" $ lambda "arity" $
    "cx" ~> "g" ~>
    "paramNames" <~ Lists.map
      (lambda "i" $ wrap _Name (Strings.cat2 (string "p") (Literals.showInt32 (var "i"))))
      (Math.range (int32 0) (Math.sub (var "arity") (int32 1))) $
    "paramExprs" <~ Lists.map
      (lambda "pn" $ JavaUtilsSource.javaIdentifierToJavaExpression @@ (JavaUtilsSource.variableToJavaIdentifier @@ var "pn"))
      (var "paramNames") $
    "call" <~ (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
      (JavaUtilsSource.methodInvocation @@ nothing @@
        (elementJavaIdentifier @@ boolean False @@ boolean False @@ var "aliases" @@ var "name") @@
        var "paramExprs")) $
    "lam" <~ encodeVariable_buildCurried @@ var "paramNames" @@ var "call" $
    -- Try to cast to the function's curried type
    "mel" <<~ right (Lexical.lookupBinding @@ var "g" @@ var "name") $
    Optionals.cases (var "mel")
      (right (var "lam"))
      (lambda "el" $
        Optionals.cases (Core.bindingTypeScheme (var "el"))
          (right (var "lam"))
          (lambda "ts" $
            "typ" <~ Core.typeSchemeBody (var "ts") $
            "jtype" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "typ" @@ var "cx" @@ var "g") $
            "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jtype" @@ var "cx") $
            right (JavaUtilsSource.javaCastExpressionToJavaExpression @@
              (JavaUtilsSource.javaCastExpression @@ var "rt" @@
                (JavaUtilsSource.javaExpressionToJavaUnaryExpression @@ var "lam")))))

-- | Build an equality clause for a single field in the equals() method.
eqClause :: TypedTermDefinition (String -> FieldType -> Java.InclusiveOrExpression)
eqClause = def "eqClause" $
  lambda "tmpName" $ lambda "ft" $ lets [
    "fname">: unwrap _Name @@ Core.fieldTypeName (var "ft"),
    "ftype">: Core.fieldTypeType (var "ft")] $
    Logic.ifElse (isBinaryType @@ var "ftype")
      (arraysEqualsClause @@ var "tmpName" @@ var "fname")
      (Logic.ifElse (isBigNumericType @@ var "ftype")
        (compareToZeroClause @@ var "tmpName" @@ var "fname")
        (equalsClause @@ var "tmpName" @@ var "fname"))

-- | Objects.equals(this.field, other.field) for null-safe comparison
equalsClause :: TypedTermDefinition (String -> String -> Java.InclusiveOrExpression)
equalsClause = def "equalsClause" $
  lambda "tmpName" $ lambda "fname" $ lets [
    "thisArg">: JavaUtilsSource.javaExpressionNameToJavaExpression
      @@ (JavaUtilsSource.fieldExpression @@ wrap Java._Identifier (string "this")
        @@ (JavaUtilsSource.javaIdentifier @@ var "fname")),
    "otherArg">: JavaUtilsSource.javaExpressionNameToJavaExpression
      @@ (JavaUtilsSource.fieldExpression @@ (JavaUtilsSource.javaIdentifier @@ var "tmpName")
        @@ (JavaUtilsSource.javaIdentifier @@ var "fname")),
    "header">: JavaDsl.methodInvocationHeaderComplex
      (JavaDsl.methodInvocationComplex
        (JavaDsl.methodInvocationVariantType (JavaUtilsSource.javaTypeName @@ wrap Java._Identifier (string "java.util.Objects")))
        (list ([] :: [TypedTerm Java.TypeArgument]))
        (wrap Java._Identifier (asTerm JavaNamesSource.equalsMethodName)))] $
    JavaUtilsSource.javaPostfixExpressionToJavaInclusiveOrExpression
      @@ (JavaUtilsSource.javaMethodInvocationToJavaPostfixExpression
        @@ (JavaDsl.methodInvocation_ (var "header") (list [var "thisArg", var "otherArg"])))

-- | Try to extract the argument type from a function application.
-- For a function like a constructor :: a -> Wrapper a with return type Wrapper a,
-- the argument type is a (the type parameter of the wrapper).
extractArgType :: TypedTermDefinition (Type -> Type -> Type)
extractArgType = def "extractArgType" $
  lambda "_lhs" $ lambda "typ" $
    cases _Type (var "typ")
      (Just $ var "typ") [
      _Type_application>>: lambda "at1" $
        cases _Type (Core.applicationTypeFunction (var "at1"))
          (Just $ var "typ") [
          _Type_application>>: lambda "_at2" $
            Core.applicationTypeArgument (var "at1")]]

-- | Extract input/output pairs for direct variable returns in
-- "context extension" functions: ... -> V -> X -> V (or W)
extractDirectReturn :: TypedTermDefinition (S.Set Name -> Type -> [(Name, Name)])
extractDirectReturn = def "extractDirectReturn" $
  lambda "tparamSet" $ lambda "t" $
    extractDirectReturn_go @@ var "tparamSet" @@ var "t"

extractDirectReturn_go :: TypedTermDefinition (S.Set Name -> Type -> [(Name, Name)])
extractDirectReturn_go = def "extractDirectReturn_go" $
  lambda "tparamSet" $ lambda "t" $
    cases _Type (Strip.deannotateType @@ var "t")
      (Just $ list ([] :: [TypedTerm (Name, Name)])) [
      _Type_function>>: lambda "ft" $
        "dom" <~ (Strip.deannotateType @@ Core.functionTypeDomain (var "ft")) $
        "cod" <~ Core.functionTypeCodomain (var "ft") $
        cases _Type (var "dom")
          (Just $ extractDirectReturn_go @@ var "tparamSet" @@ var "cod") [
          _Type_variable>>: lambda "inVar" $
            Logic.ifElse
              (Sets.member (var "inVar") (var "tparamSet"))
              (cases _Type (Strip.deannotateType @@ var "cod")
                (Just $ list ([] :: [TypedTerm (Name, Name)])) [
                _Type_function>>: lambda "ft2" $
                  "midArg" <~ (Strip.deannotateType @@ Core.functionTypeDomain (var "ft2")) $
                  "retPart" <~ (Strip.deannotateType @@ Core.functionTypeCodomain (var "ft2")) $
                  cases _Type (var "midArg")
                    (Just $
                      cases _Type (var "retPart")
                        (Just $ list ([] :: [TypedTerm (Name, Name)])) [
                        _Type_variable>>: lambda "outVar" $
                          Logic.ifElse
                            (Sets.member (var "outVar") (var "tparamSet"))
                            (list [pair (var "inVar") (var "outVar")])
                            (list ([] :: [TypedTerm (Name, Name)]))]
                    ) [
                    _Type_variable>>: lambda "midVar" $
                      Logic.ifElse
                        (Sets.member (var "midVar") (var "tparamSet"))
                        (list ([] :: [TypedTerm (Name, Name)]))
                        (cases _Type (var "retPart")
                          (Just $ list ([] :: [TypedTerm (Name, Name)])) [
                          _Type_variable>>: lambda "outVar" $
                            Logic.ifElse
                              (Sets.member (var "outVar") (var "tparamSet"))
                              (list [pair (var "inVar") (var "outVar")])
                              (list ([] :: [TypedTerm (Name, Name)]))])]])
              (extractDirectReturn_go @@ var "tparamSet" @@ var "cod")]]

-- | Extract input/output type variable pairs from function-typed parameters.
-- For a param like (A -> ... -> (B, ...)), extract (A, B) where B is
-- the first element of a pair return type.
extractInOutPair :: TypedTermDefinition (Type -> [(Name, Name)])
extractInOutPair = def "extractInOutPair" $
  lambda "t" $
    cases _Type (Strip.deannotateType @@ var "t")
      (Just $ list ([] :: [TypedTerm (Name, Name)])) [
      _Type_function>>: lambda "ft" $
        cases _Type (Strip.deannotateType @@ Core.functionTypeDomain (var "ft"))
          (Just $ list ([] :: [TypedTerm (Name, Name)])) [
          _Type_variable>>: lambda "inVar" $
            "retType" <~ (unwrapReturnType @@ Core.functionTypeCodomain (var "ft")) $
            cases _Type (Strip.deannotateType @@ var "retType")
              (Just $ list ([] :: [TypedTerm (Name, Name)])) [
              _Type_pair>>: lambda "pt" $
                cases _Type (Strip.deannotateType @@ Core.pairTypeFirst (var "pt"))
                  (Just $ list ([] :: [TypedTerm (Name, Name)])) [
                  _Type_variable>>: lambda "outVar" $
                    list [pair (var "inVar") (var "outVar")]]]]]

-- | Extract input/output pairs for direct variable returns in

extractTypeApplicationArgs :: TypedTermDefinition (Type -> [Type])
extractTypeApplicationArgs = def "extractTypeApplicationArgs" $
  lambda "typ" $ Lists.reverse (extractTypeApplicationArgs_go @@ var "typ")

extractTypeApplicationArgs_go :: TypedTermDefinition (Type -> [Type])
extractTypeApplicationArgs_go = def "extractTypeApplicationArgs_go" $
  lambda "t" $ cases _Type (var "t")
    (Just $ list ([] :: [TypedTerm Type])) [
    _Type_application>>: lambda "at" $
      Lists.cons
        (Core.applicationTypeArgument (var "at"))
        (extractTypeApplicationArgs_go @@ (Core.applicationTypeFunction (var "at")))]

-- | Convert a field type to a Java formal parameter
fieldTypeToFormalParam :: TypedTermDefinition (JavaHelpers.Aliases -> FieldType -> InferenceContext -> Graph -> Either Error Java.FormalParameter)
fieldTypeToFormalParam = def "fieldTypeToFormalParam" $
  lambda "aliases" $ lambda "ft" $
    "cx" ~> "g" ~>
    "jt" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ (Core.fieldTypeType (var "ft")) @@ var "cx" @@ var "g") $
    right (JavaUtilsSource.javaTypeToJavaFormalParameter @@ var "jt" @@ Core.fieldTypeName (var "ft"))

-- | Select elements from a list where the corresponding flag is True.
filterByFlags :: TypedTermDefinition ([a] -> [Bool] -> [a])
filterByFlags = def "filterByFlags" $
  lambda "xs" $ lambda "flags" $
    Lists.map (lambda "p" $ Pairs.first (var "p"))
      (Lists.filter (lambda "p" $ Pairs.second (var "p"))
        (Lists.zip (var "xs") (var "flags")))

-- | Filter type arguments to remove those at positions corresponding to phantom
-- or over-generalized scheme variables.
filterPhantomTypeArgs :: TypedTermDefinition (Name -> [Type] -> InferenceContext -> Graph -> Either Error [Type])
filterPhantomTypeArgs = def "filterPhantomTypeArgs" $
  lambda "calleeName" $ lambda "allTypeArgs" $
    "cx" ~> "g" ~>
    "mel" <<~ right (Lexical.lookupBinding @@ var "g" @@ var "calleeName") $
    Optionals.cases (var "mel")
      (right (var "allTypeArgs"))
      (lambda "el" $
        Optionals.cases (Core.bindingTypeScheme (var "el"))
          (right (var "allTypeArgs"))
          (lambda "ts" $
            "schemeVars" <~ Lists.filter (lambda "v" $ isSimpleName @@ var "v") (Core.typeSchemeVariables (var "ts")) $
            "schemeTypeVars" <~ collectTypeVars @@ Core.typeSchemeBody (var "ts") $
            "schemeType" <~ Core.typeSchemeBody (var "ts") $
            "nParams" <~ countFunctionParams @@ var "schemeType" $
            "peeled" <~ peelDomainTypes @@ var "nParams" @@ var "schemeType" $
            "calleeDoms" <~ Pairs.first (var "peeled") $
            "calleeCod" <~ Pairs.second (var "peeled") $
            "overgenSubst" <~ detectAccumulatorUnification @@ var "calleeDoms" @@ var "calleeCod" @@ var "schemeVars" $
            "keepFlags" <~ Lists.map
              (lambda "v" $ Logic.and
                (Sets.member (var "v") (var "schemeTypeVars"))
                (Logic.not (Maps.member (var "v") (var "overgenSubst"))))
              (var "schemeVars") $
            Logic.ifElse
              (Logic.not (Equality.equal (Lists.length (var "schemeVars")) (Lists.length (var "allTypeArgs"))))
              (right (var "allTypeArgs"))
              (right (filterPhantomTypeArgs_filterAndApply @@ var "allTypeArgs" @@ var "keepFlags" @@ var "overgenSubst"))))

-- | Helper: given type args, keep-flags, and overgen subst, filter and apply.
filterPhantomTypeArgs_filterAndApply :: TypedTermDefinition ([Type] -> [Bool] -> M.Map Name Type -> [Type])
filterPhantomTypeArgs_filterAndApply = def "filterPhantomTypeArgs_filterAndApply" $
  lambda "allTypeArgs" $ lambda "keepFlags" $ lambda "overgenSubst" $
    "filtered" <~ Lists.map
      (lambda "p" $ Pairs.first (var "p"))
      (Lists.filter (lambda "p" $ Pairs.second (var "p"))
        (Lists.zip (var "allTypeArgs") (var "keepFlags"))) $
    Logic.ifElse
      (Logic.not (Maps.null (var "overgenSubst")))
      (Lists.map (lambda "t" $ substituteTypeVarsWithTypes @@ var "overgenSubst" @@ var "t") (var "filtered"))
      (var "filtered")

-- | Find the actual lambda variable name that matches a given reference
findMatchingLambdaVar :: TypedTermDefinition (Name -> S.Set Name -> Name)
findMatchingLambdaVar = def "findMatchingLambdaVar" $
  lambda "name" $ lambda "lambdaVars" $
    Logic.ifElse (Sets.member (var "name") (var "lambdaVars"))
      (var "name")
      (Logic.ifElse (isLambdaBoundIn_isQualified @@ var "name")
        (Optionals.fromOptional (var "name")
          (Lists.find
            (lambda "lv" $ Logic.and
              (isLambdaBoundIn_isQualified @@ var "lv")
              (Equality.equal
                (Names.localNameOf @@ var "lv")
                (Names.localNameOf @@ var "name")))
            (Sets.toList (var "lambdaVars"))))
        (Logic.ifElse
          (Sets.member (wrap _Name (Names.localNameOf @@ var "name")) (var "lambdaVars"))
          (wrap _Name (Names.localNameOf @@ var "name"))
          (var "name")))

-- | Extract the type variable from the first element of a pair type.
findPairFirst :: TypedTermDefinition (Type -> Maybe Name)
findPairFirst = def "findPairFirst" $
  lambda "t" $
    cases _Type (Strip.deannotateType @@ var "t")
      (Just nothing) [
      _Type_pair>>: lambda "pt" $
        cases _Type (Strip.deannotateType @@ Core.pairTypeFirst (var "pt"))
          (Just nothing) [
          _Type_variable>>: lambda "v" $ just (var "v")]]

-- | Find the first self-referencing input var in a grouped map.
findSelfRefVar :: TypedTermDefinition (M.Map Name [Name] -> Maybe Name)
findSelfRefVar = def "findSelfRefVar" $
  lambda "grouped" $
    "selfRefs" <~ Lists.filter
      (lambda "entry" $ (Lists.elem :: TypedTerm Name -> TypedTerm [Name] -> TypedTerm Bool) (Pairs.first (var "entry")) (Pairs.second (var "entry")))
      (Maps.toList (var "grouped")) $
    Optionals.map (lambda "entry" $ Pairs.first (var "entry")) (Lists.maybeHead (var "selfRefs"))

-- | First 20 prime numbers used as hash code multipliers.
first20Primes :: TypedTermDefinition [Int]
first20Primes = def "first20Primes" $
  list (fmap bigintAsInt [bigint 2, bigint 3, bigint 5, bigint 7, bigint 11, bigint 13, bigint 17, bigint 19,
    bigint 23, bigint 29, bigint 31, bigint 37, bigint 41, bigint 43, bigint 47, bigint 53, bigint 59,
    bigint 61, bigint 67, bigint 71])

-- | Flatten a nested application chain f(a1)(a2)...(aN) into ([a1,...,aN], f).
flattenApps :: TypedTermDefinition (Term -> [Term] -> ([Term], Term))
flattenApps = def "flattenApps" $
  lambda "t" $ lambda "acc" $
    cases _Term (Strip.deannotateTerm @@ var "t")
      (Just $ pair (var "acc") (var "t")) [
      _Term_application>>: lambda "app" $
        flattenApps
          @@ (project _Application _Application_function @@ var "app")
          @@ Lists.cons (project _Application _Application_argument @@ var "app") (var "acc")]

-- | Flatten nested TermLet into a flat list of Bindings.
flattenBindings :: TypedTermDefinition ([Binding] -> [Binding])
flattenBindings = def "flattenBindings" $
  lambda "bindings" $
    Lists.bind (var "bindings") (lambda "b" $
      cases _Term (Strip.deannotateTerm @@ Core.bindingTerm (var "b"))
        (Just $ list [var "b"]) [
        _Term_let>>: lambda "lt" $
          Lists.concat2
            (flattenBindings @@ Core.letBindings (var "lt"))
            (list [Core.binding (Core.bindingName (var "b")) (Core.letBody (var "lt")) (Core.bindingTypeScheme (var "b"))])])

-- | Generate a unique name by appending increasing integers, avoiding a given set.
freshJavaName :: TypedTermDefinition (Name -> S.Set Name -> Name)
freshJavaName = def "freshJavaName" $
  lambda "base" $ lambda "avoid" $
    freshJavaName_go @@ var "base" @@ var "avoid" @@ int32 2

freshJavaName_go :: TypedTermDefinition (Name -> S.Set Name -> Int -> Name)
freshJavaName_go = def "freshJavaName_go" $
  lambda "base" $ lambda "avoid" $ lambda "i" $
    "candidate" <~ Core.name (Strings.cat2 (Core.unName (var "base")) (Literals.showInt32 (var "i"))) $
    Logic.ifElse
      (Sets.member (var "candidate") (var "avoid"))
      (freshJavaName_go @@ var "base" @@ var "avoid" @@ Math.add (var "i") (int32 1))
      (var "candidate")

-- | Generate a method invocation for a function call.
functionCall :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> Bool -> Name -> [Term] -> [Type] -> InferenceContext -> Graph -> Either Error Java.Expression)
functionCall = def "functionCall" $
  lambda "env" $ lambda "isPrim" $ lambda "name" $ lambda "args" $ lambda "typeApps" $
    "cx" ~> "g" ~>
    "aliases" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env") $
    "isLambdaBound" <~ (isLambdaBoundIn @@ var "name"
      @@ (project JavaHelpers._Aliases JavaHelpers._Aliases_lambdaVars @@ var "aliases")) $
    -- Encode arguments and generate method invocation
    ("jargs0" <<~ (Eithers.mapList (lambda "arg" $ encodeTerm @@ var "env" @@ var "arg" @@ var "cx" @@ var "g") (var "args")) $
        "wrapResult" <~ (wrapLazyArguments @@ var "g" @@ var "name" @@ var "jargs0") $
        "jargs" <~ Pairs.first (var "wrapResult") $
        "mMethodOverride" <~ Pairs.second (var "wrapResult") $
        Logic.ifElse (Logic.or (isLocalVariable @@ var "name") (var "isLambdaBound"))
          -- Local/lambda-bound: apply arguments one at a time via .apply()
          ("baseExpr" <<~ (encodeVariable @@ var "env" @@ var "name" @@ var "cx" @@ var "g") $
            right (Lists.foldl (lambda "acc" $ lambda "jarg" $ applyJavaArg @@ var "acc" @@ var "jarg")
              (var "baseExpr") (var "jargs")))
          -- Module-level functions: call with all args directly
          ("overrideMethodName" <~ (lambda "jid" $
              Optionals.cases (var "mMethodOverride")
                (var "jid")
                (lambda "m" $
                  "s" <~ (JavaDsl.unIdentifier (var "jid")) $
                  JavaDsl.identifier (Strings.cat2
                    (Strings.fromList (Lists.take
                      (Math.sub (Strings.length (var "s")) (Strings.length (asTerm JavaNamesSource.applyMethodName)))
                      (Strings.toList (var "s"))))
                    (var "m")))) $
            Logic.ifElse (Lists.null (var "typeApps"))
              -- No type applications: simple header
              ("header" <~ JavaDsl.methodInvocationHeaderSimple
                (wrap Java._MethodName (var "overrideMethodName" @@ (elementJavaIdentifier @@ var "isPrim" @@ false @@ var "aliases" @@ var "name"))) $
                right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
                  (JavaDsl.methodInvocation_ (var "header") (var "jargs"))))
              -- With type applications: need qualified invocation
              ("qn" <~ (Names.qualifyName @@ var "name") $
                "mns" <~ (Util.qualifiedNameModuleName (var "qn")) $
                "localName" <~ (Util.qualifiedNameLocal (var "qn")) $
                Optionals.cases (var "mns")
                  -- No namespace: simple header
                  ("header" <~ JavaDsl.methodInvocationHeaderSimple
                    (wrap Java._MethodName (var "overrideMethodName" @@ (elementJavaIdentifier @@ var "isPrim" @@ false @@ var "aliases" @@ var "name"))) $
                    right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
                      (JavaDsl.methodInvocation_ (var "header") (var "jargs"))))
                  (lambda "ns_" $
                    "classId" <~ (JavaUtilsSource.nameToJavaName @@ var "aliases" @@ (elementsQualifiedName @@ var "ns_")) $
                    "methodId" <~ (Logic.ifElse (var "isPrim")
                      (var "overrideMethodName" @@ (JavaDsl.identifier (Strings.cat2
                        (JavaDsl.unIdentifier (JavaUtilsSource.nameToJavaName @@ var "aliases" @@ (Names.unqualifyName @@ (Util.qualifiedName (just (var "ns_")) (Formatting.capitalize @@ var "localName")))))
                        (Strings.cat2 (string ".") (asTerm JavaNamesSource.applyMethodName)))))
                      (JavaDsl.identifier (JavaUtilsSource.sanitizeJavaName @@ var "localName"))) $
                    "jTypeArgs" <<~ (Eithers.mapList (lambda "t" $
                      "jt" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "t" @@ var "cx" @@ var "g") $
                      "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt" @@ var "cx") $
                      right (JavaDsl.typeArgumentReference (var "rt")))
                      (var "typeApps")) $
                    right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
                      (JavaUtilsSource.methodInvocationStaticWithTypeArgs @@ var "classId" @@ var "methodId" @@ var "jTypeArgs" @@ var "jargs"))))))

-- | Get the codomain type from annotations
getCodomain :: TypedTermDefinition (M.Map Name Term -> InferenceContext -> Graph -> Either Error Type)
getCodomain = def "getCodomain" $
  lambda "ann" $
    "cx" ~> "g" ~>
    Eithers.map
      (lambda "ft" $ Core.functionTypeCodomain (var "ft"))
      (getFunctionType @@ var "ann" @@ var "cx" @@ var "g")

-- | Get the function type from annotations
getFunctionType :: TypedTermDefinition (M.Map Name Term -> InferenceContext -> Graph -> Either Error FunctionType)
getFunctionType = def "getFunctionType" $
  lambda "ann" $
    "cx" ~> "g" ~>
    "mt" <<~ (getTypeE (var "cx") (var "g") (var "ann")) $
    Optionals.cases (var "mt")
      (left (Error.errorOther $ Error.otherError $ string "type annotation is required for function and elimination terms in Java"))
      (lambda "t" $ cases _Type (var "t")
        (Just $ left (Error.errorOther $ Error.otherError $ Strings.cat2 (string "expected function type, got: ") (ShowCore.type_ @@ var "t"))) [
        _Type_function>>: lambda "ft" $ right (var "ft")])

-- | Get a type annotation, converting DecodingError to Error.
getTypeE :: TypedTerm InferenceContext -> TypedTerm Graph -> TypedTerm (M.Map Name Term) -> TypedTerm (Either Error (Maybe Type))
getTypeE cx g ann = Eithers.bimap
  ("__de" ~> Error.errorOther (Error.otherError ((unwrap _DecodingError) @@ var "__de")))
  ("__a" ~> var "__a")
  (Annotations.getType @@ g @@ ann)

-- | Group pairs by their first element, collecting second elements into lists.
groupPairsByFirst :: TypedTermDefinition ([(Name, Name)] -> M.Map Name [Name])
groupPairsByFirst = def "groupPairsByFirst" $
  lambda "pairs" $
    Lists.foldl
      (lambda "m" $ lambda "p" $
        "k" <~ Pairs.first (var "p") $
        "v" <~ Pairs.second (var "p") $
        Maps.alter
          (lambda "mv" $
            Optionals.cases (var "mv") (just (list [var "v"])) (lambda "vs" $ just (Lists.concat2 (var "vs") (list [var "v"]))))
          (var "k")
          (var "m"))
      (Maps.empty)
      (var "pairs")

-- | Shared helper: Integer.compare(this.field.hashCode(), otherVar.field.hashCode())
hashCodeCompareExpr :: TypedTermDefinition (String -> String -> Java.Expression)
hashCodeCompareExpr = def "hashCodeCompareExpr" $
  lambda "otherVar" $ lambda "fname" $ lets [
    "header">: JavaDsl.methodInvocationHeaderComplex
      (JavaDsl.methodInvocationComplex
        (JavaDsl.methodInvocationVariantType (JavaUtilsSource.javaTypeName @@ wrap Java._Identifier (string "Integer")))
        (list ([] :: [TypedTerm Java.TypeArgument]))
        (wrap Java._Identifier (string "compare"))),
    "thisHashCode">: JavaUtilsSource.javaMethodInvocationToJavaExpression @@ (JavaDsl.methodInvocation_
      (JavaDsl.methodInvocationHeaderComplex
        (JavaDsl.methodInvocationComplex
          (JavaDsl.methodInvocationVariantExpression
            (JavaDsl.expressionName nothing (wrap Java._Identifier (JavaUtilsSource.sanitizeJavaName @@ var "fname"))))
          (list ([] :: [TypedTerm Java.TypeArgument]))
          (wrap Java._Identifier (asTerm JavaNamesSource.hashCodeMethodName))))
      (list ([] :: [TypedTerm Java.Expression]))),
    "otherHashCode">: JavaUtilsSource.javaMethodInvocationToJavaExpression @@ (JavaDsl.methodInvocation_
      (JavaDsl.methodInvocationHeaderComplex
        (JavaDsl.methodInvocationComplex
          (JavaDsl.methodInvocationVariantExpression
            (JavaUtilsSource.fieldExpression @@ (JavaUtilsSource.javaIdentifier @@ var "otherVar") @@ (JavaUtilsSource.javaIdentifier @@ var "fname")))
          (list ([] :: [TypedTerm Java.TypeArgument]))
          (wrap Java._Identifier (asTerm JavaNamesSource.hashCodeMethodName))))
      (list ([] :: [TypedTerm Java.Expression])))] $
    JavaUtilsSource.javaMethodInvocationToJavaExpression @@ (JavaDsl.methodInvocation_ (var "header") (list [var "thisHashCode", var "otherHashCode"]))

-- | Build a hashCode multiplier pair: prime * Objects.hashCode(field)
hashCodeMultPair :: TypedTermDefinition (Int -> Name -> Java.MultiplicativeExpression)
hashCodeMultPair = def "hashCodeMultPair" $
  lambda "i" $ lambda "fname" $ lets [
    "fnameStr">: unwrap _Name @@ var "fname",
    "lhs">: JavaDsl.multiplicativeExpressionUnary
      (JavaUtilsSource.javaPrimaryToJavaUnaryExpression
        @@ (JavaUtilsSource.javaLiteralToJavaPrimary @@ (JavaUtilsSource.javaInt @@ var "i"))),
    "rhs">: JavaUtilsSource.javaPostfixExpressionToJavaUnaryExpression
      @@ (JavaUtilsSource.javaMethodInvocationToJavaPostfixExpression
        @@ (JavaDsl.methodInvocation_
          (JavaDsl.methodInvocationHeaderComplex
            (JavaDsl.methodInvocationComplex
              (JavaDsl.methodInvocationVariantType
                (JavaUtilsSource.javaTypeName @@ wrap Java._Identifier (string "java.util.Objects")))
              (list ([] :: [TypedTerm Java.TypeArgument]))
              (wrap Java._Identifier (asTerm JavaNamesSource.hashCodeMethodName))))
          (list [JavaUtilsSource.javaExpressionNameToJavaExpression
            @@ (JavaDsl.expressionName nothing
              (wrap Java._Identifier (JavaUtilsSource.sanitizeJavaName @@ var "fnameStr")))])))] $
    JavaDsl.multiplicativeExpressionTimes
      (JavaDsl.multiplicativeExpressionBinary (var "lhs") (var "rhs"))

-- | Create a qualified reference to an inner class member
innerClassRef :: TypedTermDefinition (JavaHelpers.Aliases -> Name -> String -> Java.Identifier)
innerClassRef = def "innerClassRef" $
  lambda "aliases" $ lambda "name" $ lambda "local" $ lets [
    "id">: unwrap Java._Identifier @@ (JavaUtilsSource.nameToJavaName @@ var "aliases" @@ var "name")] $
    wrap Java._Identifier (Strings.cat2 (Strings.cat2 (var "id") (string ".")) (var "local"))

-- | Insert a branch variable into the environment
insertBranchVar :: TypedTermDefinition (Name -> JavaHelpers.JavaEnvironment -> JavaHelpers.JavaEnvironment)
insertBranchVar = def "insertBranchVar" $
  lambda "name" $ lambda "env" $ lets [
    "aliases">: project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env"] $
    record JavaHelpers._JavaEnvironment [
      JavaHelpers._JavaEnvironment_aliases>>:
        record JavaHelpers._Aliases [
          JavaHelpers._Aliases_currentNamespace>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_currentNamespace @@ var "aliases",
          JavaHelpers._Aliases_packages>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_packages @@ var "aliases",
          JavaHelpers._Aliases_branchVars>>:
            Sets.insert (var "name") (project JavaHelpers._Aliases JavaHelpers._Aliases_branchVars @@ var "aliases"),
          JavaHelpers._Aliases_recursiveVars>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_recursiveVars @@ var "aliases",
          JavaHelpers._Aliases_inScopeTypeParams>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeTypeParams @@ var "aliases",
          JavaHelpers._Aliases_polymorphicLocals>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_polymorphicLocals @@ var "aliases",
          JavaHelpers._Aliases_inScopeJavaVars>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeJavaVars @@ var "aliases",
          JavaHelpers._Aliases_varRenames>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_varRenames @@ var "aliases",
          JavaHelpers._Aliases_lambdaVars>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_lambdaVars @@ var "aliases",
          JavaHelpers._Aliases_typeVarSubst>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_typeVarSubst @@ var "aliases",
          JavaHelpers._Aliases_trustedTypeVars>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_trustedTypeVars @@ var "aliases",
          JavaHelpers._Aliases_methodCodomain>>: nothing,
          JavaHelpers._Aliases_thunkedVars>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_thunkedVars @@ var "aliases"],
      JavaHelpers._JavaEnvironment_graph>>:
        project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_graph @@ var "env"]

-- | Compute the list of interface types for a Java class declaration.
--   If serializable, includes both Serializable and Comparable<Self>.
interfaceTypes :: TypedTermDefinition (Bool -> JavaHelpers.Aliases -> [Java.TypeParameter] -> Name -> [Java.InterfaceType])
interfaceTypes = def "interfaceTypes" $
  lambda "isSer" $ lambda "aliases" $ lambda "tparams" $ lambda "elName" $ lets [
    "javaSerializableType">: JavaDsl.interfaceType
      (JavaDsl.classType
        (list ([] :: [TypedTerm Java.Annotation]))
        JavaDsl.classTypeQualifierNone
        (JavaUtilsSource.javaTypeIdentifier @@ string "Serializable")
        (list ([] :: [TypedTerm Java.TypeArgument]))),
    "selfTypeArg">: (JavaDsl.typeArgumentReference
      (JavaUtilsSource.nameToJavaReferenceType @@ var "aliases" @@ boolean False
        @@ Lists.map (lambda "tp_" $ JavaUtilsSource.typeParameterToTypeArgument @@ var "tp_") (var "tparams")
        @@ var "elName" @@ nothing) :: TypedTerm Java.TypeArgument),
    "javaComparableType">: JavaDsl.interfaceType
      (JavaDsl.classType
        (list ([] :: [TypedTerm Java.Annotation]))
        JavaDsl.classTypeQualifierNone
        (JavaUtilsSource.javaTypeIdentifier @@ string "Comparable")
        (list [var "selfTypeArg"]))] $
    Logic.ifElse (var "isSer")
      (list [var "javaSerializableType", var "javaComparableType"])
      (list ([] :: [TypedTerm Java.InterfaceType]))

-- | Check whether a Hydra type maps to BigDecimal or BigInteger in Java
isBigNumericType :: TypedTermDefinition (Type -> Bool)
isBigNumericType = def "isBigNumericType" $
  lambda "typ" $ cases _Type (Strip.deannotateType @@ var "typ")
    (Just $ boolean False) [
    _Type_literal>>: lambda "lt" $
      cases _LiteralType (var "lt") (Just $ boolean False) [
        _LiteralType_decimal>>: constant $ boolean True,
        _LiteralType_integer>>: lambda "it" $
          cases _IntegerType (var "it") (Just $ boolean False) [
            _IntegerType_bigint>>: constant $ boolean True]]]

-- | Check whether a Hydra type is the binary literal type (maps to byte[])
isBinaryType :: TypedTermDefinition (Type -> Bool)
isBinaryType = def "isBinaryType" $
  lambda "typ" $ cases _Type (Strip.deannotateType @@ var "typ")
    (Just $ boolean False) [
    _Type_literal>>: lambda "lt" $
      cases _LiteralType (var "lt") (Just $ boolean False) [
        _LiteralType_binary>>: constant $ boolean True]]

-- | Check if a union variant field's type is a unit type, by looking up the union type schema.
isFieldUnitType :: TypedTermDefinition (Name -> Name -> InferenceContext -> Graph -> Either Error Bool)
isFieldUnitType = def "isFieldUnitType" $
  lambda "typeName" $ lambda "fieldName" $
    "cx" ~> "g" ~>
    "schemaTypes" <~ Graph.graphSchemaTypes (var "g") $
    Optionals.cases (Maps.lookup (var "typeName") (var "schemaTypes"))
      (right false)
      (lambda "ts" $
        cases _Type (Strip.deannotateType @@ Core.typeSchemeBody (var "ts"))
          (Just $ right false) [
          _Type_union>>: lambda "rt" $
            right (Optionals.cases
              (Lists.find (lambda "ft" $ Equality.equal (Core.fieldTypeName (var "ft")) (var "fieldName"))
                (var "rt"))
              false
              (lambda "ft" $ Predicates.isUnitType @@ (Strip.deannotateType @@ Core.fieldTypeType (var "ft"))))])

-- | Check if a name (possibly qualified) is lambda-bound
isLambdaBoundIn :: TypedTermDefinition (Name -> S.Set Name -> Bool)
isLambdaBoundIn = def "isLambdaBoundIn" $
  lambda "name" $ lambda "lambdaVars" $
    Logic.or (Sets.member (var "name") (var "lambdaVars"))
      (Logic.or
        -- For qualified names, check if any qualified lambda var has the same local name
        (Logic.and (isLambdaBoundIn_isQualified @@ var "name")
          (Optionals.isGiven (Lists.find
            (lambda "lv" $ Logic.and
              (isLambdaBoundIn_isQualified @@ var "lv")
              (Equality.equal
                (Names.localNameOf @@ var "lv")
                (Names.localNameOf @@ var "name")))
            (Sets.toList (var "lambdaVars")))))
        -- For unqualified names, check if the local name is in lambdaVars
        (Logic.and (Logic.not (isLambdaBoundIn_isQualified @@ var "name"))
          (Sets.member (wrap _Name (Names.localNameOf @@ var "name")) (var "lambdaVars"))))

-- | Helper: check if a name is qualified (has a namespace)
isLambdaBoundIn_isQualified :: TypedTermDefinition (Name -> Bool)
isLambdaBoundIn_isQualified = def "isLambdaBoundIn_isQualified" $
  lambda "n" $ Optionals.isGiven (Util.qualifiedNameModuleName (Names.qualifyName @@ var "n"))

-- | Check if a name (possibly qualified) is lambda-bound

isLambdaBoundVariable :: TypedTermDefinition (Name -> Bool)
isLambdaBoundVariable = def "isLambdaBoundVariable" $
  lambda "name" $ lets [
    "v">: Core.unName $ var "name"] $
    Equality.lte (Strings.length (var "v")) (int32 4)

isLocalVariable :: TypedTermDefinition (Name -> Bool)
isLocalVariable = def "isLocalVariable" $
  lambda "name" $ Optionals.isNone
    (Util.qualifiedNameModuleName (Names.qualifyName @@ var "name"))

-- | Check whether a Hydra type maps to a Java type that does not implement Comparable
isNonComparableType :: TypedTermDefinition (Type -> Bool)
isNonComparableType = def "isNonComparableType" $
  lambda "typ" $ cases _Type (Strip.deannotateType @@ var "typ")
    (Just $ boolean False) [
    _Type_either>>: constant $ boolean True,
    _Type_function>>: constant $ boolean True,
    _Type_unit>>: constant $ boolean True,
    _Type_literal>>: lambda "lt" $
      cases _LiteralType (var "lt") (Just $ boolean False) [
        _LiteralType_binary>>: constant $ boolean True],
    _Type_forall>>: lambda "ft" $
      isNonComparableType @@ (Core.forallTypeBody (var "ft"))]

-- | Check if a variable is recursive (self-referencing) in the current context
isRecursiveVariable :: TypedTermDefinition (JavaHelpers.Aliases -> Name -> Bool)
isRecursiveVariable = def "isRecursiveVariable" $
  lambda "aliases" $ lambda "name" $
    Sets.member (var "name")
      (project JavaHelpers._Aliases JavaHelpers._Aliases_recursiveVars @@ var "aliases")

-- | Check whether a type is "serializable" (record, union, wrap, or forall wrapping a serializable type).
-- These are the types that get promoted to Java class declarations.
-- Delegates to the shared isNominalType in Predicates.
isSerializableJavaType :: TypedTermDefinition (Type -> Bool)
isSerializableJavaType = def "isSerializableJavaType" $
  lambda "typ" $ Predicates.isNominalType @@ var "typ"

-- | Check if a Name is simple (unqualified, no dots).
isSimpleName :: TypedTermDefinition (Name -> Bool)
isSimpleName = def "isSimpleName" $
  lambda "name" $
    Equality.equal
      (Lists.length (Strings.splitOn (string ".") (Core.unName (var "name"))))
      (int32 1)

-- | Check if a name looks like an unresolved type inference variable.
-- These are generated by the type inference engine and have the form 't' followed by digits.
isUnresolvedInferenceVar :: TypedTermDefinition (Name -> Bool)
isUnresolvedInferenceVar = def "isUnresolvedInferenceVar" $
  lambda "name" $
    "chars" <~ Strings.toList (unwrap _Name @@ var "name") $
    Optionals.fromOptional (boolean False) (Optionals.map
      (lambda "p" $ lets [
        "firstCh">: Pairs.first (var "p"),
        "rest">: Pairs.second (var "p")] $
        Logic.ifElse
          (Logic.not $ Equality.equal (var "firstCh") (int32 116))  -- 't'
          (boolean False)
          (Logic.and
            (Logic.not $ Lists.null (var "rest"))
            (Lists.null $ Lists.filter
              (lambda "c" $ Logic.not (isUnresolvedInferenceVar_isDigit @@ var "c"))
              (var "rest"))))
      (Lists.uncons (var "chars")))

isUnresolvedInferenceVar_isDigit :: TypedTermDefinition (Int -> Bool)
isUnresolvedInferenceVar_isDigit = def "isUnresolvedInferenceVar_isDigit" $
  lambda "c" $
    Logic.and (Equality.gte (var "c") (int32 48)) (Equality.lte (var "c") (int32 57))

-- | Classify a data term by its symbol class (constant, nullary function, hoisted lambda, etc.)

java11Features :: TypedTermDefinition JavaHelpers.JavaFeatures
java11Features = def "java11Features" $
  record JavaHelpers._JavaFeatures [
    JavaHelpers._JavaFeatures_supportsDiamondOperator>>: boolean True]

java8Features :: TypedTermDefinition JavaHelpers.JavaFeatures
java8Features = def "java8Features" $
  record JavaHelpers._JavaFeatures [
    JavaHelpers._JavaFeatures_supportsDiamondOperator>>: boolean False]

-- | Shared helper: reference type for Comparable, used in cast expressions for compareTo
javaComparableRefType :: TypedTermDefinition Java.ReferenceType
javaComparableRefType = def "javaComparableRefType" $
  JavaDsl.referenceTypeClassOrInterface (JavaDsl.classOrInterfaceTypeClass
    (JavaDsl.classType
      (list ([] :: [TypedTerm Java.Annotation]))
      JavaDsl.classTypeQualifierNone
      (JavaUtilsSource.javaTypeIdentifier @@ string "Comparable")
      (list ([] :: [TypedTerm Java.TypeArgument]))))

-- | Get Graph from JavaEnvironment
javaEnvGetGraph :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> Graph)
javaEnvGetGraph = def "javaEnvGetGraph" $
  lambda "env" $ project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_graph @@ var "env"

-- | Set Graph in JavaEnvironment (preserving other fields)
javaEnvSetGraph :: TypedTermDefinition (Graph -> JavaHelpers.JavaEnvironment -> JavaHelpers.JavaEnvironment)
javaEnvSetGraph = def "javaEnvSetGraph" $
  lambda "g" $ lambda "env" $
    record JavaHelpers._JavaEnvironment [
      JavaHelpers._JavaEnvironment_aliases>>:
        project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env",
      JavaHelpers._JavaEnvironment_graph>>: var "g"]

-- | Analyze a Java function term, collecting lambdas, type lambdas, lets, and type applications

javaFeatures :: TypedTermDefinition JavaHelpers.JavaFeatures
javaFeatures = def "javaFeatures" $
  asTerm java11Features

javaIdentifierToString :: TypedTermDefinition (Java.Identifier -> String)
javaIdentifierToString = def "javaIdentifierToString" $
  lambda "id" $ unwrap Java._Identifier @@ var "id"

-- | Get type arguments for a named type by looking up its definition
javaTypeArgumentsForNamedType :: TypedTermDefinition (Name -> InferenceContext -> Graph -> Either Error [Java.TypeArgument])
javaTypeArgumentsForNamedType = def "javaTypeArgumentsForNamedType" $
  lambda "tname" $
    "cx" ~> "g" ~>
    "typ" <<~ (Resolution.requireType @@ var "cx" @@ var "g" @@ var "tname") $
    right $ Lists.map (lambda "tp_" $ JavaUtilsSource.typeParameterToTypeArgument @@ var "tp_")
      (javaTypeParametersForType @@ var "typ")

-- | Helper: convert a Java literal to a Java expression

javaTypeArgumentsForType :: TypedTermDefinition (Type -> [Java.TypeArgument])
javaTypeArgumentsForType = def "javaTypeArgumentsForType" $
  lambda "typ" $ Lists.reverse
    (Lists.map JavaUtilsSource.typeParameterToTypeArgument (javaTypeParametersForType @@ var "typ"))

javaTypeParametersForType :: TypedTermDefinition (Type -> [Java.TypeParameter])
javaTypeParametersForType = def "javaTypeParametersForType" $
  lambda "typ" $ lets [
    "toParam">: lambda "name" $
      JavaUtilsSource.javaTypeParameter @@ (Formatting.capitalize @@ (Core.unName $ var "name")),
    "boundVars">: javaTypeParametersForType_bvars @@ var "typ",
    "freeVars">: Lists.filter (lambda "v" $ isLambdaBoundVariable @@ var "v")
      (Sets.toList (Variables.freeVariablesInType @@ var "typ")),
    "vars">: (Lists.nub :: TypedTerm [Name] -> TypedTerm [Name]) (Lists.concat2 (var "boundVars") (var "freeVars"))] $
    Lists.map (var "toParam") (var "vars")

javaTypeParametersForType_bvars :: TypedTermDefinition (Type -> [Name])
javaTypeParametersForType_bvars = def "javaTypeParametersForType_bvars" $
  lambda "t" $ cases _Type (var "t")
    (Just $ list ([] :: [TypedTerm Name])) [
    _Type_forall>>: lambda "ft" $
      Lists.cons
        (Core.forallTypeParameter (var "ft"))
        (javaTypeParametersForType_bvars @@ (Core.forallTypeBody (var "ft")))]

-- | Top-level entry point: convert a module to Java source files.
moduleToJava :: TypedTermDefinition (Module -> [Definition] -> InferenceContext -> Graph -> Either Error (M.Map FilePath String))
moduleToJava = def "moduleToJava" $
  lambda "mod" $ lambda "defs" $
    "cx" ~> "g" ~>
    ("units" <<~ (encodeDefinitions @@ var "mod" @@ var "defs" @@ var "cx" @@ var "g") $
      right (Maps.fromList (Lists.map
        (lambda "entry" $
          "name" <~ Pairs.first (var "entry") $
          "unit" <~ Pairs.second (var "entry") $
          pair (bindingNameToFilePath @@ var "name")
            (SerializationSource.printExpr @@ (SerializationSource.parenthesize @@ (JavaSerdeSource.compilationUnitToExpr @@ var "unit"))))
        (Maps.toList (var "units")))))

-- | Convert Name->Name map to Name->Type map (wrapping values as TypeVariable)
nameMapToTypeMap :: TypedTermDefinition (M.Map Name Name -> M.Map Name Type)
nameMapToTypeMap = def "nameMapToTypeMap" $
  lambda "m" $
    Maps.map (lambda "v" $ Core.typeVariable (var "v")) (var "m")

-- | Get the parent namespace (all but last segment), or Nothing if there is no parent.
-- E.g., "hydra.formatting" -> Just "hydra", "hydra.java.syntax" -> Just "hydra.java"
namespaceParent :: TypedTermDefinition (ModuleName -> Maybe ModuleName)
namespaceParent = def "namespaceParent" $
  lambda "ns" $ lets [
    "parts">: Strings.splitOn (string ".") (unwrap _ModuleName @@ var "ns"),
    "initParts">: Optionals.fromOptional (list ([] :: [TypedTerm String])) (Lists.maybeInit (var "parts"))] $
    Logic.ifElse (Lists.null (var "initParts"))
      nothing
      (just (wrap _ModuleName (Strings.intercalate (string ".") (var "initParts"))))

noComment :: TypedTermDefinition (Java.ClassBodyDeclaration -> Java.ClassBodyDeclarationWithComments)
noComment = def "noComment" $
  lambda "decl" $ JavaDsl.classBodyDeclarationWithComments (var "decl") nothing

-- | Wrap an interface member declaration with no comment.
noInterfaceComment :: TypedTermDefinition (Java.InterfaceMemberDeclaration -> Java.InterfaceMemberDeclarationWithComments)
noInterfaceComment = def "noInterfaceComment" $
  lambda "decl" $ record Java._InterfaceMemberDeclarationWithComments [
    Java._InterfaceMemberDeclarationWithComments_value>>: var "decl",
    Java._InterfaceMemberDeclarationWithComments_comments>>: nothing]

-- | Generate the otherwise (default) branch of a visitor.
otherwiseBranch :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> JavaHelpers.Aliases -> Type -> Type -> Name -> Java.Type -> [Java.TypeArgument] -> Term -> InferenceContext -> Graph -> Either Error Java.ClassBodyDeclarationWithComments)
otherwiseBranch = def "otherwiseBranch" $
  lambda "env" $ lambda "aliases" $ lambda "dom" $ lambda "cod" $ lambda "tname" $ lambda "jcod" $ lambda "targs" $ lambda "d" $
    "cx" ~> "g" ~>
    "jdom" <~ (JavaDsl.typeReference (JavaUtilsSource.nameToJavaReferenceType @@ var "aliases" @@ true @@ var "targs" @@ var "tname" @@ nothing)) $
    "mods" <~ list [inject Java._MethodModifier Java._MethodModifier_public unit] $
    "anns" <~ list [asTerm JavaUtilsSource.overrideAnnotation] $
    "param" <~ (JavaUtilsSource.javaTypeToJavaFormalParameter @@ var "jdom" @@ wrap _Name (string "instance")) $
    "result" <~ (JavaDsl.resultType (JavaDsl.unannType (var "jcod"))) $
    "fs" <<~ (analyzeJavaFunction @@ var "env" @@ var "d" @@ var "cx" @@ var "g") $
    "bindings" <~ (project _FunctionStructure _FunctionStructure_bindings @@ var "fs") $
    "rawBody" <~ (project _FunctionStructure _FunctionStructure_body @@ var "fs") $
    "innerBody" <~ (annotateBodyWithCod @@ var "cod" @@ var "rawBody") $
    "env2" <~ (project _FunctionStructure _FunctionStructure_environment @@ var "fs") $
    "bindResult" <<~ (bindingsToStatements @@ var "env2" @@ var "bindings" @@ var "cx" @@ var "g") $
    "bindingStmts" <~ Pairs.first (var "bindResult") $
    "env3" <~ Pairs.second (var "bindResult") $
    "jret" <<~ (encodeTerm @@ var "env3" @@ var "innerBody" @@ var "cx" @@ var "g") $
    "returnStmt" <~ (JavaDsl.blockStatementStatement (JavaUtilsSource.javaReturnStatement @@ just (var "jret"))) $
    "allStmts" <~ Lists.concat2 (var "bindingStmts") (list [var "returnStmt"]) $
    right (noComment @@ (JavaUtilsSource.methodDeclaration @@ var "mods" @@ list ([] :: [TypedTerm Java.TypeParameter]) @@ var "anns"
      @@ asTerm JavaNamesSource.otherwiseMethodName @@ list [var "param"] @@ var "result" @@ just (var "allStmts")))

-- | Peel N domain types from a function type, returning the domains and the final codomain.
peelDomainTypes :: TypedTermDefinition (Int -> Type -> ([Type], Type))
peelDomainTypes = def "peelDomainTypes" $
  lambda "n" $ lambda "t" $
    Logic.ifElse
      (Equality.lte (var "n") (int32 0))
      (pair (list ([] :: [TypedTerm Type])) (var "t"))
      (cases _Type (Strip.deannotateType @@ var "t")
        (Just $ pair (list ([] :: [TypedTerm Type])) (var "t")) [
        _Type_function>>: lambda "ft" $
          "rest" <~ (peelDomainTypes @@ Math.sub (var "n") (int32 1) @@ Core.functionTypeCodomain (var "ft")) $
          pair
            (Lists.cons (Core.functionTypeDomain (var "ft")) (Pairs.first (var "rest")))
            (Pairs.second (var "rest"))])

-- | Peel domain types from a function type, returning the list of domains and the codomain.
-- Given a count n and a type, peels up to n function types off the front.
peelDomainsAndCod :: TypedTermDefinition (Int -> Type -> ([Type], Type))
peelDomainsAndCod = def "peelDomainsAndCod" $
  lambda "n" $ lambda "t" $
    Logic.ifElse (Equality.lte (var "n") (int32 0))
      (pair (list ([] :: [TypedTerm Type])) (var "t"))
      (cases _Type (Strip.deannotateType @@ var "t")
        (Just $ pair (list ([] :: [TypedTerm Type])) (var "t")) [
        _Type_function>>: lambda "ft" $
          "rest" <~ (peelDomainsAndCod @@ Math.sub (var "n") (int32 1) @@ Core.functionTypeCodomain (var "ft")) $
          pair (Lists.cons (Core.functionTypeDomain (var "ft")) (Pairs.first (var "rest")))
            (Pairs.second (var "rest"))])

-- | Peel expected argument types from a type scheme body using a substitution
peelExpectedTypes :: TypedTermDefinition (M.Map Name Type -> Int -> Type -> [Type])
peelExpectedTypes = def "peelExpectedTypes" $
  lambda "subst" $ lambda "n" $ lambda "t" $
    Logic.ifElse (Equality.equal (var "n") (int32 0))
      (list ([] :: [TypedTerm Type]))
      (cases _Type (Strip.deannotateType @@ var "t")
        (Just $ list ([] :: [TypedTerm Type])) [
        _Type_function>>: lambda "ft" $
          Lists.cons
            (applySubstFull @@ var "subst" @@ Core.functionTypeDomain (var "ft"))
            (peelExpectedTypes @@ var "subst" @@ Math.sub (var "n") (int32 1) @@ Core.functionTypeCodomain (var "ft"))])

-- | Propagate a correct type annotation through a term. Sets the type annotation on the term
-- and, for lambdas, also recursively annotates the body with the codomain type.
propagateType :: TypedTermDefinition (Type -> Term -> Term)
propagateType = def "propagateType" $
  lambda "typ" $ lambda "term" $
    "setTypeAnn" <~ (lambda "t" $
      Annotations.setTermAnnotation @@ Constants.keyType
        @@ just (Phantoms.encoderFor _Type @@ var "typ")
        @@ var "t") $
    cases _Term (Strip.deannotateTerm @@ var "term")
      (Just $ var "setTypeAnn" @@ var "term") [
      _Term_lambda>>: lambda "lam" $
        "annotated" <~ (var "setTypeAnn" @@ var "term") $
        cases _Type (Strip.deannotateType @@ var "typ")
          (Just $ var "annotated") [
          _Type_function>>: lambda "ft" $
            propagateType_propagateIntoLambda
              @@ (Core.functionTypeCodomain (var "ft"))
              @@ var "annotated"],
      _Term_let>>: lambda "lt" $
        -- Propagate into let binding values using each binding's type scheme
        "propagatedBindings" <~ Lists.map
          ("b" ~>
            optCases (Core.bindingTypeScheme $ var "b")
              (var "b")
              ("ts" ~> Core.binding
                (Core.bindingName $ var "b")
                (propagateType @@ (Core.typeSchemeBody $ var "ts") @@ (Core.bindingTerm $ var "b"))
                (Core.bindingTypeScheme $ var "b")))
          (Core.letBindings (var "lt")) $
        var "setTypeAnn" @@
          (propagateType_rebuildLet @@ var "term"
            @@ (var "propagatedBindings")
            @@ (propagateType @@ var "typ" @@ Core.letBody (var "lt"))),
      -- Propagate into application: annotate union case elimination LHS with function type
      _Term_application>>: lambda "app" $
        "fun" <~ Core.applicationFunction (var "app") $
        "arg" <~ Core.applicationArgument (var "app") $
        "annotatedFun" <~ (cases _Term (Strip.deannotateTerm @@ var "fun")
          (Just $ var "fun") [
          _Term_cases>>: lambda "cs" $
            "dom" <~ (Resolution.nominalApplication @@ (Core.caseStatementTypeName (var "cs"))
              @@ list ([] :: [TypedTerm Type])) $
            "ft" <~ inject _Type _Type_function (Core.functionType (var "dom") (var "typ")) $
            Annotations.setTermAnnotation @@ asTerm Constants.keyType
              @@ just (Phantoms.encoderFor _Type @@ var "ft") @@ var "fun"]) $
        var "setTypeAnn" @@ Core.termApplication (Core.application (var "annotatedFun") (var "arg"))]

-- | Propagate the codomain type into a lambda's body, traversing through annotations
propagateType_propagateIntoLambda :: TypedTermDefinition (Type -> Term -> Term)
propagateType_propagateIntoLambda = def "propagateType_propagateIntoLambda" $
  lambda "cod" $ lambda "t" $
    cases _Term (var "t")
      (Just $ var "t") [
      _Term_annotated>>: lambda "at" $
        Core.termAnnotated (Core.annotatedTerm
          (propagateType_propagateIntoLambda @@ var "cod" @@ Core.annotatedTermBody (var "at"))
          (Core.annotatedTermAnnotation (var "at"))),
      _Term_lambda>>: lambda "lam" $
        Core.termLambda (Core.lambda
          (Core.lambdaParameter (var "lam"))
          (Core.lambdaDomain (var "lam"))
          (propagateType @@ var "cod" @@ Core.lambdaBody (var "lam")))]

-- | Rebuild a let expression with a new body, preserving annotations
propagateType_rebuildLet :: TypedTermDefinition (Term -> [Binding] -> Term -> Term)
propagateType_rebuildLet = def "propagateType_rebuildLet" $
  lambda "t" $ lambda "bindings" $ lambda "newBody" $
    cases _Term (var "t")
      (Just $ var "t") [
      _Term_annotated>>: lambda "at" $
        Core.termAnnotated (Core.annotatedTerm
          (propagateType_rebuildLet @@ Core.annotatedTermBody (var "at") @@ var "bindings" @@ var "newBody")
          (Core.annotatedTermAnnotation (var "at"))),
      _Term_let>>: lambda "_lt" $
        Core.termLet (Core.let_ (var "bindings") (var "newBody"))]

-- | For application chains, propagate type annotations through the chain.
-- If f is a lambda with domain annotations and N args are applied,
-- annotate f with its full type and rebuild the chain with intermediate annotations.
propagateTypesInAppChain :: TypedTermDefinition (Type -> Type -> Term -> Term)
propagateTypesInAppChain = def "propagateTypesInAppChain" $
  lambda "fixedCod" $ lambda "resultType" $ lambda "t" $
    "flattened" <~ (flattenApps @@ var "t" @@ list ([] :: [TypedTerm Term])) $
    "args" <~ Pairs.first (var "flattened") $
    "fun" <~ Pairs.second (var "flattened") $
    "lambdaDomsResult" <~ (collectLambdaDomains @@ var "fun") $
    "lambdaDoms" <~ Pairs.first (var "lambdaDomsResult") $
    "nArgs" <~ Lists.length (var "args") $
    "nLambdaDoms" <~ Lists.length (var "lambdaDoms") $
    Logic.ifElse (Logic.and (Equality.gt (var "nLambdaDoms") (int32 0))
                            (Equality.gt (var "nArgs") (int32 0)))
      -- Lambda with args applied: compute full type and rebuild with annotations
      ("bodyRetType" <~ Pairs.second (peelDomainsAndCod @@ Math.sub (var "nLambdaDoms") (var "nArgs") @@ var "resultType") $
       "funType" <~ Lists.foldl (lambda "c" $ lambda "d" $
         inject _Type _Type_function (Core.functionType (var "d") (var "c")))
         (var "bodyRetType") (Lists.reverse (var "lambdaDoms")) $
       "annotatedFun" <~ (Annotations.setTermAnnotation @@ asTerm Constants.keyType
         @@ just (encodeTypeAsTerm @@ var "funType") @@ var "fun") $
       rebuildApps @@ var "annotatedFun" @@ var "args" @@ var "funType")
      -- Not a lambda or no args: fall back to simple annotation
      (cases _Term (Strip.deannotateTerm @@ var "t")
        (Just $ Annotations.setTermAnnotation @@ asTerm Constants.keyType
          @@ just (encodeTypeAsTerm @@ var "resultType") @@ var "t") [
        _Term_application>>: lambda "app" $
          "lhs" <~ (project _Application _Application_function @@ var "app") $
          "rhs" <~ (project _Application _Application_argument @@ var "app") $
          -- Annotate case statement LHS with function type
          "annotatedLhs" <~ (cases _Term (Strip.deannotateTerm @@ var "lhs")
            (Just $ var "lhs") [
            _Term_cases>>: lambda "cs" $
              "dom" <~ (Resolution.nominalApplication @@ (Core.caseStatementTypeName (var "cs"))
                @@ list ([] :: [TypedTerm Type])) $
              "ft" <~ inject _Type _Type_function (Core.functionType (var "dom") (var "fixedCod")) $
              Annotations.setTermAnnotation @@ asTerm Constants.keyType
                @@ just (encodeTypeAsTerm @@ var "ft") @@ var "lhs"]) $
          Annotations.setTermAnnotation @@ asTerm Constants.keyType
            @@ just (encodeTypeAsTerm @@ var "resultType")
            @@ inject _Term _Term_application (Core.application (var "annotatedLhs") (var "rhs"))])

-- | Rebuild an application chain with proper type annotations at each step.
rebuildApps :: TypedTermDefinition (Term -> [Term] -> Type -> Term)
rebuildApps = def "rebuildApps" $
  lambda "f" $ lambda "args" $ lambda "fType" $
    Logic.ifElse (Lists.null (var "args"))
      (var "f")
      (cases _Type (Strip.deannotateType @@ var "fType")
        (Just $ Lists.foldl (lambda "acc" $ lambda "a" $
          inject _Term _Term_application (Core.application (var "acc") (var "a")))
          (var "f") (var "args")) [
        _Type_function>>: lambda "ft" $
          Optionals.fromOptional (var "f") (Optionals.map
            (lambda "p" $
              "arg" <~ Pairs.first (var "p") $
              "rest" <~ Pairs.second (var "p") $
              "remainingType" <~ Core.functionTypeCodomain (var "ft") $
              "app" <~ inject _Term _Term_application (Core.application (var "f") (var "arg")) $
              "annotatedApp" <~ (Annotations.setTermAnnotation @@ asTerm Constants.keyType
                @@ just (encodeTypeAsTerm @@ var "remainingType") @@ var "app") $
              rebuildApps @@ var "annotatedApp" @@ var "rest" @@ var "remainingType")
            (Lists.uncons (var "args")))])

-- | Generate a compareTo method for a record type.
recordCompareToMethod :: TypedTermDefinition (JavaHelpers.Aliases -> [Java.TypeParameter] -> Name -> [FieldType] -> Java.ClassBodyDeclaration)
recordCompareToMethod = def "recordCompareToMethod" $
  lambda "aliases" $ lambda "tparams" $ lambda "elName" $ lambda "fields" $ lets [
    "anns">: list [asTerm JavaUtilsSource.overrideAnnotation, asTerm JavaUtilsSource.suppressWarningsUncheckedAnnotation],
    "mods">: list [inject Java._MethodModifier Java._MethodModifier_public unit],
    "param">: JavaUtilsSource.javaTypeToJavaFormalParameter @@ (JavaUtilsSource.javaTypeFromTypeName @@ var "aliases" @@ var "elName") @@ wrap _Name (asTerm JavaNamesSource.otherInstanceName),
    "result">: JavaUtilsSource.javaTypeToJavaResult @@ (asTerm JavaUtilsSource.javaIntType)] $
    JavaUtilsSource.methodDeclaration @@ var "mods" @@ list ([] :: [TypedTerm Java.TypeParameter]) @@ var "anns"
      @@ (asTerm JavaNamesSource.compareToMethodName) @@ list [var "param"] @@ var "result"
      @@ just (compareToBody @@ var "aliases" @@ (asTerm JavaNamesSource.otherInstanceName) @@ var "fields")

-- | Build a record constructor.
recordConstructor :: TypedTermDefinition (JavaHelpers.Aliases -> Name -> [FieldType] -> InferenceContext -> Graph -> Either Error Java.ClassBodyDeclaration)
recordConstructor = def "recordConstructor" $
  lambda "aliases" $ lambda "elName" $ lambda "fields" $
    "cx" ~> "g" ~>
    lets [
    "assignStmts">: Lists.map
      (lambda "f" $ JavaDsl.blockStatementStatement (JavaUtilsSource.toAssignStmt @@ Core.fieldTypeName (var "f")))
      (var "fields")] $
    "params" <<~ (Eithers.mapList (lambda "f" $ fieldTypeToFormalParam @@ var "aliases" @@ var "f" @@ var "cx" @@ var "g") (var "fields")) $
    right (JavaUtilsSource.makeConstructor @@ var "aliases" @@ var "elName" @@ false @@ var "params" @@ var "assignStmts")

-- | Build the equals() method for a record class.
recordEqualsMethod :: TypedTermDefinition (JavaHelpers.Aliases -> Name -> [FieldType] -> Java.ClassBodyDeclaration)
recordEqualsMethod = def "recordEqualsMethod" $
  lambda "aliases" $ lambda "elName" $ lambda "fields" $ lets [
    "anns">: list [asTerm JavaUtilsSource.overrideAnnotation],
    "mods">: list [inject Java._MethodModifier Java._MethodModifier_public unit],
    "param">: JavaUtilsSource.javaTypeToJavaFormalParameter
      @@ (JavaUtilsSource.javaRefType @@ list ([] :: [TypedTerm Java.ReferenceType]) @@ nothing @@ string "Object")
      @@ wrap _Name (asTerm JavaNamesSource.otherInstanceName),
    "result">: JavaUtilsSource.javaTypeToJavaResult @@ (asTerm JavaUtilsSource.javaBooleanType),
    "tmpName">: string "o",
    -- if (!(other instanceof ElName)) return false;
    "instanceOfStmt">: JavaDsl.blockStatementStatement
      (JavaDsl.statementIfThen (JavaDsl.ifThenStatement
        (JavaUtilsSource.javaUnaryExpressionToJavaExpression @@
          (JavaDsl.unaryExpressionOther
            (JavaDsl.unaryExpressionNotPlusMinusNot
              (JavaUtilsSource.javaRelationalExpressionToJavaUnaryExpression @@
                (JavaUtilsSource.javaInstanceOf
                  @@ (JavaUtilsSource.javaIdentifierToJavaRelationalExpression
                    @@ (JavaUtilsSource.javaIdentifier @@ (asTerm JavaNamesSource.otherInstanceName)))
                  @@ (JavaUtilsSource.nameToJavaReferenceType @@ var "aliases" @@ false
                    @@ list ([] :: [TypedTerm Java.TypeArgument]) @@ var "elName" @@ nothing))))))
        (JavaUtilsSource.javaReturnStatement @@ just (JavaUtilsSource.javaBooleanExpression @@ false)))),
    -- ElName o = (ElName) other;
    "castStmt">: JavaUtilsSource.variableDeclarationStatement @@ var "aliases"
      @@ (JavaUtilsSource.javaTypeFromTypeName @@ var "aliases" @@ var "elName")
      @@ (JavaUtilsSource.javaIdentifier @@ var "tmpName")
      @@ (JavaUtilsSource.javaCastExpressionToJavaExpression @@
        (JavaUtilsSource.javaCastExpression
          @@ (JavaUtilsSource.nameToJavaReferenceType @@ var "aliases" @@ false
            @@ list ([] :: [TypedTerm Java.TypeArgument]) @@ var "elName" @@ nothing)
          @@ (JavaUtilsSource.javaIdentifierToJavaUnaryExpression
            @@ wrap Java._Identifier (JavaUtilsSource.sanitizeJavaName @@ (asTerm JavaNamesSource.otherInstanceName))))),
    -- return Objects.equals(this.f1, o.f1) && Objects.equals(this.f2, o.f2) && ...
    "returnAllFieldsEqual">: JavaDsl.blockStatementStatement
      (JavaUtilsSource.javaReturnStatement @@ just
        (Logic.ifElse (Lists.null (var "fields"))
          (JavaUtilsSource.javaBooleanExpression @@ true)
          (JavaUtilsSource.javaConditionalAndExpressionToJavaExpression @@
            (JavaDsl.conditionalAndExpression
              (Lists.map (lambda "f" $ eqClause @@ var "tmpName" @@ var "f") (var "fields"))))))] $
    JavaUtilsSource.methodDeclaration @@ var "mods" @@ list ([] :: [TypedTerm Java.TypeParameter]) @@ var "anns"
      @@ (asTerm JavaNamesSource.equalsMethodName) @@ list [var "param"] @@ var "result"
      @@ just (list [var "instanceOfStmt", var "castStmt", var "returnAllFieldsEqual"])

-- | Build the hashCode() method for a record class.
recordHashCodeMethod :: TypedTermDefinition ([FieldType] -> Java.ClassBodyDeclaration)
recordHashCodeMethod = def "recordHashCodeMethod" $
  lambda "fields" $ lets [
    "anns">: list [asTerm JavaUtilsSource.overrideAnnotation],
    "mods">: list [inject Java._MethodModifier Java._MethodModifier_public unit],
    "result">: JavaUtilsSource.javaTypeToJavaResult @@ (asTerm JavaUtilsSource.javaIntType),
    "returnSum">: JavaDsl.blockStatementStatement
      (Logic.ifElse (Lists.null (var "fields"))
        (JavaUtilsSource.javaReturnStatement @@ just (JavaUtilsSource.javaIntExpression @@ bigintAsInt (bigint 0)))
        (JavaUtilsSource.javaReturnStatement @@ just
          (JavaUtilsSource.javaAdditiveExpressionToJavaExpression @@
            (JavaUtilsSource.addExpressions @@
              (Lists.zipWith (asTerm hashCodeMultPair)
                (asTerm first20Primes)
                (Lists.map (lambda "f" $ Core.fieldTypeName (var "f")) (var "fields")))))))] $
    JavaUtilsSource.methodDeclaration @@ var "mods" @@ list ([] :: [TypedTerm Java.TypeParameter]) @@ var "anns"
      @@ (asTerm JavaNamesSource.hashCodeMethodName) @@ list ([] :: [TypedTerm Java.FormalParameter]) @@ var "result"
      @@ just (list [var "returnSum"])

-- | Build a record field as a public final member variable declaration.
recordMemberVar :: TypedTermDefinition (JavaHelpers.Aliases -> FieldType -> InferenceContext -> Graph -> Either Error Java.ClassBodyDeclaration)
recordMemberVar = def "recordMemberVar" $
  lambda "aliases" $ lambda "ft" $
    "cx" ~> "g" ~>
    lets [
    "mods">: list [inject Java._FieldModifier Java._FieldModifier_public unit,
                   inject Java._FieldModifier Java._FieldModifier_final unit],
    "fname">: Core.fieldTypeName (var "ft"),
    "ftype">: Core.fieldTypeType (var "ft")] $
    "jt" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "ftype" @@ var "cx" @@ var "g") $
    right (JavaUtilsSource.javaMemberField @@ var "mods" @@ var "jt"
      @@ (JavaUtilsSource.fieldNameToJavaVariableDeclarator @@ var "fname"))

-- | Build a "with" method for a record field.
recordWithMethod :: TypedTermDefinition (JavaHelpers.Aliases -> Name -> [FieldType] -> FieldType -> InferenceContext -> Graph -> Either Error Java.ClassBodyDeclaration)
recordWithMethod = def "recordWithMethod" $
  lambda "aliases" $ lambda "elName" $ lambda "fields" $ lambda "field" $
    "cx" ~> "g" ~>
    lets [
    "mods">: list [inject Java._MethodModifier Java._MethodModifier_public unit],
    "anns">: list ([] :: [TypedTerm Java.Annotation]),
    "methodName">: Strings.cat2 (string "with")
      (Formatting.nonAlnumToUnderscores @@ (Formatting.capitalize @@ (unwrap _Name @@ Core.fieldTypeName (var "field")))),
    "result">: JavaUtilsSource.referenceTypeToResult
      @@ (JavaUtilsSource.nameToJavaReferenceType @@ var "aliases" @@ false
        @@ list ([] :: [TypedTerm Java.TypeArgument]) @@ var "elName" @@ nothing),
    "consId">: wrap Java._Identifier (JavaUtilsSource.sanitizeJavaName @@ (Names.localNameOf @@ var "elName")),
    "fieldArgs">: Lists.map (lambda "f" $ JavaUtilsSource.fieldNameToJavaExpression @@ Core.fieldTypeName (var "f")) (var "fields"),
    "returnStmt">: JavaDsl.blockStatementStatement
      (JavaUtilsSource.javaReturnStatement @@ just
        (JavaUtilsSource.javaConstructorCall @@ (JavaUtilsSource.javaConstructorName @@ var "consId" @@ nothing)
          @@ var "fieldArgs" @@ nothing))] $
    "param" <<~ (fieldTypeToFormalParam @@ var "aliases" @@ var "field" @@ var "cx" @@ var "g") $
    right (JavaUtilsSource.methodDeclaration @@ var "mods" @@ list ([] :: [TypedTerm Java.TypeParameter])
      @@ var "anns" @@ var "methodName" @@ list [var "param"] @@ var "result"
      @@ just (list [var "returnStmt"]))

-- | Given a partial argSubst, fill in unresolved vars from unused IR types.
resolveTypeApps :: TypedTermDefinition ([Name] -> [Type] -> M.Map Name Type -> [Type])
resolveTypeApps = def "resolveTypeApps" $
  lambda "schemeVars" $ lambda "fallbackTypeApps" $ lambda "argSubst" $
    "resolvedVars" <~ Sets.fromList (Maps.keys (var "argSubst")) $
    "unresolvedVars" <~ Lists.filter (lambda "v" $ Logic.not (Sets.member (var "v") (var "resolvedVars"))) (var "schemeVars") $
    "usedTypes" <~ Sets.fromList (Maps.elems (var "argSubst")) $
    "unusedIrTypes" <~ Lists.filter (lambda "t" $ Logic.not (Sets.member (var "t") (var "usedTypes"))) (var "fallbackTypeApps") $
    "remainingSubst" <~ Maps.fromList (Lists.zip (var "unresolvedVars") (var "unusedIrTypes")) $
    "fullSubst" <~ Maps.union (var "argSubst") (var "remainingSubst") $
    Lists.map (lambda "v" $ Maps.findWithDefault (Core.typeVariable (var "v")) (var "v") (var "fullSubst"))
      (var "schemeVars")

-- | For each group where the input var appears in its own output list,
-- substitute all other output vars to that input var.
selfRefSubstitution :: TypedTermDefinition (M.Map Name [Name] -> M.Map Name Name)
selfRefSubstitution = def "selfRefSubstitution" $
  lambda "grouped" $
    Lists.foldl
      (lambda "subst" $ lambda "entry" $
        selfRefSubstitution_processGroup @@ var "subst" @@ Pairs.first (var "entry") @@ Pairs.second (var "entry"))
      (Maps.empty)
      (Maps.toList (var "grouped"))

selfRefSubstitution_processGroup :: TypedTermDefinition (M.Map Name Name -> Name -> [Name] -> M.Map Name Name)
selfRefSubstitution_processGroup = def "selfRefSubstitution_processGroup" $
  lambda "subst" $ lambda "inVar" $ lambda "outVars" $
    Logic.ifElse
      ((Lists.elem :: TypedTerm Name -> TypedTerm [Name] -> TypedTerm Bool) (var "inVar") (var "outVars"))
      (Lists.foldl
        (lambda "s" $ lambda "v" $
          Logic.ifElse
            (Equality.equal (var "v") (var "inVar"))
            (var "s")
            (Maps.insert (var "v") (var "inVar") (var "s")))
        (var "subst")
        (var "outVars"))
      (var "subst")

-- | Direct-return substitution: for each input var with >=2 self-refs and

serializableTypes :: TypedTermDefinition (Bool -> [Java.InterfaceType])
serializableTypes = def "serializableTypes" $
  lambda "isSer" $ lets [
    "javaSerializableType">: JavaDsl.interfaceType
      (JavaDsl.classType
        (list ([] :: [TypedTerm Java.Annotation]))
        JavaDsl.classTypeQualifierNone
        (JavaUtilsSource.javaTypeIdentifier @@ string "Serializable")
        (list ([] :: [TypedTerm Java.TypeArgument])))] $
    Logic.ifElse (var "isSer")
      (list [var "javaSerializableType"])
      (list ([] :: [TypedTerm Java.InterfaceType]))

-- | Split a constant declaration into a field + helper method to avoid large <clinit>
splitConstantInitializer :: TypedTermDefinition (Java.InterfaceMemberDeclaration -> [Java.InterfaceMemberDeclaration])
splitConstantInitializer = def "splitConstantInitializer" $
  lambda "member" $ cases Java._InterfaceMemberDeclaration (var "member")
    (Just $ list [var "member"]) [
    Java._InterfaceMemberDeclaration_constant>>: lambda "cd" $
      Lists.bind
        (project Java._ConstantDeclaration Java._ConstantDeclaration_variables @@ var "cd")
        (splitConstantInitializer_splitVar
          @@ (project Java._ConstantDeclaration Java._ConstantDeclaration_modifiers @@ var "cd")
          @@ (project Java._ConstantDeclaration Java._ConstantDeclaration_type @@ var "cd"))]

-- | Helper for splitConstantInitializer: split a single variable declarator
splitConstantInitializer_splitVar :: TypedTermDefinition ([Java.ConstantModifier] -> Java.UnannType -> Java.VariableDeclarator -> [Java.InterfaceMemberDeclaration])
splitConstantInitializer_splitVar = def "splitConstantInitializer_splitVar" $
  lambda "mods" $ lambda "utype" $ lambda "vd" $ lets [
    "vid">: project Java._VariableDeclarator Java._VariableDeclarator_id @@ var "vd",
    "mInit">: project Java._VariableDeclarator Java._VariableDeclarator_initializer @@ var "vd"] $
    Optionals.cases (var "mInit")
      -- No initializer: keep as-is
      (list [inject Java._InterfaceMemberDeclaration Java._InterfaceMemberDeclaration_constant
        (record Java._ConstantDeclaration [
          Java._ConstantDeclaration_modifiers>>: var "mods",
          Java._ConstantDeclaration_type>>: var "utype",
          Java._ConstantDeclaration_variables>>: list [var "vd"]])])
      (lambda "init_" $
        cases Java._VariableInitializer (var "init_")
          -- Not an expression initializer: keep as-is
          (Just $ list [inject Java._InterfaceMemberDeclaration Java._InterfaceMemberDeclaration_constant
            (record Java._ConstantDeclaration [
              Java._ConstantDeclaration_modifiers>>: var "mods",
              Java._ConstantDeclaration_type>>: var "utype",
              Java._ConstantDeclaration_variables>>: list [var "vd"]])]) [
          Java._VariableInitializer_expression>>: lambda "expr" $ lets [
            "varName">: javaIdentifierToString @@
              (project Java._VariableDeclaratorId Java._VariableDeclaratorId_identifier @@ var "vid"),
            "helperName">: Strings.cat2 (string "_init_") (var "varName"),
            "callExpr">: JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocation @@ nothing @@ wrap Java._Identifier (var "helperName")
                @@ list ([] :: [TypedTerm Java.Expression])),
            "field">: inject Java._InterfaceMemberDeclaration Java._InterfaceMemberDeclaration_constant
              (record Java._ConstantDeclaration [
                Java._ConstantDeclaration_modifiers>>: var "mods",
                Java._ConstantDeclaration_type>>: var "utype",
                Java._ConstantDeclaration_variables>>: list [
                  record Java._VariableDeclarator [
                    Java._VariableDeclarator_id>>: var "vid",
                    Java._VariableDeclarator_initializer>>: just
                      (inject Java._VariableInitializer Java._VariableInitializer_expression
                        (var "callExpr"))]]]),
            "returnSt">: inject Java._BlockStatement Java._BlockStatement_statement
              (JavaUtilsSource.javaReturnStatement @@ just (var "expr")),
            "resultType">: inject Java._Result Java._Result_type (var "utype"),
            "helper">: JavaUtilsSource.interfaceMethodDeclaration
              @@ list [
                inject Java._InterfaceMethodModifier Java._InterfaceMethodModifier_static unit,
                inject Java._InterfaceMethodModifier Java._InterfaceMethodModifier_private unit]
              @@ list ([] :: [TypedTerm Java.TypeParameter])
              @@ var "helperName"
              @@ list ([] :: [TypedTerm Java.FormalParameter])
              @@ var "resultType"
              @@ just (list [var "returnSt"])] $
            list [var "field", var "helper"]])

-- | Strip all forall wrappers from a type, returning the body
stripForalls :: TypedTermDefinition (Type -> Type)
stripForalls = def "stripForalls" $
  lambda "t" $ cases _Type (Strip.deannotateType @@ var "t")
    (Just $ var "t") [
    _Type_forall>>: lambda "fa" $
      stripForalls @@ Core.forallTypeBody (var "fa")]

-- | Substitute type variables with types
substituteTypeVarsWithTypes :: TypedTermDefinition (M.Map Name Type -> Type -> Type)
substituteTypeVarsWithTypes = def "substituteTypeVarsWithTypes" $
  lambda "subst" $ lambda "t" $ substituteTypeVarsWithTypes_go @@ var "subst" @@ (Strip.deannotateType @@ var "t")

-- | Helper for substituteTypeVarsWithTypes
substituteTypeVarsWithTypes_go :: TypedTermDefinition (M.Map Name Type -> Type -> Type)
substituteTypeVarsWithTypes_go = def "substituteTypeVarsWithTypes_go" $
  lambda "subst" $ lambda "t" $ cases _Type (Strip.deannotateType @@ var "t")
    (Just $ var "t") [
    _Type_variable>>: lambda "v" $
      Optionals.cases (Maps.lookup (var "v") (var "subst")) (var "t") (lambda "rep" $ var "rep"),
    _Type_function>>: lambda "ft" $
      Core.typeFunction (Core.functionType
        (substituteTypeVarsWithTypes_go @@ var "subst" @@ Core.functionTypeDomain (var "ft"))
        (substituteTypeVarsWithTypes_go @@ var "subst" @@ Core.functionTypeCodomain (var "ft"))),
    _Type_application>>: lambda "at" $
      Core.typeApplication (Core.applicationType
        (substituteTypeVarsWithTypes_go @@ var "subst" @@ Core.applicationTypeFunction (var "at"))
        (substituteTypeVarsWithTypes_go @@ var "subst" @@ (project _ApplicationType _ApplicationType_argument @@ var "at"))),
    _Type_list>>: lambda "inner" $
      Core.typeList (substituteTypeVarsWithTypes_go @@ var "subst" @@ var "inner"),
    _Type_set>>: lambda "inner" $
      Core.typeSet (substituteTypeVarsWithTypes_go @@ var "subst" @@ var "inner"),
    _Type_optional>>: lambda "inner" $
      Core.typeOptional (substituteTypeVarsWithTypes_go @@ var "subst" @@ var "inner"),
    _Type_map>>: lambda "mt" $
      Core.typeMap (Core.mapType
        (substituteTypeVarsWithTypes_go @@ var "subst" @@ Core.mapTypeKeys (var "mt"))
        (substituteTypeVarsWithTypes_go @@ var "subst" @@ Core.mapTypeValues (var "mt"))),
    _Type_pair>>: lambda "pt" $
      Core.typePair (Core.pairType
        (substituteTypeVarsWithTypes_go @@ var "subst" @@ Core.pairTypeFirst (var "pt"))
        (substituteTypeVarsWithTypes_go @@ var "subst" @@ Core.pairTypeSecond (var "pt"))),
    _Type_either>>: lambda "et" $
      Core.typeEither (Core.eitherType
        (substituteTypeVarsWithTypes_go @@ var "subst" @@ Core.eitherTypeLeft (var "et"))
        (substituteTypeVarsWithTypes_go @@ var "subst" @@ Core.eitherTypeRight (var "et"))),
    _Type_forall>>: lambda "ft" $
      Core.typeForall (Core.forallType
        (Core.forallTypeParameter (var "ft"))
        (substituteTypeVarsWithTypes_go @@ var "subst" @@ Core.forallTypeBody (var "ft")))]

-- | Shared helper: tagCmp != 0
tagCmpNotZeroExpr :: TypedTermDefinition Java.Expression
tagCmpNotZeroExpr = def "tagCmpNotZeroExpr" $ lets [
    "lhs">: JavaUtilsSource.javaRelationalExpressionToJavaEqualityExpression @@
      (JavaUtilsSource.javaPostfixExpressionToJavaRelationalExpression @@
        (JavaDsl.postfixExpressionName (JavaDsl.expressionName nothing (JavaUtilsSource.javaIdentifier @@ string "tagCmp")))),
    "rhs">: JavaUtilsSource.javaPostfixExpressionToJavaRelationalExpression @@
      (JavaDsl.postfixExpressionPrimary (JavaUtilsSource.javaLiteralToJavaPrimary @@ (JavaUtilsSource.javaInt @@ bigintAsInt (bigint 0))))] $
    JavaUtilsSource.javaEqualityExpressionToJavaExpression @@
      (JavaDsl.equalityExpressionNotEqual (JavaDsl.equalityExpressionBinary (var "lhs") (var "rhs")))

-- | Shared helper: this.getClass().getName().compareTo(other.getClass().getName())
-- Used in variant compareTo to compare by class name for tag ordering.
tagCompareExpr :: TypedTermDefinition Java.Expression
tagCompareExpr = def "tagCompareExpr" $ lets [
    "thisGetClass">: JavaDsl.methodInvocation_
      (JavaDsl.methodInvocationHeaderComplex
        (JavaDsl.methodInvocationComplex
          (JavaDsl.methodInvocationVariantPrimary (JavaUtilsSource.javaExpressionToJavaPrimary @@ (asTerm JavaUtilsSource.javaThis)))
          (list ([] :: [TypedTerm Java.TypeArgument]))
          (wrap Java._Identifier (string "getClass"))))
      (list ([] :: [TypedTerm Java.Expression])),
    "thisGetName">: JavaDsl.methodInvocation_
      (JavaDsl.methodInvocationHeaderComplex
        (JavaDsl.methodInvocationComplex
          (JavaDsl.methodInvocationVariantPrimary (JavaUtilsSource.javaMethodInvocationToJavaPrimary @@ var "thisGetClass"))
          (list ([] :: [TypedTerm Java.TypeArgument]))
          (wrap Java._Identifier (string "getName"))))
      (list ([] :: [TypedTerm Java.Expression])),
    "otherGetClass">: JavaDsl.methodInvocation_
      (JavaDsl.methodInvocationHeaderComplex
        (JavaDsl.methodInvocationComplex
          (JavaDsl.methodInvocationVariantExpression
            (JavaDsl.expressionName nothing (wrap Java._Identifier (asTerm JavaNamesSource.otherInstanceName))))
          (list ([] :: [TypedTerm Java.TypeArgument]))
          (wrap Java._Identifier (string "getClass"))))
      (list ([] :: [TypedTerm Java.Expression])),
    "otherGetName">: JavaDsl.methodInvocation_
      (JavaDsl.methodInvocationHeaderComplex
        (JavaDsl.methodInvocationComplex
          (JavaDsl.methodInvocationVariantPrimary (JavaUtilsSource.javaMethodInvocationToJavaPrimary @@ var "otherGetClass"))
          (list ([] :: [TypedTerm Java.TypeArgument]))
          (wrap Java._Identifier (string "getName"))))
      (list ([] :: [TypedTerm Java.Expression]))] $
    JavaUtilsSource.javaMethodInvocationToJavaExpression @@ (JavaDsl.methodInvocation_
      (JavaDsl.methodInvocationHeaderComplex
        (JavaDsl.methodInvocationComplex
          (JavaDsl.methodInvocationVariantPrimary (JavaUtilsSource.javaMethodInvocationToJavaPrimary @@ var "thisGetName"))
          (list ([] :: [TypedTerm Java.TypeArgument]))
          (wrap Java._Identifier (asTerm JavaNamesSource.compareToMethodName))))
      (list [JavaUtilsSource.javaMethodInvocationToJavaExpression @@ var "otherGetName"]))

-- | Take N type arguments from the accumulated type applications list,
-- converting them to Java TypeArguments via javaTypeToJavaReferenceType.
takeTypeArgs :: TypedTermDefinition (String -> Int -> [Java.Type] -> InferenceContext -> Graph -> Either Error [Java.TypeArgument])
takeTypeArgs = def "takeTypeArgs" $
  lambda "label" $ lambda "n" $ lambda "tyapps" $
    "cx" ~> "g" ~>
    Logic.ifElse (Equality.lt (Lists.length (var "tyapps")) (var "n"))
      (left (Error.errorOther $ Error.otherError $ Strings.cat (list [string "needed type arguments for ", var "label", string ", found too few"])))
      (Eithers.mapList (lambda "jt" $
        "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt" @@ var "cx") $
        right (JavaDsl.typeArgumentReference (var "rt")))
        (Lists.take (var "n") (var "tyapps")))

-- | Dispatch type to class declaration.
toClassDecl :: TypedTermDefinition (Bool -> Bool -> JavaHelpers.Aliases -> [Java.TypeParameter] -> Name -> Type -> InferenceContext -> Graph -> Either Error Java.ClassDeclaration)
toClassDecl = def "toClassDecl" $
  lambda "isInner" $ lambda "isSer" $ lambda "aliases" $ lambda "tparams" $ lambda "elName" $ lambda "t" $
    "cx" ~> "g" ~>
    "wrap" <~ (lambda "t'" $
      declarationForRecordType @@ var "isInner" @@ var "isSer" @@ var "aliases" @@ var "tparams" @@ var "elName"
        @@ (list [Core.fieldType (wrap _Name (string "value")) (Strip.deannotateType @@ var "t'")]) @@ var "cx" @@ var "g") $
    cases _Type (Strip.deannotateType @@ var "t")
      (Just $ var "wrap" @@ var "t") [
      _Type_record>>: lambda "rt" $
        declarationForRecordType @@ var "isInner" @@ var "isSer" @@ var "aliases" @@ var "tparams" @@ var "elName"
          @@ (var "rt") @@ var "cx" @@ var "g",
      _Type_union>>: lambda "rt" $
        declarationForUnionType @@ var "isSer" @@ var "aliases" @@ var "tparams" @@ var "elName"
          @@ (var "rt") @@ var "cx" @@ var "g",
      _Type_forall>>: lambda "fa" $
        "v" <~ Core.forallTypeParameter (var "fa") $
        "body" <~ Core.forallTypeBody (var "fa") $
        "param" <~ (JavaUtilsSource.javaTypeParameter @@ (Formatting.capitalize @@ (Core.unName (var "v")))) $
        toClassDecl @@ false @@ var "isSer" @@ var "aliases"
          @@ (Lists.concat2 (var "tparams") (list [var "param"])) @@ var "elName" @@ var "body" @@ var "cx" @@ var "g",
      _Type_wrap>>: lambda "wt" $
        declarationForRecordType @@ var "isInner" @@ var "isSer" @@ var "aliases" @@ var "tparams" @@ var "elName"
          @@ (list [Core.fieldType (wrap _Name (string "value")) (var "wt")]) @@ var "cx" @@ var "g"]

-- | Initialize a recursive binding with AtomicReference (for toDeclInit).
toDeclInit :: TypedTermDefinition (JavaHelpers.Aliases -> Graph -> S.Set Name -> [Binding] -> Name -> InferenceContext -> Graph -> Either Error (Maybe Java.BlockStatement))
toDeclInit = def "toDeclInit" $
  lambda "aliasesExt" $ lambda "gExt" $ lambda "recursiveVars" $ lambda "flatBindings" $ lambda "name" $
    "cx" ~> "g" ~>
    Logic.ifElse (Sets.member (var "name") (var "recursiveVars"))
      ("binding" <~ Optionals.fromOptional (Core.binding (var "name") Core.termUnit nothing) (Lists.maybeHead (Lists.filter (lambda "b" $ Equality.equal (Core.bindingName (var "b")) (var "name")) (var "flatBindings"))) $
        "value" <~ Core.bindingTerm (var "binding") $
        "typ" <<~ Optionals.cases (Core.bindingTypeScheme (var "binding"))
          (Checking.typeOfTerm @@ var "cx" @@ var "gExt" @@ var "value")
          (lambda "ts" $ right (Core.typeSchemeBody (var "ts"))) $
        "jtype" <<~ (encodeType @@ var "aliasesExt" @@ Sets.empty @@ var "typ" @@ var "cx" @@ var "g") $
        "id" <~ (JavaUtilsSource.variableToJavaIdentifier @@ var "name") $
        "arid" <~ (JavaDsl.identifier (string "java.util.concurrent.atomic.AtomicReference")) $
        "aid" <~ (JavaDsl.annotatedIdentifier (list ([] :: [TypedTerm Java.Annotation])) (var "arid")) $
        "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jtype" @@ var "cx") $
        "targs" <~ (typeArgsOrDiamond @@ list [JavaDsl.typeArgumentReference (var "rt")]) $
        "ci" <~ record Java._ClassOrInterfaceTypeToInstantiate [
          Java._ClassOrInterfaceTypeToInstantiate_identifiers>>: list [var "aid"],
          Java._ClassOrInterfaceTypeToInstantiate_typeArguments>>: just (var "targs")] $
        "body" <~ (JavaUtilsSource.javaConstructorCall @@ var "ci" @@ list ([] :: [TypedTerm Java.Expression]) @@ nothing) $
        "pkg" <~ (JavaNamesSource.javaPackageName @@ list [string "java", string "util", string "concurrent", string "atomic"]) $
        "artype" <~ (JavaUtilsSource.javaRefType @@ list [var "rt"] @@ just (var "pkg") @@ string "AtomicReference") $
        right (just (JavaUtilsSource.variableDeclarationStatement @@ var "aliasesExt" @@ var "artype" @@ var "id" @@ var "body")))
      (right nothing)

-- | Declare or set a binding value (for toDeclStatement).
toDeclStatement :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> JavaHelpers.Aliases -> Graph -> S.Set Name -> S.Set Name -> [Binding] -> Name -> InferenceContext -> Graph -> Either Error Java.BlockStatement)
toDeclStatement = def "toDeclStatement" $
  lambda "envExt" $ lambda "aliasesExt" $ lambda "gExt" $ lambda "recursiveVars" $ lambda "thunkedVars" $ lambda "flatBindings" $ lambda "name" $
    "cx" ~> "g" ~>
    "binding" <~ Optionals.fromOptional (Core.binding (var "name") Core.termUnit nothing) (Lists.maybeHead (Lists.filter (lambda "b" $ Equality.equal (Core.bindingName (var "b")) (var "name")) (var "flatBindings"))) $
    "value" <~ Core.bindingTerm (var "binding") $
    "typ" <<~ Optionals.cases (Core.bindingTypeScheme (var "binding"))
      (Checking.typeOfTerm @@ var "cx" @@ var "gExt" @@ var "value")
      (lambda "ts" $ right (Core.typeSchemeBody (var "ts"))) $
    "jtype" <<~ (encodeType @@ var "aliasesExt" @@ Sets.empty @@ var "typ" @@ var "cx" @@ var "g") $
    "id" <~ (JavaUtilsSource.variableToJavaIdentifier @@ var "name") $
    "annotatedValue" <~ (Annotations.setTermAnnotation @@ asTerm Constants.keyType
      @@ just (encodeTypeAsTerm @@ var "typ")
      @@ var "value") $
    "rhs" <<~ (encodeTerm @@ var "envExt" @@ var "annotatedValue" @@ var "cx" @@ var "g") $
    Logic.ifElse (Sets.member (var "name") (var "recursiveVars"))
      -- Recursive: call .set() on AtomicReference
      (right (JavaDsl.blockStatementStatement (JavaUtilsSource.javaMethodInvocationToJavaStatement @@
        (JavaUtilsSource.methodInvocation @@
          just (Phantoms.left (JavaDsl.expressionName nothing (var "id")))
          @@ JavaDsl.identifier (asTerm JavaNamesSource.setMethodName) @@ list [var "rhs"]))))
      (Logic.ifElse (Sets.member (var "name") (var "thunkedVars"))
        -- Thunked: wrap in Lazy<T>
        ("rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jtype" @@ var "cx") $
          "lazyType" <~ (JavaUtilsSource.javaRefType @@ list [var "rt"] @@ asTerm JavaNamesSource.hydraUtilPackageName @@ string "Lazy") $
          "lambdaBody" <~ inject Java._LambdaBody Java._LambdaBody_expression (var "rhs") $
          "supplierLambda" <~ (JavaDsl.expressionLambda (JavaDsl.lambdaExpression
            (inject Java._LambdaParameters Java._LambdaParameters_tuple (list ([] :: [TypedTerm Java.FormalParameter])))
            (var "lambdaBody"))) $
          "targs" <~ (typeArgsOrDiamond @@ list [JavaDsl.typeArgumentReference (var "rt")]) $
          "lazyExpr" <~ (JavaUtilsSource.javaConstructorCall
            @@ (JavaUtilsSource.javaConstructorName @@ JavaDsl.identifier (string "hydra.util.Lazy") @@ just (var "targs"))
            @@ list [var "supplierLambda"] @@ nothing) $
          right (JavaUtilsSource.variableDeclarationStatement @@ var "aliasesExt" @@ var "lazyType" @@ var "id" @@ var "lazyExpr"))
        -- Normal: simple variable declaration
        (right (JavaUtilsSource.variableDeclarationStatement @@ var "aliasesExt" @@ var "jtype" @@ var "id" @@ var "rhs")))

-- | Try to infer the function type from lambda structure when type annotations are unavailable.
-- The "funTerm" argument should be a function-form Term (lambda, project, cases, or unwrap).
tryInferFunctionType :: TypedTermDefinition (Term -> Maybe Type)
tryInferFunctionType = def "tryInferFunctionType" $
  lambda "funTerm" $
    cases _Term (Strip.deannotateTerm @@ var "funTerm")
      (Just nothing) [
      _Term_lambda>>: lambda "lam" $
        Optionals.bind (Core.lambdaDomain (var "lam")) (lambda "dom" $
          "mCod" <~ (cases _Term (Core.lambdaBody (var "lam"))
            (Just nothing) [
            _Term_annotated>>: lambda "at" $
              Optionals.bind
                (Maps.lookup (Constants.keyType) (Annotations.getAnnotationMap @@ Core.annotatedTermAnnotation (var "at")))
                (lambda "typeTerm" $
                  decodeTypeFromTerm @@ var "typeTerm"),
            _Term_lambda>>: lambda "_innerLam" $
              tryInferFunctionType @@ (Core.lambdaBody (var "lam"))]) $
          Optionals.map (lambda "cod" $
            Core.typeFunction (Core.functionType (var "dom") (var "cod")))
            (var "mCod"))]

-- | Fallback cast for TermTypeApplication: re-annotate the body with corrected type
-- before encoding, then cast.
typeAppFallbackCast :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> JavaHelpers.Aliases -> [M.Map Name Term] -> [Java.Type] -> Java.Type -> Term -> Type -> InferenceContext -> Graph -> Either Error Java.Expression)
typeAppFallbackCast = def "typeAppFallbackCast" $
  lambda "env" $ lambda "aliases" $ lambda "anns" $ lambda "tyapps" $
    lambda "jatyp" $ lambda "body" $ lambda "typ" $
      "cx" ~> "g" ~>
      "annotatedBody" <~ (Annotations.setTermAnnotation @@ asTerm Constants.keyType
        @@ just (encodeTypeAsTerm @@ var "typ") @@ var "body") $
      "jbody" <<~ (encodeTermInternal @@ var "env" @@ var "anns" @@ (Lists.cons (var "jatyp") (var "tyapps")) @@ var "annotatedBody" @@ var "cx" @@ var "g") $
      "jtype" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "typ" @@ var "cx" @@ var "g") $
      "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jtype" @@ var "cx") $
      right (JavaUtilsSource.javaCastExpressionToJavaExpression @@
        (JavaUtilsSource.javaCastExpression @@ var "rt" @@ (JavaUtilsSource.javaExpressionToJavaUnaryExpression @@ var "jbody")))

-- | Handle TermTypeApplication when the innermost body is a variable.
-- Generates explicit type witnesses for nullary static methods and hoisted lambdas
-- instead of casts, which Java can't resolve for methods with unconstrained type params.
typeAppNullaryOrHoisted :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> JavaHelpers.Aliases -> [M.Map Name Term] -> [Java.Type] -> Java.Type -> Term -> Type -> Name -> JavaHelpers.JavaSymbolClass -> [Type] -> InferenceContext -> Graph -> Either Error Java.Expression)
typeAppNullaryOrHoisted = def "typeAppNullaryOrHoisted" $
  lambda "env" $ lambda "aliases" $ lambda "anns" $ lambda "tyapps" $
    lambda "jatyp" $ lambda "body" $ lambda "correctedTyp" $ lambda "varName" $
      lambda "cls" $ lambda "allTypeArgs" $
        "cx" ~> "g" ~>
        "qn" <~ (Names.qualifyName @@ var "varName") $
        "mns" <~ Util.qualifiedNameModuleName (var "qn") $
        "localName" <~ Util.qualifiedNameLocal (var "qn") $
        cases JavaHelpers._JavaSymbolClass (var "cls")
          (Just $ typeAppFallbackCast @@ var "env" @@ var "aliases" @@ var "anns" @@ var "tyapps"
            @@ var "jatyp" @@ var "body" @@ var "correctedTyp" @@ var "cx" @@ var "g") [
          JavaHelpers._JavaSymbolClass_nullaryFunction>>: lambda "_u" $
            Optionals.cases (var "mns")
              (typeAppFallbackCast @@ var "env" @@ var "aliases" @@ var "anns" @@ var "tyapps"
                @@ var "jatyp" @@ var "body" @@ var "correctedTyp" @@ var "cx" @@ var "g")
              (lambda "ns_" $
                "classId" <~ (JavaUtilsSource.nameToJavaName @@ var "aliases"
                  @@ (elementsQualifiedName @@ var "ns_")) $
                "methodId" <~ JavaDsl.identifier (JavaUtilsSource.sanitizeJavaName @@ var "localName") $
                "filteredTypeArgs" <<~ (filterPhantomTypeArgs @@ var "varName" @@ var "allTypeArgs" @@ var "cx" @@ var "g") $
                "jTypeArgs" <<~ (Eithers.mapList (lambda "t" $
                  "jt" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "t" @@ var "cx" @@ var "g") $
                  "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt" @@ var "cx") $
                  right (inject Java._TypeArgument Java._TypeArgument_reference (var "rt")))
                  (var "filteredTypeArgs")) $
                right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
                  (JavaUtilsSource.methodInvocationStaticWithTypeArgs @@ var "classId" @@ var "methodId"
                    @@ var "jTypeArgs" @@ list ([] :: [TypedTerm Java.Expression])))),
          JavaHelpers._JavaSymbolClass_hoistedLambda>>: lambda "arity" $
            Optionals.cases (var "mns")
              (typeAppFallbackCast @@ var "env" @@ var "aliases" @@ var "anns" @@ var "tyapps"
                @@ var "jatyp" @@ var "body" @@ var "correctedTyp" @@ var "cx" @@ var "g")
              (lambda "ns_" $
                "classId" <~ (JavaUtilsSource.nameToJavaName @@ var "aliases"
                  @@ (elementsQualifiedName @@ var "ns_")) $
                "methodId" <~ JavaDsl.identifier (JavaUtilsSource.sanitizeJavaName @@ var "localName") $
                "filteredTypeArgs" <<~ (filterPhantomTypeArgs @@ var "varName" @@ var "allTypeArgs" @@ var "cx" @@ var "g") $
                "jTypeArgs" <<~ (Eithers.mapList (lambda "t" $
                  "jt" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "t" @@ var "cx" @@ var "g") $
                  "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt" @@ var "cx") $
                  right (inject Java._TypeArgument Java._TypeArgument_reference (var "rt")))
                  (var "filteredTypeArgs")) $
                "paramNames" <~ Lists.map (lambda "i" $ Core.name (Strings.cat2 (string "p") (Literals.showInt32 (var "i"))))
                  (Math.range (int32 0) (Math.sub (var "arity") (int32 1))) $
                "paramExprs" <~ Lists.map (lambda "p" $
                  JavaUtilsSource.javaIdentifierToJavaExpression @@ (JavaUtilsSource.variableToJavaIdentifier @@ var "p"))
                  (var "paramNames") $
                "call" <~ (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
                  (JavaUtilsSource.methodInvocationStaticWithTypeArgs @@ var "classId" @@ var "methodId"
                    @@ var "jTypeArgs" @@ var "paramExprs")) $
                right (buildCurriedLambda @@ var "paramNames" @@ var "call"))]

-- | Flatten a nested application chain f(a1)(a2)...(aN) into ([a1,...,aN], f).

typeArgsOrDiamond :: TypedTermDefinition ([Java.TypeArgument] -> Java.TypeArgumentsOrDiamond)
typeArgsOrDiamond = def "typeArgsOrDiamond" $
  lambda "args" $
    Logic.ifElse
      (project JavaHelpers._JavaFeatures JavaHelpers._JavaFeatures_supportsDiamondOperator @@ javaFeatures)
      JavaDsl.typeArgumentsOrDiamondDiamond
      (JavaDsl.typeArgumentsOrDiamondArguments (var "args"))

-- | Shallow structural match for types.
-- TypeVariable: check names match. TypeWrap: check type names match. Otherwise: True.
typesMatch :: TypedTermDefinition (Type -> Type -> Bool)
typesMatch = def "typesMatch" $
  lambda "a" $ lambda "b" $ cases _Type (var "a")
    (Just $ boolean True) [
    _Type_variable>>: lambda "va" $
      cases _Type (var "b")
        (Just $ boolean True) [
        _Type_variable>>: lambda "vb" $
          Equality.equal (var "va") (var "vb")],
    _Type_wrap>>: lambda "wa" $
      cases _Type (var "b")
        (Just $ boolean True) [
        _Type_wrap>>: lambda "wb" $
          Equality.equal (var "wa") (var "wb")]]

-- | Unwrap nested function types to get the final return type.
-- Also looks through type application wrappers (TypeApplication).
unwrapReturnType :: TypedTermDefinition (Type -> Type)
unwrapReturnType = def "unwrapReturnType" $
  lambda "t" $
    cases _Type (Strip.deannotateType @@ var "t")
      (Just $ var "t") [
      _Type_function>>: lambda "ft" $
        unwrapReturnType @@ Core.functionTypeCodomain (var "ft"),
      _Type_application>>: lambda "at" $
        unwrapReturnType @@ Core.applicationTypeArgument (var "at")]

-- | Generate a compareTo method for a union variant (inner) class.
-- Takes the parent type as the compareTo parameter.
-- First compares variant class names for tag ordering,
-- then casts 'other' to the same variant class and compares the 'value' field.
variantCompareToMethod :: TypedTermDefinition (JavaHelpers.Aliases -> [Java.TypeParameter] -> Name -> Name -> [FieldType] -> Java.ClassBodyDeclaration)
variantCompareToMethod = def "variantCompareToMethod" $
  lambda "aliases" $ lambda "tparams" $ lambda "parentName" $ lambda "variantName" $ lambda "fields" $ lets [
    "anns">: list [asTerm JavaUtilsSource.overrideAnnotation, asTerm JavaUtilsSource.suppressWarningsUncheckedAnnotation],
    "mods">: list [inject Java._MethodModifier Java._MethodModifier_public unit],
    "param">: JavaUtilsSource.javaTypeToJavaFormalParameter @@ (JavaUtilsSource.javaTypeFromTypeName @@ var "aliases" @@ var "parentName") @@ wrap _Name (asTerm JavaNamesSource.otherInstanceName),
    "result">: JavaUtilsSource.javaTypeToJavaResult @@ (asTerm JavaUtilsSource.javaIntType),
    "varTmpName">: string "o",
    "tagDeclStmt">: JavaUtilsSource.variableDeclarationStatement @@ var "aliases" @@ (asTerm JavaUtilsSource.javaIntType)
      @@ (JavaUtilsSource.javaIdentifier @@ string "tagCmp") @@ (asTerm tagCompareExpr),
    "tagReturnStmt">: JavaDsl.blockStatementStatement
      (JavaDsl.statementIfThen (JavaDsl.ifThenStatement (asTerm tagCmpNotZeroExpr)
        (JavaUtilsSource.javaReturnStatement @@ just
          (JavaUtilsSource.javaExpressionNameToJavaExpression @@ (JavaDsl.expressionName nothing (JavaUtilsSource.javaIdentifier @@ string "tagCmp")))))),
    "variantJavaType">: JavaUtilsSource.javaTypeFromTypeName @@ var "aliases" @@ var "variantName",
    "castOtherExpr">: JavaUtilsSource.javaCastExpressionToJavaExpression @@
      (JavaUtilsSource.javaCastExpression
        @@ (JavaUtilsSource.nameToJavaReferenceType @@ var "aliases" @@ false @@ list ([] :: [TypedTerm Java.TypeArgument]) @@ var "variantName" @@ nothing)
        @@ (JavaUtilsSource.javaIdentifierToJavaUnaryExpression @@ wrap Java._Identifier (asTerm JavaNamesSource.otherInstanceName))),
    "castDeclStmt">: JavaUtilsSource.variableDeclarationStatement @@ var "aliases" @@ var "variantJavaType"
      @@ (JavaUtilsSource.javaIdentifier @@ var "varTmpName") @@ var "castOtherExpr",
    "emptyReturn">: list [JavaDsl.blockStatementStatement (JavaUtilsSource.javaReturnStatement @@ just (JavaUtilsSource.javaIntExpression @@ bigintAsInt (bigint 0)))],
    "valueCompareStmt">: Logic.ifElse (Lists.null (var "fields"))
      (var "emptyReturn")
      (Lists.concat2 (list [var "castDeclStmt"]) (compareToBody @@ var "aliases" @@ var "varTmpName" @@ var "fields")),
    "body">: Lists.concat2 (list [var "tagDeclStmt", var "tagReturnStmt"]) (var "valueCompareStmt")] $
    JavaUtilsSource.methodDeclaration @@ var "mods" @@ list ([] :: [TypedTerm Java.TypeParameter]) @@ var "anns"
      @@ (asTerm JavaNamesSource.compareToMethodName) @@ list [var "param"] @@ var "result"
      @@ just (var "body")

-- | Generate a visit branch for a field of a union type.
visitBranch :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> JavaHelpers.Aliases -> Type -> Name -> Java.Type -> [Java.TypeArgument] -> CaseAlternative -> InferenceContext -> Graph -> Either Error Java.ClassBodyDeclarationWithComments)
visitBranch = def "visitBranch" $
  lambda "env" $ lambda "aliases" $ lambda "dom" $ lambda "tname" $ lambda "jcod" $ lambda "targs" $ lambda "field" $
    "cx" ~> "g" ~>
    -- Compute the domain type for this specific branch
    "jdom" <~ (JavaDsl.typeReference (JavaUtilsSource.nameToJavaReferenceType @@ var "aliases" @@ true @@ var "targs"
      @@ var "tname" @@ just (Formatting.capitalize @@ (Core.unName (Core.caseAlternativeName (var "field")))))) $
    "mods" <~ list [inject Java._MethodModifier Java._MethodModifier_public unit] $
    "anns" <~ list [asTerm JavaUtilsSource.overrideAnnotation] $
    "result" <~ (JavaDsl.resultType (JavaDsl.unannType (var "jcod"))) $
    -- Field terms are lambdas; apply to special var that encodes to instance.value
    cases _Term (Strip.deannotateTerm @@ Core.caseAlternativeHandler (var "field"))
      (Just $ left (Error.errorOther $ Error.otherError $ Strings.cat2 (string "visitBranch: field term is not a lambda: ") (ShowCore.term @@ Core.caseAlternativeHandler (var "field")))) [
      _Term_lambda>>: lambda "lam" $
        withLambda @@ var "env" @@ var "lam" @@ (lambda "env2" $
          "lambdaParam" <~ Core.lambdaParameter (var "lam") $
          "body" <~ Core.lambdaBody (var "lam") $
          "env3" <~ (insertBranchVar @@ var "lambdaParam" @@ var "env2") $
          "fs" <<~ (analyzeJavaFunction @@ var "env3" @@ var "body" @@ var "cx" @@ var "g") $
          "bindings" <~ (project _FunctionStructure _FunctionStructure_bindings @@ var "fs") $
          "innerBody" <~ (project _FunctionStructure _FunctionStructure_body @@ var "fs") $
          "env4" <~ (project _FunctionStructure _FunctionStructure_environment @@ var "fs") $
          "bindResult" <<~ (bindingsToStatements @@ var "env4" @@ var "bindings" @@ var "cx" @@ var "g") $
          "bindingStmts" <~ Pairs.first (var "bindResult") $
          "env5" <~ Pairs.second (var "bindResult") $
          "jret" <<~ (encodeTerm @@ var "env5" @@ var "innerBody" @@ var "cx" @@ var "g") $
          "param" <~ (JavaUtilsSource.javaTypeToJavaFormalParameter @@ var "jdom" @@ var "lambdaParam") $
          "returnStmt" <~ (JavaDsl.blockStatementStatement (JavaUtilsSource.javaReturnStatement @@ just (var "jret"))) $
          "allStmts" <~ Lists.concat2 (var "bindingStmts") (list [var "returnStmt"]) $
          right (noComment @@ (JavaUtilsSource.methodDeclaration @@ var "mods" @@ list ([] :: [TypedTerm Java.TypeParameter]) @@ var "anns"
            @@ asTerm JavaNamesSource.visitMethodName @@ list [var "param"] @@ var "result" @@ just (var "allStmts"))))]

-- | Wrap a class body declaration with a Javadoc comment.
withCommentString :: TypedTermDefinition (String -> Java.ClassBodyDeclaration -> Java.ClassBodyDeclarationWithComments)
withCommentString = def "withCommentString" $
  lambda "comment" $ lambda "decl" $
    JavaDsl.classBodyDeclarationWithComments (var "decl") (just (var "comment"))

-- | Wrap an interface member declaration with a Javadoc comment.
withInterfaceCommentString :: TypedTermDefinition (String -> Java.InterfaceMemberDeclaration -> Java.InterfaceMemberDeclarationWithComments)
withInterfaceCommentString = def "withInterfaceCommentString" $
  lambda "comment" $ lambda "decl" $
    record Java._InterfaceMemberDeclarationWithComments [
      Java._InterfaceMemberDeclarationWithComments_value>>: var "decl",
      Java._InterfaceMemberDeclarationWithComments_comments>>: just (var "comment")]

-- | Execute a computation in the context of a lambda, extending both the Graph
-- and aliasesLambdaVars with the lambda parameter.
withLambda :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> Lambda -> (JavaHelpers.JavaEnvironment -> a) -> a)
withLambda = def "withLambda" $
  lambda "env" $ lambda "lam" $ lambda "k" $
    Environment.withLambdaContext @@ javaEnvGetGraph @@ javaEnvSetGraph @@ var "env" @@ var "lam" @@
      (lambda "env1" $
        "aliases" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env1") $
        "aliases2" <~ (record JavaHelpers._Aliases [
          JavaHelpers._Aliases_currentNamespace>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_currentNamespace @@ var "aliases",
          JavaHelpers._Aliases_packages>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_packages @@ var "aliases",
          JavaHelpers._Aliases_branchVars>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_branchVars @@ var "aliases",
          JavaHelpers._Aliases_recursiveVars>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_recursiveVars @@ var "aliases",
          JavaHelpers._Aliases_inScopeTypeParams>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeTypeParams @@ var "aliases",
          JavaHelpers._Aliases_polymorphicLocals>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_polymorphicLocals @@ var "aliases",
          JavaHelpers._Aliases_inScopeJavaVars>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeJavaVars @@ var "aliases",
          JavaHelpers._Aliases_varRenames>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_varRenames @@ var "aliases",
          JavaHelpers._Aliases_lambdaVars>>:
            Sets.insert (Core.lambdaParameter (var "lam"))
              (project JavaHelpers._Aliases JavaHelpers._Aliases_lambdaVars @@ var "aliases"),
          JavaHelpers._Aliases_typeVarSubst>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_typeVarSubst @@ var "aliases",
          JavaHelpers._Aliases_trustedTypeVars>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_trustedTypeVars @@ var "aliases",
          JavaHelpers._Aliases_methodCodomain>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_methodCodomain @@ var "aliases",
          JavaHelpers._Aliases_thunkedVars>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_thunkedVars @@ var "aliases"]) $
        "env2" <~ (record JavaHelpers._JavaEnvironment [
          JavaHelpers._JavaEnvironment_aliases>>: var "aliases2",
          JavaHelpers._JavaEnvironment_graph>>:
            project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_graph @@ var "env1"]) $
        var "k" @@ var "env2")

-- | Execute a computation in the context of a type lambda
withTypeLambda :: TypedTermDefinition (JavaHelpers.JavaEnvironment -> TypeLambda -> (JavaHelpers.JavaEnvironment -> a) -> a)
withTypeLambda = def "withTypeLambda" $
  Environment.withTypeLambdaContext @@ javaEnvGetGraph @@ javaEnvSetGraph

-- | Wrap a single expression in a Supplier lambda: () -> expr
wrapInSupplierLambda :: TypedTermDefinition (Java.Expression -> Java.Expression)
wrapInSupplierLambda = def "wrapInSupplierLambda" $
  lambda "expr" $
    inject Java._Expression Java._Expression_lambda
      (JavaDsl.lambdaExpression
        (inject Java._LambdaParameters Java._LambdaParameters_tuple
          (list ([] :: [TypedTerm Java.FormalParameter])))
        (inject Java._LambdaBody Java._LambdaBody_expression (var "expr")))

-- | Look up a primitive by name and return its per-parameter laziness flags
-- (the `isLazy` flag of each signature parameter), in order. Empty if the name
-- is not a registered primitive. The single source of truth for which arguments
-- must be thunked, replacing per-coder hard-coded name tables (issue #391).
lazyFlagsForPrimitive :: TypedTermDefinition (Graph -> Name -> [Bool])
lazyFlagsForPrimitive = def "lazyFlagsForPrimitive" $
  lambda "g" $ lambda "name" $
    Optionals.cases (Maps.lookup (var "name") (Graph.graphPrimitives (var "g")))
      (list ([] :: [TypedTerm Bool]))
      (lambda "prim" $
        Lists.map (lambda "p" $ Typing.parameterIsLazy (var "p"))
          (Typing.termSignatureParameters
            (Packaging.primitiveDefinitionSignature
              (Graph.primitiveDefinition (var "prim")))))

-- | For primitives requiring lazy evaluation, wrap the lazy-flagged arguments in
-- Supplier lambdas. Java eagerly evaluates all method arguments, so e.g. ifElse
-- branches must be wrapped in () -> expr and called via IfElse.lazy(); maybe's
-- default must be wrapped to avoid constructing expensive values on the success
-- path. Which positions are lazy comes from the primitive's `isLazy` metadata
-- (issue #391), not a hard-coded name table. Only fires when the primitive is
-- fully applied (argc == parameter count) and has at least one lazy parameter.
-- The returned Maybe String is the Java method-name override: ifElse dispatches
-- to `lazy`, the others to `applyLazy`.
wrapLazyArguments :: TypedTermDefinition (Graph -> Name -> [Java.Expression] -> ([Java.Expression], Maybe String))
wrapLazyArguments = def "wrapLazyArguments" $
  lambda "g" $ lambda "name" $ lambda "args" $ lets [
    "lazyFlags">: lazyFlagsForPrimitive @@ var "g" @@ var "name",
    "anyLazy">: Lists.foldl (lambda "b" $ lambda "f" $ Logic.or (var "b") (var "f")) false (var "lazyFlags")] $
    Logic.ifElse
      (Logic.and (var "anyLazy")
        (Equality.equal (Lists.length (var "args")) (Lists.length (var "lazyFlags"))))
      (pair
        (Lists.map
          (lambda "pair" $ Logic.ifElse (Pairs.second (var "pair"))
            (wrapInSupplierLambda @@ Pairs.first (var "pair"))
            (Pairs.first (var "pair")))
          (Lists.zip (var "args") (var "lazyFlags")))
        (just (Logic.ifElse (Equality.equal (var "name") (Core.nameLift _logic_ifElse))
          (string "lazy")
          (string "applyLazy"))))
      (pair (var "args") (nothing :: TypedTerm (Maybe String)))
