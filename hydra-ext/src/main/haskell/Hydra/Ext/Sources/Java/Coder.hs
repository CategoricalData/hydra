-- | Java code generator in Hydra DSL.
-- This module provides DSL versions of Java code generation functions.

module Hydra.Ext.Sources.Java.Coder where

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
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Kernel.Terms.Arity           as Arity
import qualified Hydra.Dsl.Meta.Graph                       as Graph
import           Prelude hiding ((++))

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Coerce (coerce)

-- Additional imports
import qualified Hydra.Ext.Java.Syntax as Java
import qualified Hydra.Ext.Java.Helpers as JavaHelpers
import qualified Hydra.Ext.Dsl.Java.Syntax as JavaDsl
import qualified Hydra.Ext.Sources.Java.Syntax as JavaSyntax
import qualified Hydra.Ext.Sources.Java.Helpers as JavaHelpersSource
import qualified Hydra.Ext.Sources.Java.Language as JavaLanguageSource
import qualified Hydra.Ext.Sources.Java.Names as JavaNamesSource
import qualified Hydra.Ext.Sources.Java.Serde as JavaSerdeSource
import qualified Hydra.Ext.Sources.Java.Utils as JavaUtilsSource
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Serialization  as SerializationSource


def :: String -> TTerm a -> TBinding a
def = definitionInModule module_


-- | Get a type annotation, converting DecodingError to InContext Error.
getTypeE :: TTerm Context -> TTerm Graph -> TTerm (M.Map Name Term) -> TTerm (Either (InContext Error) (Maybe Type))
getTypeE cx g ann = Eithers.bimap
  ("__de" ~> Ctx.inContext (Error.errorOther $ Error.otherError (Error.unDecodingError @@ var "__de")) cx)
  ("__a" ~> var "__a")
  (Annotations.getType @@ g @@ ann)

-- | Reference to the hydra.encode.core.type function (Type -> Term encoder)
encodeTypeAsTerm :: TTerm (Type -> Term)
encodeTypeAsTerm = TTerm $ TermVariable $ Name "hydra.encode.core.type"

ns :: Namespace
ns = Namespace "hydra.ext.java.coder"

module_ :: Module
module_ = Module ns elements
    [JavaUtilsSource.ns, JavaNamesSource.ns, JavaSerdeSource.ns, moduleNamespace JavaLanguageSource.module_, Formatting.ns, Names.ns, Rewriting.ns, CoderUtils.ns, Lexical.ns, Schemas.ns, ShowCore.ns, Annotations.ns, Constants.ns,
      Inference.ns, Sorting.ns, Arity.ns, moduleNamespace DecodeCore.module_, moduleNamespace EncodeCore.module_, SerializationSource.ns]
    (JavaHelpersSource.ns:JavaSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Java code generator: converts Hydra modules to Java source code"
  where
    elements = [
      -- Feature constants
      toBinding java8Features,
      toBinding java11Features,
      toBinding javaFeatures,
      -- Class modifiers
      toBinding classModsPublic,
      -- Comment/annotation helpers
      toBinding noComment,
      -- Type argument helpers
      toBinding typeArgsOrDiamond,
      -- Naming helpers
      toBinding bindingNameToFilePath,
      toBinding javaIdentifierToString,
      -- Type variable helpers
      toBinding boundTypeVariables,
      toBinding extractTypeApplicationArgs,
      toBinding extractTypeApplicationArgs_go,
      toBinding javaTypeParametersForType,
      toBinding javaTypeParametersForType_bvars,
      toBinding javaTypeArgumentsForType,
      -- Predicate helpers
      toBinding isLambdaBoundVariable,
      toBinding isLocalVariable,
      -- Serializable types
      toBinding serializableTypes,
      -- Literal type encoding
      toBinding encodeLiteralType,
      toBinding encodeLiteralType_simple,
      -- Namespace helpers
      toBinding elementsClassName,
      -- Aliases field accessors
      toBinding isRecursiveVariable,
      -- Interface types
      toBinding interfaceTypes,
      -- Type predicates
      toBinding isNonComparableType,
      toBinding isBinaryType,
      toBinding isBigNumericType,
      -- Environment helpers
      toBinding innerClassRef,
      -- Type manipulation
      toBinding peelExpectedTypes,
      toBinding applySubstFull,
      toBinding collectForallParams,
      toBinding stripForalls,
      toBinding collectTypeVars,
      toBinding collectTypeVars_go,
      toBinding substituteTypeVarsWithTypes,
      toBinding substituteTypeVarsWithTypes_go,
      -- Comment helpers
      toBinding addComment,
      -- Environment helpers
      toBinding insertBranchVar,
      -- Either helpers
      toBinding getCodomain,
      toBinding getFunctionType,
      -- Lazy arguments
      toBinding wrapLazyArguments,
      toBinding wrapInSupplierLambda,
      -- Element naming
      toBinding elementJavaIdentifier,
      toBinding elementJavaIdentifier_qualify,
      -- Lambda variable helpers
      toBinding isLambdaBoundIn,
      toBinding isLambdaBoundIn_isQualified,
      toBinding findMatchingLambdaVar,
      -- Elements interface
      toBinding constructElementsInterface,
      -- Constant initializer splitting
      toBinding splitConstantInitializer,
      toBinding splitConstantInitializer_splitVar,
      -- Inference variable detection
      toBinding isUnresolvedInferenceVar,
      toBinding isUnresolvedInferenceVar_isDigit,
      -- Data classification
      toBinding classifyDataTerm,
      toBinding classifyDataTerm_countLambdaParams,
      toBinding classifyDataTerm_stripTypeLambdas,
      toBinding classifyDataReference,
      -- Type encoding
      toBinding encodeType,
      toBinding encodeType_resolveIfTypedef,
      -- Type arguments for named types
      toBinding javaTypeArgumentsForNamedType,
      -- Literal encoding
      toBinding encodeLiteral,
      toBinding encodeLiteral_litExp,
      toBinding encodeLiteral_primCast,
      toBinding encodeLiteral_encodeFloat,
      toBinding encodeLiteral_encodeInteger,
      -- Field type to formal parameter
      toBinding fieldTypeToFormalParam,
      -- Cast helper
      toBinding applyCastIfSafe,
      -- Variable encoding
      toBinding encodeVariable,
      toBinding encodeVariable_buildCurried,
      toBinding encodeVariable_hoistedLambdaCase,
      -- Nullary constant encoding
      toBinding encodeNullaryConstant,
      toBinding encodeNullaryConstant_typeArgsFromReturnType,
      -- Type variable substitution builders
      toBinding buildTypeVarSubst,
      toBinding buildTypeVarSubst_go,
      toBinding buildTypeSubst,
      toBinding buildTypeSubst_go,
      -- Environment getter/setter helpers
      toBinding javaEnvGetGraph,
      toBinding javaEnvSetGraph,
      -- Function analysis
      toBinding analyzeJavaFunction,
      -- Lambda context management
      toBinding withLambda,
      toBinding withTypeLambda,
      -- Type propagation
      toBinding propagateType,
      toBinding propagateType_propagateIntoLambda,
      toBinding propagateType_rebuildLet,
      -- Extracted helpers from bindingsToStatements
      toBinding flattenBindings,
      toBinding dedupBindings,
      toBinding freshJavaName,
      toBinding freshJavaName_go,
      toBinding needsThunking,
      toBinding bindingIsFunctionType,
      -- Extracted helpers from encodeTerm
      toBinding decodeTypeFromTerm,
      toBinding tryInferFunctionType,
      toBinding collectTypeApps,
      toBinding collectTypeApps0,
      -- Type structure helpers (extracted from correctTypeApps/filterPhantomTypeArgs/detectAccumulatorUnification)
      toBinding countFunctionParams,
      toBinding peelDomainTypes,
      toBinding unwrapReturnType,
      toBinding findPairFirst,
      -- Helpers extracted from detectAccumulatorUnification
      toBinding extractInOutPair,
      toBinding extractDirectReturn,
      toBinding extractDirectReturn_go,
      toBinding nameMapToTypeMap,
      -- detectAccumulatorUnification and its helpers
      toBinding groupPairsByFirst,
      toBinding selfRefSubstitution,
      toBinding selfRefSubstitution_processGroup,
      toBinding directRefSubstitution,
      toBinding directRefSubstitution_processGroup,
      toBinding findSelfRefVar,
      toBinding detectAccumulatorUnification,
      -- Batch 16: typesMatch, isSimpleName, filterPhantomTypeArgs
      toBinding typesMatch,
      toBinding isSimpleName,
      toBinding filterPhantomTypeArgs,
      toBinding filterPhantomTypeArgs_filterAndApply,
      -- Batch 17: filterByFlags, applySubstSimple, buildArgSubst
      toBinding filterByFlags,
      toBinding applySubstSimple,
      toBinding buildArgSubst,
      -- Batch 18: resolveTypeApps, correctTypeAppsWithArgs, correctTypeApps
      toBinding resolveTypeApps,
      toBinding correctTypeAppsWithArgs,
      toBinding correctTypeApps,
      -- Batch 19: buildSubstFromAnnotations, applyOvergenSubstToTermAnnotations
      toBinding buildSubstFromAnnotations_go,
      toBinding buildSubstFromAnnotations,
      toBinding applyOvergenSubstToTermAnnotations_go,
      toBinding applyOvergenSubstToTermAnnotations,
      -- Batch 20: compareTo helpers, recordCompareToMethod, variantCompareToMethod
      toBinding javaComparableRefType,
      toBinding comparableCompareExpr,
      toBinding arraysCompareExpr,
      toBinding hashCodeCompareExpr,
      toBinding compareFieldExpr,
      toBinding cmpNotZeroExpr,
      toBinding cmpDeclStatement,
      toBinding compareAndReturnStmts,
      toBinding compareToBody,
      toBinding tagCompareExpr,
      toBinding tagCmpNotZeroExpr,
      toBinding recordCompareToMethod,
      toBinding variantCompareToMethod,
      -- Batch 21: record type declaration helpers
      toBinding recordMemberVar,
      toBinding recordWithMethod,
      toBinding recordConstructor,
      toBinding eqClause,
      toBinding equalsClause,
      toBinding arraysEqualsClause,
      toBinding compareToZeroClause,
      toBinding recordEqualsMethod,
      toBinding hashCodeMultPair,
      toBinding first20Primes,
      toBinding recordHashCodeMethod,
      -- Batch 22: constantDecl + declarationForRecordType
      toBinding constantDecl,
      toBinding constantDeclForFieldType,
      toBinding constantDeclForTypeName,
      toBinding declarationForRecordType,
      toBinding declarationForRecordType',
      -- Batch 23: encodeTerm and supporting functions
      toBinding takeTypeArgs,
      toBinding isFieldUnitType,
      toBinding encodeTerm,
      toBinding encodeTermInternal,
      -- Batch 24: encodeApplication and supporting functions
      toBinding annotateLambdaArgs,
      toBinding applyJavaArg,
      toBinding encodeApplication,
      toBinding encodeApplication_fallback,
      -- Batch 25: functionCall
      toBinding functionCall,
      -- Batch 26: encodeFunction
      toBinding buildCurriedLambda,
      toBinding encodeFunction,
      -- Batch 27: encodeElimination and supporting functions
      toBinding extractArgType,
      toBinding annotateBodyWithCod,
      toBinding domTypeArgs,
      toBinding otherwiseBranch,
      toBinding visitBranch,
      toBinding encodeElimination,
      -- Batch 28: bindingsToStatements and supporting functions
      toBinding toDeclInit,
      toBinding toDeclStatement,
      toBinding bindingsToStatements,
      -- Batch 29: toClassDecl and Tier 5 entry points
      toBinding toClassDecl,
      toBinding declarationForUnionType,
      toBinding augmentVariantClass,
      toBinding encodeTypeDefinition,
      toBinding peelDomainsAndCod,
      toBinding isSerializableJavaType,
      toBinding correctCastType,
      toBinding typeAppFallbackCast,
      toBinding typeAppNullaryOrHoisted,
      toBinding flattenApps,
      toBinding collectLambdaDomains,
      toBinding rebuildApps,
      toBinding propagateTypesInAppChain,
      -- Tail-call optimization
      toBinding encodeTermTCO,
      toBinding encodeTermDefinition,
      toBinding encodeDefinitions,
      toBinding moduleToJava]

-- =============================================================================
-- Feature constants
-- =============================================================================

java8Features :: TBinding JavaHelpers.JavaFeatures
java8Features = def "java8Features" $
  record JavaHelpers._JavaFeatures [
    JavaHelpers._JavaFeatures_supportsDiamondOperator>>: boolean False]

java11Features :: TBinding JavaHelpers.JavaFeatures
java11Features = def "java11Features" $
  record JavaHelpers._JavaFeatures [
    JavaHelpers._JavaFeatures_supportsDiamondOperator>>: boolean True]

javaFeatures :: TBinding JavaHelpers.JavaFeatures
javaFeatures = def "javaFeatures" $
  asTerm java11Features

-- =============================================================================
-- Class modifiers
-- =============================================================================

classModsPublic :: TBinding [Java.ClassModifier]
classModsPublic = def "classModsPublic" $
  list [inject Java._ClassModifier Java._ClassModifier_public unit]

-- =============================================================================
-- Comment/annotation helpers
-- =============================================================================

noComment :: TBinding (Java.ClassBodyDeclaration -> Java.ClassBodyDeclarationWithComments)
noComment = def "noComment" $
  lambda "decl" $ JavaDsl.classBodyDeclarationWithComments (var "decl") nothing

-- =============================================================================
-- Type argument helpers
-- =============================================================================

typeArgsOrDiamond :: TBinding ([Java.TypeArgument] -> Java.TypeArgumentsOrDiamond)
typeArgsOrDiamond = def "typeArgsOrDiamond" $
  lambda "args" $
    Logic.ifElse
      (project JavaHelpers._JavaFeatures JavaHelpers._JavaFeatures_supportsDiamondOperator @@ javaFeatures)
      JavaDsl.typeArgumentsOrDiamondDiamond
      (JavaDsl.typeArgumentsOrDiamondArguments (var "args"))

-- =============================================================================
-- Naming helpers
-- =============================================================================

bindingNameToFilePath :: TBinding (Name -> String)
bindingNameToFilePath = def "bindingNameToFilePath" $
  lambda "name" $ lets [
    "qn">: Names.qualifyName @@ var "name",
    "ns_">: Module.qualifiedNameNamespace (var "qn"),
    "local">: Module.qualifiedNameLocal (var "qn"),
    "sanitized">: Formatting.sanitizeWithUnderscores @@ JavaLanguageSource.reservedWords @@ var "local",
    "unq">: Names.unqualifyName @@ Module.qualifiedName (var "ns_") (var "sanitized")] $
    CoderUtils.nameToFilePath @@ Util.caseConventionCamel @@ Util.caseConventionPascal
      @@ wrap _FileExtension (string "java") @@ var "unq"

javaIdentifierToString :: TBinding (Java.Identifier -> String)
javaIdentifierToString = def "javaIdentifierToString" $
  lambda "id" $ unwrap Java._Identifier @@ var "id"

-- =============================================================================
-- Type variable helpers
-- =============================================================================

boundTypeVariables :: TBinding (Type -> [Name])
boundTypeVariables = def "boundTypeVariables" $
  lambda "typ" $ cases _Type (var "typ")
    (Just $ list ([] :: [TTerm Name])) [
    _Type_annotated>>: lambda "at" $
      boundTypeVariables @@ (Core.annotatedTypeBody (var "at")),
    _Type_forall>>: lambda "ft" $
      Lists.cons
        (Core.forallTypeParameter (var "ft"))
        (boundTypeVariables @@ (Core.forallTypeBody (var "ft")))]

extractTypeApplicationArgs :: TBinding (Type -> [Type])
extractTypeApplicationArgs = def "extractTypeApplicationArgs" $
  lambda "typ" $ Lists.reverse (extractTypeApplicationArgs_go @@ var "typ")

extractTypeApplicationArgs_go :: TBinding (Type -> [Type])
extractTypeApplicationArgs_go = def "extractTypeApplicationArgs_go" $
  lambda "t" $ cases _Type (var "t")
    (Just $ list ([] :: [TTerm Type])) [
    _Type_application>>: lambda "at" $
      Lists.cons
        (Core.applicationTypeArgument (var "at"))
        (extractTypeApplicationArgs_go @@ (Core.applicationTypeFunction (var "at")))]

javaTypeParametersForType :: TBinding (Type -> [Java.TypeParameter])
javaTypeParametersForType = def "javaTypeParametersForType" $
  lambda "typ" $ lets [
    "toParam">: lambda "name" $
      JavaUtilsSource.javaTypeParameter @@ (Formatting.capitalize @@ (Core.unName $ var "name")),
    "boundVars">: javaTypeParametersForType_bvars @@ var "typ",
    "freeVars">: Lists.filter (lambda "v" $ isLambdaBoundVariable @@ var "v")
      (Sets.toList (Rewriting.freeVariablesInType @@ var "typ")),
    "vars">: (Lists.nub :: TTerm [Name] -> TTerm [Name]) (Lists.concat2 (var "boundVars") (var "freeVars"))] $
    Lists.map (var "toParam") (var "vars")

javaTypeParametersForType_bvars :: TBinding (Type -> [Name])
javaTypeParametersForType_bvars = def "javaTypeParametersForType_bvars" $
  lambda "t" $ cases _Type (var "t")
    (Just $ list ([] :: [TTerm Name])) [
    _Type_forall>>: lambda "ft" $
      Lists.cons
        (Core.forallTypeParameter (var "ft"))
        (javaTypeParametersForType_bvars @@ (Core.forallTypeBody (var "ft")))]

javaTypeArgumentsForType :: TBinding (Type -> [Java.TypeArgument])
javaTypeArgumentsForType = def "javaTypeArgumentsForType" $
  lambda "typ" $ Lists.reverse
    (Lists.map JavaUtilsSource.typeParameterToTypeArgument (javaTypeParametersForType @@ var "typ"))

-- =============================================================================
-- Predicate helpers
-- =============================================================================

isLambdaBoundVariable :: TBinding (Name -> Bool)
isLambdaBoundVariable = def "isLambdaBoundVariable" $
  lambda "name" $ lets [
    "v">: Core.unName $ var "name"] $
    Equality.lte (Strings.length (var "v")) (int32 4)

isLocalVariable :: TBinding (Name -> Bool)
isLocalVariable = def "isLocalVariable" $
  lambda "name" $ Maybes.isNothing
    (Module.qualifiedNameNamespace (Names.qualifyName @@ var "name"))

-- =============================================================================
-- Serializable types
-- =============================================================================

serializableTypes :: TBinding (Bool -> [Java.InterfaceType])
serializableTypes = def "serializableTypes" $
  lambda "isSer" $ lets [
    "javaSerializableType">: JavaDsl.interfaceType
      (JavaDsl.classType
        (list ([] :: [TTerm Java.Annotation]))
        JavaDsl.classTypeQualifierNone
        (JavaUtilsSource.javaTypeIdentifier @@ string "Serializable")
        (list ([] :: [TTerm Java.TypeArgument])))] $
    Logic.ifElse (var "isSer")
      (list [var "javaSerializableType"])
      (list ([] :: [TTerm Java.InterfaceType]))

-- =============================================================================
-- Literal type encoding
-- =============================================================================

-- | Helper: encode a simple Java reference type by class name (no package, no type arguments)
encodeLiteralType_simple :: TBinding (String -> Context -> Graph -> Either (InContext Error) Java.Type)
encodeLiteralType_simple = def "encodeLiteralType_simple" $
  lambda "n" $ 
    "cx" ~> "g" ~>
    right (JavaUtilsSource.javaRefType
    @@ list ([] :: [TTerm Java.ReferenceType])
    @@ nothing
    @@ var "n")

-- | Encode a Hydra literal type to a Java type
encodeLiteralType :: TBinding (LiteralType -> Context -> Graph -> Either (InContext Error) Java.Type)
encodeLiteralType = def "encodeLiteralType" $
  lambda "lt" $ 
    "cx" ~> "g" ~>
    cases _LiteralType (var "lt") Nothing [
    _LiteralType_binary>>: constant $
      right (JavaDsl.typeReference
        (JavaDsl.referenceTypeArray
          (JavaDsl.arrayType
            (JavaDsl.dims (list [list ([] :: [TTerm Java.Annotation])]))
            (JavaDsl.arrayTypeVariantPrimitive
              (JavaDsl.primitiveTypeWithAnnotations
                (JavaDsl.primitiveTypeNumeric (JavaDsl.numericTypeIntegral JavaDsl.integralTypeByte))
                (list ([] :: [TTerm Java.Annotation]))))))),
    _LiteralType_boolean>>: constant $
      encodeLiteralType_simple @@ string "Boolean" @@ var "cx" @@ var "g",
    _LiteralType_float>>: lambda "ft" $
      cases _FloatType (var "ft") Nothing [
        _FloatType_bigfloat>>: constant $
          right (JavaUtilsSource.javaRefType
            @@ list ([] :: [TTerm Java.ReferenceType])
            @@ just (JavaNamesSource.javaPackageName @@ list [string "java", string "math"])
            @@ string "BigDecimal"),
        _FloatType_float32>>: constant $
          encodeLiteralType_simple @@ string "Float" @@ var "cx" @@ var "g",
        _FloatType_float64>>: constant $
          encodeLiteralType_simple @@ string "Double" @@ var "cx" @@ var "g"],
    _LiteralType_integer>>: lambda "it" $
      cases _IntegerType (var "it") Nothing [
        _IntegerType_bigint>>: constant $
          right (JavaUtilsSource.javaRefType
            @@ list ([] :: [TTerm Java.ReferenceType])
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
            @@ list ([] :: [TTerm Java.ReferenceType])
            @@ just (JavaNamesSource.javaPackageName @@ list [string "java", string "math"])
            @@ string "BigInteger")],
    _LiteralType_string>>: constant $
      encodeLiteralType_simple @@ string "String" @@ var "cx" @@ var "g"]

-- =============================================================================
-- Namespace helpers
-- =============================================================================

-- | Convert a namespace to an elements class name (e.g., "hydra.ext.java.syntax" -> "Syntax")
elementsClassName :: TBinding (Namespace -> String)
elementsClassName = def "elementsClassName" $
  lambda "ns" $ lets [
    "nsStr">: unwrap _Namespace @@ var "ns",
    "parts">: Strings.splitOn (string ".") (var "nsStr")] $
    Formatting.sanitizeWithUnderscores @@ JavaLanguageSource.reservedWords
      @@ (Formatting.capitalize @@ (Lists.last (var "parts")))

-- =============================================================================
-- Aliases field accessors
-- =============================================================================

-- | Check if a variable is recursive (self-referencing) in the current context
isRecursiveVariable :: TBinding (JavaHelpers.Aliases -> Name -> Bool)
isRecursiveVariable = def "isRecursiveVariable" $
  lambda "aliases" $ lambda "name" $
    Sets.member (var "name")
      (project JavaHelpers._Aliases JavaHelpers._Aliases_recursiveVars @@ var "aliases")

-- =============================================================================
-- Interface types
-- =============================================================================

-- | Compute the list of interface types for a Java class declaration.
--   If serializable, includes both Serializable and Comparable<Self>.
interfaceTypes :: TBinding (Bool -> JavaHelpers.Aliases -> [Java.TypeParameter] -> Name -> [Java.InterfaceType])
interfaceTypes = def "interfaceTypes" $
  lambda "isSer" $ lambda "aliases" $ lambda "tparams" $ lambda "elName" $ lets [
    "javaSerializableType">: JavaDsl.interfaceType
      (JavaDsl.classType
        (list ([] :: [TTerm Java.Annotation]))
        JavaDsl.classTypeQualifierNone
        (JavaUtilsSource.javaTypeIdentifier @@ string "Serializable")
        (list ([] :: [TTerm Java.TypeArgument]))),
    "selfTypeArg">: (JavaDsl.typeArgumentReference
      (JavaUtilsSource.nameToJavaReferenceType @@ var "aliases" @@ boolean False
        @@ Lists.map (lambda "tp_" $ JavaUtilsSource.typeParameterToTypeArgument @@ var "tp_") (var "tparams")
        @@ var "elName" @@ nothing) :: TTerm Java.TypeArgument),
    "javaComparableType">: JavaDsl.interfaceType
      (JavaDsl.classType
        (list ([] :: [TTerm Java.Annotation]))
        JavaDsl.classTypeQualifierNone
        (JavaUtilsSource.javaTypeIdentifier @@ string "Comparable")
        (list [var "selfTypeArg"]))] $
    Logic.ifElse (var "isSer")
      (list [var "javaSerializableType", var "javaComparableType"])
      (list ([] :: [TTerm Java.InterfaceType]))

-- =============================================================================
-- Type predicates
-- =============================================================================

-- | Check whether a Hydra type maps to a Java type that does not implement Comparable
isNonComparableType :: TBinding (Type -> Bool)
isNonComparableType = def "isNonComparableType" $
  lambda "typ" $ cases _Type (Rewriting.deannotateType @@ var "typ")
    (Just $ boolean False) [
    _Type_list>>: constant $ boolean True,
    _Type_set>>: constant $ boolean True,
    _Type_map>>: constant $ boolean True,
    _Type_maybe>>: constant $ boolean True,
    _Type_either>>: constant $ boolean True,
    _Type_function>>: constant $ boolean True,
    _Type_unit>>: constant $ boolean True,
    _Type_literal>>: lambda "lt" $
      cases _LiteralType (var "lt") (Just $ boolean False) [
        _LiteralType_binary>>: constant $ boolean True],
    _Type_forall>>: lambda "ft" $
      isNonComparableType @@ (Core.forallTypeBody (var "ft"))]

-- | Check whether a Hydra type is the binary literal type (maps to byte[])
isBinaryType :: TBinding (Type -> Bool)
isBinaryType = def "isBinaryType" $
  lambda "typ" $ cases _Type (Rewriting.deannotateType @@ var "typ")
    (Just $ boolean False) [
    _Type_literal>>: lambda "lt" $
      cases _LiteralType (var "lt") (Just $ boolean False) [
        _LiteralType_binary>>: constant $ boolean True]]

-- | Check whether a Hydra type maps to BigDecimal or BigInteger in Java
isBigNumericType :: TBinding (Type -> Bool)
isBigNumericType = def "isBigNumericType" $
  lambda "typ" $ cases _Type (Rewriting.deannotateType @@ var "typ")
    (Just $ boolean False) [
    _Type_literal>>: lambda "lt" $
      cases _LiteralType (var "lt") (Just $ boolean False) [
        _LiteralType_float>>: lambda "ft" $
          cases _FloatType (var "ft") (Just $ boolean False) [
            _FloatType_bigfloat>>: constant $ boolean True],
        _LiteralType_integer>>: lambda "it" $
          cases _IntegerType (var "it") (Just $ boolean False) [
            _IntegerType_bigint>>: constant $ boolean True]]]

-- =============================================================================
-- Environment helpers
-- =============================================================================

-- | Create a qualified reference to an inner class member
innerClassRef :: TBinding (JavaHelpers.Aliases -> Name -> String -> Java.Identifier)
innerClassRef = def "innerClassRef" $
  lambda "aliases" $ lambda "name" $ lambda "local" $ lets [
    "id">: unwrap Java._Identifier @@ (JavaUtilsSource.nameToJavaName @@ var "aliases" @@ var "name")] $
    wrap Java._Identifier (Strings.cat2 (Strings.cat2 (var "id") (string ".")) (var "local"))

-- =============================================================================
-- Type manipulation
-- =============================================================================

-- | Peel expected argument types from a type scheme body using a substitution
peelExpectedTypes :: TBinding (M.Map Name Type -> Int -> Type -> [Type])
peelExpectedTypes = def "peelExpectedTypes" $
  lambda "subst" $ lambda "n" $ lambda "t" $
    Logic.ifElse (Equality.equal (var "n") (int32 0))
      (list ([] :: [TTerm Type]))
      (cases _Type (Rewriting.deannotateType @@ var "t")
        (Just $ list ([] :: [TTerm Type])) [
        _Type_function>>: lambda "ft" $
          Lists.cons
            (applySubstFull @@ var "subst" @@ Core.functionTypeDomain (var "ft"))
            (peelExpectedTypes @@ var "subst" @@ Math.sub (var "n") (int32 1) @@ Core.functionTypeCodomain (var "ft"))])

-- | Recursively apply a type substitution
applySubstFull :: TBinding (M.Map Name Type -> Type -> Type)
applySubstFull = def "applySubstFull" $
  lambda "s" $ lambda "t" $ cases _Type (Rewriting.deannotateType @@ var "t")
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
    _Type_maybe>>: lambda "inner" $
      Core.typeMaybe (applySubstFull @@ var "s" @@ var "inner"),
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

-- | Collect forall-bound type parameter names from a type
collectForallParams :: TBinding (Type -> [Name])
collectForallParams = def "collectForallParams" $
  lambda "t" $ cases _Type (Rewriting.deannotateType @@ var "t")
    (Just $ list ([] :: [TTerm Name])) [
    _Type_forall>>: lambda "fa" $
      Lists.cons (Core.forallTypeParameter (var "fa"))
        (collectForallParams @@ Core.forallTypeBody (var "fa"))]

-- | Strip all forall wrappers from a type, returning the body
stripForalls :: TBinding (Type -> Type)
stripForalls = def "stripForalls" $
  lambda "t" $ cases _Type (Rewriting.deannotateType @@ var "t")
    (Just $ var "t") [
    _Type_forall>>: lambda "fa" $
      stripForalls @@ Core.forallTypeBody (var "fa")]

-- | Collect all type variable names from a type
collectTypeVars :: TBinding (Type -> S.Set Name)
collectTypeVars = def "collectTypeVars" $
  lambda "typ" $ collectTypeVars_go @@ (Rewriting.deannotateType @@ var "typ")

-- | Helper for collectTypeVars
collectTypeVars_go :: TBinding (Type -> S.Set Name)
collectTypeVars_go = def "collectTypeVars_go" $
  lambda "t" $ cases _Type (var "t")
    (Just $ (Sets.empty :: TTerm (S.Set Name))) [
    _Type_variable>>: lambda "name" $
      Sets.singleton (var "name"),
    _Type_function>>: lambda "ft" $
      Sets.union
        (collectTypeVars_go @@ (Rewriting.deannotateType @@ Core.functionTypeDomain (var "ft")))
        (collectTypeVars_go @@ (Rewriting.deannotateType @@ Core.functionTypeCodomain (var "ft"))),
    _Type_application>>: lambda "at" $
      Sets.union
        (collectTypeVars_go @@ (Rewriting.deannotateType @@ Core.applicationTypeFunction (var "at")))
        (collectTypeVars_go @@ (Rewriting.deannotateType @@ (project _ApplicationType _ApplicationType_argument @@ var "at"))),
    _Type_list>>: lambda "inner" $
      collectTypeVars_go @@ (Rewriting.deannotateType @@ var "inner"),
    _Type_set>>: lambda "inner" $
      collectTypeVars_go @@ (Rewriting.deannotateType @@ var "inner"),
    _Type_maybe>>: lambda "inner" $
      collectTypeVars_go @@ (Rewriting.deannotateType @@ var "inner"),
    _Type_map>>: lambda "mt" $
      Sets.union
        (collectTypeVars_go @@ (Rewriting.deannotateType @@ Core.mapTypeKeys (var "mt")))
        (collectTypeVars_go @@ (Rewriting.deannotateType @@ Core.mapTypeValues (var "mt"))),
    _Type_pair>>: lambda "pt" $
      Sets.union
        (collectTypeVars_go @@ (Rewriting.deannotateType @@ Core.pairTypeFirst (var "pt")))
        (collectTypeVars_go @@ (Rewriting.deannotateType @@ Core.pairTypeSecond (var "pt"))),
    _Type_either>>: lambda "et" $
      Sets.union
        (collectTypeVars_go @@ (Rewriting.deannotateType @@ Core.eitherTypeLeft (var "et")))
        (collectTypeVars_go @@ (Rewriting.deannotateType @@ Core.eitherTypeRight (var "et"))),
    _Type_forall>>: lambda "ft" $
      collectTypeVars_go @@ (Rewriting.deannotateType @@ Core.forallTypeBody (var "ft"))]

-- | Substitute type variables with types
substituteTypeVarsWithTypes :: TBinding (M.Map Name Type -> Type -> Type)
substituteTypeVarsWithTypes = def "substituteTypeVarsWithTypes" $
  lambda "subst" $ lambda "t" $ substituteTypeVarsWithTypes_go @@ var "subst" @@ (Rewriting.deannotateType @@ var "t")

-- | Helper for substituteTypeVarsWithTypes
substituteTypeVarsWithTypes_go :: TBinding (M.Map Name Type -> Type -> Type)
substituteTypeVarsWithTypes_go = def "substituteTypeVarsWithTypes_go" $
  lambda "subst" $ lambda "t" $ cases _Type (Rewriting.deannotateType @@ var "t")
    (Just $ var "t") [
    _Type_variable>>: lambda "v" $
      Maybes.cases (Maps.lookup (var "v") (var "subst")) (var "t") (lambda "rep" $ var "rep"),
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
    _Type_maybe>>: lambda "inner" $
      Core.typeMaybe (substituteTypeVarsWithTypes_go @@ var "subst" @@ var "inner"),
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

-- =============================================================================
-- Comment helpers
-- =============================================================================

-- | Add a comment from a FieldType to a class body declaration
addComment :: TBinding (Java.ClassBodyDeclaration -> FieldType -> Context -> Graph -> Either (InContext Error) Java.ClassBodyDeclarationWithComments)
addComment = def "addComment" $
  lambda "decl" $ lambda "field" $
        "cx" ~> "g" ~>
        Eithers.map
          (lambda "c" $ JavaDsl.classBodyDeclarationWithComments (var "decl") (var "c"))
          (CoderUtils.commentsFromFieldType @@ var "cx" @@ var "g" @@ var "field")

-- =============================================================================
-- Environment helpers (continued)
-- =============================================================================

-- | Insert a branch variable into the environment
insertBranchVar :: TBinding (Name -> JavaHelpers.JavaEnvironment -> JavaHelpers.JavaEnvironment)
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
          JavaHelpers._Aliases_methodCodomain>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_methodCodomain @@ var "aliases",
          JavaHelpers._Aliases_thunkedVars>>:
            project JavaHelpers._Aliases JavaHelpers._Aliases_thunkedVars @@ var "aliases"],
      JavaHelpers._JavaEnvironment_graph>>:
        project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_graph @@ var "env"]

-- =============================================================================
-- Either helpers
-- =============================================================================

-- | Get the codomain type from annotations
getCodomain :: TBinding (M.Map Name Term -> Context -> Graph -> Either (InContext Error) Type)
getCodomain = def "getCodomain" $
  lambda "ann" $
    "cx" ~> "g" ~>
    Eithers.map
      (lambda "ft" $ Core.functionTypeCodomain (var "ft"))
      (getFunctionType @@ var "ann" @@ var "cx" @@ var "g")

-- | Get the function type from annotations
getFunctionType :: TBinding (M.Map Name Term -> Context -> Graph -> Either (InContext Error) FunctionType)
getFunctionType = def "getFunctionType" $
  lambda "ann" $
    "cx" ~> "g" ~>
    "mt" <<~ (getTypeE (var "cx") (var "g") (var "ann")) $
    Maybes.cases (var "mt")
      (Ctx.failInContext (Error.errorOther $ Error.otherError $ string "type annotation is required for function and elimination terms in Java") (var "cx"))
      (lambda "t" $ cases _Type (var "t")
        (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError $ Strings.cat2 (string "expected function type, got: ") (ShowCore.type_ @@ var "t")) (var "cx")) [
        _Type_function>>: lambda "ft" $ right (var "ft")])

-- =============================================================================
-- Lazy arguments
-- =============================================================================

-- | Wrap a single expression in a Supplier lambda: () -> expr
wrapInSupplierLambda :: TBinding (Java.Expression -> Java.Expression)
wrapInSupplierLambda = def "wrapInSupplierLambda" $
  lambda "expr" $
    inject Java._Expression Java._Expression_lambda
      (JavaDsl.lambdaExpression
        (inject Java._LambdaParameters Java._LambdaParameters_tuple
          (list ([] :: [TTerm Java.FormalParameter])))
        (inject Java._LambdaBody Java._LambdaBody_expression (var "expr")))

-- | For primitives requiring lazy evaluation, wrap branch arguments in Supplier lambdas.
-- Java eagerly evaluates all method arguments, so ifElse branches must be wrapped
-- in () -> expr and called via IfElse.lazy(). Similarly, maybe's nothing case must be
-- wrapped to avoid constructing expensive error messages on the success path.
wrapLazyArguments :: TBinding (Name -> [Java.Expression] -> ([Java.Expression], Maybe String))
wrapLazyArguments = def "wrapLazyArguments" $
  lambda "name" $ lambda "args" $
    Logic.ifElse
      (Logic.and
        (Equality.equal (var "name") (Core.nameLift _logic_ifElse))
        (Equality.equal (Lists.length (var "args")) (int32 3)))
      (pair
        (list [
          Lists.at (int32 0) (var "args"),
          wrapInSupplierLambda @@ (Lists.at (int32 1) (var "args")),
          wrapInSupplierLambda @@ (Lists.at (int32 2) (var "args"))])
        (just (string "lazy")))
      (Logic.ifElse
        (Logic.and
          (Equality.equal (var "name") (Core.nameLift _maybes_maybe))
          (Equality.equal (Lists.length (var "args")) (int32 3)))
        (pair
          (list [
            wrapInSupplierLambda @@ (Lists.at (int32 0) (var "args")),
            Lists.at (int32 1) (var "args"),
            Lists.at (int32 2) (var "args")])
          (just (string "applyLazy")))
      (Logic.ifElse
        (Logic.and
          (Equality.equal (var "name") (Core.nameLift _maybes_cases))
          (Equality.equal (Lists.length (var "args")) (int32 3)))
        (pair
          (list [
            Lists.at (int32 0) (var "args"),
            wrapInSupplierLambda @@ (Lists.at (int32 1) (var "args")),
            Lists.at (int32 2) (var "args")])
          (just (string "applyLazy")))
      (Logic.ifElse
        (Logic.and
          (Equality.equal (var "name") (Core.nameLift _maps_findWithDefault))
          (Equality.equal (Lists.length (var "args")) (int32 3)))
        (pair
          (list [
            wrapInSupplierLambda @@ (Lists.at (int32 0) (var "args")),
            Lists.at (int32 1) (var "args"),
            Lists.at (int32 2) (var "args")])
          (just (string "applyLazy")))
      (Logic.ifElse
        (Logic.and
          (Logic.or
            (Equality.equal (var "name") (Core.nameLift _maybes_fromMaybe))
            (Logic.or
              (Equality.equal (var "name") (Core.nameLift _eithers_fromLeft))
              (Equality.equal (var "name") (Core.nameLift _eithers_fromRight))))
          (Equality.equal (Lists.length (var "args")) (int32 2)))
        (pair
          (list [
            wrapInSupplierLambda @@ (Lists.at (int32 0) (var "args")),
            Lists.at (int32 1) (var "args")])
          (just (string "applyLazy")))
        (pair (var "args") (nothing :: TTerm (Maybe String)))))))

-- =============================================================================
-- Element naming
-- =============================================================================

-- | Generate a Java identifier for a data element (variable, constant, function, etc.)
elementJavaIdentifier :: TBinding (Bool -> Bool -> JavaHelpers.Aliases -> Name -> Java.Identifier)
elementJavaIdentifier = def "elementJavaIdentifier" $
  lambda "isPrim" $ lambda "isMethod" $ lambda "aliases" $ lambda "name" $ lets [
    "qn">: Names.qualifyName @@ var "name",
    "ns_">: Module.qualifiedNameNamespace (var "qn"),
    "local">: Module.qualifiedNameLocal (var "qn"),
    "sep">: Logic.ifElse (var "isMethod") (string "::") (string ".")] $
    Logic.ifElse (var "isPrim")
      (wrap Java._Identifier (Strings.cat2
        (Strings.cat2
          (elementJavaIdentifier_qualify @@ var "aliases" @@ var "ns_"
            @@ (Formatting.capitalize @@ var "local"))
          (string "."))
        (asTerm JavaNamesSource.applyMethodName)))
      (Maybes.cases (var "ns_")
        (wrap Java._Identifier (JavaUtilsSource.sanitizeJavaName @@ var "local"))
        (lambda "n" $ wrap Java._Identifier (Strings.cat2
          (Strings.cat2
            (elementJavaIdentifier_qualify @@ var "aliases" @@ just (var "n")
              @@ (elementsClassName @@ var "n"))
            (var "sep"))
          (JavaUtilsSource.sanitizeJavaName @@ var "local"))))

-- | Helper for elementJavaIdentifier: qualify a name through the aliases
elementJavaIdentifier_qualify :: TBinding (JavaHelpers.Aliases -> Maybe Namespace -> String -> String)
elementJavaIdentifier_qualify = def "elementJavaIdentifier_qualify" $
  lambda "aliases" $ lambda "mns" $ lambda "s" $
    unwrap Java._Identifier @@ (JavaUtilsSource.nameToJavaName @@ var "aliases"
      @@ (Names.unqualifyName @@ Module.qualifiedName (var "mns") (var "s")))

-- =============================================================================
-- Lambda variable helpers
-- =============================================================================

-- | Helper: check if a name is qualified (has a namespace)
isLambdaBoundIn_isQualified :: TBinding (Name -> Bool)
isLambdaBoundIn_isQualified = def "isLambdaBoundIn_isQualified" $
  lambda "n" $ Maybes.isJust (Module.qualifiedNameNamespace (Names.qualifyName @@ var "n"))

-- | Check if a name (possibly qualified) is lambda-bound
isLambdaBoundIn :: TBinding (Name -> S.Set Name -> Bool)
isLambdaBoundIn = def "isLambdaBoundIn" $
  lambda "name" $ lambda "lambdaVars" $
    Logic.or (Sets.member (var "name") (var "lambdaVars"))
      (Logic.or
        -- For qualified names, check if any qualified lambda var has the same local name
        (Logic.and (isLambdaBoundIn_isQualified @@ var "name")
          (Maybes.isJust (Lists.find
            (lambda "lv" $ Logic.and
              (isLambdaBoundIn_isQualified @@ var "lv")
              (Equality.equal
                (Names.localNameOf @@ var "lv")
                (Names.localNameOf @@ var "name")))
            (Sets.toList (var "lambdaVars")))))
        -- For unqualified names, check if the local name is in lambdaVars
        (Logic.and (Logic.not (isLambdaBoundIn_isQualified @@ var "name"))
          (Sets.member (wrap _Name (Names.localNameOf @@ var "name")) (var "lambdaVars"))))

-- | Find the actual lambda variable name that matches a given reference
findMatchingLambdaVar :: TBinding (Name -> S.Set Name -> Name)
findMatchingLambdaVar = def "findMatchingLambdaVar" $
  lambda "name" $ lambda "lambdaVars" $
    Logic.ifElse (Sets.member (var "name") (var "lambdaVars"))
      (var "name")
      (Logic.ifElse (isLambdaBoundIn_isQualified @@ var "name")
        (Maybes.fromMaybe (var "name")
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

-- =============================================================================
-- Elements interface
-- =============================================================================

-- | Construct an elements interface for a module's data definitions
constructElementsInterface :: TBinding (Module -> [Java.InterfaceMemberDeclaration] -> (Name, Java.CompilationUnit))
constructElementsInterface = def "constructElementsInterface" $
  lambda "mod" $ lambda "members" $ lets [
    "pkg">: JavaUtilsSource.javaPackageDeclaration @@ Module.moduleNamespace (var "mod"),
    "mods">: list [inject Java._InterfaceModifier Java._InterfaceModifier_public unit],
    "className">: elementsClassName @@ Module.moduleNamespace (var "mod"),
    "elName">: Names.unqualifyName @@ Module.qualifiedName
      (just (Module.moduleNamespace (var "mod"))) (var "className"),
    "body">: wrap Java._InterfaceBody (var "members"),
    "itf">: inject Java._TypeDeclaration Java._TypeDeclaration_interface
      (inject Java._InterfaceDeclaration Java._InterfaceDeclaration_normalInterface
        (record Java._NormalInterfaceDeclaration [
          Java._NormalInterfaceDeclaration_modifiers>>: var "mods",
          Java._NormalInterfaceDeclaration_identifier>>:
            JavaUtilsSource.javaTypeIdentifier @@ var "className",
          Java._NormalInterfaceDeclaration_parameters>>:
            list ([] :: [TTerm Java.TypeParameter]),
          Java._NormalInterfaceDeclaration_extends>>:
            list ([] :: [TTerm Java.InterfaceType]),
          Java._NormalInterfaceDeclaration_body>>: var "body"])),
    "decl">: record Java._TypeDeclarationWithComments [
      Java._TypeDeclarationWithComments_value>>: var "itf",
      Java._TypeDeclarationWithComments_comments>>: Module.moduleDescription (var "mod")]] $
    pair (var "elName")
      (inject Java._CompilationUnit Java._CompilationUnit_ordinary
        (record Java._OrdinaryCompilationUnit [
          Java._OrdinaryCompilationUnit_package>>: just (var "pkg"),
          Java._OrdinaryCompilationUnit_imports>>:
            list ([] :: [TTerm Java.ImportDeclaration]),
          Java._OrdinaryCompilationUnit_types>>: list [var "decl"]]))

-- =============================================================================
-- Constant initializer splitting
-- =============================================================================

-- | Split a constant declaration into a field + helper method to avoid large <clinit>
splitConstantInitializer :: TBinding (Java.InterfaceMemberDeclaration -> [Java.InterfaceMemberDeclaration])
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
splitConstantInitializer_splitVar :: TBinding ([Java.ConstantModifier] -> Java.UnannType -> Java.VariableDeclarator -> [Java.InterfaceMemberDeclaration])
splitConstantInitializer_splitVar = def "splitConstantInitializer_splitVar" $
  lambda "mods" $ lambda "utype" $ lambda "vd" $ lets [
    "vid">: project Java._VariableDeclarator Java._VariableDeclarator_id @@ var "vd",
    "mInit">: project Java._VariableDeclarator Java._VariableDeclarator_initializer @@ var "vd"] $
    Maybes.cases (var "mInit")
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
                @@ list ([] :: [TTerm Java.Expression])),
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
              @@ list ([] :: [TTerm Java.TypeParameter])
              @@ var "helperName"
              @@ list ([] :: [TTerm Java.FormalParameter])
              @@ var "resultType"
              @@ just (list [var "returnSt"])] $
            list [var "field", var "helper"]])

-- =============================================================================
-- Inference variable detection
-- =============================================================================

-- | Check if a name looks like an unresolved type inference variable.
-- These are generated by the type inference engine and have the form 't' followed by digits.
isUnresolvedInferenceVar :: TBinding (Name -> Bool)
isUnresolvedInferenceVar = def "isUnresolvedInferenceVar" $
  lambda "name" $
    "chars" <~ Strings.toList (unwrap _Name @@ var "name") $
    Logic.ifElse (Lists.null (var "chars"))
      (boolean False)
      (Logic.ifElse
        (Logic.not $ Equality.equal (Lists.head (var "chars")) (int32 116))  -- 't'
        (boolean False)
        ("rest" <~ Lists.tail (var "chars") $
          Logic.and
            (Logic.not $ Lists.null (var "rest"))
            (Lists.null $ Lists.filter
              (lambda "c" $ Logic.not (isUnresolvedInferenceVar_isDigit @@ var "c"))
              (var "rest"))))

isUnresolvedInferenceVar_isDigit :: TBinding (Int -> Bool)
isUnresolvedInferenceVar_isDigit = def "isUnresolvedInferenceVar_isDigit" $
  lambda "c" $
    Logic.and (Equality.gte (var "c") (int32 48)) (Equality.lte (var "c") (int32 57))

-- =============================================================================
-- Data classification
-- =============================================================================

-- | Classify a data term by its symbol class (constant, nullary function, hoisted lambda, etc.)
classifyDataTerm :: TBinding (TypeScheme -> Term -> JavaHelpers.JavaSymbolClass)
classifyDataTerm = def "classifyDataTerm" $
  lambda "ts" $ lambda "term" $
    Logic.ifElse (Rewriting.isLambda @@ var "term")
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
classifyDataTerm_countLambdaParams :: TBinding (Term -> Int)
classifyDataTerm_countLambdaParams = def "classifyDataTerm_countLambdaParams" $
  lambda "t" $
    cases _Term (Rewriting.deannotateTerm @@ var "t")
      (Just $ int32 0) [
      _Term_function>>: lambda "f" $
        cases _Function (var "f")
          (Just $ int32 0) [
          _Function_lambda>>: lambda "lam" $
            Math.add (int32 1)
              (classifyDataTerm_countLambdaParams @@ (project _Lambda _Lambda_body @@ var "lam"))],
      _Term_let>>: lambda "lt" $
        classifyDataTerm_countLambdaParams @@ (project _Let _Let_body @@ var "lt")]

-- | Strip type lambda wrappers from a term
classifyDataTerm_stripTypeLambdas :: TBinding (Term -> Term)
classifyDataTerm_stripTypeLambdas = def "classifyDataTerm_stripTypeLambdas" $
  lambda "t" $
    cases _Term (Rewriting.deannotateTerm @@ var "t")
      (Just $ var "t") [
      _Term_typeLambda>>: lambda "tl" $
        classifyDataTerm_stripTypeLambdas @@ (project _TypeLambda _TypeLambda_body @@ var "tl")]

-- | Classify a data reference by looking up its element and classifying its term
classifyDataReference :: TBinding (Name -> Context -> Graph -> Either (InContext Error) JavaHelpers.JavaSymbolClass)
classifyDataReference = def "classifyDataReference" $
  lambda "name" $
    "cx" ~> "g" ~>
    "mel" <<~ right (Lexical.dereferenceElement @@ var "g" @@ var "name") $
    Maybes.cases (var "mel")
      -- Not found: treat as local variable
      (right $ inject JavaHelpers._JavaSymbolClass JavaHelpers._JavaSymbolClass_localVariable unit)
      (lambda "el" $
        Maybes.cases (Core.bindingType (var "el"))
          (Ctx.failInContext (Error.errorOther $ Error.otherError $ Strings.cat2 (string "no type scheme for element ") ((unwrap _Name @@ Core.bindingName (var "el")))) (var "cx"))
          (lambda "ts" $
            right $ classifyDataTerm @@ var "ts" @@ Core.bindingTerm (var "el")))

-- =============================================================================
-- Type encoding
-- =============================================================================

-- | Encode a Hydra type as a Java type
encodeType :: TBinding (JavaHelpers.Aliases -> S.Set Name -> Type -> Context -> Graph -> Either (InContext Error) Java.Type)
encodeType = def "encodeType" $
  lambda "aliases" $ lambda "boundVars" $ lambda "t" $
    "cx" ~> "g" ~>
    "inScopeTypeParams" <~ project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeTypeParams @@ var "aliases" $
    "typeVarSubst" <~ project JavaHelpers._Aliases JavaHelpers._Aliases_typeVarSubst @@ var "aliases" $
    cases _Type (Rewriting.deannotateType @@ var "t")
      (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError $ Strings.cat2 (string "can't encode unsupported type in Java: ") (ShowCore.type_ @@ var "t")) (var "cx")) [
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
          @@ asTerm JavaNamesSource.hydraUtilPackageName
          @@ string "ConsList"),
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
          @@ asTerm JavaNamesSource.hydraUtilPackageName
          @@ string "PersistentMap"),
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
          @@ list ([] :: [TTerm Java.ReferenceType])
          @@ asTerm JavaNamesSource.javaLangPackageName
          @@ string "Void"),
      _Type_record>>: lambda "rt" $
        Logic.ifElse
          (Logic.and
            (Equality.equal (Core.rowTypeTypeName (var "rt")) (wrap _Name (string "hydra.core.Unit")))
            (Lists.null (Core.rowTypeFields (var "rt"))))
          (right (JavaUtilsSource.javaRefType
            @@ list ([] :: [TTerm Java.ReferenceType])
            @@ asTerm JavaNamesSource.javaLangPackageName
            @@ string "Void"))
          (right (inject Java._Type Java._Type_reference
            (JavaUtilsSource.nameToJavaReferenceType @@ var "aliases" @@ boolean True
              @@ (javaTypeArgumentsForType @@ var "t")
              @@ Core.rowTypeTypeName (var "rt")
              @@ nothing))),
      _Type_maybe>>: lambda "ot" $
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
          @@ asTerm JavaNamesSource.hydraUtilPackageName
          @@ string "PersistentSet"),
      _Type_union>>: lambda "rt" $
        right (inject Java._Type Java._Type_reference
          (JavaUtilsSource.nameToJavaReferenceType @@ var "aliases" @@ boolean True
            @@ (javaTypeArgumentsForType @@ var "t")
            @@ Core.rowTypeTypeName (var "rt")
            @@ nothing)),
      _Type_variable>>: lambda "name0" $
        -- Apply type variable substitution
        "name" <~ Maybes.fromMaybe (var "name0") (Maps.lookup (var "name0") (var "typeVarSubst")) $
        -- Check if it's a typedef that should be resolved
        "resolved" <<~ (encodeType_resolveIfTypedef @@ var "aliases" @@ var "boundVars" @@ var "inScopeTypeParams" @@ var "name" @@ var "cx" @@ var "g") $
        Maybes.cases (var "resolved")
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
                          @@ list ([] :: [TTerm Java.ReferenceType])
                          @@ asTerm JavaNamesSource.javaLangPackageName
                          @@ string "Object"))))
                  -- Named reference
                  (inject Java._Type Java._Type_reference
                    (JavaUtilsSource.nameToJavaReferenceType @@ var "aliases" @@ boolean True
                      @@ list ([] :: [TTerm Java.TypeArgument])
                      @@ var "name"
                      @@ nothing)))))
          -- Typedef resolved: encode the resolved type
          (lambda "resolvedType" $
            encodeType @@ var "aliases" @@ var "boundVars" @@ var "resolvedType" @@ var "cx" @@ var "g"),
      _Type_wrap>>: lambda "wt" $
        right (inject Java._Type Java._Type_reference
          (JavaUtilsSource.nameToJavaReferenceType @@ var "aliases" @@ boolean True
            @@ list ([] :: [TTerm Java.TypeArgument])
            @@ (Core.wrappedTypeTypeName (var "wt"))
            @@ nothing))]

-- | Resolve a TypeVariable name if it refers to a typedef (simple type alias)
encodeType_resolveIfTypedef :: TBinding (JavaHelpers.Aliases -> S.Set Name -> S.Set Name -> Name -> Context -> Graph -> Either (InContext Error) (Maybe Type))
encodeType_resolveIfTypedef = def "encodeType_resolveIfTypedef" $
  lambda "aliases" $ lambda "boundVars" $ lambda "inScopeTypeParams" $ lambda "name" $
    "cx" ~> "g" ~>
    Logic.ifElse (Logic.or (Sets.member (var "name") (var "boundVars")) (Sets.member (var "name") (var "inScopeTypeParams")))
      (right nothing)
      (Logic.ifElse (isLambdaBoundVariable @@ var "name")
        (right nothing)
        ("schemaTypes" <~ Graph.graphSchemaTypes (var "g") $
          Maybes.cases (Maps.lookup (var "name") (var "schemaTypes"))
            (right nothing)
            (lambda "ts" $
              Logic.ifElse (Logic.not (Lists.null (Core.typeSchemeVariables (var "ts"))))
                (right nothing)
                (cases _Type (Rewriting.deannotateType @@ Core.typeSchemeType (var "ts"))
                  (Just $ right (just (Core.typeSchemeType (var "ts")))) [
                  _Type_record>>: lambda "_" $ right nothing,
                  _Type_union>>: lambda "_" $ right nothing,
                  _Type_wrap>>: lambda "_" $ right nothing]))))

-- =============================================================================
-- Type arguments for named types
-- =============================================================================

-- | Get type arguments for a named type by looking up its definition
javaTypeArgumentsForNamedType :: TBinding (Name -> Context -> Graph -> Either (InContext Error) [Java.TypeArgument])
javaTypeArgumentsForNamedType = def "javaTypeArgumentsForNamedType" $
  lambda "tname" $
    "cx" ~> "g" ~>
    "typ" <<~ (Schemas.requireType @@ var "cx" @@ var "g" @@ var "tname") $
    right $ Lists.map (lambda "tp_" $ JavaUtilsSource.typeParameterToTypeArgument @@ var "tp_")
      (javaTypeParametersForType @@ var "typ")

-- =============================================================================
-- Literal encoding
-- =============================================================================

-- | Helper: convert a Java literal to a Java expression
encodeLiteral_litExp :: TBinding (Java.Literal -> Java.Expression)
encodeLiteral_litExp = def "encodeLiteral_litExp" $
  lambda "l" $ JavaUtilsSource.javaLiteralToJavaExpression @@ var "l"

-- | Helper: cast an expression to a primitive type
encodeLiteral_primCast :: TBinding (Java.PrimitiveType -> Java.Expression -> Java.Expression)
encodeLiteral_primCast = def "encodeLiteral_primCast" $
  lambda "pt" $ lambda "expr" $
    JavaUtilsSource.javaCastExpressionToJavaExpression @@
      (JavaUtilsSource.javaCastPrimitive @@ var "pt" @@
        (JavaUtilsSource.javaExpressionToJavaUnaryExpression @@ var "expr"))

-- | Encode a float value to a Java expression
encodeLiteral_encodeFloat :: TBinding (FloatValue -> Java.Expression)
encodeLiteral_encodeFloat = def "encodeLiteral_encodeFloat" $
  lambda "f" $
    cases _FloatValue (var "f") Nothing [
      _FloatValue_bigfloat>>: "v" ~>
        JavaUtilsSource.javaConstructorCall @@
          (JavaUtilsSource.javaConstructorName @@
            (JavaDsl.identifier $ string "java.math.BigDecimal") @@ nothing) @@
          list [encodeLiteral @@ inject _Literal _Literal_string (Literals.showBigfloat $ var "v")] @@
          nothing,
      _FloatValue_float32>>: "v" ~>
        encodeLiteral_primCast @@
          (JavaDsl.primitiveTypeNumeric $ JavaDsl.numericTypeFloatingPoint JavaDsl.floatingPointTypeFloat) @@
          (encodeLiteral_litExp @@
            (JavaDsl.literalFloatingPoint $ JavaDsl.floatingPointLiteral $
              Literals.float32ToBigfloat (var "v"))),
      _FloatValue_float64>>: "v" ~>
        encodeLiteral_litExp @@
          (JavaDsl.literalFloatingPoint $ JavaDsl.floatingPointLiteral $
            Literals.float64ToBigfloat (var "v"))]

-- | Encode an integer value to a Java expression
encodeLiteral_encodeInteger :: TBinding (IntegerValue -> Java.Expression)
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

-- | Encode a literal value to a Java expression
encodeLiteral :: TBinding (Literal -> Java.Expression)
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
      _Literal_float>>: "f" ~> encodeLiteral_encodeFloat @@ var "f",
      _Literal_integer>>: "i" ~> encodeLiteral_encodeInteger @@ var "i",
      _Literal_string>>: "s" ~>
        encodeLiteral_litExp @@ (JavaUtilsSource.javaString @@ var "s")]

-- =============================================================================
-- Field type to formal parameter
-- =============================================================================

-- | Convert a field type to a Java formal parameter
fieldTypeToFormalParam :: TBinding (JavaHelpers.Aliases -> FieldType -> Context -> Graph -> Either (InContext Error) Java.FormalParameter)
fieldTypeToFormalParam = def "fieldTypeToFormalParam" $
  lambda "aliases" $ lambda "ft" $
    "cx" ~> "g" ~>
    "jt" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ (Core.fieldTypeType (var "ft")) @@ var "cx" @@ var "g") $
    right (JavaUtilsSource.javaTypeToJavaFormalParameter @@ var "jt" @@ Core.fieldTypeName (var "ft"))

-- =============================================================================
-- Cast helper
-- =============================================================================

-- | Apply a lambda cast if the type is safe (doesn't contain potentially wrong type variables).
applyCastIfSafe :: TBinding (JavaHelpers.Aliases -> Type -> Java.Expression -> Context -> Graph -> Either (InContext Error) Java.Expression)
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

-- =============================================================================
-- Variable encoding
-- =============================================================================

-- | Build a curried lambda from a list of parameter names wrapping a call expression
encodeVariable_buildCurried :: TBinding ([Name] -> Java.Expression -> Java.Expression)
encodeVariable_buildCurried = def "encodeVariable_buildCurried" $
  lambda "params" $ lambda "inner" $
    Logic.ifElse (Lists.null (var "params"))
      (var "inner")
      (JavaUtilsSource.javaLambda @@ Lists.head (var "params") @@
        (encodeVariable_buildCurried @@
          Lists.tail (var "params") @@ var "inner"))

-- | Handle the HoistedLambda case of encodeVariable
encodeVariable_hoistedLambdaCase :: TBinding (JavaHelpers.Aliases -> Name -> Int -> Context -> Graph -> Either (InContext Error) Java.Expression)
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
    "mel" <<~ right (Lexical.dereferenceElement @@ var "g" @@ var "name") $
    Maybes.cases (var "mel")
      (right (var "lam"))
      (lambda "el" $
        Maybes.cases (Core.bindingType (var "el"))
          (right (var "lam"))
          (lambda "ts" $
            "typ" <~ Core.typeSchemeType (var "ts") $
            "jtype" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "typ" @@ var "cx" @@ var "g") $
            "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jtype" @@ var "cx") $
            right (JavaUtilsSource.javaCastExpressionToJavaExpression @@
              (JavaUtilsSource.javaCastExpression @@ var "rt" @@
                (JavaUtilsSource.javaExpressionToJavaUnaryExpression @@ var "lam")))))

-- | Encode a variable reference as a Java expression
encodeVariable :: TBinding (JavaHelpers.JavaEnvironment -> Name -> Context -> Graph -> Either (InContext Error) Java.Expression)
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
              @@ (JavaDsl.identifier (asTerm JavaNamesSource.getMethodName)) @@ (list ([] :: [TTerm Java.Expression])))))
          -- Thunked variables use .get()
          (Logic.ifElse (Logic.and
              (Sets.member (var "name") (project JavaHelpers._Aliases JavaHelpers._Aliases_thunkedVars @@ var "aliases"))
              (Logic.not (isLambdaBoundIn @@ var "name" @@ (project JavaHelpers._Aliases JavaHelpers._Aliases_lambdaVars @@ var "aliases"))))
            (right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocation @@
                (just (Phantoms.left (JavaDsl.expressionName nothing (var "jid"))))
                @@ (JavaDsl.identifier (asTerm JavaNamesSource.getMethodName)) @@ (list ([] :: [TTerm Java.Expression])))))
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
                       (list ([] :: [TTerm Java.Expression])))),
                 JavaHelpers._JavaSymbolClass_unaryFunction>>: lambda "_" $
                   right (JavaUtilsSource.javaIdentifierToJavaExpression @@ (elementJavaIdentifier @@ boolean False @@ boolean True @@ var "aliases" @@ var "name"))]))))))

-- =============================================================================
-- Nullary constant encoding
-- =============================================================================

-- | Extract type arguments from the return type for generic method calls
encodeNullaryConstant_typeArgsFromReturnType :: TBinding (JavaHelpers.Aliases -> Type -> Context -> Graph -> Either (InContext Error) [Java.TypeArgument])
encodeNullaryConstant_typeArgsFromReturnType = def "encodeNullaryConstant_typeArgsFromReturnType" $
  lambda "aliases" $ lambda "t" $
    "cx" ~> "g" ~>
    cases _Type (Rewriting.deannotateType @@ var "t")
      (Just $ right (list ([] :: [TTerm Java.TypeArgument]))) [
      _Type_set>>: "st" ~>
        "jst" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "st" @@ var "cx" @@ var "g") $
        "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jst" @@ var "cx") $
        right (list [JavaDsl.typeArgumentReference (var "rt")]),
      _Type_list>>: "lt_" ~>
        "jlt" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "lt_" @@ var "cx" @@ var "g") $
        "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jlt" @@ var "cx") $
        right (list [JavaDsl.typeArgumentReference (var "rt")]),
      _Type_maybe>>: "mt" ~>
        "jmt" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "mt" @@ var "cx" @@ var "g") $
        "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jmt" @@ var "cx") $
        right (list [JavaDsl.typeArgumentReference (var "rt")]),
      _Type_map>>: "mp" ~>
        "jkt" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ (project _MapType _MapType_keys @@ var "mp") @@ var "cx" @@ var "g") $
        "rk" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jkt" @@ var "cx") $
        "jvt" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ (project _MapType _MapType_values @@ var "mp") @@ var "cx" @@ var "g") $
        "rv" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jvt" @@ var "cx") $
        right (list [JavaDsl.typeArgumentReference (var "rk"), JavaDsl.typeArgumentReference (var "rv")])]

-- | Encode a nullary constant function as a Java expression
encodeNullaryConstant :: TBinding (JavaHelpers.JavaEnvironment -> Type -> Function -> Context -> Graph -> Either (InContext Error) Java.Expression)
encodeNullaryConstant = def "encodeNullaryConstant" $
  lambda "env" $ lambda "typ" $ lambda "fun" $
    "cx" ~> "g" ~>
    "aliases" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env") $
    cases _Function (var "fun")
      (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError $ Strings.cat2 (string "unexpected ") (Strings.cat2 (string "nullary function") (Strings.cat2 (string " in ") (ShowCore.function @@ var "fun")))) (var "cx")) [
      _Function_primitive>>: "name" ~>
        "targs" <<~ (encodeNullaryConstant_typeArgsFromReturnType @@ var "aliases" @@ var "typ" @@ var "cx" @@ var "g") $
        Logic.ifElse (Lists.null (var "targs"))
          -- Simple call without type arguments
          ("header" <~ (inject Java._MethodInvocation_Header Java._MethodInvocation_Header_simple
            (wrap Java._MethodName
              (elementJavaIdentifier @@ boolean True @@ boolean False @@ var "aliases" @@ var "name"))) $
           right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
             (record Java._MethodInvocation [
               Java._MethodInvocation_header>>: var "header",
               Java._MethodInvocation_arguments>>: list ([] :: [TTerm Java.Expression])])))
          -- Call with type arguments: split "ClassName.apply" into class and method
          ("fullName" <~ (unwrap Java._Identifier @@ (elementJavaIdentifier @@ boolean True @@ boolean False @@ var "aliases" @@ var "name")) $
           "parts" <~ Strings.splitOn (string ".") (var "fullName") $
           "className" <~ JavaDsl.identifier (Strings.intercalate (string ".") (Lists.init (var "parts"))) $
           "methodName" <~ JavaDsl.identifier (Lists.last (var "parts")) $
           right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
             (JavaUtilsSource.methodInvocationStaticWithTypeArgs @@ var "className" @@ var "methodName" @@ var "targs" @@ (list ([] :: [TTerm Java.Expression])))))]

-- =============================================================================
-- Type variable substitution builders
-- =============================================================================

-- | Build a type variable substitution by structurally matching a "fresh" type (from inference
-- annotations) against a "canonical" type (from the type scheme). When both types have a
-- TypeVariable at the same structural position, maps the fresh name to the canonical name.
-- Only includes mappings where the names actually differ.
buildTypeVarSubst :: TBinding (S.Set Name -> Type -> Type -> M.Map Name Name)
buildTypeVarSubst = def "buildTypeVarSubst" $
  lambda "schemeVarSet" $ lambda "freshTyp" $ lambda "canonTyp" $
    buildTypeVarSubst_go @@ var "schemeVarSet"
      @@ (Rewriting.deannotateType @@ var "freshTyp")
      @@ (Rewriting.deannotateType @@ var "canonTyp")

-- | Recursive helper for buildTypeVarSubst. Takes deannotated types.
buildTypeVarSubst_go :: TBinding (S.Set Name -> Type -> Type -> M.Map Name Name)
buildTypeVarSubst_go = def "buildTypeVarSubst_go" $
  lambda "svs" $ lambda "ft" $ lambda "ct" $
    "goSub" <~ (lambda "a" $ lambda "b" $
      buildTypeVarSubst_go @@ var "svs"
        @@ (Rewriting.deannotateType @@ var "a")
        @@ (Rewriting.deannotateType @@ var "b")) $
    cases _Type (var "ft")
      (Just $ -- Default: check if ct is a forall, and if so unwrap it
        cases _Type (var "ct")
          (Just (Maps.empty :: TTerm (M.Map Name Name))) [
          _Type_forall>>: lambda "cfa" $
            buildTypeVarSubst_go @@ var "svs" @@ var "ft"
              @@ (Rewriting.deannotateType @@ Core.forallTypeBody (var "cfa"))]) [
      _Type_variable>>: lambda "fn" $
        cases _Type (var "ct")
          (Just (Maps.empty :: TTerm (M.Map Name Name))) [
          _Type_variable>>: lambda "cn" $
            Logic.ifElse
              (Logic.and (Logic.not (Equality.equal (var "fn") (var "cn"))) (Sets.member (var "cn") (var "svs")))
              (Maps.singleton (var "fn") (var "cn"))
              (Maps.empty :: TTerm (M.Map Name Name))],
      _Type_function>>: lambda "fft" $
        cases _Type (var "ct")
          (Just (Maps.empty :: TTerm (M.Map Name Name))) [
          _Type_function>>: lambda "cft" $
            Maps.union
              (var "goSub" @@ Core.functionTypeDomain (var "fft") @@ Core.functionTypeDomain (var "cft"))
              (var "goSub" @@ Core.functionTypeCodomain (var "fft") @@ Core.functionTypeCodomain (var "cft"))],
      _Type_application>>: lambda "fat" $
        cases _Type (var "ct")
          (Just (Maps.empty :: TTerm (M.Map Name Name))) [
          _Type_application>>: lambda "cat" $
            Maps.union
              (var "goSub" @@ Core.applicationTypeFunction (var "fat") @@ Core.applicationTypeFunction (var "cat"))
              (var "goSub" @@ (project _ApplicationType _ApplicationType_argument @@ var "fat")
                           @@ (project _ApplicationType _ApplicationType_argument @@ var "cat"))],
      _Type_list>>: lambda "fl" $
        cases _Type (var "ct")
          (Just (Maps.empty :: TTerm (M.Map Name Name))) [
          _Type_list>>: lambda "cl" $
            var "goSub" @@ var "fl" @@ var "cl"],
      _Type_set>>: lambda "fs" $
        cases _Type (var "ct")
          (Just (Maps.empty :: TTerm (M.Map Name Name))) [
          _Type_set>>: lambda "cs" $
            var "goSub" @@ var "fs" @@ var "cs"],
      _Type_maybe>>: lambda "fm" $
        cases _Type (var "ct")
          (Just (Maps.empty :: TTerm (M.Map Name Name))) [
          _Type_maybe>>: lambda "cm" $
            var "goSub" @@ var "fm" @@ var "cm"],
      _Type_map>>: lambda "fmt" $
        cases _Type (var "ct")
          (Just (Maps.empty :: TTerm (M.Map Name Name))) [
          _Type_map>>: lambda "cmt" $
            Maps.union
              (var "goSub" @@ Core.mapTypeKeys (var "fmt") @@ Core.mapTypeKeys (var "cmt"))
              (var "goSub" @@ Core.mapTypeValues (var "fmt") @@ Core.mapTypeValues (var "cmt"))],
      _Type_pair>>: lambda "fpt" $
        cases _Type (var "ct")
          (Just (Maps.empty :: TTerm (M.Map Name Name))) [
          _Type_pair>>: lambda "cpt" $
            Maps.union
              (var "goSub" @@ Core.pairTypeFirst (var "fpt") @@ Core.pairTypeFirst (var "cpt"))
              (var "goSub" @@ Core.pairTypeSecond (var "fpt") @@ Core.pairTypeSecond (var "cpt"))],
      _Type_either>>: lambda "fet" $
        cases _Type (var "ct")
          (Just (Maps.empty :: TTerm (M.Map Name Name))) [
          _Type_either>>: lambda "cet" $
            Maps.union
              (var "goSub" @@ Core.eitherTypeLeft (var "fet") @@ Core.eitherTypeLeft (var "cet"))
              (var "goSub" @@ Core.eitherTypeRight (var "fet") @@ Core.eitherTypeRight (var "cet"))],
      _Type_forall>>: lambda "ffa" $
        cases _Type (var "ct")
          (Just $ -- ct is not a forall, but ft is: unwrap ft and recurse
            buildTypeVarSubst_go @@ var "svs"
              @@ (Rewriting.deannotateType @@ Core.forallTypeBody (var "ffa"))
              @@ var "ct") [
          _Type_forall>>: lambda "cfa" $
            var "goSub" @@ Core.forallTypeBody (var "ffa") @@ Core.forallTypeBody (var "cfa")]]

-- | Build a mapping from scheme type variables to actual types by structurally matching
-- a scheme type against an actual type. Only maps variables that are in the schemeVarSet.
buildTypeSubst :: TBinding (S.Set Name -> Type -> Type -> M.Map Name Type)
buildTypeSubst = def "buildTypeSubst" $
  lambda "schemeVarSet" $ lambda "schemeType" $ lambda "actualType" $
    buildTypeSubst_go @@ var "schemeVarSet"
      @@ (Rewriting.deannotateType @@ var "schemeType")
      @@ (Rewriting.deannotateType @@ var "actualType")

-- | Recursive helper for buildTypeSubst. Takes deannotated types.
buildTypeSubst_go :: TBinding (S.Set Name -> Type -> Type -> M.Map Name Type)
buildTypeSubst_go = def "buildTypeSubst_go" $
  lambda "svs" $ lambda "st" $ lambda "at" $
    "goSub" <~ (lambda "a" $ lambda "b" $
      buildTypeSubst_go @@ var "svs"
        @@ (Rewriting.deannotateType @@ var "a")
        @@ (Rewriting.deannotateType @@ var "b")) $
    cases _Type (var "st")
      (Just (Maps.empty :: TTerm (M.Map Name Type))) [
      _Type_variable>>: lambda "v" $
        Logic.ifElse (Sets.member (var "v") (var "svs"))
          (Maps.singleton (var "v") (var "at"))
          (Maps.empty :: TTerm (M.Map Name Type)),
      _Type_function>>: lambda "sft" $
        cases _Type (var "at")
          (Just (Maps.empty :: TTerm (M.Map Name Type))) [
          _Type_function>>: lambda "aft" $
            Maps.union
              (var "goSub" @@ Core.functionTypeDomain (var "sft") @@ Core.functionTypeDomain (var "aft"))
              (var "goSub" @@ Core.functionTypeCodomain (var "sft") @@ Core.functionTypeCodomain (var "aft"))],
      _Type_application>>: lambda "sat" $
        cases _Type (var "at")
          (Just (Maps.empty :: TTerm (M.Map Name Type))) [
          _Type_application>>: lambda "aat" $
            Maps.union
              (var "goSub" @@ Core.applicationTypeFunction (var "sat") @@ Core.applicationTypeFunction (var "aat"))
              (var "goSub" @@ (project _ApplicationType _ApplicationType_argument @@ var "sat")
                           @@ (project _ApplicationType _ApplicationType_argument @@ var "aat"))],
      _Type_list>>: lambda "sl" $
        cases _Type (var "at")
          (Just (Maps.empty :: TTerm (M.Map Name Type))) [
          _Type_list>>: lambda "al" $
            var "goSub" @@ var "sl" @@ var "al"],
      _Type_set>>: lambda "ss" $
        cases _Type (var "at")
          (Just (Maps.empty :: TTerm (M.Map Name Type))) [
          _Type_set>>: lambda "as'" $
            var "goSub" @@ var "ss" @@ var "as'"],
      _Type_maybe>>: lambda "sm" $
        cases _Type (var "at")
          (Just (Maps.empty :: TTerm (M.Map Name Type))) [
          _Type_maybe>>: lambda "am" $
            var "goSub" @@ var "sm" @@ var "am"],
      _Type_map>>: lambda "smt" $
        cases _Type (var "at")
          (Just (Maps.empty :: TTerm (M.Map Name Type))) [
          _Type_map>>: lambda "amt" $
            Maps.union
              (var "goSub" @@ Core.mapTypeKeys (var "smt") @@ Core.mapTypeKeys (var "amt"))
              (var "goSub" @@ Core.mapTypeValues (var "smt") @@ Core.mapTypeValues (var "amt"))],
      _Type_pair>>: lambda "spt" $
        cases _Type (var "at")
          (Just (Maps.empty :: TTerm (M.Map Name Type))) [
          _Type_pair>>: lambda "apt" $
            Maps.union
              (var "goSub" @@ Core.pairTypeFirst (var "spt") @@ Core.pairTypeFirst (var "apt"))
              (var "goSub" @@ Core.pairTypeSecond (var "spt") @@ Core.pairTypeSecond (var "apt"))],
      _Type_either>>: lambda "set'" $
        cases _Type (var "at")
          (Just (Maps.empty :: TTerm (M.Map Name Type))) [
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

-- =============================================================================
-- Environment getter/setter helpers
-- =============================================================================

-- | Get Graph from JavaEnvironment
javaEnvGetGraph :: TBinding (JavaHelpers.JavaEnvironment -> Graph)
javaEnvGetGraph = def "javaEnvGetGraph" $
  lambda "env" $ project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_graph @@ var "env"

-- | Set Graph in JavaEnvironment (preserving other fields)
javaEnvSetGraph :: TBinding (Graph -> JavaHelpers.JavaEnvironment -> JavaHelpers.JavaEnvironment)
javaEnvSetGraph = def "javaEnvSetGraph" $
  lambda "g" $ lambda "env" $
    record JavaHelpers._JavaEnvironment [
      JavaHelpers._JavaEnvironment_aliases>>:
        project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env",
      JavaHelpers._JavaEnvironment_graph>>: var "g"]

-- =============================================================================
-- Function analysis
-- =============================================================================

-- | Analyze a Java function term, collecting lambdas, type lambdas, lets, and type applications
analyzeJavaFunction :: TBinding (JavaHelpers.JavaEnvironment -> Term -> Context -> Graph -> Either (InContext Error) (FunctionStructure JavaHelpers.JavaEnvironment))
analyzeJavaFunction = def "analyzeJavaFunction" $
  lambda "env" $ lambda "term" $
    "cx" ~> "g" ~>
    CoderUtils.analyzeFunctionTerm @@ var "cx" @@ javaEnvGetGraph @@ javaEnvSetGraph @@ var "env" @@ var "term"

-- =============================================================================
-- Lambda context management
-- =============================================================================

-- | Execute a computation in the context of a lambda, extending both the Graph
-- and aliasesLambdaVars with the lambda parameter.
withLambda :: TBinding (JavaHelpers.JavaEnvironment -> Lambda -> (JavaHelpers.JavaEnvironment -> a) -> a)
withLambda = def "withLambda" $
  lambda "env" $ lambda "lam" $ lambda "k" $
    Schemas.withLambdaContext @@ javaEnvGetGraph @@ javaEnvSetGraph @@ var "env" @@ var "lam" @@
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
withTypeLambda :: TBinding (JavaHelpers.JavaEnvironment -> TypeLambda -> (JavaHelpers.JavaEnvironment -> a) -> a)
withTypeLambda = def "withTypeLambda" $
  Schemas.withTypeLambdaContext @@ javaEnvGetGraph @@ javaEnvSetGraph

-- =============================================================================
-- Type propagation
-- =============================================================================

-- | Propagate a correct type annotation through a term. Sets the type annotation on the term
-- and, for lambdas, also recursively annotates the body with the codomain type.
propagateType :: TBinding (Type -> Term -> Term)
propagateType = def "propagateType" $
  lambda "typ" $ lambda "term" $
    "setTypeAnn" <~ (lambda "t" $
      Annotations.setTermAnnotation @@ Constants.key_type
        @@ just (Phantoms.encoderFor _Type @@ var "typ")
        @@ var "t") $
    cases _Term (Rewriting.deannotateTerm @@ var "term")
      (Just $ var "setTypeAnn" @@ var "term") [
      _Term_function>>: lambda "f" $
        cases _Function (var "f")
          (Just $ var "setTypeAnn" @@ var "term") [
          _Function_lambda>>: lambda "lam" $
            "annotated" <~ (var "setTypeAnn" @@ var "term") $
            cases _Type (Rewriting.deannotateType @@ var "typ")
              (Just $ var "annotated") [
              _Type_function>>: lambda "ft" $
                propagateType_propagateIntoLambda
                  @@ (Core.functionTypeCodomain (var "ft"))
                  @@ var "annotated"]],
      _Term_let>>: lambda "lt" $
        var "setTypeAnn" @@
          (propagateType_rebuildLet @@ var "term"
            @@ (Core.letBindings (var "lt"))
            @@ (propagateType @@ var "typ" @@ Core.letBody (var "lt")))]

-- | Propagate the codomain type into a lambda's body, traversing through annotations
propagateType_propagateIntoLambda :: TBinding (Type -> Term -> Term)
propagateType_propagateIntoLambda = def "propagateType_propagateIntoLambda" $
  lambda "cod" $ lambda "t" $
    cases _Term (var "t")
      (Just $ var "t") [
      _Term_annotated>>: lambda "at" $
        Core.termAnnotated (Core.annotatedTerm
          (propagateType_propagateIntoLambda @@ var "cod" @@ Core.annotatedTermBody (var "at"))
          (Core.annotatedTermAnnotation (var "at"))),
      _Term_function>>: lambda "f" $
        cases _Function (var "f")
          (Just $ var "t") [
          _Function_lambda>>: lambda "lam" $
            Core.termFunction (Core.functionLambda (Core.lambda
              (Core.lambdaParameter (var "lam"))
              (Core.lambdaDomain (var "lam"))
              (propagateType @@ var "cod" @@ Core.lambdaBody (var "lam"))))]]

-- | Rebuild a let expression with a new body, preserving annotations
propagateType_rebuildLet :: TBinding (Term -> [Binding] -> Term -> Term)
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

-- =============================================================================
-- Extracted helpers from bindingsToStatements
-- =============================================================================

-- | Flatten nested TermLet into a flat list of Bindings.
flattenBindings :: TBinding ([Binding] -> [Binding])
flattenBindings = def "flattenBindings" $
  lambda "bindings" $
    Lists.bind (var "bindings") (lambda "b" $
      cases _Term (Rewriting.deannotateTerm @@ Core.bindingTerm (var "b"))
        (Just $ list [var "b"]) [
        _Term_let>>: lambda "lt" $
          Lists.concat2
            (flattenBindings @@ Core.letBindings (var "lt"))
            (list [Core.binding (Core.bindingName (var "b")) (Core.letBody (var "lt")) (Core.bindingType (var "b"))])])

-- | Deduplicate binding names that collide with in-scope variables.
dedupBindings :: TBinding (S.Set Name -> [Binding] -> [Binding])
dedupBindings = def "dedupBindings" $
  lambda "inScope" $ lambda "bs" $
    Logic.ifElse
      (Lists.null (var "bs"))
      (list ([] :: [TTerm Binding]))
      ("b" <~ Lists.head (var "bs") $
       "rest" <~ Lists.tail (var "bs") $
       "name" <~ Core.bindingName (var "b") $
       Logic.ifElse
         (Sets.member (var "name") (var "inScope"))
         ("newName" <~ (freshJavaName @@ var "name" @@ var "inScope") $
          "subst" <~ Maps.singleton (var "name") (var "newName") $
          "rest2" <~ Lists.map
            (lambda "b2" $ Core.binding
              (Core.bindingName (var "b2"))
              (Rewriting.substituteVariables @@ var "subst" @@ Core.bindingTerm (var "b2"))
              (Core.bindingType (var "b2")))
            (var "rest") $
          Lists.cons
            (Core.binding (var "newName") (Core.bindingTerm (var "b")) (Core.bindingType (var "b")))
            (dedupBindings @@ Sets.insert (var "newName") (var "inScope") @@ var "rest2"))
         (Lists.cons (var "b")
           (dedupBindings @@ Sets.insert (var "name") (var "inScope") @@ var "rest")))

-- | Generate a unique name by appending increasing integers, avoiding a given set.
freshJavaName :: TBinding (Name -> S.Set Name -> Name)
freshJavaName = def "freshJavaName" $
  lambda "base" $ lambda "avoid" $
    freshJavaName_go @@ var "base" @@ var "avoid" @@ int32 2

freshJavaName_go :: TBinding (Name -> S.Set Name -> Int -> Name)
freshJavaName_go = def "freshJavaName_go" $
  lambda "base" $ lambda "avoid" $ lambda "i" $
    "candidate" <~ Core.name (Strings.cat2 (Core.unName (var "base")) (Literals.showInt32 (var "i"))) $
    Logic.ifElse
      (Sets.member (var "candidate") (var "avoid"))
      (freshJavaName_go @@ var "base" @@ var "avoid" @@ Math.add (var "i") (int32 1))
      (var "candidate")

-- | Check if a term structurally needs lazy evaluation.
needsThunking :: TBinding (Term -> Bool)
needsThunking = def "needsThunking" $
  lambda "t" $
    cases _Term (Rewriting.deannotateTerm @@ var "t")
      (Just $ Lists.foldl
        (lambda "b" $ lambda "st" $ Logic.or (var "b") (needsThunking @@ var "st"))
        (boolean False)
        (Rewriting.subterms @@ var "t")) [
      _Term_let>>: lambda "_lt" $ boolean True,
      _Term_typeApplication>>: lambda "_ta" $ boolean True,
      _Term_typeLambda>>: lambda "_tl" $ boolean True]

-- | Check if a Binding has function type.
bindingIsFunctionType :: TBinding (Binding -> Bool)
bindingIsFunctionType = def "bindingIsFunctionType" $
  lambda "b" $
    Maybes.maybe
      -- No type scheme: check term structure
      (cases _Term (Rewriting.deannotateTerm @@ Core.bindingTerm (var "b"))
        (Just $ boolean False) [
        _Term_function>>: lambda "_f" $ boolean True])
      -- Has type scheme: check type
      (lambda "ts" $
        cases _Type (Rewriting.deannotateType @@ Core.typeSchemeType (var "ts"))
          (Just $ boolean False) [
          _Type_function>>: lambda "_ft" $ boolean True,
          _Type_forall>>: lambda "fa" $
            cases _Type (Rewriting.deannotateType @@ Core.forallTypeBody (var "fa"))
              (Just $ boolean False) [
              _Type_function>>: lambda "_ft2" $ boolean True]])
      (Core.bindingType (var "b"))

-- =============================================================================
-- Extracted helpers from encodeTerm
-- =============================================================================

-- | Decode a Type from its term encoding (limited subset).
decodeTypeFromTerm :: TBinding (Term -> Maybe Type)
decodeTypeFromTerm = def "decodeTypeFromTerm" $
  lambda "term" $
    cases _Term (Rewriting.deannotateTerm @@ var "term")
      (Just nothing) [
      _Term_union>>: lambda "inj" $
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
                   Maybes.bind
                     (Lists.safeHead (Lists.filter
                       (lambda "f" $ Equality.equal (Core.fieldName (var "f")) (Core.name (string "body")))
                       (Core.recordFields (var "rec"))))
                     (lambda "bodyField" $
                       decodeTypeFromTerm @@ Core.fieldTerm (var "bodyField"))])
               (Logic.ifElse
                 (Equality.equal (var "fname") (string "application"))
                 (cases _Term (var "fterm")
                   (Just nothing) [
                   _Term_record>>: lambda "rec" $
                     Maybes.bind
                       (Lists.safeHead (Lists.filter
                         (lambda "f" $ Equality.equal (Core.fieldName (var "f")) (Core.name (string "function")))
                         (Core.recordFields (var "rec"))))
                       (lambda "funcField" $
                         Maybes.bind (decodeTypeFromTerm @@ Core.fieldTerm (var "funcField")) (lambda "func" $
                           Maybes.bind
                             (Lists.safeHead (Lists.filter
                               (lambda "f" $ Equality.equal (Core.fieldName (var "f")) (Core.name (string "argument")))
                               (Core.recordFields (var "rec"))))
                             (lambda "argField" $
                               Maybes.map
                                 (lambda "arg" $ Core.typeApplication (Core.applicationType (var "func") (var "arg")))
                                 (decodeTypeFromTerm @@ Core.fieldTerm (var "argField")))))])
                 (Logic.ifElse
                   (Equality.equal (var "fname") (string "function"))
                   (cases _Term (var "fterm")
                     (Just nothing) [
                     _Term_record>>: lambda "rec" $
                       Maybes.bind
                         (Lists.safeHead (Lists.filter
                           (lambda "f" $ Equality.equal (Core.fieldName (var "f")) (Core.name (string "domain")))
                           (Core.recordFields (var "rec"))))
                         (lambda "domField" $
                           Maybes.bind (decodeTypeFromTerm @@ Core.fieldTerm (var "domField")) (lambda "dom" $
                             Maybes.bind
                               (Lists.safeHead (Lists.filter
                                 (lambda "f" $ Equality.equal (Core.fieldName (var "f")) (Core.name (string "codomain")))
                                 (Core.recordFields (var "rec"))))
                               (lambda "codField" $
                                 Maybes.map
                                   (lambda "cod" $ Core.typeFunction (Core.functionType (var "dom") (var "cod")))
                                   (decodeTypeFromTerm @@ Core.fieldTerm (var "codField")))))])
                   (Logic.ifElse
                     (Equality.equal (var "fname") (string "literal"))
                     (cases _Term (var "fterm")
                       (Just nothing) [
                       _Term_union>>: lambda "litInj" $
                         Logic.ifElse
                           (Equality.equal (Core.unName (Core.fieldName (Core.injectionField (var "litInj")))) (string "string"))
                           (just (Core.typeLiteral (inject _LiteralType _LiteralType_string unit)))
                           nothing])
                     nothing)))))
          nothing]

-- | Try to infer the function type from lambda structure when type annotations are unavailable.
tryInferFunctionType :: TBinding (Function -> Maybe Type)
tryInferFunctionType = def "tryInferFunctionType" $
  lambda "fun" $
    cases _Function (var "fun")
      (Just nothing) [
      _Function_lambda>>: lambda "lam" $
        Maybes.bind (Core.lambdaDomain (var "lam")) (lambda "dom" $
          "mCod" <~ (cases _Term (Core.lambdaBody (var "lam"))
            (Just nothing) [
            _Term_annotated>>: lambda "at" $
              Maybes.bind
                (Maps.lookup (Constants.key_type) (Core.annotatedTermAnnotation (var "at")))
                (lambda "typeTerm" $
                  decodeTypeFromTerm @@ var "typeTerm"),
            _Term_function>>: lambda "innerFun" $
              tryInferFunctionType @@ var "innerFun"]) $
          Maybes.map (lambda "cod" $
            Core.typeFunction (Core.functionType (var "dom") (var "cod")))
            (var "mCod"))]

-- | Collect type arguments from nested TermTypeApplication chain, stripping annotations.
collectTypeApps :: TBinding (Term -> [Type] -> (Term, [Type]))
collectTypeApps = def "collectTypeApps" $
  lambda "t" $ lambda "acc" $
    cases _Term (Rewriting.deannotateTerm @@ var "t")
      (Just $ pair (Rewriting.deannotateTerm @@ var "t") (var "acc")) [
      _Term_typeApplication>>: lambda "ta" $
        collectTypeApps
          @@ Core.typeApplicationTermBody (var "ta")
          @@ Lists.cons (Core.typeApplicationTermType (var "ta")) (var "acc")]

-- | Like collectTypeApps but preserves the original (annotated) term when no more type apps.
collectTypeApps0 :: TBinding (Term -> [Type] -> (Term, [Type]))
collectTypeApps0 = def "collectTypeApps0" $
  lambda "t" $ lambda "acc" $
    cases _Term (Rewriting.deannotateTerm @@ var "t")
      (Just $ pair (var "t") (var "acc")) [
      _Term_typeApplication>>: lambda "ta" $
        collectTypeApps0
          @@ Core.typeApplicationTermBody (var "ta")
          @@ Lists.cons (Core.typeApplicationTermType (var "ta")) (var "acc")]

-- =============================================================================
-- Type structure helpers (extracted from correctTypeApps/filterPhantomTypeArgs/detectAccumulatorUnification)
-- =============================================================================

-- | Count the number of parameters in a function type by peeling domain types.
countFunctionParams :: TBinding (Type -> Int)
countFunctionParams = def "countFunctionParams" $
  lambda "t" $
    cases _Type (Rewriting.deannotateType @@ var "t")
      (Just $ int32 0) [
      _Type_function>>: lambda "ft" $
        Math.add (int32 1) (countFunctionParams @@ Core.functionTypeCodomain (var "ft"))]

-- | Peel N domain types from a function type, returning the domains and the final codomain.
peelDomainTypes :: TBinding (Int -> Type -> ([Type], Type))
peelDomainTypes = def "peelDomainTypes" $
  lambda "n" $ lambda "t" $
    Logic.ifElse
      (Equality.lte (var "n") (int32 0))
      (pair (list ([] :: [TTerm Type])) (var "t"))
      (cases _Type (Rewriting.deannotateType @@ var "t")
        (Just $ pair (list ([] :: [TTerm Type])) (var "t")) [
        _Type_function>>: lambda "ft" $
          "rest" <~ (peelDomainTypes @@ Math.sub (var "n") (int32 1) @@ Core.functionTypeCodomain (var "ft")) $
          pair
            (Lists.cons (Core.functionTypeDomain (var "ft")) (Pairs.first (var "rest")))
            (Pairs.second (var "rest"))])

-- | Unwrap nested function types to get the final return type.
-- Also looks through type application wrappers (TypeApplication).
unwrapReturnType :: TBinding (Type -> Type)
unwrapReturnType = def "unwrapReturnType" $
  lambda "t" $
    cases _Type (Rewriting.deannotateType @@ var "t")
      (Just $ var "t") [
      _Type_function>>: lambda "ft" $
        unwrapReturnType @@ Core.functionTypeCodomain (var "ft"),
      _Type_application>>: lambda "at" $
        unwrapReturnType @@ Core.applicationTypeArgument (var "at")]

-- | Extract the type variable from the first element of a pair type.
findPairFirst :: TBinding (Type -> Maybe Name)
findPairFirst = def "findPairFirst" $
  lambda "t" $
    cases _Type (Rewriting.deannotateType @@ var "t")
      (Just nothing) [
      _Type_pair>>: lambda "pt" $
        cases _Type (Rewriting.deannotateType @@ Core.pairTypeFirst (var "pt"))
          (Just nothing) [
          _Type_variable>>: lambda "v" $ just (var "v")]]

-- =============================================================================
-- Helpers extracted from detectAccumulatorUnification
-- =============================================================================

-- | Extract input/output type variable pairs from function-typed parameters.
-- For a param like (A -> ... -> (B, ...)), extract (A, B) where B is
-- the first element of a pair return type.
extractInOutPair :: TBinding (Type -> [(Name, Name)])
extractInOutPair = def "extractInOutPair" $
  lambda "t" $
    cases _Type (Rewriting.deannotateType @@ var "t")
      (Just $ list ([] :: [TTerm (Name, Name)])) [
      _Type_function>>: lambda "ft" $
        cases _Type (Rewriting.deannotateType @@ Core.functionTypeDomain (var "ft"))
          (Just $ list ([] :: [TTerm (Name, Name)])) [
          _Type_variable>>: lambda "inVar" $
            "retType" <~ (unwrapReturnType @@ Core.functionTypeCodomain (var "ft")) $
            cases _Type (Rewriting.deannotateType @@ var "retType")
              (Just $ list ([] :: [TTerm (Name, Name)])) [
              _Type_pair>>: lambda "pt" $
                cases _Type (Rewriting.deannotateType @@ Core.pairTypeFirst (var "pt"))
                  (Just $ list ([] :: [TTerm (Name, Name)])) [
                  _Type_variable>>: lambda "outVar" $
                    list [pair (var "inVar") (var "outVar")]]]]]

-- | Extract input/output pairs for direct variable returns in
-- "context extension" functions: ... -> V -> X -> V (or W)
extractDirectReturn :: TBinding (S.Set Name -> Type -> [(Name, Name)])
extractDirectReturn = def "extractDirectReturn" $
  lambda "tparamSet" $ lambda "t" $
    extractDirectReturn_go @@ var "tparamSet" @@ var "t"

extractDirectReturn_go :: TBinding (S.Set Name -> Type -> [(Name, Name)])
extractDirectReturn_go = def "extractDirectReturn_go" $
  lambda "tparamSet" $ lambda "t" $
    cases _Type (Rewriting.deannotateType @@ var "t")
      (Just $ list ([] :: [TTerm (Name, Name)])) [
      _Type_function>>: lambda "ft" $
        "dom" <~ (Rewriting.deannotateType @@ Core.functionTypeDomain (var "ft")) $
        "cod" <~ Core.functionTypeCodomain (var "ft") $
        cases _Type (var "dom")
          (Just $ extractDirectReturn_go @@ var "tparamSet" @@ var "cod") [
          _Type_variable>>: lambda "inVar" $
            Logic.ifElse
              (Sets.member (var "inVar") (var "tparamSet"))
              (cases _Type (Rewriting.deannotateType @@ var "cod")
                (Just $ list ([] :: [TTerm (Name, Name)])) [
                _Type_function>>: lambda "ft2" $
                  "midArg" <~ (Rewriting.deannotateType @@ Core.functionTypeDomain (var "ft2")) $
                  "retPart" <~ (Rewriting.deannotateType @@ Core.functionTypeCodomain (var "ft2")) $
                  cases _Type (var "midArg")
                    (Just $
                      cases _Type (var "retPart")
                        (Just $ list ([] :: [TTerm (Name, Name)])) [
                        _Type_variable>>: lambda "outVar" $
                          Logic.ifElse
                            (Sets.member (var "outVar") (var "tparamSet"))
                            (list [pair (var "inVar") (var "outVar")])
                            (list ([] :: [TTerm (Name, Name)]))]
                    ) [
                    _Type_variable>>: lambda "midVar" $
                      Logic.ifElse
                        (Sets.member (var "midVar") (var "tparamSet"))
                        (list ([] :: [TTerm (Name, Name)]))
                        (cases _Type (var "retPart")
                          (Just $ list ([] :: [TTerm (Name, Name)])) [
                          _Type_variable>>: lambda "outVar" $
                            Logic.ifElse
                              (Sets.member (var "outVar") (var "tparamSet"))
                              (list [pair (var "inVar") (var "outVar")])
                              (list ([] :: [TTerm (Name, Name)]))])]])
              (extractDirectReturn_go @@ var "tparamSet" @@ var "cod")]]

-- | Convert Name->Name map to Name->Type map (wrapping values as TypeVariable)
nameMapToTypeMap :: TBinding (M.Map Name Name -> M.Map Name Type)
nameMapToTypeMap = def "nameMapToTypeMap" $
  lambda "m" $
    Maps.map (lambda "v" $ Core.typeVariable (var "v")) (var "m")

-- =============================================================================
-- detectAccumulatorUnification and helpers
-- =============================================================================

-- | Group pairs by their first element, collecting second elements into lists.
groupPairsByFirst :: TBinding ([(Name, Name)] -> M.Map Name [Name])
groupPairsByFirst = def "groupPairsByFirst" $
  lambda "pairs" $
    Lists.foldl
      (lambda "m" $ lambda "p" $
        "k" <~ Pairs.first (var "p") $
        "v" <~ Pairs.second (var "p") $
        Maps.alter
          (lambda "mv" $
            Maybes.maybe
              (just (list [var "v"]))
              (lambda "vs" $ just (Lists.concat2 (var "vs") (list [var "v"])))
              (var "mv"))
          (var "k")
          (var "m"))
      (Maps.empty)
      (var "pairs")

-- | For each group where the input var appears in its own output list,
-- substitute all other output vars to that input var.
selfRefSubstitution :: TBinding (M.Map Name [Name] -> M.Map Name Name)
selfRefSubstitution = def "selfRefSubstitution" $
  lambda "grouped" $
    Lists.foldl
      (lambda "subst" $ lambda "entry" $
        selfRefSubstitution_processGroup @@ var "subst" @@ Pairs.first (var "entry") @@ Pairs.second (var "entry"))
      (Maps.empty)
      (Maps.toList (var "grouped"))

selfRefSubstitution_processGroup :: TBinding (M.Map Name Name -> Name -> [Name] -> M.Map Name Name)
selfRefSubstitution_processGroup = def "selfRefSubstitution_processGroup" $
  lambda "subst" $ lambda "inVar" $ lambda "outVars" $
    Logic.ifElse
      ((Lists.elem :: TTerm Name -> TTerm [Name] -> TTerm Bool) (var "inVar") (var "outVars"))
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
-- safe non-self vars, substitute those vars to the input var.
directRefSubstitution :: TBinding (S.Set Name -> Maybe Name -> M.Map Name [Name] -> M.Map Name Name)
directRefSubstitution = def "directRefSubstitution" $
  lambda "directInputVars" $ lambda "codVar" $ lambda "grouped" $
    Lists.foldl
      (lambda "subst" $ lambda "entry" $
        directRefSubstitution_processGroup
          @@ var "directInputVars" @@ var "codVar"
          @@ var "subst" @@ Pairs.first (var "entry") @@ Pairs.second (var "entry"))
      (Maps.empty)
      (Maps.toList (var "grouped"))

directRefSubstitution_processGroup :: TBinding (S.Set Name -> Maybe Name -> M.Map Name Name -> Name -> [Name] -> M.Map Name Name)
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

-- | Find the first self-referencing input var in a grouped map.
findSelfRefVar :: TBinding (M.Map Name [Name] -> Maybe Name)
findSelfRefVar = def "findSelfRefVar" $
  lambda "grouped" $
    "selfRefs" <~ Lists.filter
      (lambda "entry" $ (Lists.elem :: TTerm Name -> TTerm [Name] -> TTerm Bool) (Pairs.first (var "entry")) (Pairs.second (var "entry")))
      (Maps.toList (var "grouped")) $
    Logic.ifElse
      (Lists.null (var "selfRefs"))
      nothing
      (just (Pairs.first (Lists.head (var "selfRefs"))))

-- | Detect over-generalized type variables in a scheme type.
detectAccumulatorUnification :: TBinding ([Type] -> Type -> [Name] -> M.Map Name Type)
detectAccumulatorUnification = def "detectAccumulatorUnification" $
  lambda "doms" $ lambda "cod" $ lambda "tparams" $
    "tparamSet" <~ Sets.fromList (var "tparams") $
    "allPairs" <~ Lists.bind (var "doms") (lambda "d" $ extractInOutPair @@ var "d") $
    "groupedByInput" <~ (groupPairsByFirst @@ var "allPairs") $
    "selfRefSubst" <~ (selfRefSubstitution @@ var "groupedByInput") $
    "directPairs" <~ Lists.bind (var "doms") (lambda "d" $ extractDirectReturn @@ var "tparamSet" @@ var "d") $
    "groupedDirect" <~ (groupPairsByFirst @@ var "directPairs") $
    "directInputVars" <~ Sets.fromList (Lists.map (lambda "p" $ Pairs.first (var "p")) (var "directPairs")) $
    "codVar" <~ (cases _Type (Rewriting.deannotateType @@ var "cod")
      (Just nothing) [
      _Type_variable>>: lambda "v" $ just (var "v")]) $
    "directRefSubst" <~ (directRefSubstitution @@ var "directInputVars" @@ var "codVar" @@ var "groupedDirect") $
    "codSubst" <~ (Maybes.maybe
      (Maps.empty)
      (lambda "cv" $
        Logic.ifElse
          (Maps.member (var "cv") (var "selfRefSubst"))
          (Maps.empty)
          (Maybes.maybe
            (Maps.empty)
            (lambda "refVar" $
              Logic.ifElse
                (Equality.equal (var "cv") (var "refVar"))
                (Maps.empty)
                (Maps.singleton (var "cv") (var "refVar")))
            (findSelfRefVar @@ var "groupedByInput")))
      (findPairFirst @@ var "cod")) $
    "domVars" <~ Sets.fromList (Lists.bind (var "doms") (lambda "d" $ Sets.toList (collectTypeVars @@ var "d"))) $
    "danglingSubst" <~ (Maybes.maybe
      (Maps.empty)
      (lambda "cv" $
        Logic.ifElse
          (Sets.member (var "cv") (var "domVars"))
          (Maps.empty)
          (Maybes.maybe
            (Maps.empty)
            (lambda "refVar" $ Maps.singleton (var "cv") (Core.typeVariable (var "refVar")))
            (findSelfRefVar @@ var "groupedByInput")))
      (findPairFirst @@ var "cod")) $
    Maps.union (Maps.union (Maps.union
      (nameMapToTypeMap @@ var "selfRefSubst")
      (nameMapToTypeMap @@ var "codSubst"))
      (var "danglingSubst"))
      (nameMapToTypeMap @@ var "directRefSubst")

-- =============================================================================
-- Batch 16: typesMatch, isSimpleName, filterPhantomTypeArgs
-- =============================================================================

-- | Shallow structural match for types.
-- TypeVariable: check names match. TypeWrap: check type names match. Otherwise: True.
typesMatch :: TBinding (Type -> Type -> Bool)
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
          Equality.equal (Core.wrappedTypeTypeName (var "wa")) (Core.wrappedTypeTypeName (var "wb"))]]

-- | Check if a Name is simple (unqualified, no dots).
isSimpleName :: TBinding (Name -> Bool)
isSimpleName = def "isSimpleName" $
  lambda "name" $
    Equality.equal
      (Lists.length (Strings.splitOn (string ".") (Core.unName (var "name"))))
      (int32 1)

-- | Filter type arguments to remove those at positions corresponding to phantom
-- or over-generalized scheme variables.
filterPhantomTypeArgs :: TBinding (Name -> [Type] -> Context -> Graph -> Either (InContext Error) [Type])
filterPhantomTypeArgs = def "filterPhantomTypeArgs" $
  lambda "calleeName" $ lambda "allTypeArgs" $
    "cx" ~> "g" ~>
    "mel" <<~ right (Lexical.dereferenceElement @@ var "g" @@ var "calleeName") $
    Maybes.cases (var "mel")
      (right (var "allTypeArgs"))
      (lambda "el" $
        Maybes.cases (Core.bindingType (var "el"))
          (right (var "allTypeArgs"))
          (lambda "ts" $
            "schemeVars" <~ Lists.filter (lambda "v" $ isSimpleName @@ var "v") (Core.typeSchemeVariables (var "ts")) $
            "schemeTypeVars" <~ collectTypeVars @@ Core.typeSchemeType (var "ts") $
            "schemeType" <~ Core.typeSchemeType (var "ts") $
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
filterPhantomTypeArgs_filterAndApply :: TBinding ([Type] -> [Bool] -> M.Map Name Type -> [Type])
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

-- =============================================================================
-- Batch 17: filterByFlags, applySubstSimple, buildArgSubst
-- =============================================================================

-- | Select elements from a list where the corresponding flag is True.
filterByFlags :: TBinding ([a] -> [Bool] -> [a])
filterByFlags = def "filterByFlags" $
  lambda "xs" $ lambda "flags" $
    Lists.map (lambda "p" $ Pairs.first (var "p"))
      (Lists.filter (lambda "p" $ Pairs.second (var "p"))
        (Lists.zip (var "xs") (var "flags")))

-- | Simple top-level-only type variable substitution.
applySubstSimple :: TBinding (M.Map Name Type -> Type -> Type)
applySubstSimple = def "applySubstSimple" $
  lambda "subst" $ lambda "t" $
    cases _Type (Rewriting.deannotateType @@ var "t")
      (Just $ var "t") [
      _Type_variable>>: lambda "v" $
        Maps.findWithDefault (var "t") (var "v") (var "subst")]

-- | Build a type substitution from scheme domain types and actual argument types.
buildArgSubst :: TBinding (S.Set Name -> [Type] -> [Type] -> M.Map Name Type)
buildArgSubst = def "buildArgSubst" $
  lambda "schemeVarSet" $ lambda "schemeDoms" $ lambda "argTypes" $
    Maps.fromList (Lists.bind
      (Lists.zip (var "schemeDoms") (var "argTypes"))
      (lambda "p" $
        "sdom" <~ Pairs.first (var "p") $
        "argType" <~ Pairs.second (var "p") $
        cases _Type (Rewriting.deannotateType @@ var "sdom")
          (Just $ list ([] :: [TTerm (Name, Type)])) [
          _Type_variable>>: lambda "v" $
            Logic.ifElse
              (Sets.member (var "v") (var "schemeVarSet"))
              (list [pair (var "v") (var "argType")])
              (list ([] :: [TTerm (Name, Type)]))]))

-- =============================================================================
-- Batch 18: resolveTypeApps, correctTypeAppsWithArgs, correctTypeApps
-- =============================================================================

-- | Given a partial argSubst, fill in unresolved vars from unused IR types.
resolveTypeApps :: TBinding ([Name] -> [Type] -> M.Map Name Type -> [Type])
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

-- | Try to verify and correct IR type args using annotation-based arg types.
correctTypeAppsWithArgs :: TBinding ([Name] -> [Type] -> Type -> [Term] -> Context -> Graph -> Either (InContext Error) [Type])
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
      (Logic.not (Lists.null (Lists.filter (lambda "m" $ Maybes.isNothing (var "m")) (var "mArgTypes"))))
      (right (var "fallbackTypeApps"))
      ("argTypes" <~ Lists.bind (var "mArgTypes")
        (lambda "m" $ Maybes.cases (var "m") (list ([] :: [TTerm Type])) (lambda "x" $ Lists.pure (var "x"))) $
      "irDoms" <~ Lists.map (lambda "d" $ applySubstSimple @@ var "irSubst" @@ var "d") (var "schemeDoms") $
      "domsMatch" <~ Lists.null (Lists.filter
        (lambda "p" $ Logic.not (typesMatch @@ (Rewriting.deannotateType @@ Pairs.first (var "p"))
                                              @@ (Rewriting.deannotateType @@ Pairs.second (var "p"))))
        (Lists.zip (var "irDoms") (var "argTypes"))) $
      Logic.ifElse (var "domsMatch")
        (right (var "fallbackTypeApps"))
        (right (resolveTypeApps @@ var "schemeVars" @@ var "fallbackTypeApps"
          @@ (buildArgSubst @@ var "schemeVarSet" @@ var "schemeDoms" @@ var "argTypes"))))

-- | Compute corrected type applications for a function call.
correctTypeApps :: TBinding (Graph -> Name -> [Term] -> [Type] -> Context -> Graph -> Either (InContext Error) [Type])
correctTypeApps = def "correctTypeApps" $
  lambda "gr" $ lambda "name" $ lambda "args" $ lambda "fallbackTypeApps" $
    "cx" ~> "g" ~>
    "mel" <<~ right (Lexical.dereferenceElement @@ var "g" @@ var "name") $
    Maybes.cases (var "mel")
      (right (var "fallbackTypeApps"))
      (lambda "el" $
        Maybes.cases (Core.bindingType (var "el"))
          (right (var "fallbackTypeApps"))
          (lambda "ts" $
            "schemeType" <~ Core.typeSchemeType (var "ts") $
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

-- =============================================================================
-- Over-generalization substitution builders (Batch 19)
-- =============================================================================

-- | Recursive helper for buildSubstFromAnnotations. Walks a term and compares lambda domain types
-- (normalized) against annotation map types (NOT normalized) to recover the fresh→canonical mapping.
-- Pure function: takes the graph and term directly.
buildSubstFromAnnotations_go :: TBinding (S.Set Name -> Graph -> Term -> M.Map Name Name)
buildSubstFromAnnotations_go = def "buildSubstFromAnnotations_go" $
  lambda "schemeVarSet" $ lambda "g" $ lambda "term" $
    cases _Term (var "term")
      (Just Maps.empty) [
      _Term_annotated>>: lambda "at" $
        "body" <~ Core.annotatedTermBody (var "at") $
        "anns" <~ Core.annotatedTermAnnotation (var "at") $
        "bodySubst" <~ (buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ var "body") $
        "annSubst" <~ Maybes.cases (Maps.lookup Constants.key_type (var "anns"))
          Maps.empty
          (lambda "typeTerm" $
            Eithers.either_
              (lambda "_" Maps.empty)
              (lambda "annType" $
                cases _Term (Rewriting.deannotateTerm @@ var "body")
                  (Just Maps.empty) [
                  _Term_function>>: lambda "f" $
                    cases _Function (var "f")
                      (Just Maps.empty) [
                      _Function_lambda>>: lambda "lam" $
                        Maybes.cases (Core.lambdaDomain (var "lam"))
                          Maps.empty
                          (lambda "dom" $
                            cases _Type (Rewriting.deannotateType @@ var "annType")
                              (Just Maps.empty) [
                              _Type_function>>: lambda "ft" $
                                buildTypeVarSubst @@ var "schemeVarSet"
                                  @@ Core.functionTypeDomain (var "ft")
                                  @@ var "dom"])]])
              (Phantoms.decoderFor _Type @@ var "g" @@ var "typeTerm")) $
        Maps.union (var "annSubst") (var "bodySubst"),
      _Term_application>>: lambda "app" $
        Maps.union
          (buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ Core.applicationFunction (var "app"))
          (buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ Core.applicationArgument (var "app")),
      _Term_function>>: lambda "f" $
        cases _Function (var "f")
          (Just Maps.empty) [
          _Function_lambda>>: lambda "lam" $
            buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ Core.lambdaBody (var "lam"),
          _Function_elimination>>: lambda "elim" $
            cases _Elimination (var "elim")
              (Just Maps.empty) [
              _Elimination_union>>: lambda "cs" $
                "defSubst" <~ Maybes.cases (Core.caseStatementDefault (var "cs"))
                  Maps.empty
                  (lambda "d" $ buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ var "d") $
                "caseSubsts" <~ Lists.foldl
                  (lambda "acc" $ lambda "fld" $
                    Maps.union (var "acc") (buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ Core.fieldTerm (var "fld")))
                  Maps.empty
                  (Core.caseStatementCases (var "cs")) $
                Maps.union (var "defSubst") (var "caseSubsts")]],
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
      _Term_maybe>>: lambda "mt" $
        Maybes.cases (var "mt")
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

-- | Build a type variable substitution by walking a term and comparing lambda domain types
-- against annotation map types. Returns a Map Name Name (fresh→canonical mapping).
buildSubstFromAnnotations :: TBinding (S.Set Name -> Term -> Context -> Graph -> Either (InContext Error) (M.Map Name Name))
buildSubstFromAnnotations = def "buildSubstFromAnnotations" $
  lambda "schemeVarSet" $ lambda "term" $
    "cx" ~> "g" ~>
    right (buildSubstFromAnnotations_go @@ var "schemeVarSet" @@ var "g" @@ var "term")

-- | Recursive helper for applyOvergenSubstToTermAnnotations. Walks a term and applies
-- a type substitution to all type annotations. Also updates lambda domains and type applications.
-- Pure function: takes the substitution, graph context, and term directly.
applyOvergenSubstToTermAnnotations_go :: TBinding (M.Map Name Type -> Graph -> Term -> Term)
applyOvergenSubstToTermAnnotations_go = def "applyOvergenSubstToTermAnnotations_go" $
  lambda "subst" $ lambda "cx" $ lambda "term" $
    cases _Term (var "term")
      (Just $ var "term") [
      _Term_annotated>>: lambda "at" $
        "inner" <~ Core.annotatedTermBody (var "at") $
        "ann" <~ Core.annotatedTermAnnotation (var "at") $
        "ann'" <~ Maybes.cases (Maps.lookup Constants.key_type (var "ann"))
          (var "ann")
          (lambda "typeTerm" $
            Eithers.either_
              (lambda "_" $ var "ann")
              (lambda "t" $
                "t'" <~ (substituteTypeVarsWithTypes @@ var "subst" @@ var "t") $
                Maps.insert (asTerm Constants.key_type) (Phantoms.encoderFor _Type @@ var "t'") (var "ann"))
              (Phantoms.decoderFor _Type @@ var "cx" @@ var "typeTerm")) $
        Core.termAnnotated (Core.annotatedTerm
          (applyOvergenSubstToTermAnnotations_go @@ var "subst" @@ var "cx" @@ var "inner")
          (var "ann'")),
      _Term_application>>: lambda "app" $
        Core.termApplication (Core.application
          (applyOvergenSubstToTermAnnotations_go @@ var "subst" @@ var "cx" @@ Core.applicationFunction (var "app"))
          (applyOvergenSubstToTermAnnotations_go @@ var "subst" @@ var "cx" @@ Core.applicationArgument (var "app"))),
      _Term_function>>: lambda "f" $
        cases _Function (var "f")
          (Just $ var "term") [
          _Function_lambda>>: lambda "lam" $
            Core.termFunction (Core.functionLambda (Core.lambda
              (Core.lambdaParameter (var "lam"))
              (Maybes.map (lambda "d" $ substituteTypeVarsWithTypes @@ var "subst" @@ var "d") (Core.lambdaDomain (var "lam")))
              (applyOvergenSubstToTermAnnotations_go @@ var "subst" @@ var "cx" @@ Core.lambdaBody (var "lam")))),
          _Function_elimination>>: lambda "elim" $
            cases _Elimination (var "elim")
              (Just $ var "term") [
              _Elimination_union>>: lambda "cs" $
                Core.termFunction (Core.functionElimination (Core.eliminationUnion (Core.caseStatement
                  (Core.caseStatementTypeName (var "cs"))
                  (Maybes.map (lambda "d" $ applyOvergenSubstToTermAnnotations_go @@ var "subst" @@ var "cx" @@ var "d") (Core.caseStatementDefault (var "cs")))
                  (Lists.map (lambda "fld" $ Core.field (Core.fieldName (var "fld")) (applyOvergenSubstToTermAnnotations_go @@ var "subst" @@ var "cx" @@ Core.fieldTerm (var "fld"))) (Core.caseStatementCases (var "cs"))))))]],
      _Term_let>>: lambda "lt" $
        Core.termLet (Core.let_
          (Lists.map (lambda "b" $ Core.binding (Core.bindingName (var "b")) (applyOvergenSubstToTermAnnotations_go @@ var "subst" @@ var "cx" @@ Core.bindingTerm (var "b")) (Core.bindingType (var "b"))) (Core.letBindings (var "lt")))
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

-- | Apply a type substitution to all type annotations in a term.
-- Monadic wrapper that gets the graph state and delegates to the pure helper.
applyOvergenSubstToTermAnnotations :: TBinding (M.Map Name Type -> Term -> Context -> Graph -> Either (InContext Error) Term)
applyOvergenSubstToTermAnnotations = def "applyOvergenSubstToTermAnnotations" $
  lambda "subst" $ lambda "term0" $
    "cx" ~> "g" ~>
    right (applyOvergenSubstToTermAnnotations_go @@ var "subst" @@ var "g" @@ var "term0")

-- =============================================================================
-- Batch 20: compareTo helpers, recordCompareToMethod, variantCompareToMethod
-- =============================================================================

-- | Helper: coerce bigint to int for javaInt/javaIntExpression TBinding (phantom type mismatch workaround)
bigintAsInt :: TTerm Integer -> TTerm Int
bigintAsInt = coerce

-- | Shared helper: reference type for Comparable, used in cast expressions for compareTo
javaComparableRefType :: TBinding Java.ReferenceType
javaComparableRefType = def "javaComparableRefType" $
  JavaDsl.referenceTypeClassOrInterface (JavaDsl.classOrInterfaceTypeClass
    (JavaDsl.classType
      (list ([] :: [TTerm Java.Annotation]))
      JavaDsl.classTypeQualifierNone
      (JavaUtilsSource.javaTypeIdentifier @@ string "Comparable")
      (list ([] :: [TTerm Java.TypeArgument]))))

-- | Shared helper: ((Comparable) this.field).compareTo(otherVar.field)
comparableCompareExpr :: TBinding (String -> String -> Java.Expression)
comparableCompareExpr = def "comparableCompareExpr" $
  lambda "otherVar" $ lambda "fname" $ lets [
    "arg">: JavaUtilsSource.javaExpressionNameToJavaExpression @@
      (JavaUtilsSource.fieldExpression @@ (JavaUtilsSource.javaIdentifier @@ var "otherVar") @@ (JavaUtilsSource.javaIdentifier @@ var "fname")),
    "castVar">: JavaDsl.methodInvocationVariantPrimary (JavaUtilsSource.javaExpressionToJavaPrimary @@
      (JavaUtilsSource.javaCastExpressionToJavaExpression @@
        (JavaUtilsSource.javaCastExpression @@ (asTerm javaComparableRefType)
          @@ (JavaUtilsSource.javaIdentifierToJavaUnaryExpression @@ wrap Java._Identifier (JavaUtilsSource.sanitizeJavaName @@ var "fname"))))),
    "header">: JavaDsl.methodInvocationHeaderComplex
      (JavaDsl.methodInvocationComplex (var "castVar") (list ([] :: [TTerm Java.TypeArgument]))
        (wrap Java._Identifier (asTerm JavaNamesSource.compareToMethodName)))] $
    JavaUtilsSource.javaMethodInvocationToJavaExpression @@ (JavaDsl.methodInvocation_ (var "header") (list [var "arg"]))

-- | Shared helper: java.util.Arrays.compare(this.field, otherVar.field)
arraysCompareExpr :: TBinding (String -> String -> Java.Expression)
arraysCompareExpr = def "arraysCompareExpr" $
  lambda "otherVar" $ lambda "fname" $ lets [
    "header">: JavaDsl.methodInvocationHeaderComplex
      (JavaDsl.methodInvocationComplex
        (JavaDsl.methodInvocationVariantType (JavaUtilsSource.javaTypeName @@ wrap Java._Identifier (string "java.util.Arrays")))
        (list ([] :: [TTerm Java.TypeArgument]))
        (wrap Java._Identifier (string "compare"))),
    "arg1">: JavaUtilsSource.javaExpressionNameToJavaExpression @@
      (JavaDsl.expressionName nothing (wrap Java._Identifier (JavaUtilsSource.sanitizeJavaName @@ var "fname"))),
    "arg2">: JavaUtilsSource.javaExpressionNameToJavaExpression @@
      (JavaUtilsSource.fieldExpression @@ (JavaUtilsSource.javaIdentifier @@ var "otherVar") @@ (JavaUtilsSource.javaIdentifier @@ var "fname"))] $
    JavaUtilsSource.javaMethodInvocationToJavaExpression @@ (JavaDsl.methodInvocation_ (var "header") (list [var "arg1", var "arg2"]))

-- | Shared helper: Integer.compare(this.field.hashCode(), otherVar.field.hashCode())
hashCodeCompareExpr :: TBinding (String -> String -> Java.Expression)
hashCodeCompareExpr = def "hashCodeCompareExpr" $
  lambda "otherVar" $ lambda "fname" $ lets [
    "header">: JavaDsl.methodInvocationHeaderComplex
      (JavaDsl.methodInvocationComplex
        (JavaDsl.methodInvocationVariantType (JavaUtilsSource.javaTypeName @@ wrap Java._Identifier (string "Integer")))
        (list ([] :: [TTerm Java.TypeArgument]))
        (wrap Java._Identifier (string "compare"))),
    "thisHashCode">: JavaUtilsSource.javaMethodInvocationToJavaExpression @@ (JavaDsl.methodInvocation_
      (JavaDsl.methodInvocationHeaderComplex
        (JavaDsl.methodInvocationComplex
          (JavaDsl.methodInvocationVariantExpression
            (JavaDsl.expressionName nothing (wrap Java._Identifier (JavaUtilsSource.sanitizeJavaName @@ var "fname"))))
          (list ([] :: [TTerm Java.TypeArgument]))
          (wrap Java._Identifier (asTerm JavaNamesSource.hashCodeMethodName))))
      (list ([] :: [TTerm Java.Expression]))),
    "otherHashCode">: JavaUtilsSource.javaMethodInvocationToJavaExpression @@ (JavaDsl.methodInvocation_
      (JavaDsl.methodInvocationHeaderComplex
        (JavaDsl.methodInvocationComplex
          (JavaDsl.methodInvocationVariantExpression
            (JavaUtilsSource.fieldExpression @@ (JavaUtilsSource.javaIdentifier @@ var "otherVar") @@ (JavaUtilsSource.javaIdentifier @@ var "fname")))
          (list ([] :: [TTerm Java.TypeArgument]))
          (wrap Java._Identifier (asTerm JavaNamesSource.hashCodeMethodName))))
      (list ([] :: [TTerm Java.Expression])))] $
    JavaUtilsSource.javaMethodInvocationToJavaExpression @@ (JavaDsl.methodInvocation_ (var "header") (list [var "thisHashCode", var "otherHashCode"]))

-- | Shared helper: dispatch to appropriate comparison expression based on field type
compareFieldExpr :: TBinding (String -> FieldType -> Java.Expression)
compareFieldExpr = def "compareFieldExpr" $
  lambda "otherVar" $ lambda "ft" $
    "fname" <~ Core.unName (Core.fieldTypeName (var "ft")) $
    "ftype" <~ Core.fieldTypeType (var "ft") $
    Logic.ifElse (isBinaryType @@ var "ftype")
      (arraysCompareExpr @@ var "otherVar" @@ var "fname")
      (Logic.ifElse (isNonComparableType @@ var "ftype")
        (hashCodeCompareExpr @@ var "otherVar" @@ var "fname")
        (comparableCompareExpr @@ var "otherVar" @@ var "fname"))

-- | Shared helper: cmp != 0 expression
cmpNotZeroExpr :: TBinding Java.Expression
cmpNotZeroExpr = def "cmpNotZeroExpr" $ lets [
    "lhs">: JavaUtilsSource.javaRelationalExpressionToJavaEqualityExpression @@
      (JavaUtilsSource.javaPostfixExpressionToJavaRelationalExpression @@
        (JavaDsl.postfixExpressionName (JavaDsl.expressionName nothing (JavaUtilsSource.javaIdentifier @@ string "cmp")))),
    "rhs">: JavaUtilsSource.javaPostfixExpressionToJavaRelationalExpression @@
      (JavaDsl.postfixExpressionPrimary (JavaUtilsSource.javaLiteralToJavaPrimary @@ (JavaUtilsSource.javaInt @@ bigintAsInt (bigint 0))))] $
    JavaUtilsSource.javaEqualityExpressionToJavaExpression @@
      (JavaDsl.equalityExpressionNotEqual (JavaDsl.equalityExpressionBinary (var "lhs") (var "rhs")))

-- | Shared helper: int cmp = 0; declaration
cmpDeclStatement :: TBinding (JavaHelpers.Aliases -> Java.BlockStatement)
cmpDeclStatement = def "cmpDeclStatement" $
  lambda "aliases" $
    JavaUtilsSource.variableDeclarationStatement @@ var "aliases" @@ (asTerm JavaUtilsSource.javaIntType)
      @@ (JavaUtilsSource.javaIdentifier @@ string "cmp") @@ (JavaUtilsSource.javaIntExpression @@ bigintAsInt (bigint 0))

-- | Shared helper: cmp = expr; if (cmp != 0) return cmp;
compareAndReturnStmts :: TBinding (String -> FieldType -> [Java.BlockStatement])
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

-- | Shared helper: build the compareTo method body for a list of fields
compareToBody :: TBinding (JavaHelpers.Aliases -> String -> [FieldType] -> [Java.BlockStatement])
compareToBody = def "compareToBody" $
  lambda "aliases" $ lambda "otherVar" $ lambda "fields" $
    Logic.ifElse (Lists.null (var "fields"))
      (list [JavaDsl.blockStatementStatement (JavaUtilsSource.javaReturnStatement @@ just (JavaUtilsSource.javaIntExpression @@ bigintAsInt (bigint 0)))])
      (Logic.ifElse (Equality.equal (Lists.length (var "fields")) (int32 1))
        (list [JavaDsl.blockStatementStatement (JavaUtilsSource.javaReturnStatement @@ just (compareFieldExpr @@ var "otherVar" @@ Lists.head (var "fields")))])
        (Lists.concat2
          (list [cmpDeclStatement @@ var "aliases"])
          (Lists.concat2
            (Lists.concat (Lists.map (lambda "f" $ compareAndReturnStmts @@ var "otherVar" @@ var "f") (Lists.init (var "fields"))))
            (list [JavaDsl.blockStatementStatement (JavaUtilsSource.javaReturnStatement @@ just (compareFieldExpr @@ var "otherVar" @@ Lists.last (var "fields")))]))
          ))

-- | Generate a compareTo method for a record type.
recordCompareToMethod :: TBinding (JavaHelpers.Aliases -> [Java.TypeParameter] -> Name -> [FieldType] -> Java.ClassBodyDeclaration)
recordCompareToMethod = def "recordCompareToMethod" $
  lambda "aliases" $ lambda "tparams" $ lambda "elName" $ lambda "fields" $ lets [
    "anns">: list [asTerm JavaUtilsSource.overrideAnnotation, asTerm JavaUtilsSource.suppressWarningsUncheckedAnnotation],
    "mods">: list [inject Java._MethodModifier Java._MethodModifier_public unit],
    "param">: JavaUtilsSource.javaTypeToJavaFormalParameter @@ (JavaUtilsSource.javaTypeFromTypeName @@ var "aliases" @@ var "elName") @@ wrap _Name (asTerm JavaNamesSource.otherInstanceName),
    "result">: JavaUtilsSource.javaTypeToJavaResult @@ (asTerm JavaUtilsSource.javaIntType)] $
    JavaUtilsSource.methodDeclaration @@ var "mods" @@ list ([] :: [TTerm Java.TypeParameter]) @@ var "anns"
      @@ (asTerm JavaNamesSource.compareToMethodName) @@ list [var "param"] @@ var "result"
      @@ just (compareToBody @@ var "aliases" @@ (asTerm JavaNamesSource.otherInstanceName) @@ var "fields")

-- | Shared helper: this.getClass().getName().compareTo(other.getClass().getName())
-- Used in variant compareTo to compare by class name for tag ordering.
tagCompareExpr :: TBinding Java.Expression
tagCompareExpr = def "tagCompareExpr" $ lets [
    "thisGetClass">: JavaDsl.methodInvocation_
      (JavaDsl.methodInvocationHeaderComplex
        (JavaDsl.methodInvocationComplex
          (JavaDsl.methodInvocationVariantPrimary (JavaUtilsSource.javaExpressionToJavaPrimary @@ (asTerm JavaUtilsSource.javaThis)))
          (list ([] :: [TTerm Java.TypeArgument]))
          (wrap Java._Identifier (string "getClass"))))
      (list ([] :: [TTerm Java.Expression])),
    "thisGetName">: JavaDsl.methodInvocation_
      (JavaDsl.methodInvocationHeaderComplex
        (JavaDsl.methodInvocationComplex
          (JavaDsl.methodInvocationVariantPrimary (JavaUtilsSource.javaMethodInvocationToJavaPrimary @@ var "thisGetClass"))
          (list ([] :: [TTerm Java.TypeArgument]))
          (wrap Java._Identifier (string "getName"))))
      (list ([] :: [TTerm Java.Expression])),
    "otherGetClass">: JavaDsl.methodInvocation_
      (JavaDsl.methodInvocationHeaderComplex
        (JavaDsl.methodInvocationComplex
          (JavaDsl.methodInvocationVariantExpression
            (JavaDsl.expressionName nothing (wrap Java._Identifier (asTerm JavaNamesSource.otherInstanceName))))
          (list ([] :: [TTerm Java.TypeArgument]))
          (wrap Java._Identifier (string "getClass"))))
      (list ([] :: [TTerm Java.Expression])),
    "otherGetName">: JavaDsl.methodInvocation_
      (JavaDsl.methodInvocationHeaderComplex
        (JavaDsl.methodInvocationComplex
          (JavaDsl.methodInvocationVariantPrimary (JavaUtilsSource.javaMethodInvocationToJavaPrimary @@ var "otherGetClass"))
          (list ([] :: [TTerm Java.TypeArgument]))
          (wrap Java._Identifier (string "getName"))))
      (list ([] :: [TTerm Java.Expression]))] $
    JavaUtilsSource.javaMethodInvocationToJavaExpression @@ (JavaDsl.methodInvocation_
      (JavaDsl.methodInvocationHeaderComplex
        (JavaDsl.methodInvocationComplex
          (JavaDsl.methodInvocationVariantPrimary (JavaUtilsSource.javaMethodInvocationToJavaPrimary @@ var "thisGetName"))
          (list ([] :: [TTerm Java.TypeArgument]))
          (wrap Java._Identifier (asTerm JavaNamesSource.compareToMethodName))))
      (list [JavaUtilsSource.javaMethodInvocationToJavaExpression @@ var "otherGetName"]))

-- | Shared helper: tagCmp != 0
tagCmpNotZeroExpr :: TBinding Java.Expression
tagCmpNotZeroExpr = def "tagCmpNotZeroExpr" $ lets [
    "lhs">: JavaUtilsSource.javaRelationalExpressionToJavaEqualityExpression @@
      (JavaUtilsSource.javaPostfixExpressionToJavaRelationalExpression @@
        (JavaDsl.postfixExpressionName (JavaDsl.expressionName nothing (JavaUtilsSource.javaIdentifier @@ string "tagCmp")))),
    "rhs">: JavaUtilsSource.javaPostfixExpressionToJavaRelationalExpression @@
      (JavaDsl.postfixExpressionPrimary (JavaUtilsSource.javaLiteralToJavaPrimary @@ (JavaUtilsSource.javaInt @@ bigintAsInt (bigint 0))))] $
    JavaUtilsSource.javaEqualityExpressionToJavaExpression @@
      (JavaDsl.equalityExpressionNotEqual (JavaDsl.equalityExpressionBinary (var "lhs") (var "rhs")))

-- | Generate a compareTo method for a union variant (inner) class.
-- Takes the parent type as the compareTo parameter.
-- First compares variant class names for tag ordering,
-- then casts 'other' to the same variant class and compares the 'value' field.
variantCompareToMethod :: TBinding (JavaHelpers.Aliases -> [Java.TypeParameter] -> Name -> Name -> [FieldType] -> Java.ClassBodyDeclaration)
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
        @@ (JavaUtilsSource.nameToJavaReferenceType @@ var "aliases" @@ false @@ list ([] :: [TTerm Java.TypeArgument]) @@ var "variantName" @@ nothing)
        @@ (JavaUtilsSource.javaIdentifierToJavaUnaryExpression @@ wrap Java._Identifier (asTerm JavaNamesSource.otherInstanceName))),
    "castDeclStmt">: JavaUtilsSource.variableDeclarationStatement @@ var "aliases" @@ var "variantJavaType"
      @@ (JavaUtilsSource.javaIdentifier @@ var "varTmpName") @@ var "castOtherExpr",
    "emptyReturn">: list [JavaDsl.blockStatementStatement (JavaUtilsSource.javaReturnStatement @@ just (JavaUtilsSource.javaIntExpression @@ bigintAsInt (bigint 0)))],
    "valueCompareStmt">: Logic.ifElse (Lists.null (var "fields"))
      (var "emptyReturn")
      (Lists.concat2 (list [var "castDeclStmt"]) (compareToBody @@ var "aliases" @@ var "varTmpName" @@ var "fields")),
    "body">: Lists.concat2 (list [var "tagDeclStmt", var "tagReturnStmt"]) (var "valueCompareStmt")] $
    JavaUtilsSource.methodDeclaration @@ var "mods" @@ list ([] :: [TTerm Java.TypeParameter]) @@ var "anns"
      @@ (asTerm JavaNamesSource.compareToMethodName) @@ list [var "param"] @@ var "result"
      @@ just (var "body")

-- =============================================================================
-- Batch 21: Record type declaration helpers
-- =============================================================================

-- | Build a record field as a public final member variable declaration.
recordMemberVar :: TBinding (JavaHelpers.Aliases -> FieldType -> Context -> Graph -> Either (InContext Error) Java.ClassBodyDeclaration)
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
recordWithMethod :: TBinding (JavaHelpers.Aliases -> Name -> [FieldType] -> FieldType -> Context -> Graph -> Either (InContext Error) Java.ClassBodyDeclaration)
recordWithMethod = def "recordWithMethod" $
  lambda "aliases" $ lambda "elName" $ lambda "fields" $ lambda "field" $
    "cx" ~> "g" ~>
    lets [
    "mods">: list [inject Java._MethodModifier Java._MethodModifier_public unit],
    "anns">: list ([] :: [TTerm Java.Annotation]),
    "methodName">: Strings.cat2 (string "with")
      (Formatting.nonAlnumToUnderscores @@ (Formatting.capitalize @@ (unwrap _Name @@ Core.fieldTypeName (var "field")))),
    "result">: JavaUtilsSource.referenceTypeToResult
      @@ (JavaUtilsSource.nameToJavaReferenceType @@ var "aliases" @@ false
        @@ list ([] :: [TTerm Java.TypeArgument]) @@ var "elName" @@ nothing),
    "consId">: wrap Java._Identifier (JavaUtilsSource.sanitizeJavaName @@ (Names.localNameOf @@ var "elName")),
    "fieldArgs">: Lists.map (lambda "f" $ JavaUtilsSource.fieldNameToJavaExpression @@ Core.fieldTypeName (var "f")) (var "fields"),
    "returnStmt">: JavaDsl.blockStatementStatement
      (JavaUtilsSource.javaReturnStatement @@ just
        (JavaUtilsSource.javaConstructorCall @@ (JavaUtilsSource.javaConstructorName @@ var "consId" @@ nothing)
          @@ var "fieldArgs" @@ nothing))] $
    "param" <<~ (fieldTypeToFormalParam @@ var "aliases" @@ var "field" @@ var "cx" @@ var "g") $
    right (JavaUtilsSource.methodDeclaration @@ var "mods" @@ list ([] :: [TTerm Java.TypeParameter])
      @@ var "anns" @@ var "methodName" @@ list [var "param"] @@ var "result"
      @@ just (list [var "returnStmt"]))

-- | Build a record constructor.
recordConstructor :: TBinding (JavaHelpers.Aliases -> Name -> [FieldType] -> Context -> Graph -> Either (InContext Error) Java.ClassBodyDeclaration)
recordConstructor = def "recordConstructor" $
  lambda "aliases" $ lambda "elName" $ lambda "fields" $
    "cx" ~> "g" ~>
    lets [
    "assignStmts">: Lists.map
      (lambda "f" $ JavaDsl.blockStatementStatement (JavaUtilsSource.toAssignStmt @@ Core.fieldTypeName (var "f")))
      (var "fields")] $
    "params" <<~ (Eithers.mapList (lambda "f" $ fieldTypeToFormalParam @@ var "aliases" @@ var "f" @@ var "cx" @@ var "g") (var "fields")) $
    right (JavaUtilsSource.makeConstructor @@ var "aliases" @@ var "elName" @@ false @@ var "params" @@ var "assignStmts")

-- | Build an equality clause for a single field in the equals() method.
eqClause :: TBinding (String -> FieldType -> Java.InclusiveOrExpression)
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
equalsClause :: TBinding (String -> String -> Java.InclusiveOrExpression)
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
        (list ([] :: [TTerm Java.TypeArgument]))
        (wrap Java._Identifier (asTerm JavaNamesSource.equalsMethodName)))] $
    JavaUtilsSource.javaPostfixExpressionToJavaInclusiveOrExpression
      @@ (JavaUtilsSource.javaMethodInvocationToJavaPostfixExpression
        @@ (JavaDsl.methodInvocation_ (var "header") (list [var "thisArg", var "otherArg"])))

-- | java.util.Arrays.equals(this.field, other.field) for byte[] fields
arraysEqualsClause :: TBinding (String -> String -> Java.InclusiveOrExpression)
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
        (list ([] :: [TTerm Java.TypeArgument]))
        (wrap Java._Identifier (asTerm JavaNamesSource.equalsMethodName)))] $
    JavaUtilsSource.javaPostfixExpressionToJavaInclusiveOrExpression
      @@ (JavaUtilsSource.javaMethodInvocationToJavaPostfixExpression
        @@ (JavaDsl.methodInvocation_ (var "header") (list [var "thisArg", var "otherArg"])))

-- | this.field.compareTo(other.field) == 0 for BigDecimal/BigInteger fields
compareToZeroClause :: TBinding (String -> String -> Java.InclusiveOrExpression)
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
        (list ([] :: [TTerm Java.TypeArgument]))
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

-- | Build the equals() method for a record class.
recordEqualsMethod :: TBinding (JavaHelpers.Aliases -> Name -> [FieldType] -> Java.ClassBodyDeclaration)
recordEqualsMethod = def "recordEqualsMethod" $
  lambda "aliases" $ lambda "elName" $ lambda "fields" $ lets [
    "anns">: list [asTerm JavaUtilsSource.overrideAnnotation],
    "mods">: list [inject Java._MethodModifier Java._MethodModifier_public unit],
    "param">: JavaUtilsSource.javaTypeToJavaFormalParameter
      @@ (JavaUtilsSource.javaRefType @@ list ([] :: [TTerm Java.ReferenceType]) @@ nothing @@ string "Object")
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
                    @@ list ([] :: [TTerm Java.TypeArgument]) @@ var "elName" @@ nothing))))))
        (JavaUtilsSource.javaReturnStatement @@ just (JavaUtilsSource.javaBooleanExpression @@ false)))),
    -- ElName o = (ElName) other;
    "castStmt">: JavaUtilsSource.variableDeclarationStatement @@ var "aliases"
      @@ (JavaUtilsSource.javaTypeFromTypeName @@ var "aliases" @@ var "elName")
      @@ (JavaUtilsSource.javaIdentifier @@ var "tmpName")
      @@ (JavaUtilsSource.javaCastExpressionToJavaExpression @@
        (JavaUtilsSource.javaCastExpression
          @@ (JavaUtilsSource.nameToJavaReferenceType @@ var "aliases" @@ false
            @@ list ([] :: [TTerm Java.TypeArgument]) @@ var "elName" @@ nothing)
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
    JavaUtilsSource.methodDeclaration @@ var "mods" @@ list ([] :: [TTerm Java.TypeParameter]) @@ var "anns"
      @@ (asTerm JavaNamesSource.equalsMethodName) @@ list [var "param"] @@ var "result"
      @@ just (list [var "instanceOfStmt", var "castStmt", var "returnAllFieldsEqual"])

-- | Build a hashCode multiplier pair: prime * Objects.hashCode(field)
hashCodeMultPair :: TBinding (Int -> Name -> Java.MultiplicativeExpression)
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
              (list ([] :: [TTerm Java.TypeArgument]))
              (wrap Java._Identifier (asTerm JavaNamesSource.hashCodeMethodName))))
          (list [JavaUtilsSource.javaExpressionNameToJavaExpression
            @@ (JavaDsl.expressionName nothing
              (wrap Java._Identifier (JavaUtilsSource.sanitizeJavaName @@ var "fnameStr")))])))] $
    JavaDsl.multiplicativeExpressionTimes
      (JavaDsl.multiplicativeExpressionBinary (var "lhs") (var "rhs"))

-- | First 20 prime numbers used as hash code multipliers.
first20Primes :: TBinding [Int]
first20Primes = def "first20Primes" $
  list (fmap bigintAsInt [bigint 2, bigint 3, bigint 5, bigint 7, bigint 11, bigint 13, bigint 17, bigint 19,
    bigint 23, bigint 29, bigint 31, bigint 37, bigint 41, bigint 43, bigint 47, bigint 53, bigint 59,
    bigint 61, bigint 67, bigint 71])

-- | Build the hashCode() method for a record class.
recordHashCodeMethod :: TBinding ([FieldType] -> Java.ClassBodyDeclaration)
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
    JavaUtilsSource.methodDeclaration @@ var "mods" @@ list ([] :: [TTerm Java.TypeParameter]) @@ var "anns"
      @@ (asTerm JavaNamesSource.hashCodeMethodName) @@ list ([] :: [TTerm Java.FormalParameter]) @@ var "result"
      @@ just (list [var "returnSum"])

-- =============================================================================
-- Batch 22: constantDecl, declarationForRecordType, and entry-point functions
-- =============================================================================

-- | Create a constant field declaration (e.g., public static final Name TYPE_ = new Name("..."))
constantDecl :: TBinding (String -> JavaHelpers.Aliases -> Name -> Context -> Graph -> Either (InContext Error) Java.ClassBodyDeclarationWithComments)
constantDecl = def "constantDecl" $
  lambda "javaName" $ lambda "aliases" $ lambda "name" $
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
    right (noComment @@ (JavaUtilsSource.javaMemberField @@ var "mods" @@ var "jt" @@ var "var"))

-- | Create a constant field declaration for a field name.
constantDeclForFieldType :: TBinding (JavaHelpers.Aliases -> FieldType -> Context -> Graph -> Either (InContext Error) Java.ClassBodyDeclarationWithComments)
constantDeclForFieldType = def "constantDeclForFieldType" $
  lambda "aliases" $ lambda "ftyp" $
    "cx" ~> "g" ~>
    lets [
    "name">: Core.fieldTypeName (var "ftyp"),
    "javaName">: Formatting.nonAlnumToUnderscores @@ (Formatting.convertCase @@ Util.caseConventionCamel @@ Util.caseConventionUpperSnake @@ (unwrap _Name @@ var "name"))] $
    constantDecl @@ var "javaName" @@ var "aliases" @@ var "name" @@ var "cx" @@ var "g"

-- | Create a constant field declaration for a type name.
constantDeclForTypeName :: TBinding (JavaHelpers.Aliases -> Name -> Context -> Graph -> Either (InContext Error) Java.ClassBodyDeclarationWithComments)
constantDeclForTypeName = def "constantDeclForTypeName" $
  lambda "aliases" $ lambda "name" $
    "cx" ~> "g" ~>
    constantDecl @@ string "TYPE_" @@ var "aliases" @@ var "name" @@ var "cx" @@ var "g"

-- | Create a record type class declaration (without parent class).
declarationForRecordType :: TBinding (Bool -> Bool -> JavaHelpers.Aliases -> [Java.TypeParameter] -> Name
  -> [FieldType] -> Context -> Graph -> Either (InContext Error) Java.ClassDeclaration)
declarationForRecordType = def "declarationForRecordType" $
  lambda "isInner" $ lambda "isSer" $ lambda "aliases" $ lambda "tparams" $ lambda "elName" $ lambda "fields" $
    "cx" ~> "g" ~>
    declarationForRecordType' @@ var "isInner" @@ var "isSer" @@ var "aliases" @@ var "tparams" @@ var "elName" @@ nothing @@ var "fields" @@ var "cx" @@ var "g"

-- | Create a record type class declaration (with optional parent class).
declarationForRecordType' :: TBinding (Bool -> Bool -> JavaHelpers.Aliases -> [Java.TypeParameter] -> Name -> Maybe Name -> [FieldType]
  -> Context -> Graph -> Either (InContext Error) Java.ClassDeclaration)
declarationForRecordType' = def "declarationForRecordType'" $
  lambda "isInner" $ lambda "isSer" $ lambda "aliases" $ lambda "tparams" $ lambda "elName" $ lambda "parentName" $ lambda "fields" $
    "cx" ~> "g" ~>
    "memberVars" <<~ (Eithers.mapList (lambda "f" $ recordMemberVar @@ var "aliases" @@ var "f" @@ var "cx" @@ var "g") (var "fields")) $
    "memberVars'" <<~ (Eithers.mapList (lambda "p" $ addComment @@ (Pairs.first (var "p")) @@ (Pairs.second (var "p")) @@ var "cx" @@ var "g")
      (Lists.zip (var "memberVars") (var "fields"))) $
    "withMethods" <<~ (Logic.ifElse (Equality.gt (Lists.length (var "fields")) (int32 1))
      (Eithers.mapList (lambda "f" $ recordWithMethod @@ var "aliases" @@ var "elName" @@ var "fields" @@ var "f" @@ var "cx" @@ var "g") (var "fields"))
      (right (list ([] :: [TTerm Java.ClassBodyDeclaration])))) $
    "cons" <<~ (recordConstructor @@ var "aliases" @@ var "elName" @@ var "fields" @@ var "cx" @@ var "g") $
    "tn" <<~ (Logic.ifElse (var "isInner")
      (right (list ([] :: [TTerm Java.ClassBodyDeclarationWithComments])))
      ("d" <<~ (constantDeclForTypeName @@ var "aliases" @@ var "elName" @@ var "cx" @@ var "g") $
        "dfields" <<~ (Eithers.mapList (lambda "f" $ constantDeclForFieldType @@ var "aliases" @@ var "f" @@ var "cx" @@ var "g") (var "fields")) $
        right (Lists.cons (var "d") (var "dfields")))) $
    "comparableMethods" <~ (Maybes.cases (var "parentName")
      (Logic.ifElse (Logic.and (Logic.not (var "isInner")) (var "isSer"))
        (list [recordCompareToMethod @@ var "aliases" @@ var "tparams" @@ var "elName" @@ var "fields"])
        (list ([] :: [TTerm Java.ClassBodyDeclaration])))
      (lambda "pn" $ Logic.ifElse (var "isSer")
        (list [variantCompareToMethod @@ var "aliases" @@ var "tparams" @@ var "pn" @@ var "elName" @@ var "fields"])
        (list ([] :: [TTerm Java.ClassBodyDeclaration])))) $
    "bodyDecls" <~ (Lists.concat2 (var "tn") (Lists.concat2 (var "memberVars'")
      (Lists.map (lambda "x" $ noComment @@ var "x")
        (Lists.concat2
          (list [var "cons",
                 recordEqualsMethod @@ var "aliases" @@ var "elName" @@ var "fields",
                 recordHashCodeMethod @@ var "fields"])
          (Lists.concat2 (var "comparableMethods") (var "withMethods")))))) $
    "ifaces" <~ (Logic.ifElse (var "isInner")
      (serializableTypes @@ var "isSer")
      (interfaceTypes @@ var "isSer" @@ var "aliases" @@ var "tparams" @@ var "elName")) $
    right (JavaUtilsSource.javaClassDeclaration @@ var "aliases" @@ var "tparams" @@ var "elName"
      @@ asTerm classModsPublic @@ nothing @@ var "ifaces" @@ var "bodyDecls")

-- =============================================================================
-- Batch 23: Mutual recursion core — encodeTerm and friends
-- =============================================================================

-- | Take N type arguments from the accumulated type applications list,
-- converting them to Java TypeArguments via javaTypeToJavaReferenceType.
takeTypeArgs :: TBinding (String -> Int -> [Java.Type] -> Context -> Graph -> Either (InContext Error) [Java.TypeArgument])
takeTypeArgs = def "takeTypeArgs" $
  lambda "label" $ lambda "n" $ lambda "tyapps" $
    "cx" ~> "g" ~>
    Logic.ifElse (Equality.lt (Lists.length (var "tyapps")) (var "n"))
      (Ctx.failInContext (Error.errorOther $ Error.otherError $ Strings.cat (list [string "needed type arguments for ", var "label", string ", found too few"])) (var "cx"))
      (Eithers.mapList (lambda "jt" $
        "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt" @@ var "cx") $
        right (JavaDsl.typeArgumentReference (var "rt")))
        (Lists.take (var "n") (var "tyapps")))

-- | Check if a union variant field's type is a unit type, by looking up the union type schema.
isFieldUnitType :: TBinding (Name -> Name -> Context -> Graph -> Either (InContext Error) Bool)
isFieldUnitType = def "isFieldUnitType" $
  lambda "typeName" $ lambda "fieldName" $
    "cx" ~> "g" ~>
    "schemaTypes" <~ Graph.graphSchemaTypes (var "g") $
    Maybes.cases (Maps.lookup (var "typeName") (var "schemaTypes"))
      (right false)
      (lambda "ts" $
        cases _Type (Rewriting.deannotateType @@ Core.typeSchemeType (var "ts"))
          (Just $ right false) [
          _Type_union>>: lambda "rt" $
            right (Maybes.cases
              (Lists.find (lambda "ft" $ Equality.equal (Core.fieldTypeName (var "ft")) (var "fieldName"))
                (Core.rowTypeFields (var "rt")))
              false
              (lambda "ft" $ Schemas.isUnitType @@ (Rewriting.deannotateType @@ Core.fieldTypeType (var "ft"))))])

-- | Encode a Hydra term as a Java expression.
-- Wrapper that calls encodeTermInternal with empty accumulators.
encodeTerm :: TBinding (JavaHelpers.JavaEnvironment -> Term -> Context -> Graph -> Either (InContext Error) Java.Expression)
encodeTerm = def "encodeTerm" $
  lambda "env" $ lambda "term" $
    "cx" ~> "g" ~>
    encodeTermInternal @@ var "env" @@ list ([] :: [TTerm (M.Map Name Term)]) @@ list ([] :: [TTerm Java.Type]) @@ var "term" @@ var "cx" @@ var "g"

-- | Internal term encoder with annotation and type-application accumulators.
encodeTermInternal :: TBinding (JavaHelpers.JavaEnvironment -> [M.Map Name Term] -> [Java.Type] -> Term -> Context -> Graph -> Either (InContext Error) Java.Expression)
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
          @@ Lists.cons (Core.annotatedTermAnnotation (var "at")) (var "anns")
          @@ var "tyapps"
          @@ (Core.annotatedTermBody (var "at")) @@ var "cx" @@ var "g",

      -- TermApplication: delegate to encodeApplication
      _Term_application>>: lambda "app" $
        encodeApplication @@ var "env" @@ var "app" @@ var "cx" @@ var "g",

      -- TermEither: left or right
      -- Extract the Either type from annotations so we can pass component types to inner terms.
      -- Without this, inner lambdas lose their type context and type variables get concretized.
      _Term_either>>: lambda "et" $
        "targs" <<~ (takeTypeArgs @@ string "either" @@ int32 2 @@ var "tyapps" @@ var "cx" @@ var "g") $
        "combinedAnns" <~ Lists.foldl (lambda "acc" $ lambda "m" $ Maps.union (var "acc") (var "m")) Maps.empty (var "anns") $
        "mEitherType" <<~ (getTypeE (var "cx") (var "g") (var "combinedAnns")) $
        "branchTypes" <~ (Maybes.bind (var "mEitherType") (lambda "etyp" $
          cases _Type (Rewriting.deannotateType @@ var "etyp")
            (Just nothing) [
            _Type_either>>: lambda "et2" $
              just (pair (Core.eitherTypeLeft (var "et2")) (Core.eitherTypeRight (var "et2")))])) $
        "encodeWithType" <~ (lambda "branchType" $ lambda "t1" $
          "annotated" <~ (Annotations.setTermAnnotation @@ asTerm Constants.key_type
            @@ just (encodeTypeAsTerm @@ var "branchType") @@ var "t1") $
          encodeTermInternal @@ var "env" @@ var "anns" @@ list ([] :: [TTerm Java.Type]) @@ var "annotated" @@ var "cx" @@ var "g") $
        Eithers.either_
          (lambda "term1" $
            "expr" <<~ (Maybes.cases (var "branchTypes")
              (var "encode" @@ var "term1")
              (lambda "bt" $ var "encodeWithType" @@ Pairs.first (var "bt") @@ var "term1")) $
            right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocationStaticWithTypeArgs
                @@ JavaDsl.identifier (string "hydra.util.Either")
                @@ JavaDsl.identifier (string "left")
                @@ var "targs" @@ list [var "expr"])))
          (lambda "term1" $
            "expr" <<~ (Maybes.cases (var "branchTypes")
              (var "encode" @@ var "term1")
              (lambda "bt" $ var "encodeWithType" @@ Pairs.second (var "bt") @@ var "term1")) $
            right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocationStaticWithTypeArgs
                @@ JavaDsl.identifier (string "hydra.util.Either")
                @@ JavaDsl.identifier (string "right")
                @@ var "targs" @@ list [var "expr"])))
          (var "et"),

      -- TermFunction: encode function with type from annotations
      _Term_function>>: lambda "f" $
        ("combinedAnns" <~ Lists.foldl (lambda "acc" $ lambda "m" $ Maps.union (var "acc") (var "m")) Maps.empty (var "anns") $
        "mt" <<~ (getTypeE (var "cx") (var "g") (var "combinedAnns")) $
        "typ" <<~ (Maybes.cases (var "mt")
          (Maybes.cases (tryInferFunctionType @@ var "f")
            (CoderUtils.typeOfTerm @@ var "cx" @@ var "g" @@ var "term")
            (lambda "inferredType" $ right (var "inferredType")))
          (lambda "t" $ right (var "t"))) $
        cases _Type (Rewriting.deannotateType @@ var "typ")
          (Just $ encodeNullaryConstant @@ var "env" @@ var "typ" @@ var "f" @@ var "cx" @@ var "g") [
          _Type_function>>: lambda "ft" $
            encodeFunction @@ var "env" @@ (Core.functionTypeDomain (var "ft")) @@ (Core.functionTypeCodomain (var "ft")) @@ var "f" @@ var "cx" @@ var "g"]),

      -- TermLet: convert let bindings to block-bodied nullary lambda with .get()
      _Term_let>>: lambda "lt" $
        ("bindings" <~ Core.letBindings (var "lt") $
        "body" <~ Core.letBody (var "lt") $
        Logic.ifElse (Lists.null (var "bindings"))
          (encodeTermInternal @@ var "env" @@ var "anns" @@ list ([] :: [TTerm Java.Type]) @@ var "body" @@ var "cx" @@ var "g")
          ("bindResult" <<~ (bindingsToStatements @@ var "env" @@ var "bindings" @@ var "cx" @@ var "g") $
            "bindingStmts" <~ Pairs.first (var "bindResult") $
            "env2" <~ Pairs.second (var "bindResult") $
            "jbody" <<~ (encodeTermInternal @@ var "env2" @@ var "anns" @@ list ([] :: [TTerm Java.Type]) @@ var "body" @@ var "cx" @@ var "g") $
            "returnSt" <~ JavaDsl.blockStatementStatement (JavaUtilsSource.javaReturnStatement @@ just (var "jbody")) $
            "block" <~ (wrap Java._Block (Lists.concat2 (var "bindingStmts") (list [var "returnSt"]))) $
            "nullaryLambda" <~ JavaDsl.expressionLambda
              (JavaDsl.lambdaExpression
                (JavaDsl.lambdaParametersTuple (list ([] :: [TTerm Java.FormalParameter])))
                (JavaDsl.lambdaBodyBlock (var "block"))) $
            "combinedAnns" <~ Lists.foldl (lambda "acc" $ lambda "m" $ Maps.union (var "acc") (var "m")) Maps.empty (var "anns") $
            "g2" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_graph @@ var "env2") $
            "aliases2" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env2") $
            "mt" <<~ (getTypeE (var "cx") (var "g") (var "combinedAnns")) $
            "letType" <<~ (Maybes.cases (var "mt")
              (CoderUtils.typeOfTerm @@ var "cx" @@ var "g2" @@ var "body")
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
                (JavaUtilsSource.javaExpressionToJavaPrimary @@ var "castExpr")) @@ JavaDsl.identifier (string "get") @@ list ([] :: [TTerm Java.Expression]))))),

      -- TermList: ConsList.empty() or ConsList.of(elements)
      _Term_list>>: lambda "els" $
        Logic.ifElse (Lists.null (var "els"))
          ("targs" <<~ (takeTypeArgs @@ string "list" @@ int32 1 @@ var "tyapps" @@ var "cx" @@ var "g") $
            right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocationStaticWithTypeArgs
                @@ JavaDsl.identifier (string "hydra.util.ConsList")
                @@ JavaDsl.identifier (string "empty")
                @@ var "targs" @@ list ([] :: [TTerm Java.Expression]))))
          ("jels" <<~ (Eithers.mapList (var "encode") (var "els")) $
            right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocationStatic
                @@ JavaDsl.identifier (string "hydra.util.ConsList")
                @@ JavaDsl.identifier (string "of")
                @@ var "jels"))),

      -- TermLiteral: direct encoding
      _Term_literal>>: lambda "l" $
        right (encodeLiteral @@ var "l"),

      -- TermMap: PersistentMap.empty() or PersistentMap.ofEntries(PersistentMap.entry(k,v), ...)
      _Term_map>>: lambda "m" $
        Logic.ifElse (Maps.null (var "m"))
          ("targs" <<~ (takeTypeArgs @@ string "map" @@ int32 2 @@ var "tyapps" @@ var "cx" @@ var "g") $
            right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocationStaticWithTypeArgs
                @@ JavaDsl.identifier (string "hydra.util.PersistentMap")
                @@ JavaDsl.identifier (string "empty")
                @@ var "targs" @@ list ([] :: [TTerm Java.Expression]))))
          ("jkeys" <<~ (Eithers.mapList (var "encode") (Maps.keys (var "m"))) $
            "jvals" <<~ (Eithers.mapList (var "encode") (Maps.elems (var "m"))) $
            "pairExprs" <~ Lists.map
              (lambda "kv" $ JavaUtilsSource.javaMethodInvocationToJavaExpression @@
                (JavaUtilsSource.methodInvocationStatic
                  @@ JavaDsl.identifier (string "hydra.util.PersistentMap")
                  @@ JavaDsl.identifier (string "entry")
                  @@ list [Pairs.first (var "kv"), Pairs.second (var "kv")]))
              (Lists.zip (var "jkeys") (var "jvals")) $
            right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocationStatic
                @@ JavaDsl.identifier (string "hydra.util.PersistentMap")
                @@ JavaDsl.identifier (string "ofEntries")
                @@ var "pairExprs"))),

      -- TermMaybe: Maybe.nothing() or Maybe.just(x)
      _Term_maybe>>: lambda "mt" $
        Maybes.cases (var "mt")
          ("targs" <<~ (takeTypeArgs @@ string "maybe" @@ int32 1 @@ var "tyapps" @@ var "cx" @@ var "g") $
            right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocationStaticWithTypeArgs
                @@ JavaDsl.identifier (string "hydra.util.Maybe")
                @@ JavaDsl.identifier (string "nothing")
                @@ var "targs" @@ list ([] :: [TTerm Java.Expression]))))
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
          (Schemas.requireType @@ var "cx" @@ var "g" @@ var "recName") $
        "strippedRecTyp" <~ Maybes.map (lambda "recTyp" $ stripForalls @@ (Rewriting.deannotateType @@ var "recTyp"))
          (var "mRecordType") $
        "mFieldTypeMap" <~ (Maybes.bind (var "strippedRecTyp") (lambda "bodyTyp" $
          cases _Type (var "bodyTyp")
            (Just nothing) [
            _Type_record>>: lambda "rt" $
              just (Maps.fromList (Lists.map
                (lambda "ft" $ pair (Core.fieldTypeName (var "ft")) (Core.fieldTypeType (var "ft")))
                (Core.rowTypeFields (var "rt"))))])) $
        -- Build type variable substitution from the annotation type's type arguments
        "combinedAnnsRec" <~ Lists.foldl (lambda "acc" $ lambda "m" $ Maps.union (var "acc") (var "m")) Maps.empty (var "anns") $
        "mAnnotType" <<~ (getTypeE (var "cx") (var "g") (var "combinedAnnsRec")) $
        "mTypeSubst" <~ (Maybes.bind (var "mAnnotType") (lambda "annTyp" $
          Maybes.bind (var "mRecordType") (lambda "recTyp" $
            -- Extract type args from annotation and type params from definition
            "args" <~ (extractTypeApplicationArgs @@ (Rewriting.deannotateType @@ var "annTyp")) $
            "params" <~ (collectForallParams @@ (Rewriting.deannotateType @@ var "recTyp")) $
            Logic.ifElse (Logic.or (Lists.null (var "args")) (Logic.not (Equality.equal (Lists.length (var "args")) (Lists.length (var "params")))))
              nothing
              (just (Maps.fromList (Lists.zip (var "params") (var "args"))))))) $
        "encodeField" <~ (lambda "fld" $
          Maybes.cases (var "mFieldTypeMap")
            (var "encode" @@ Core.fieldTerm (var "fld"))
            (lambda "ftmap" $
              "mftyp" <~ Maps.lookup (Core.fieldName (var "fld")) (var "ftmap") $
              Maybes.cases (var "mftyp")
                (var "encode" @@ Core.fieldTerm (var "fld"))
                (lambda "ftyp" $
                  -- Apply type substitution to the field type if available
                  "resolvedType" <~ Maybes.cases (var "mTypeSubst")
                    (var "ftyp")
                    (lambda "subst" $ applySubstFull @@ var "subst" @@ var "ftyp") $
                  -- Annotate the field term with the resolved type before encoding
                  "annotatedFieldTerm" <~ (Annotations.setTermAnnotation @@ asTerm Constants.key_type
                    @@ just (encodeTypeAsTerm @@ var "resolvedType") @@ Core.fieldTerm (var "fld")) $
                  encodeTermInternal @@ var "env" @@ var "anns" @@ list ([] :: [TTerm Java.Type]) @@ var "annotatedFieldTerm" @@ var "cx" @@ var "g"))) $
        "fieldExprs" <<~ (Eithers.mapList (var "encodeField") (Core.recordFields (var "rec"))) $
        "consId" <~ (JavaUtilsSource.nameToJavaName @@ var "aliases" @@ var "recName") $
        "mtargs" <<~ (Logic.ifElse (Logic.not (Lists.null (var "tyapps")))
          ("rts" <<~ (Eithers.mapList (lambda "jt" $ JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt" @@ var "cx") (var "tyapps")) $
            right (just (JavaDsl.typeArgumentsOrDiamondArguments
              (Lists.map (lambda "rt" $ JavaDsl.typeArgumentReference (var "rt")) (var "rts")))))
          -- tyapps is empty: try to extract type args from annotation
          ("combinedAnns" <~ Lists.foldl (lambda "acc" $ lambda "m" $ Maps.union (var "acc") (var "m")) Maps.empty (var "anns") $
           "mtyp" <<~ (getTypeE (var "cx") (var "g") (var "combinedAnns")) $
           Maybes.cases (var "mtyp")
             (right nothing)
             (lambda "annTyp" $
               "typeArgs" <~ (extractTypeApplicationArgs @@ (Rewriting.deannotateType @@ var "annTyp")) $
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

      -- TermSet: PersistentSet.empty() or PersistentSet.of(elements)
      _Term_set>>: lambda "s" $
        Logic.ifElse (Sets.null (var "s"))
          ("targs" <<~ (takeTypeArgs @@ string "set" @@ int32 1 @@ var "tyapps" @@ var "cx" @@ var "g") $
            right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocationStaticWithTypeArgs
                @@ JavaDsl.identifier (string "hydra.util.PersistentSet")
                @@ JavaDsl.identifier (string "empty")
                @@ var "targs" @@ list ([] :: [TTerm Java.Expression]))))
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
          "annotatedBody" <~ (Maybes.cases (var "mtyp")
            (Core.typeLambdaBody (var "tl"))
            (lambda "t" $ cases _Type (var "t") (Just $ Core.typeLambdaBody (var "tl")) [
              _Type_forall>>: lambda "fa" $
                Annotations.setTermAnnotation @@ asTerm Constants.key_type
                  @@ just (encodeTypeAsTerm @@ Core.forallTypeBody (var "fa"))
                  @@ Core.typeLambdaBody (var "tl")])) $
          encodeTerm @@ var "env2" @@ var "annotatedBody" @@ var "cx" @@ var "g"),

      -- TermUnion: new Variant(args)
      _Term_union>>: lambda "inj" $
        "injTypeName" <~ Core.injectionTypeName (var "inj") $
        "injField" <~ Core.injectionField (var "inj") $
        "injFieldName" <~ Core.fieldName (var "injField") $
        "injFieldTerm" <~ Core.fieldTerm (var "injField") $
        "typeId" <~ JavaDsl.unIdentifier (JavaUtilsSource.nameToJavaName @@ var "aliases" @@ var "injTypeName") $
        "consId" <~ JavaDsl.identifier (Strings.cat (list [var "typeId", string ".", JavaUtilsSource.sanitizeJavaName @@ (Formatting.capitalize @@ (unwrap _Name @@ var "injFieldName"))])) $
        "fieldIsUnit" <<~ (isFieldUnitType @@ var "injTypeName" @@ var "injFieldName" @@ var "cx" @@ var "g") $
        "args" <<~ (Logic.ifElse (Logic.or (Schemas.isUnitTerm @@ (Rewriting.deannotateTerm @@ var "injFieldTerm")) (var "fieldIsUnit"))
          (right (list ([] :: [TTerm Java.Expression])))
          ("ex" <<~ (var "encode" @@ var "injFieldTerm") $
            right (list [var "ex"]))) $
        right (JavaUtilsSource.javaConstructorCall
          @@ (JavaUtilsSource.javaConstructorName @@ var "consId" @@ nothing)
          @@ var "args" @@ nothing),

      -- TermVariable: encode variable reference
      _Term_variable>>: lambda "name" $
        encodeVariable @@ var "env" @@ var "name" @@ var "cx" @@ var "g",

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
        "typ" <<~ (Maybes.cases (var "mtyp")
          (CoderUtils.typeOfTerm @@ var "cx" @@ var "g" @@ var "term")
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
              ("eitherBranchTypes" <~ pair (Lists.head (var "allTypeArgs")) (Lists.head (Lists.tail (var "allTypeArgs"))) $
                "jTypeArgs" <<~ (Eithers.mapList (lambda "t" $
                  "jt" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "t" @@ var "cx" @@ var "g") $
                  JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt" @@ var "cx") (var "allTypeArgs")) $
                "eitherTargs" <~ Lists.map (lambda "rt" $ JavaDsl.typeArgumentReference (var "rt")) (var "jTypeArgs") $
                "encodeEitherBranch" <~ (lambda "branchType" $ lambda "t1" $
                  "annotated" <~ (Annotations.setTermAnnotation @@ asTerm Constants.key_type
                    @@ just (encodeTypeAsTerm @@ var "branchType") @@ var "t1") $
                  encodeTermInternal @@ var "env" @@ var "anns" @@ list ([] :: [TTerm Java.Type]) @@ var "annotated" @@ var "cx" @@ var "g") $
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

-- =============================================================================
-- Batch 24: encodeApplication and supporting functions
-- =============================================================================

-- | Annotate lambda arguments with expected types computed from the callee's type scheme
-- and type applications. This corrects type annotations that normalizeTypeVariablesInTerm
-- may have made inconsistent with the outer scope.
annotateLambdaArgs :: TBinding (Name -> [Type] -> [Term] -> Context -> Graph -> Either (InContext Error) [Term])
annotateLambdaArgs = def "annotateLambdaArgs" $
  lambda "cname" $ lambda "tApps" $ lambda "argTerms" $
    "cx" ~> "g" ~>
    Logic.ifElse (Lists.null (var "tApps"))
      (right (var "argTerms"))
      -- Look up the type scheme from either elements or primitives
      ("mts" <<~ (
        "mel" <<~ right (Lexical.dereferenceElement @@ var "g" @@ var "cname") $
        Maybes.cases (var "mel")
          (right (Maybes.map
              (lambda "prim" $ Graph.primitiveType (var "prim"))
              (Maps.lookup (var "cname") (Graph.graphPrimitives (var "g")))))
          (lambda "el" $ right (Core.bindingType (var "el")))) $
      Maybes.cases (var "mts")
        (right (var "argTerms"))
        (lambda "ts" $
          "schemeType" <~ Core.typeSchemeType (var "ts") $
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

-- | Apply a Java argument to a Java expression using .apply() method invocation.
applyJavaArg :: TBinding (Java.Expression -> Java.Expression -> Java.Expression)
applyJavaArg = def "applyJavaArg" $
  lambda "expr" $ lambda "jarg" $
    JavaUtilsSource.javaMethodInvocationToJavaExpression @@
      (JavaUtilsSource.methodInvocation @@ just (right
        (JavaUtilsSource.javaExpressionToJavaPrimary @@ var "expr"))
        @@ JavaDsl.identifier (asTerm JavaNamesSource.applyMethodName)
        @@ list [var "jarg"])

-- | Encode a function application.
encodeApplication :: TBinding (JavaHelpers.JavaEnvironment -> Application -> Context -> Graph -> Either (InContext Error) Java.Expression)
encodeApplication = def "encodeApplication" $
  lambda "env" $ lambda "app" $
    "cx" ~> "g0" ~>
    "aliases" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env") $
    "g" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_graph @@ var "env") $
    -- Gather function, args, and type applications
    "gathered" <~ (CoderUtils.gatherArgsWithTypeApps
      @@ (inject _Term _Term_application (var "app"))
      @@ list ([] :: [TTerm Term])
      @@ list ([] :: [TTerm Type])) $
    "fun" <~ Pairs.first (var "gathered") $
    "args" <~ Pairs.first (Pairs.second (var "gathered")) $
    "typeApps" <~ Pairs.second (Pairs.second (var "gathered")) $
    -- Get the function's arity from its type
    "mfunTyp" <<~ (getTypeE (var "cx") (var "g") (Annotations.termAnnotationInternal @@ var "fun")) $
    "funTyp" <<~ (Maybes.cases (var "mfunTyp")
      (CoderUtils.typeOfTerm @@ var "cx" @@ var "g" @@ var "fun")
      (lambda "t" $ right (var "t"))) $
    "arity" <~ (Arity.typeArity @@ var "funTyp") $
    -- Determine callee name for type annotation correction
    "deannotatedFun" <~ (Rewriting.deannotateTerm @@ var "fun") $
    "calleeName" <~ (cases _Term (var "deannotatedFun")
      (Just nothing) [
      _Term_function>>: lambda "f" $
        cases _Function (var "f")
          (Just nothing) [
          _Function_primitive>>: lambda "n" $ just (var "n")],
      _Term_variable>>: lambda "n" $ just (var "n")]) $
    -- Annotate lambda args if we have a callee name
    "annotatedArgs" <<~ (Maybes.cases (var "calleeName")
      (right (var "args"))
      (lambda "cname" $ annotateLambdaArgs @@ var "cname" @@ var "typeApps" @@ var "args" @@ var "cx" @@ var "g")) $
    -- Dispatch based on the deannotated function form
    cases _Term (var "deannotatedFun")
      (Just $ encodeApplication_fallback @@ var "env" @@ var "aliases" @@ var "g" @@ var "typeApps"
        @@ (Core.applicationFunction (var "app")) @@ (Core.applicationArgument (var "app")) @@ var "cx" @@ var "g") [
      _Term_function>>: lambda "f" $
        cases _Function (var "f")
          (Just $ encodeApplication_fallback @@ var "env" @@ var "aliases" @@ var "g" @@ var "typeApps"
            @@ (Core.applicationFunction (var "app")) @@ (Core.applicationArgument (var "app")) @@ var "cx" @@ var "g") [
          _Function_primitive>>: lambda "name" $
            "hargs" <~ Lists.take (var "arity") (var "annotatedArgs") $
            "rargs" <~ Lists.drop (var "arity") (var "annotatedArgs") $
            "initialCall" <<~ (functionCall @@ var "env" @@ true @@ var "name" @@ var "hargs" @@ (list ([] :: [TTerm Type])) @@ var "cx" @@ var "g") $
            Eithers.foldl (lambda "acc" $ lambda "h" $
              "jarg" <<~ (encodeTerm @@ var "env" @@ var "h" @@ var "cx" @@ var "g") $
              right (applyJavaArg @@ var "acc" @@ var "jarg"))
              (var "initialCall") (var "rargs")],
      _Term_variable>>: lambda "name" $
        -- Check if this is a recursive let-bound variable (not shadowed by lambda parameter)
        Logic.ifElse (Logic.and (isRecursiveVariable @@ var "aliases" @@ var "name")
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
              (list ([] :: [TTerm Type]))
              ("allVars" <~ Sets.unions (Lists.map (lambda "t" $ collectTypeVars @@ var "t") (var "typeApps")) $
                Logic.ifElse (Logic.not (Sets.null (Sets.difference (var "allVars") (var "inScope"))))
                  (list ([] :: [TTerm Type]))
                  (Logic.ifElse (Sets.null (Sets.difference (var "allVars") (var "trusted")))
                    (var "typeApps")
                    (list ([] :: [TTerm Type]))))) $
            -- Correct the type application ordering
            "safeTypeApps" <<~ (Logic.ifElse (Lists.null (var "filteredTypeApps"))
              (right (list ([] :: [TTerm Type])))
              (correctTypeApps @@ var "g" @@ var "name" @@ var "hargs" @@ var "filteredTypeApps" @@ var "cx" @@ var "g")) $
            -- Filter phantom type args
            "finalTypeApps" <<~ (filterPhantomTypeArgs @@ var "name" @@ var "safeTypeApps" @@ var "cx" @@ var "g") $
            "initialCall" <<~ (functionCall @@ var "env" @@ false @@ var "name" @@ var "hargs" @@ var "finalTypeApps" @@ var "cx" @@ var "g") $
            Eithers.foldl (lambda "acc" $ lambda "h" $
              "jarg" <<~ (encodeTerm @@ var "env" @@ var "h" @@ var "cx" @@ var "g") $
              right (applyJavaArg @@ var "acc" @@ var "jarg"))
              (var "initialCall") (var "rargs"))]

-- | Fallback path for encodeApplication — used for eliminations and default expressions.
encodeApplication_fallback :: TBinding (JavaHelpers.JavaEnvironment -> JavaHelpers.Aliases -> Graph -> [Type] -> Term -> Term -> Context -> Graph -> Either (InContext Error) Java.Expression)
encodeApplication_fallback = def "encodeApplication_fallback" $
  lambda "env" $ lambda "aliases" $ lambda "gr" $ lambda "typeApps" $ lambda "lhs" $ lambda "rhs" $
    "cx" ~> "g" ~>
    ("mt" <<~ (getTypeE (var "cx") (var "g") (Annotations.termAnnotationInternal @@ var "lhs")) $
    "t" <<~ (Maybes.cases (var "mt")
      (CoderUtils.typeOfTerm @@ var "cx" @@ var "g" @@ var "lhs")
      (lambda "typ" $ right (var "typ"))) $
    cases _Type (Rewriting.deannotateTypeParameters @@ (Rewriting.deannotateType @@ var "t"))
      (Just $
        -- Non-function type: encode as generic .apply() call
        "jfun" <<~ (encodeTerm @@ var "env" @@ var "lhs" @@ var "cx" @@ var "g") $
        "jarg" <<~ (encodeTerm @@ var "env" @@ var "rhs" @@ var "cx" @@ var "g") $
        right (applyJavaArg @@ var "jfun" @@ var "jarg")) [
      _Type_function>>: lambda "ft" $
        "dom" <~ Core.functionTypeDomain (var "ft") $
        "cod" <~ Core.functionTypeCodomain (var "ft") $
        cases _Term (Rewriting.deannotateTerm @@ var "lhs")
          (Just $
            -- defaultExpression: apply using .apply()
            "jfun" <<~ (encodeTerm @@ var "env" @@ var "lhs" @@ var "cx" @@ var "g") $
            "jarg" <<~ (encodeTerm @@ var "env" @@ var "rhs" @@ var "cx" @@ var "g") $
            right (applyJavaArg @@ var "jfun" @@ var "jarg")) [
          _Term_function>>: lambda "f" $
            cases _Function (var "f")
              (Just $
                -- defaultExpression
                "jfun" <<~ (encodeTerm @@ var "env" @@ var "lhs" @@ var "cx" @@ var "g") $
                "jarg" <<~ (encodeTerm @@ var "env" @@ var "rhs" @@ var "cx" @@ var "g") $
                right (applyJavaArg @@ var "jfun" @@ var "jarg")) [
              _Function_elimination>>: lambda "e" $
                "jarg" <<~ (encodeTerm @@ var "env" @@ var "rhs" @@ var "cx" @@ var "g") $
                -- If dom has no type args, try to get a richer type from the argument
                "enrichedDom" <<~ (Logic.ifElse
                  (Logic.not (Lists.null (javaTypeArgumentsForType @@ var "dom")))
                  (right (var "dom"))
                  ("mrt" <<~ (getTypeE (var "cx") (var "g") (Annotations.termAnnotationInternal @@ var "rhs")) $
                    Maybes.cases (var "mrt")
                      ("rt" <<~ (CoderUtils.typeOfTerm @@ var "cx" @@ var "g" @@ var "rhs") $
                        right (Logic.ifElse (Logic.not (Lists.null (javaTypeArgumentsForType @@ var "rt")))
                          (var "rt")
                          (var "dom")))
                      (lambda "rt" $
                        right (Logic.ifElse (Logic.not (Lists.null (javaTypeArgumentsForType @@ var "rt")))
                          (var "rt")
                          (var "dom"))))) $
                encodeElimination @@ var "env" @@ just (var "jarg") @@ var "enrichedDom" @@ var "cod" @@ var "e" @@ var "cx" @@ var "g"]]])

-- =============================================================================
-- Stubs for mutual recursion (to be fully promoted in batch 25+)
-- =============================================================================

-- | Try to extract the argument type from a function application.
-- For a function like a constructor :: a -> Wrapper a with return type Wrapper a,
-- the argument type is a (the type parameter of the wrapper).
extractArgType :: TBinding (Type -> Type -> Type)
extractArgType = def "extractArgType" $
  lambda "_lhs" $ lambda "typ" $
    cases _Type (var "typ")
      (Just $ var "typ") [
      _Type_application>>: lambda "at1" $
        cases _Type (Core.applicationTypeFunction (var "at1"))
          (Just $ var "typ") [
          _Type_application>>: lambda "_at2" $
            Core.applicationTypeArgument (var "at1")]]

-- | Annotate a term body with the expected codomain type, propagating through
-- applications so that inner type-applied subterms also get correct annotations.
annotateBodyWithCod :: TBinding (Type -> Term -> Term)
annotateBodyWithCod = def "annotateBodyWithCod" $
  lambda "typ" $ lambda "term" $
    "setAnn" <~ (lambda "t" $
      Annotations.setTermAnnotation @@ asTerm Constants.key_type
        @@ just (encodeTypeAsTerm @@ var "typ")
        @@ var "t") $
    cases _Term (Rewriting.deannotateTerm @@ var "term")
      (Just $ var "setAnn" @@ var "term") [
      -- For type applications, annotate the whole thing with the expected type
      _Term_typeApplication>>: lambda "_ta" $
        var "setAnn" @@ var "term",
      -- For applications, annotate the application with the overall type,
      -- and also annotate arguments that have type applications
      _Term_application>>: lambda "app" $
        "lhs" <~ Core.applicationFunction (var "app") $
        "rhs" <~ Core.applicationArgument (var "app") $
        "annotatedRhs" <~ cases _Term (Rewriting.deannotateTerm @@ var "rhs")
          (Just $ var "rhs") [
          _Term_typeApplication>>: lambda "_ta2" $
            annotateBodyWithCod @@ (extractArgType @@ var "lhs" @@ var "typ") @@ var "rhs"] $
        var "setAnn" @@ (inject _Term _Term_application (record _Application [
          _Application_function>>: var "lhs",
          _Application_argument>>: var "annotatedRhs"]))]

-- | Extract Java type arguments from a domain type.
-- Uses actual type application args when available, falling back to javaTypeArgumentsForType.
domTypeArgs :: TBinding (JavaHelpers.Aliases -> Type -> Context -> Graph -> Either (InContext Error) [Java.TypeArgument])
domTypeArgs = def "domTypeArgs" $
  lambda "aliases" $ lambda "d" $
    "cx" ~> "g" ~>
    "args" <~ (extractTypeApplicationArgs @@ (Rewriting.deannotateType @@ var "d")) $
    Logic.ifElse (Logic.not (Lists.null (var "args")))
      (Eithers.mapList (lambda "t" $
        "jt" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "t" @@ var "cx" @@ var "g") $
        "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt" @@ var "cx") $
        right (JavaDsl.typeArgumentReference (var "rt")))
        (var "args"))
      (right (javaTypeArgumentsForType @@ var "d"))

-- | Generate the otherwise (default) branch of a visitor.
otherwiseBranch :: TBinding (JavaHelpers.JavaEnvironment -> JavaHelpers.Aliases -> Type -> Type -> Name -> Java.Type -> [Java.TypeArgument] -> Term -> Context -> Graph -> Either (InContext Error) Java.ClassBodyDeclarationWithComments)
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
    right (noComment @@ (JavaUtilsSource.methodDeclaration @@ var "mods" @@ list ([] :: [TTerm Java.TypeParameter]) @@ var "anns"
      @@ asTerm JavaNamesSource.otherwiseMethodName @@ list [var "param"] @@ var "result" @@ just (var "allStmts")))

-- | Generate a visit branch for a field of a union type.
visitBranch :: TBinding (JavaHelpers.JavaEnvironment -> JavaHelpers.Aliases -> Type -> Name -> Java.Type -> [Java.TypeArgument] -> Field -> Context -> Graph -> Either (InContext Error) Java.ClassBodyDeclarationWithComments)
visitBranch = def "visitBranch" $
  lambda "env" $ lambda "aliases" $ lambda "dom" $ lambda "tname" $ lambda "jcod" $ lambda "targs" $ lambda "field" $
    "cx" ~> "g" ~>
    -- Compute the domain type for this specific branch
    "jdom" <~ (JavaDsl.typeReference (JavaUtilsSource.nameToJavaReferenceType @@ var "aliases" @@ true @@ var "targs"
      @@ var "tname" @@ just (Formatting.capitalize @@ (Core.unName (Core.fieldName (var "field")))))) $
    "mods" <~ list [inject Java._MethodModifier Java._MethodModifier_public unit] $
    "anns" <~ list [asTerm JavaUtilsSource.overrideAnnotation] $
    "result" <~ (JavaDsl.resultType (JavaDsl.unannType (var "jcod"))) $
    -- Field terms are lambdas; apply to special var that encodes to instance.value
    cases _Term (Rewriting.deannotateTerm @@ Core.fieldTerm (var "field"))
      (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError $ Strings.cat2 (string "visitBranch: field term is not a lambda: ") (ShowCore.term @@ Core.fieldTerm (var "field"))) (var "cx")) [
      _Term_function>>: lambda "f" $
        cases _Function (var "f")
          (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError $ Strings.cat2 (string "visitBranch: field term is not a lambda: ") (ShowCore.term @@ Core.fieldTerm (var "field"))) (var "cx")) [
          _Function_lambda>>: lambda "lam" $
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
              right (noComment @@ (JavaUtilsSource.methodDeclaration @@ var "mods" @@ list ([] :: [TTerm Java.TypeParameter]) @@ var "anns"
                @@ asTerm JavaNamesSource.visitMethodName @@ list [var "param"] @@ var "result" @@ just (var "allStmts"))))]]

-- | Encode an elimination expression.
encodeElimination :: TBinding (JavaHelpers.JavaEnvironment -> Maybe Java.Expression -> Type -> Type -> Elimination -> Context -> Graph -> Either (InContext Error) Java.Expression)
encodeElimination = def "encodeElimination" $
  lambda "env" $ lambda "marg" $ lambda "dom" $ lambda "cod" $ lambda "elm" $
    "cx" ~> "g" ~>
    "aliases" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env") $
    cases _Elimination (var "elm")
      (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError $ Strings.cat2 (string "unexpected ") (Strings.cat2 (string "elimination case") (Strings.cat2 (string " in ") (string "encodeElimination")))) (var "cx")) [

      -- EliminationRecord: field projection
      _Elimination_record>>: lambda "proj" $
        "fname" <~ (Core.projectionField (var "proj")) $
        "jdom0" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "dom" @@ var "cx" @@ var "g") $
        "jdomr" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jdom0" @@ var "cx") $
        Maybes.cases (var "marg")
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

      -- EliminationUnion: case statement
      _Elimination_union>>: lambda "cs" $
        "tname" <~ (project _CaseStatement _CaseStatement_typeName @@ var "cs") $
        "def_" <~ (project _CaseStatement _CaseStatement_default @@ var "cs") $
        "fields" <~ (project _CaseStatement _CaseStatement_cases @@ var "cs") $
        Maybes.cases (var "marg")
          -- No arg: wrap elimination in a lambda
          ("uVar" <~ wrap _Name (string "u") $
            "typedLambda" <~ (inject _Term _Term_function (inject _Function _Function_lambda (record _Lambda [
              _Lambda_parameter>>: var "uVar",
              _Lambda_domain>>: just (var "dom"),
              _Lambda_body>>: inject _Term _Term_application (record _Application [
                _Application_function>>: inject _Term _Term_function (inject _Function _Function_elimination (var "elm")),
                _Application_argument>>: inject _Term _Term_variable (var "uVar")])]))) $
            encodeTerm @@ var "env" @@ var "typedLambda" @@ var "cx" @@ var "g")
          -- With arg: apply elimination to visitor
          (lambda "jarg" $
            "prim" <~ (JavaUtilsSource.javaExpressionToJavaPrimary @@ var "jarg") $
            "consId" <~ (innerClassRef @@ var "aliases" @@ var "tname" @@ asTerm JavaNamesSource.partialVisitorName) $
            "jcod" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "cod" @@ var "cx" @@ var "g") $
            "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jcod" @@ var "cx") $
            "domArgs" <<~ (domTypeArgs @@ var "aliases" @@ var "dom" @@ var "cx" @@ var "g") $
            "targs" <~ (typeArgsOrDiamond @@ (Lists.concat2 (var "domArgs") (list [JavaDsl.typeArgumentReference (var "rt")]))) $
            "otherwiseBranches" <<~ (Maybes.cases (var "def_")
              (right (list ([] :: [TTerm Java.ClassBodyDeclarationWithComments])))
              (lambda "d" $
                "b" <<~ (otherwiseBranch @@ var "env" @@ var "aliases" @@ var "dom" @@ var "cod" @@ var "tname" @@ var "jcod" @@ var "domArgs" @@ var "d" @@ var "cx" @@ var "g") $
                right (list [var "b"]))) $
            "visitBranches" <<~ (Eithers.mapList (lambda "f" $ visitBranch @@ var "env" @@ var "aliases" @@ var "dom" @@ var "tname" @@ var "jcod" @@ var "domArgs" @@ var "f" @@ var "cx" @@ var "g") (var "fields")) $
            "body" <~ wrap Java._ClassBody (Lists.concat2 (var "otherwiseBranches") (var "visitBranches")) $
            "visitor" <~ (JavaUtilsSource.javaConstructorCall @@ (JavaUtilsSource.javaConstructorName @@ var "consId" @@ just (var "targs")) @@ list ([] :: [TTerm Java.Expression]) @@ just (var "body")) $
            right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
              (JavaUtilsSource.methodInvocation @@ just (right (var "prim"))
                @@ JavaDsl.identifier (asTerm JavaNamesSource.acceptMethodName) @@ list [var "visitor"]))),

      -- EliminationWrap: unwrap a newtype
      _Elimination_wrap>>: lambda "wrapName" $
        "withArg" <~ (lambda "ja" $
          JavaUtilsSource.javaFieldAccessToJavaExpression @@ (record Java._FieldAccess [
            Java._FieldAccess_qualifier>>: inject Java._FieldAccess_Qualifier Java._FieldAccess_Qualifier_primary
              (JavaUtilsSource.javaExpressionToJavaPrimary @@ var "ja"),
            Java._FieldAccess_identifier>>: JavaUtilsSource.javaIdentifier @@ asTerm JavaNamesSource.valueFieldName])) $
        right (Maybes.cases (var "marg")
          -- No arg: generate lambda for unwrapping
          ("wVar" <~ wrap _Name (string "wrapped") $
            "wArg" <~ (JavaUtilsSource.javaIdentifierToJavaExpression @@ (JavaUtilsSource.variableToJavaIdentifier @@ var "wVar")) $
            JavaUtilsSource.javaLambda @@ var "wVar" @@ (var "withArg" @@ var "wArg"))
          -- With arg: field access
          (lambda "jarg" $ var "withArg" @@ var "jarg"))]

-- | Build a curried lambda chain from a list of parameter names wrapping an inner expression.
-- E.g., buildCurriedLambda [p0, p1] inner = javaLambda p0 (javaLambda p1 inner)
buildCurriedLambda :: TBinding ([Name] -> Java.Expression -> Java.Expression)
buildCurriedLambda = def "buildCurriedLambda" $
  lambda "params" $ lambda "inner" $
    Lists.foldl
      (lambda "acc" $ lambda "p" $ JavaUtilsSource.javaLambda @@ var "p" @@ var "acc")
      (var "inner")
      (Lists.reverse (var "params"))

-- | Encode a function.
encodeFunction :: TBinding (JavaHelpers.JavaEnvironment -> Type -> Type -> Function -> Context -> Graph -> Either (InContext Error) Java.Expression)
encodeFunction = def "encodeFunction" $
  lambda "env" $ lambda "dom" $ lambda "cod" $ lambda "fun" $
    "cx" ~> "g" ~>
    "aliases" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env") $
    cases _Function (var "fun")
      (Just $ right (encodeLiteral @@ (inject _Literal _Literal_string
        (Strings.cat2 (string "Unimplemented function variant: ") (ShowCore.function @@ var "fun"))))) [

      -- FunctionElimination: delegate to encodeElimination
      _Function_elimination>>: lambda "elm" $
        (encodeElimination @@ var "env" @@ nothing @@ var "dom" @@ var "cod" @@ var "elm" @@ var "cx" @@ var "g"),

      -- FunctionLambda: encode as Java lambda
      _Function_lambda>>: lambda "lam" $
        (withLambda @@ var "env" @@ var "lam" @@ (lambda "env2" $
          "lambdaVar" <~ Core.lambdaParameter (var "lam") $
          "body" <~ Core.lambdaBody (var "lam") $
          cases _Term (Rewriting.deannotateTerm @@ var "body")
            (Just $
              -- Body is not a lambda: analyze and encode normally
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
                _FunctionType_codomain>>: var "cod"])) @@ var "lam1" @@ var "cx" @@ var "g") [

            -- Body is another lambda: recursively encode it
            _Term_function>>: lambda "f2" $
              cases _Function (var "f2")
                (Just $
                  -- Not a lambda — fall through to the default case above
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
                    _FunctionType_codomain>>: var "cod"])) @@ var "lam1" @@ var "cx" @@ var "g") [

                _Function_lambda>>: lambda "innerLam" $
                  cases _Type (Rewriting.deannotateType @@ var "cod")
                    (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError $ Strings.cat2 (string "expected function type for lambda body, but got: ")
                      (ShowCore.type_ @@ var "cod")) (var "cx")) [
                    _Type_function>>: lambda "ft" $
                      "dom2" <~ Core.functionTypeDomain (var "ft") $
                      "cod2" <~ Core.functionTypeCodomain (var "ft") $
                      "innerJavaLambda" <<~ (encodeFunction @@ var "env2" @@ var "dom2" @@ var "cod2"
                        @@ (inject _Function _Function_lambda (var "innerLam")) @@ var "cx" @@ var "g") $
                      "lam1" <~ (JavaUtilsSource.javaLambda @@ var "lambdaVar" @@ var "innerJavaLambda") $
                      applyCastIfSafe @@ var "aliases" @@ (inject _Type _Type_function (record _FunctionType [
                        _FunctionType_domain>>: var "dom",
                        _FunctionType_codomain>>: var "cod"])) @@ var "lam1" @@ var "cx" @@ var "g"]]])),

      -- FunctionPrimitive: method reference or curried wrapper
      _Function_primitive>>: lambda "name" $
        "classWithApply" <~ (JavaDsl.unIdentifier (elementJavaIdentifier @@ true @@ false @@ var "aliases" @@ var "name")) $
        "suffix" <~ Strings.cat2 (string ".") (asTerm JavaNamesSource.applyMethodName) $
        "className" <~ Strings.fromList (Lists.take
          (Math.sub (Strings.length (var "classWithApply")) (Strings.length (var "suffix")))
          (Strings.toList (var "classWithApply"))) $
        "arity" <~ (Arity.typeArity @@ (inject _Type _Type_function (record _FunctionType [
          _FunctionType_domain>>: var "dom",
          _FunctionType_codomain>>: var "cod"]))) $
        Logic.ifElse (Equality.lte (var "arity") (int32 1))
          -- Single-arg: method reference
          (right (JavaUtilsSource.javaIdentifierToJavaExpression @@
            (JavaDsl.identifier (Strings.cat (list [var "className", string "::", asTerm JavaNamesSource.applyMethodName])))))
          -- Multi-arg: curried lambda wrapper
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
              (JavaUtilsSource.javaCastExpression @@ var "rt" @@ (JavaUtilsSource.javaExpressionToJavaUnaryExpression @@ var "curried"))))]

-- | Generate a method invocation for a function call.
functionCall :: TBinding (JavaHelpers.JavaEnvironment -> Bool -> Name -> [Term] -> [Type] -> Context -> Graph -> Either (InContext Error) Java.Expression)
functionCall = def "functionCall" $
  lambda "env" $ lambda "isPrim" $ lambda "name" $ lambda "args" $ lambda "typeApps" $
    "cx" ~> "g" ~>
    "aliases" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env") $
    "isLambdaBound" <~ (isLambdaBoundIn @@ var "name"
      @@ (project JavaHelpers._Aliases JavaHelpers._Aliases_lambdaVars @@ var "aliases")) $
    -- Encode arguments and generate method invocation
    ("jargs0" <<~ (Eithers.mapList (lambda "arg" $ encodeTerm @@ var "env" @@ var "arg" @@ var "cx" @@ var "g") (var "args")) $
        "wrapResult" <~ (wrapLazyArguments @@ var "name" @@ var "jargs0") $
        "jargs" <~ Pairs.first (var "wrapResult") $
        "mMethodOverride" <~ Pairs.second (var "wrapResult") $
        Logic.ifElse (Logic.or (isLocalVariable @@ var "name") (var "isLambdaBound"))
          -- Local/lambda-bound: apply arguments one at a time via .apply()
          ("baseExpr" <<~ (encodeVariable @@ var "env" @@ var "name" @@ var "cx" @@ var "g") $
            right (Lists.foldl (lambda "acc" $ lambda "jarg" $ applyJavaArg @@ var "acc" @@ var "jarg")
              (var "baseExpr") (var "jargs")))
          -- Module-level functions: call with all args directly
          ("overrideMethodName" <~ (lambda "jid" $
              Maybes.cases (var "mMethodOverride")
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
                "mns" <~ (Module.qualifiedNameNamespace (var "qn")) $
                "localName" <~ (Module.qualifiedNameLocal (var "qn")) $
                Maybes.cases (var "mns")
                  -- No namespace: simple header
                  ("header" <~ JavaDsl.methodInvocationHeaderSimple
                    (wrap Java._MethodName (var "overrideMethodName" @@ (elementJavaIdentifier @@ var "isPrim" @@ false @@ var "aliases" @@ var "name"))) $
                    right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
                      (JavaDsl.methodInvocation_ (var "header") (var "jargs"))))
                  (lambda "ns_" $
                    "classId" <~ (JavaUtilsSource.nameToJavaName @@ var "aliases" @@ (Names.unqualifyName @@ (Module.qualifiedName (just (var "ns_")) (elementsClassName @@ var "ns_")))) $
                    "methodId" <~ (Logic.ifElse (var "isPrim")
                      (var "overrideMethodName" @@ (JavaDsl.identifier (Strings.cat2
                        (JavaDsl.unIdentifier (JavaUtilsSource.nameToJavaName @@ var "aliases" @@ (Names.unqualifyName @@ (Module.qualifiedName (just (var "ns_")) (Formatting.capitalize @@ var "localName")))))
                        (Strings.cat2 (string ".") (asTerm JavaNamesSource.applyMethodName)))))
                      (JavaDsl.identifier (JavaUtilsSource.sanitizeJavaName @@ var "localName"))) $
                    "jTypeArgs" <<~ (Eithers.mapList (lambda "t" $
                      "jt" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "t" @@ var "cx" @@ var "g") $
                      "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt" @@ var "cx") $
                      right (JavaDsl.typeArgumentReference (var "rt")))
                      (var "typeApps")) $
                    right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
                      (JavaUtilsSource.methodInvocationStaticWithTypeArgs @@ var "classId" @@ var "methodId" @@ var "jTypeArgs" @@ var "jargs"))))))

-- | Initialize a recursive binding with AtomicReference (for toDeclInit).
toDeclInit :: TBinding (JavaHelpers.Aliases -> Graph -> S.Set Name -> [Binding] -> Name -> Context -> Graph -> Either (InContext Error) (Maybe Java.BlockStatement))
toDeclInit = def "toDeclInit" $
  lambda "aliasesExt" $ lambda "gExt" $ lambda "recursiveVars" $ lambda "flatBindings" $ lambda "name" $
    "cx" ~> "g" ~>
    Logic.ifElse (Sets.member (var "name") (var "recursiveVars"))
      ("binding" <~ Lists.head (Lists.filter (lambda "b" $ Equality.equal (Core.bindingName (var "b")) (var "name")) (var "flatBindings")) $
        "value" <~ Core.bindingTerm (var "binding") $
        "typ" <<~ Maybes.cases (Core.bindingType (var "binding"))
          (CoderUtils.typeOfTerm @@ var "cx" @@ var "gExt" @@ var "value")
          (lambda "ts" $ right (Core.typeSchemeType (var "ts"))) $
        "jtype" <<~ (encodeType @@ var "aliasesExt" @@ Sets.empty @@ var "typ" @@ var "cx" @@ var "g") $
        "id" <~ (JavaUtilsSource.variableToJavaIdentifier @@ var "name") $
        "arid" <~ (JavaDsl.identifier (string "java.util.concurrent.atomic.AtomicReference")) $
        "aid" <~ (JavaDsl.annotatedIdentifier (list ([] :: [TTerm Java.Annotation])) (var "arid")) $
        "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jtype" @@ var "cx") $
        "targs" <~ (typeArgsOrDiamond @@ list [JavaDsl.typeArgumentReference (var "rt")]) $
        "ci" <~ record Java._ClassOrInterfaceTypeToInstantiate [
          Java._ClassOrInterfaceTypeToInstantiate_identifiers>>: list [var "aid"],
          Java._ClassOrInterfaceTypeToInstantiate_typeArguments>>: just (var "targs")] $
        "body" <~ (JavaUtilsSource.javaConstructorCall @@ var "ci" @@ list ([] :: [TTerm Java.Expression]) @@ nothing) $
        "pkg" <~ (JavaNamesSource.javaPackageName @@ list [string "java", string "util", string "concurrent", string "atomic"]) $
        "artype" <~ (JavaUtilsSource.javaRefType @@ list [var "rt"] @@ just (var "pkg") @@ string "AtomicReference") $
        right (just (JavaUtilsSource.variableDeclarationStatement @@ var "aliasesExt" @@ var "artype" @@ var "id" @@ var "body")))
      (right nothing)

-- | Declare or set a binding value (for toDeclStatement).
toDeclStatement :: TBinding (JavaHelpers.JavaEnvironment -> JavaHelpers.Aliases -> Graph -> S.Set Name -> S.Set Name -> [Binding] -> Name -> Context -> Graph -> Either (InContext Error) Java.BlockStatement)
toDeclStatement = def "toDeclStatement" $
  lambda "envExt" $ lambda "aliasesExt" $ lambda "gExt" $ lambda "recursiveVars" $ lambda "thunkedVars" $ lambda "flatBindings" $ lambda "name" $
    "cx" ~> "g" ~>
    "binding" <~ Lists.head (Lists.filter (lambda "b" $ Equality.equal (Core.bindingName (var "b")) (var "name")) (var "flatBindings")) $
    "value" <~ Core.bindingTerm (var "binding") $
    "typ" <<~ Maybes.cases (Core.bindingType (var "binding"))
      (CoderUtils.typeOfTerm @@ var "cx" @@ var "gExt" @@ var "value")
      (lambda "ts" $ right (Core.typeSchemeType (var "ts"))) $
    "jtype" <<~ (encodeType @@ var "aliasesExt" @@ Sets.empty @@ var "typ" @@ var "cx" @@ var "g") $
    "id" <~ (JavaUtilsSource.variableToJavaIdentifier @@ var "name") $
    "annotatedValue" <~ (Annotations.setTermAnnotation @@ asTerm Constants.key_type
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
            (inject Java._LambdaParameters Java._LambdaParameters_tuple (list ([] :: [TTerm Java.FormalParameter])))
            (var "lambdaBody"))) $
          "targs" <~ (typeArgsOrDiamond @@ list [JavaDsl.typeArgumentReference (var "rt")]) $
          "lazyExpr" <~ (JavaUtilsSource.javaConstructorCall
            @@ (JavaUtilsSource.javaConstructorName @@ JavaDsl.identifier (string "hydra.util.Lazy") @@ just (var "targs"))
            @@ list [var "supplierLambda"] @@ nothing) $
          right (JavaUtilsSource.variableDeclarationStatement @@ var "aliasesExt" @@ var "lazyType" @@ var "id" @@ var "lazyExpr"))
        -- Normal: simple variable declaration
        (right (JavaUtilsSource.variableDeclarationStatement @@ var "aliasesExt" @@ var "jtype" @@ var "id" @@ var "rhs")))

-- | Convert let-bindings to Java block statements.
bindingsToStatements :: TBinding (JavaHelpers.JavaEnvironment -> [Binding] -> Context -> Graph -> Either (InContext Error) ([Java.BlockStatement], JavaHelpers.JavaEnvironment))
bindingsToStatements = def "bindingsToStatements" $
  lambda "env" $ lambda "bindings" $
    "cx" ~> "g0" ~>
    "aliases" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env") $
    "g" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_graph @@ var "env") $
    -- Flatten nested lets then deduplicate names
    "flatBindings" <~ (dedupBindings @@ (project JavaHelpers._Aliases JavaHelpers._Aliases_inScopeJavaVars @@ var "aliases")
      @@ (flattenBindings @@ var "bindings")) $
    -- Extend Graph with flattened bindings
    "gExtended" <~ (Schemas.extendGraphForLet @@ CoderUtils.bindingMetadata @@ var "g"
      @@ record _Let [
        _Let_bindings>>: var "flatBindings",
        _Let_body>>: inject _Term _Term_variable (wrap _Name (string "dummy"))]) $
    -- Compute binding vars
    "bindingVars" <~ Sets.fromList (Lists.map (lambda "b" $ Core.bindingName (var "b")) (var "flatBindings")) $
    -- Build dependency graph
    "allDeps" <~ Maps.fromList (Lists.map
      (lambda "b" $
        "key" <~ Core.bindingName (var "b") $
        "deps" <~ Sets.intersection (var "bindingVars") (Rewriting.freeVariablesInTerm @@ Core.bindingTerm (var "b")) $
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
          ("singleName" <~ Lists.head (var "names") $
            Maybes.cases (Maps.lookup (var "singleName") (var "allDeps"))
              (list ([] :: [TTerm Name]))
              (lambda "deps" $
                Logic.ifElse (Sets.member (var "singleName") (var "deps"))
                  (list [var "singleName"])
                  (list ([] :: [TTerm Name]))))
          (var "names"))
      (var "sorted"))) $
    -- Identify thunked vars
    "thunkedVars" <~ Sets.fromList (Lists.concat (Lists.map
      (lambda "b" $
        "bname" <~ Core.bindingName (var "b") $
        Logic.ifElse (Logic.and
          (Logic.not (Sets.member (var "bname") (var "recursiveVars")))
          (Logic.and
            (needsThunking @@ Core.bindingTerm (var "b"))
            (Logic.not (bindingIsFunctionType @@ var "b"))))
          (list [var "bname"])
          (list ([] :: [TTerm Name])))
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
      (right (pair (list ([] :: [TTerm Java.BlockStatement])) (var "envExtended")))
      ("groups" <<~ (Eithers.mapList
        (lambda "names" $
          -- For each group: generate init statements (for recursive vars) and decl statements
          "inits" <<~ (Eithers.mapList (lambda "n" $ toDeclInit @@ var "aliasesExtended" @@ var "gExtended" @@ var "recursiveVars" @@ var "flatBindings" @@ var "n" @@ var "cx" @@ var "g") (var "names")) $
          "decls" <<~ (Eithers.mapList (lambda "n" $ toDeclStatement @@ var "envExtended" @@ var "aliasesExtended" @@ var "gExtended" @@ var "recursiveVars" @@ var "thunkedVars" @@ var "flatBindings" @@ var "n" @@ var "cx" @@ var "g") (var "names")) $
          right (Lists.concat2 (Maybes.cat (var "inits")) (var "decls")))
        (var "sorted")) $
        right (pair (Lists.concat (var "groups")) (var "envExtended")))

-- =============================================================================
-- Batch 29: Tier 5 entry points
-- =============================================================================

-- | Dispatch type to class declaration.
toClassDecl :: TBinding (Bool -> Bool -> JavaHelpers.Aliases -> [Java.TypeParameter] -> Name -> Type -> Context -> Graph -> Either (InContext Error) Java.ClassDeclaration)
toClassDecl = def "toClassDecl" $
  lambda "isInner" $ lambda "isSer" $ lambda "aliases" $ lambda "tparams" $ lambda "elName" $ lambda "t" $
    "cx" ~> "g" ~>
    "wrap" <~ (lambda "t'" $
      declarationForRecordType @@ var "isInner" @@ var "isSer" @@ var "aliases" @@ var "tparams" @@ var "elName"
        @@ (list [Core.fieldType (wrap _Name (string "value")) (Rewriting.deannotateType @@ var "t'")]) @@ var "cx" @@ var "g") $
    cases _Type (Rewriting.deannotateType @@ var "t")
      (Just $ var "wrap" @@ var "t") [
      _Type_record>>: lambda "rt" $
        declarationForRecordType @@ var "isInner" @@ var "isSer" @@ var "aliases" @@ var "tparams" @@ var "elName"
          @@ (Core.rowTypeFields (var "rt")) @@ var "cx" @@ var "g",
      _Type_union>>: lambda "rt" $
        declarationForUnionType @@ var "isSer" @@ var "aliases" @@ var "tparams" @@ var "elName"
          @@ (Core.rowTypeFields (var "rt")) @@ var "cx" @@ var "g",
      _Type_forall>>: lambda "fa" $
        "v" <~ Core.forallTypeParameter (var "fa") $
        "body" <~ Core.forallTypeBody (var "fa") $
        "param" <~ (JavaUtilsSource.javaTypeParameter @@ (Formatting.capitalize @@ (Core.unName (var "v")))) $
        toClassDecl @@ false @@ var "isSer" @@ var "aliases"
          @@ (Lists.concat2 (var "tparams") (list [var "param"])) @@ var "elName" @@ var "body" @@ var "cx" @@ var "g",
      _Type_wrap>>: lambda "wt" $
        "wtype" <~ (Core.wrappedTypeBody (var "wt")) $
        declarationForRecordType @@ var "isInner" @@ var "isSer" @@ var "aliases" @@ var "tparams" @@ var "elName"
          @@ (list [Core.fieldType (wrap _Name (string "value")) (var "wtype")]) @@ var "cx" @@ var "g"]

-- | Augment a variant class declaration for union types.
-- Adds public static final modifiers, sets parent class extends, and adds accept method.
augmentVariantClass :: TBinding (JavaHelpers.Aliases -> [Java.TypeParameter] -> Name -> Java.ClassDeclaration -> Java.ClassDeclaration)
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
        "acceptDecl" <~ (noComment @@ (JavaUtilsSource.toAcceptMethod @@ false @@ var "tparams")) $
        "newBody" <~ wrap Java._ClassBody (Lists.concat2 (var "oldDecls") (list [var "acceptDecl"])) $
        inject Java._ClassDeclaration Java._ClassDeclaration_normal (record Java._NormalClassDeclaration [
          Java._NormalClassDeclaration_modifiers>>: var "newMods",
          Java._NormalClassDeclaration_identifier>>:
            project Java._NormalClassDeclaration Java._NormalClassDeclaration_identifier @@ var "ncd",
          Java._NormalClassDeclaration_parameters>>: var "tparams",
          Java._NormalClassDeclaration_extends>>: just (var "extendsPart"),
          Java._NormalClassDeclaration_implements>>:
            project Java._NormalClassDeclaration Java._NormalClassDeclaration_implements @@ var "ncd",
          Java._NormalClassDeclaration_body>>: var "newBody"])]

-- | Generate class declaration for a union type.
declarationForUnionType :: TBinding (Bool -> JavaHelpers.Aliases -> [Java.TypeParameter] -> Name -> [FieldType] -> Context -> Graph -> Either (InContext Error) Java.ClassDeclaration)
declarationForUnionType = def "declarationForUnionType" $
  lambda "isSer" $ lambda "aliases" $ lambda "tparams" $ lambda "elName" $ lambda "fields" $
    "cx" ~> "g" ~>
    -- Generate variant subclasses
    "variantClasses" <<~ (Eithers.mapList (lambda "ft" $
      "fname" <~ (project _FieldType _FieldType_name @@ var "ft") $
      "ftype" <~ (project _FieldType _FieldType_type @@ var "ft") $
      "rfields" <~ Logic.ifElse (Schemas.isUnitType @@ (Rewriting.deannotateType @@ var "ftype"))
        (list ([] :: [TTerm FieldType]))
        (list [Core.fieldType (wrap _Name (string "value")) (Rewriting.deannotateType @@ var "ftype")]) $
      "varName" <~ (JavaUtilsSource.variantClassName @@ false @@ var "elName" @@ var "fname") $
      "innerDecl" <<~ (declarationForRecordType' @@ true @@ var "isSer" @@ var "aliases" @@ (list ([] :: [TTerm Java.TypeParameter]))
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
      @@ list ([] :: [TTerm Java.FormalParameter]) @@ list ([] :: [TTerm Java.BlockStatement])) $
    "acceptDecl" <~ (JavaUtilsSource.toAcceptMethod @@ true @@ var "tparams") $
    -- Build visitor and partial visitor interfaces
    "vtparams" <~ Lists.concat2 (var "tparams") (list [JavaUtilsSource.javaTypeParameter @@ asTerm JavaNamesSource.visitorReturnParameter]) $
    "visitorMethods" <~ Lists.map
      (lambda "ft" $
        "fname" <~ (project _FieldType _FieldType_name @@ var "ft") $
        "typeArgs" <~ Lists.map (lambda "tp" $ JavaUtilsSource.typeParameterToTypeArgument @@ var "tp") (var "tparams") $
        "varRef" <~ (JavaUtilsSource.javaClassTypeToJavaType @@
          (JavaUtilsSource.nameToJavaClassType @@ var "aliases" @@ false @@ var "typeArgs"
            @@ (JavaUtilsSource.variantClassName @@ false @@ var "elName" @@ var "fname") @@ nothing)) $
        "param" <~ (JavaUtilsSource.javaTypeToJavaFormalParameter @@ var "varRef" @@ wrap _Name (string "instance")) $
        "resultR" <~ (JavaUtilsSource.javaTypeToJavaResult @@ (JavaDsl.typeReference (asTerm JavaUtilsSource.visitorTypeVariable))) $
        JavaUtilsSource.interfaceMethodDeclaration @@ list ([] :: [TTerm Java.InterfaceMethodModifier]) @@ list ([] :: [TTerm Java.TypeParameter])
          @@ asTerm JavaNamesSource.visitMethodName @@ list [var "param"] @@ var "resultR" @@ nothing)
      (var "fields") $
    "visitorBody" <~ wrap Java._InterfaceBody (var "visitorMethods") $
    "visitor" <~ (JavaUtilsSource.javaInterfaceDeclarationToJavaClassBodyDeclaration @@
      (record Java._NormalInterfaceDeclaration [
        Java._NormalInterfaceDeclaration_modifiers>>: list [inject Java._InterfaceModifier Java._InterfaceModifier_public unit],
        Java._NormalInterfaceDeclaration_identifier>>: wrap Java._TypeIdentifier (JavaDsl.identifier (asTerm JavaNamesSource.visitorName)),
        Java._NormalInterfaceDeclaration_parameters>>: var "vtparams",
        Java._NormalInterfaceDeclaration_extends>>: list ([] :: [TTerm Java.InterfaceType]),
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
    "otherwiseDecl" <~ (JavaUtilsSource.interfaceMethodDeclaration @@ var "defaultMod" @@ list ([] :: [TTerm Java.TypeParameter])
      @@ asTerm JavaNamesSource.otherwiseMethodName @@ list [var "mainInstanceParam"] @@ var "resultR"
      @@ just (list [var "throwStmt"])) $
    -- Partial visitor visit methods: default to calling otherwise()
    "pvVisitMethods" <~ Lists.map
      (lambda "ft" $
        "fname" <~ (project _FieldType _FieldType_name @@ var "ft") $
        "varRef" <~ (JavaUtilsSource.javaClassTypeToJavaType @@
          (JavaUtilsSource.nameToJavaClassType @@ var "aliases" @@ false @@ var "typeArgs"
            @@ (JavaUtilsSource.variantClassName @@ false @@ var "elName" @@ var "fname") @@ nothing)) $
        "param" <~ (JavaUtilsSource.javaTypeToJavaFormalParameter @@ var "varRef" @@ wrap _Name (string "instance")) $
        "mi" <~ (JavaUtilsSource.methodInvocation @@ nothing
              @@ JavaDsl.identifier (asTerm JavaNamesSource.otherwiseMethodName)
              @@ list [JavaUtilsSource.javaIdentifierToJavaExpression @@ (JavaDsl.identifier (string "instance"))]) $
        "returnOtherwise" <~ (JavaDsl.blockStatementStatement (JavaUtilsSource.javaReturnStatement @@ just
          (JavaUtilsSource.javaPrimaryToJavaExpression @@
            (JavaUtilsSource.javaMethodInvocationToJavaPrimary @@ var "mi")))) $
        JavaUtilsSource.interfaceMethodDeclaration @@ var "defaultMod" @@ list ([] :: [TTerm Java.TypeParameter])
          @@ asTerm JavaNamesSource.visitMethodName @@ list [var "param"] @@ var "resultR"
          @@ just (list [var "returnOtherwise"]))
      (var "fields") $
    "pvBody" <~ wrap Java._InterfaceBody (list [var "otherwiseDecl"] `Lists.concat2` var "pvVisitMethods") $
    "partialVisitor" <~ (JavaUtilsSource.javaInterfaceDeclarationToJavaClassBodyDeclaration @@
      (record Java._NormalInterfaceDeclaration [
        Java._NormalInterfaceDeclaration_modifiers>>: list [inject Java._InterfaceModifier Java._InterfaceModifier_public unit],
        Java._NormalInterfaceDeclaration_identifier>>: wrap Java._TypeIdentifier (JavaDsl.identifier (asTerm JavaNamesSource.partialVisitorName)),
        Java._NormalInterfaceDeclaration_parameters>>: var "vtparams",
        Java._NormalInterfaceDeclaration_extends>>: list [wrap Java._InterfaceType (var "visitorClassType")],
        Java._NormalInterfaceDeclaration_body>>: var "pvBody"])) $
    -- Build constant declarations
    "tn0" <<~ (constantDeclForTypeName @@ var "aliases" @@ var "elName" @@ var "cx" @@ var "g") $
    "tn1" <<~ (Eithers.mapList (lambda "ft" $ constantDeclForFieldType @@ var "aliases" @@ var "ft" @@ var "cx" @@ var "g") (var "fields")) $
    "tn" <~ list [var "tn0"] `Lists.concat2` var "tn1" $
    "otherDecls" <~ Lists.map (lambda "d" $ noComment @@ var "d")
      (list [var "privateConst", var "acceptDecl", var "visitor", var "partialVisitor"]) $
    "bodyDecls" <~ Lists.concat (list [var "tn", var "otherDecls", var "variantDecls'"]) $
    "mods" <~ Lists.concat2 (asTerm classModsPublic) (list [inject Java._ClassModifier Java._ClassModifier_abstract unit]) $
    right (JavaUtilsSource.javaClassDeclaration @@ var "aliases" @@ var "tparams" @@ var "elName" @@ var "mods"
      @@ nothing @@ (interfaceTypes @@ var "isSer" @@ var "aliases" @@ var "tparams" @@ var "elName") @@ var "bodyDecls")

-- | Encode a type definition as a Java compilation unit.
encodeTypeDefinition :: TBinding (Java.PackageDeclaration -> JavaHelpers.Aliases -> TypeDefinition -> Context -> Graph -> Either (InContext Error) (Name, Java.CompilationUnit))
encodeTypeDefinition = def "encodeTypeDefinition" $
  lambda "pkg" $ lambda "aliases" $ lambda "tdef" $
    "cx" ~> "g" ~>
    "name" <~ (project _TypeDefinition _TypeDefinition_name @@ var "tdef") $
    "typ" <~ (project _TypeDefinition _TypeDefinition_type @@ var "tdef") $
    -- Check if serializable
    "serializable" <~ (isSerializableJavaType @@ var "typ") $
    "imports" <~ Logic.ifElse (var "serializable")
      (list [inject Java._ImportDeclaration Java._ImportDeclaration_singleType
        (wrap Java._SingleTypeImportDeclaration
          (JavaUtilsSource.javaTypeName @@ (JavaDsl.identifier (string "java.io.Serializable"))))])
      (list ([] :: [TTerm Java.ImportDeclaration])) $
    "decl" <<~ (toClassDecl @@ false @@ var "serializable" @@ var "aliases"
      @@ (list ([] :: [TTerm Java.TypeParameter])) @@ var "name" @@ var "typ" @@ var "cx" @@ var "g") $
    "comment" <<~ (Annotations.getTypeDescription @@ var "cx" @@ var "g" @@ var "typ") $
    "tdecl" <~ record Java._TypeDeclarationWithComments [
      Java._TypeDeclarationWithComments_value>>: inject Java._TypeDeclaration Java._TypeDeclaration_class (var "decl"),
      Java._TypeDeclarationWithComments_comments>>: var "comment"] $
    right (pair (var "name")
      (inject Java._CompilationUnit Java._CompilationUnit_ordinary (record Java._OrdinaryCompilationUnit [
        Java._OrdinaryCompilationUnit_package>>: just (var "pkg"),
        Java._OrdinaryCompilationUnit_imports>>: var "imports",
        Java._OrdinaryCompilationUnit_types>>: list [var "tdecl"]])))

-- | Peel domain types from a function type, returning the list of domains and the codomain.
-- Given a count n and a type, peels up to n function types off the front.
peelDomainsAndCod :: TBinding (Int -> Type -> ([Type], Type))
peelDomainsAndCod = def "peelDomainsAndCod" $
  lambda "n" $ lambda "t" $
    Logic.ifElse (Equality.lte (var "n") (int32 0))
      (pair (list ([] :: [TTerm Type])) (var "t"))
      (cases _Type (Rewriting.deannotateType @@ var "t")
        (Just $ pair (list ([] :: [TTerm Type])) (var "t")) [
        _Type_function>>: lambda "ft" $
          "rest" <~ (peelDomainsAndCod @@ Math.sub (var "n") (int32 1) @@ Core.functionTypeCodomain (var "ft")) $
          pair (Lists.cons (Core.functionTypeDomain (var "ft")) (Pairs.first (var "rest")))
            (Pairs.second (var "rest"))])

-- | Check whether a type is "serializable" (record, union, wrap, or forall wrapping a serializable type).
-- These are the types that get promoted to Java class declarations.
isSerializableJavaType :: TBinding (Type -> Bool)
isSerializableJavaType = def "isSerializableJavaType" $
  lambda "typ" $
    cases _Type (Rewriting.deannotateType @@ var "typ")
      (Just false) [
      _Type_record>>: lambda "rt" $ true,
      _Type_union>>: lambda "rt" $ true,
      _Type_wrap>>: lambda "wt" $ true,
      _Type_forall>>: lambda "fa" $
        isSerializableJavaType @@ Core.forallTypeBody (var "fa")]

-- | Correct the cast type for pair terms. When we have a TermTypeApplication wrapping
-- a TermPair with exactly 2 type args, reconstruct the pair type from the type args
-- (which have been correctly renamed by normalizeTypeVariablesInTerm) instead of using
-- the annotation type (which may have stale variable names).
correctCastType :: TBinding (Term -> [Type] -> Type -> Context -> Graph -> Either (InContext Error) Type)
correctCastType = def "correctCastType" $
  lambda "innerBody" $ lambda "typeArgs" $ lambda "fallback" $
    "cx" ~> "g" ~>
    cases _Term (Rewriting.deannotateTerm @@ var "innerBody")
      (Just $ right (var "fallback")) [
      _Term_pair>>: lambda "_p" $
        Logic.ifElse (Equality.equal (Lists.length (var "typeArgs")) (int32 2))
          (right (inject _Type _Type_pair (Core.pairType
            (Lists.head (var "typeArgs"))
            (Lists.head (Lists.tail (var "typeArgs"))))))
          (right (var "fallback"))]

-- | Fallback cast for TermTypeApplication: re-annotate the body with corrected type
-- before encoding, then cast.
typeAppFallbackCast :: TBinding (JavaHelpers.JavaEnvironment -> JavaHelpers.Aliases -> [M.Map Name Term] -> [Java.Type] -> Java.Type -> Term -> Type -> Context -> Graph -> Either (InContext Error) Java.Expression)
typeAppFallbackCast = def "typeAppFallbackCast" $
  lambda "env" $ lambda "aliases" $ lambda "anns" $ lambda "tyapps" $
    lambda "jatyp" $ lambda "body" $ lambda "typ" $
      "cx" ~> "g" ~>
      "annotatedBody" <~ (Annotations.setTermAnnotation @@ asTerm Constants.key_type
        @@ just (encodeTypeAsTerm @@ var "typ") @@ var "body") $
      "jbody" <<~ (encodeTermInternal @@ var "env" @@ var "anns" @@ (Lists.cons (var "jatyp") (var "tyapps")) @@ var "annotatedBody" @@ var "cx" @@ var "g") $
      "jtype" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "typ" @@ var "cx" @@ var "g") $
      "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jtype" @@ var "cx") $
      right (JavaUtilsSource.javaCastExpressionToJavaExpression @@
        (JavaUtilsSource.javaCastExpression @@ var "rt" @@ (JavaUtilsSource.javaExpressionToJavaUnaryExpression @@ var "jbody")))

-- | Handle TermTypeApplication when the innermost body is a variable.
-- Generates explicit type witnesses for nullary static methods and hoisted lambdas
-- instead of casts, which Java can't resolve for methods with unconstrained type params.
typeAppNullaryOrHoisted :: TBinding (JavaHelpers.JavaEnvironment -> JavaHelpers.Aliases -> [M.Map Name Term] -> [Java.Type] -> Java.Type -> Term -> Type -> Name -> JavaHelpers.JavaSymbolClass -> [Type] -> Context -> Graph -> Either (InContext Error) Java.Expression)
typeAppNullaryOrHoisted = def "typeAppNullaryOrHoisted" $
  lambda "env" $ lambda "aliases" $ lambda "anns" $ lambda "tyapps" $
    lambda "jatyp" $ lambda "body" $ lambda "correctedTyp" $ lambda "varName" $
      lambda "cls" $ lambda "allTypeArgs" $
        "cx" ~> "g" ~>
        "qn" <~ (Names.qualifyName @@ var "varName") $
        "mns" <~ Module.qualifiedNameNamespace (var "qn") $
        "localName" <~ Module.qualifiedNameLocal (var "qn") $
        cases JavaHelpers._JavaSymbolClass (var "cls")
          (Just $ typeAppFallbackCast @@ var "env" @@ var "aliases" @@ var "anns" @@ var "tyapps"
            @@ var "jatyp" @@ var "body" @@ var "correctedTyp" @@ var "cx" @@ var "g") [
          JavaHelpers._JavaSymbolClass_nullaryFunction>>: lambda "_u" $
            Maybes.cases (var "mns")
              (typeAppFallbackCast @@ var "env" @@ var "aliases" @@ var "anns" @@ var "tyapps"
                @@ var "jatyp" @@ var "body" @@ var "correctedTyp" @@ var "cx" @@ var "g")
              (lambda "ns_" $
                "classId" <~ (JavaUtilsSource.nameToJavaName @@ var "aliases"
                  @@ (Names.unqualifyName @@ Module.qualifiedName (just (var "ns_")) (elementsClassName @@ var "ns_"))) $
                "methodId" <~ JavaDsl.identifier (JavaUtilsSource.sanitizeJavaName @@ var "localName") $
                "filteredTypeArgs" <<~ (filterPhantomTypeArgs @@ var "varName" @@ var "allTypeArgs" @@ var "cx" @@ var "g") $
                "jTypeArgs" <<~ (Eithers.mapList (lambda "t" $
                  "jt" <<~ (encodeType @@ var "aliases" @@ Sets.empty @@ var "t" @@ var "cx" @@ var "g") $
                  "rt" <<~ (JavaUtilsSource.javaTypeToJavaReferenceType @@ var "jt" @@ var "cx") $
                  right (inject Java._TypeArgument Java._TypeArgument_reference (var "rt")))
                  (var "filteredTypeArgs")) $
                right (JavaUtilsSource.javaMethodInvocationToJavaExpression @@
                  (JavaUtilsSource.methodInvocationStaticWithTypeArgs @@ var "classId" @@ var "methodId"
                    @@ var "jTypeArgs" @@ list ([] :: [TTerm Java.Expression])))),
          JavaHelpers._JavaSymbolClass_hoistedLambda>>: lambda "arity" $
            Maybes.cases (var "mns")
              (typeAppFallbackCast @@ var "env" @@ var "aliases" @@ var "anns" @@ var "tyapps"
                @@ var "jatyp" @@ var "body" @@ var "correctedTyp" @@ var "cx" @@ var "g")
              (lambda "ns_" $
                "classId" <~ (JavaUtilsSource.nameToJavaName @@ var "aliases"
                  @@ (Names.unqualifyName @@ Module.qualifiedName (just (var "ns_")) (elementsClassName @@ var "ns_"))) $
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
flattenApps :: TBinding (Term -> [Term] -> ([Term], Term))
flattenApps = def "flattenApps" $
  lambda "t" $ lambda "acc" $
    cases _Term (Rewriting.deannotateTerm @@ var "t")
      (Just $ pair (var "acc") (var "t")) [
      _Term_application>>: lambda "app" $
        flattenApps
          @@ (project _Application _Application_function @@ var "app")
          @@ Lists.cons (project _Application _Application_argument @@ var "app") (var "acc")]

-- | Collect domain annotations from a chain of nested lambdas.
-- Returns (domains, innerBody).
collectLambdaDomains :: TBinding (Term -> ([Type], Term))
collectLambdaDomains = def "collectLambdaDomains" $
  lambda "t" $
    cases _Term (Rewriting.deannotateTerm @@ var "t")
      (Just $ pair (list ([] :: [TTerm Type])) (var "t")) [
      _Term_function>>: lambda "f" $
        cases _Function (var "f")
          (Just $ pair (list ([] :: [TTerm Type])) (var "t")) [
          _Function_lambda>>: lambda "lam" $
            Maybes.cases (Core.lambdaDomain (var "lam"))
              (pair (list ([] :: [TTerm Type])) (var "t"))
              (lambda "dom" $
                "rest" <~ (collectLambdaDomains @@ Core.lambdaBody (var "lam")) $
                pair (Lists.cons (var "dom") (Pairs.first (var "rest")))
                  (Pairs.second (var "rest")))]]

-- | Rebuild an application chain with proper type annotations at each step.
rebuildApps :: TBinding (Term -> [Term] -> Type -> Term)
rebuildApps = def "rebuildApps" $
  lambda "f" $ lambda "args" $ lambda "fType" $
    Logic.ifElse (Lists.null (var "args"))
      (var "f")
      (cases _Type (Rewriting.deannotateType @@ var "fType")
        (Just $ Lists.foldl (lambda "acc" $ lambda "a" $
          inject _Term _Term_application (Core.application (var "acc") (var "a")))
          (var "f") (var "args")) [
        _Type_function>>: lambda "ft" $
          "arg" <~ Lists.head (var "args") $
          "rest" <~ Lists.tail (var "args") $
          "remainingType" <~ Core.functionTypeCodomain (var "ft") $
          "app" <~ inject _Term _Term_application (Core.application (var "f") (var "arg")) $
          "annotatedApp" <~ (Annotations.setTermAnnotation @@ asTerm Constants.key_type
            @@ just (encodeTypeAsTerm @@ var "remainingType") @@ var "app") $
          rebuildApps @@ var "annotatedApp" @@ var "rest" @@ var "remainingType"])

-- | For application chains, propagate type annotations through the chain.
-- If f is a lambda with domain annotations and N args are applied,
-- annotate f with its full type and rebuild the chain with intermediate annotations.
propagateTypesInAppChain :: TBinding (Type -> Type -> Term -> Term)
propagateTypesInAppChain = def "propagateTypesInAppChain" $
  lambda "fixedCod" $ lambda "resultType" $ lambda "t" $
    "flattened" <~ (flattenApps @@ var "t" @@ list ([] :: [TTerm Term])) $
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
       "annotatedFun" <~ (Annotations.setTermAnnotation @@ asTerm Constants.key_type
         @@ just (encodeTypeAsTerm @@ var "funType") @@ var "fun") $
       rebuildApps @@ var "annotatedFun" @@ var "args" @@ var "funType")
      -- Not a lambda or no args: fall back to simple annotation
      (cases _Term (Rewriting.deannotateTerm @@ var "t")
        (Just $ Annotations.setTermAnnotation @@ asTerm Constants.key_type
          @@ just (encodeTypeAsTerm @@ var "resultType") @@ var "t") [
        _Term_application>>: lambda "app" $
          "lhs" <~ (project _Application _Application_function @@ var "app") $
          "rhs" <~ (project _Application _Application_argument @@ var "app") $
          -- Annotate case statement LHS with function type
          "annotatedLhs" <~ (cases _Term (Rewriting.deannotateTerm @@ var "lhs")
            (Just $ var "lhs") [
            _Term_function>>: lambda "fn" $
              cases _Function (var "fn")
                (Just $ var "lhs") [
                _Function_elimination>>: lambda "elim" $
                  cases _Elimination (var "elim")
                    (Just $ var "lhs") [
                    _Elimination_union>>: lambda "cs" $
                      "dom" <~ (Schemas.nominalApplication @@ (Core.caseStatementTypeName (var "cs"))
                        @@ list ([] :: [TTerm Type])) $
                      "ft" <~ inject _Type _Type_function (Core.functionType (var "dom") (var "fixedCod")) $
                      Annotations.setTermAnnotation @@ asTerm Constants.key_type
                        @@ just (encodeTypeAsTerm @@ var "ft") @@ var "lhs"]]]) $
          Annotations.setTermAnnotation @@ asTerm Constants.key_type
            @@ just (encodeTypeAsTerm @@ var "resultType")
            @@ inject _Term _Term_application (Core.application (var "annotatedLhs") (var "rhs"))])

-- =============================================================================
-- Tail-call optimization (TCO) helpers
-- =============================================================================

-- | Encode a term for TCO: self-tail-calls become param reassignment + continue.
--   Returns a list of Java BlockStatements (if/instanceof checks + return/continue).
--   tcoVarRenames maps original parameter names to snapshot names (e.g. term -> term_tco).
--   Non-continue paths use the snapshot names so lambdas capture effectively-final variables.
encodeTermTCO :: TBinding (JavaHelpers.JavaEnvironment -> Name -> [Name] -> M.Map Name Name -> Int -> Term -> Context -> Graph -> Either (InContext Error) [Java.BlockStatement])
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
    "stripped" <~ (Rewriting.deannotateAndDetypeTerm @@ var "term") $
    -- Check if this term is a direct self-tail-call: funcName(args...)
    "gathered" <~ (CoderUtils.gatherApplications @@ var "stripped") $
    "gatherArgs" <~ (Pairs.first $ var "gathered") $
    "gatherFun" <~ (Pairs.second $ var "gathered") $
    "strippedFun" <~ (Rewriting.deannotateAndDetypeTerm @@ var "gatherFun") $
    -- Check for self-call pattern: Variable(funcName)
    "isSelfCall" <~ (cases _Term (var "strippedFun")
      (Just false) [
        _Term_variable>>: "n" ~> Equality.equal (var "n") (var "funcName")]) $
    Logic.ifElse (Logic.and (var "isSelfCall")
                            (Equality.equal (Lists.length $ var "gatherArgs") (Lists.length $ var "paramNames")))
      -- TAIL CALL: emit param reassignment + continue
      (-- Filter out self-assignments (e.g. x = x) so params remain effectively final for lambdas
        "changePairs" <~ Lists.filter ("pair" ~>
          Logic.not (cases _Term (Rewriting.deannotateAndDetypeTerm @@ Pairs.second (var "pair"))
            (Just false) [
            _Term_variable>>: "n" ~> Equality.equal (var "n") (Pairs.first (var "pair"))]))
          (Lists.zip (var "paramNames") (var "gatherArgs")) $
        "changedParams" <~ Lists.map (unaryFunction Pairs.first) (var "changePairs") $
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
              (wrap Java._ContinueStatement (nothing :: TTerm (Maybe Java.Identifier)))))) $
        right $ Lists.concat2 (var "assignments") (list [var "continueStmt"]))
      -- NOT a self-call: check for let-expression or case statement application
      (cases _Term (var "stripped")
        (Just $
          -- Default: check for case statement application
          "gathered2" <~ (CoderUtils.gatherApplications @@ var "term") $
        "args2" <~ (Pairs.first $ var "gathered2") $
        "body2" <~ (Pairs.second $ var "gathered2") $
        Logic.ifElse (Equality.equal (Lists.length $ var "args2") (int32 1))
          -- Single argument: try to match as case statement
          ("arg" <~ (Lists.head $ var "args2") $
            cases _Term (Rewriting.deannotateAndDetypeTerm @@ var "body2") (Just $
              -- Default: not a case statement, encode as return
              "expr" <<~ (encodeTerm @@ var "env" @@ var "term" @@ var "cx" @@ var "g") $
              right $ list [JavaDsl.blockStatementStatement (JavaUtilsSource.javaReturnStatement @@ just (var "expr"))]) [
              _Term_function>>: "f" ~>
                cases _Function (var "f") (Just $
                  "expr" <<~ (encodeTerm @@ var "env" @@ var "term" @@ var "cx" @@ var "g") $
                  right $ list [JavaDsl.blockStatementStatement (JavaUtilsSource.javaReturnStatement @@ just (var "expr"))]) [
                  _Function_elimination>>: "e" ~>
                    cases _Elimination (var "e") (Just $
                      "expr" <<~ (encodeTerm @@ var "env" @@ var "term" @@ var "cx" @@ var "g") $
                      right $ list [JavaDsl.blockStatementStatement (JavaUtilsSource.javaReturnStatement @@ just (var "expr"))]) [
                      _Elimination_union>>: "cs" ~>
                        -- Case statement: generate if/instanceof chain
                        "aliases" <~ (project JavaHelpers._JavaEnvironment JavaHelpers._JavaEnvironment_aliases @@ var "env") $
                        "tname" <~ (Core.caseStatementTypeName $ var "cs") $
                        "dflt" <~ (Core.caseStatementDefault $ var "cs") $
                        "cases_" <~ (Core.caseStatementCases $ var "cs") $
                        "domArgs" <<~ (domTypeArgs @@ var "aliases" @@ (Schemas.nominalApplication @@ var "tname" @@ list ([] :: [TTerm Type])) @@ var "cx" @@ var "g") $
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
                          "fieldName" <~ (Core.fieldName (var "field")) $
                          -- Build the variant reference type for instanceof
                          "variantRefType" <~ (JavaUtilsSource.nameToJavaReferenceType @@ var "aliases" @@ true @@ var "domArgs"
                            @@ var "tname" @@ just (Formatting.capitalize @@ (Core.unName (var "fieldName")))) $
                          -- Extract the lambda body from this case branch
                          cases _Term (Rewriting.deannotateTerm @@ Core.fieldTerm (var "field"))
                            (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError $ string "TCO: case branch is not a lambda") (var "cx")) [
                            _Term_function>>: "f2" ~>
                              cases _Function (var "f2")
                                (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError $ string "TCO: case branch is not a lambda") (var "cx")) [
                                _Function_lambda>>: "lam" ~>
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
                                  "isBranchTailCall" <~ (CoderUtils.isTailRecursiveInTailPosition @@ var "funcName" @@ var "branchBody") $
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
                                    (JavaDsl.statementIfThen (JavaDsl.ifThenStatement (var "condExpr") (var "ifBody"))))]])
                          (var "cases_")) $
                        -- Default: return the expression (or the arg for otherwise)
                        "defaultStmt" <<~ (Maybes.cases (var "dflt")
                          -- No default: return the argument unchanged
                          (right $ list [JavaDsl.blockStatementStatement
                            (JavaUtilsSource.javaReturnStatement @@ just (var "jArg"))])
                          ("d" ~>
                            -- Default is a value to return, not a function to apply to the argument
                            "dExpr" <<~ (encodeTerm @@ var "env" @@ var "d" @@ var "cx" @@ var "g") $
                            right $ list [JavaDsl.blockStatementStatement
                              (JavaUtilsSource.javaReturnStatement @@ just (var "dExpr"))])) $
                        right $ Lists.concat (list [list [var "matchDecl"], var "ifBlocks", var "defaultStmt"])]]])
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

-- | Encode a term definition as a Java interface method declaration.
-- This is the most complex function — it handles type parameters, lambda analysis,
-- type variable substitution, accumulator unification, and body annotation.
encodeTermDefinition :: TBinding (JavaHelpers.JavaEnvironment -> TermDefinition -> Context -> Graph -> Either (InContext Error) Java.InterfaceMemberDeclaration)
encodeTermDefinition = def "encodeTermDefinition" $
  lambda "env" $ lambda "tdef" $
    "cx" ~> "g" ~>
    "name" <~ (project _TermDefinition _TermDefinition_name @@ var "tdef") $
    "term0" <~ (project _TermDefinition _TermDefinition_term @@ var "tdef") $
    "ts" <~ (project _TermDefinition _TermDefinition_type @@ var "tdef") $
    -- Unshadow variables
    ("term" <~ (Rewriting.unshadowVariables @@ var "term0") $
      "fs" <<~ (analyzeJavaFunction @@ var "env" @@ var "term" @@ var "cx" @@ var "g") $
      -- Get type parameters from scheme
      "schemeVars" <~ Lists.filter (lambda "v" $ isSimpleName @@ var "v") (Core.typeSchemeVariables (var "ts")) $
      "termVars" <~ (project _FunctionStructure _FunctionStructure_typeParams @@ var "fs") $
      "schemeTypeVars" <~ (collectTypeVars @@ Core.typeSchemeType (var "ts")) $
      "usedSchemeVars" <~ Lists.filter (lambda "v" $ Sets.member (var "v") (var "schemeTypeVars")) (var "schemeVars") $
      "tparams" <~ Logic.ifElse (Lists.null (var "usedSchemeVars")) (var "termVars") (var "usedSchemeVars") $
      "params" <~ (project _FunctionStructure _FunctionStructure_params @@ var "fs") $
      "bindings" <~ (project _FunctionStructure _FunctionStructure_bindings @@ var "fs") $
      "body" <~ (project _FunctionStructure _FunctionStructure_body @@ var "fs") $
      "doms" <~ (project _FunctionStructure _FunctionStructure_domains @@ var "fs") $
      "env2" <~ (project _FunctionStructure _FunctionStructure_environment @@ var "fs") $
      -- Derive codomain from TypeScheme
      "schemeType" <~ Core.typeSchemeType (var "ts") $
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
      "overgenVarSubst" <~ Maps.fromList (Maybes.cat (Lists.map
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
      "constraints" <~ Maybes.fromMaybe (Maps.empty) (Core.typeSchemeConstraints (var "ts")) $
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
      --     (CoderUtils.isSelfTailRecursive @@ var "name" @@ var "body")) $
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
          "noCond" <~ (nothing :: TTerm (Maybe Java.Expression)) $
          "whileStmt" <~ (JavaDsl.blockStatementStatement
            (JavaDsl.statementWhile (record Java._WhileStatement [
              Java._WhileStatement_cond>>: var "noCond",
              Java._WhileStatement_body>>: var "whileBodyBlock"]))) $
          right $ list [var "whileStmt"])
        -- Normal path: encode body as expression with return
        ("jbody" <<~ (encodeTerm @@ var "env3" @@ var "annotatedBody" @@ var "cx" @@ var "g") $
          "returnSt" <~ (JavaDsl.blockStatementStatement (JavaUtilsSource.javaReturnStatement @@ just (var "jbody"))) $
          right $ Lists.concat2 (var "bindingStmts") (list [var "returnSt"]))) $
      right (JavaUtilsSource.interfaceMethodDeclaration @@ var "mods" @@ var "jparams"
        @@ var "jname" @@ var "jformalParams" @@ var "result"
        @@ just (var "methodBody")))

-- | Encode all definitions in a module to Java compilation units.
encodeDefinitions :: TBinding (Module -> [Definition] -> Context -> Graph -> Either (InContext Error) (M.Map Name Java.CompilationUnit))
encodeDefinitions = def "encodeDefinitions" $
  lambda "mod" $ lambda "defs" $
    "cx" ~> "g" ~>
    "aliases" <~ (JavaUtilsSource.importAliasesForModule @@ var "mod") $
    "env" <~ (record JavaHelpers._JavaEnvironment [
      JavaHelpers._JavaEnvironment_aliases>>: var "aliases",
      JavaHelpers._JavaEnvironment_graph>>: var "g"]) $
    "pkg" <~ (JavaUtilsSource.javaPackageDeclaration @@ (Module.moduleNamespace (var "mod"))) $
    "partitioned" <~ (Schemas.partitionDefinitions @@ var "defs") $
    "typeDefs" <~ Pairs.first (var "partitioned") $
    "termDefs" <~ Pairs.second (var "partitioned") $
    -- Filter out typedefs (non-record/union/wrap types)
    "nonTypedefDefs" <~ Lists.filter (lambda "td" $
      "typ" <~ (project _TypeDefinition _TypeDefinition_type @@ var "td") $
      isSerializableJavaType @@ (var "typ"))
      (var "typeDefs") $
    "typeUnits" <<~ (Eithers.mapList (lambda "td" $ encodeTypeDefinition @@ var "pkg" @@ var "aliases" @@ var "td" @@ var "cx" @@ var "g") (var "nonTypedefDefs")) $
    "termUnits" <<~ Logic.ifElse (Lists.null (var "termDefs"))
      (right (list ([] :: [TTerm (Name, Java.CompilationUnit)])))
      ("dataMembers" <<~ (Eithers.mapList (lambda "td" $ encodeTermDefinition @@ var "env" @@ var "td" @@ var "cx" @@ var "g") (var "termDefs")) $
        right (list [constructElementsInterface @@ var "mod" @@ var "dataMembers"])) $
    right (Maps.fromList (Lists.concat2 (var "typeUnits") (var "termUnits")))

-- | Top-level entry point: convert a module to Java source files.
moduleToJava :: TBinding (Module -> [Definition] -> Context -> Graph -> Either (InContext Error) (M.Map FilePath String))
moduleToJava = def "moduleToJava" $
  lambda "mod" $ lambda "defs" $
    "cx" ~> "g" ~>
    ("units" <<~ (encodeDefinitions @@ var "mod" @@ var "defs" @@ var "cx" @@ var "g") $
      right (Maps.fromList (Lists.map
        (lambda "entry" $
          "name" <~ Pairs.first (var "entry") $
          "unit" <~ Pairs.second (var "entry") $
          pair (bindingNameToFilePath @@ var "name")
            (SerializationSource.printExpr @@ (SerializationSource.parenthesize @@ (JavaSerdeSource.writeCompilationUnit @@ var "unit"))))
        (Maps.toList (var "units")))))

