{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Annotations where

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
import qualified Hydra.Dsl.Lib.Io                 as Io
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
--import qualified Hydra.Sources.Tier2.Adapters as Adapters
--import qualified Hydra.Sources.Tier2.AdapterUtils as AdapterUtils
--import qualified Hydra.Sources.Tier2.Annotations as Annotations
--import qualified Hydra.Sources.Tier2.Arity as Arity
import qualified Hydra.Sources.Tier2.Decode.Core as DecodeCore
--import qualified Hydra.Sources.Tier2.CoreLanguage as CoreLanguage
import qualified Hydra.Sources.Tier2.Errors as Errors
import qualified Hydra.Sources.Tier2.Extract.Core as ExtractCore
import qualified Hydra.Sources.Tier2.Flows as Flows_
--import qualified Hydra.Sources.Tier2.GrammarToModule as GrammarToModule
--import qualified Hydra.Sources.Tier2.Inference as Inference
import qualified Hydra.Sources.Tier2.Lexical as Lexical
--import qualified Hydra.Sources.Tier2.LiteralAdapters as LiteralAdapters
--import qualified Hydra.Sources.Tier2.Describe.Core as DescribeCore
--import qualified Hydra.Sources.Tier2.Qnames as Qnames
--import qualified Hydra.Sources.Tier2.Reduction as Reduction
import qualified Hydra.Sources.Tier2.Rewriting as Rewriting
--import qualified Hydra.Sources.Tier2.Schemas as Schemas
--import qualified Hydra.Sources.Tier2.Serialization as Serialization
--import qualified Hydra.Sources.Tier2.Sorting as Sorting
--import qualified Hydra.Sources.Tier2.Substitution as Substitution
--import qualified Hydra.Sources.Tier2.Tarjan as Tarjan
--import qualified Hydra.Sources.Tier2.Templating as Templating
--import qualified Hydra.Sources.Tier2.TermAdapters as TermAdapters
--import qualified Hydra.Sources.Tier2.TermEncoding as TermEncoding
--import qualified Hydra.Sources.Tier2.Unification as Unification
import qualified Hydra.Sources.Tier2.Variants as Variants


hydraAnnotationsModule :: Module
hydraAnnotationsModule = Module (Namespace "hydra.annotations") elements
    [Decode.hydraDecodeModule, DecodeCore.hydraCoreDecodingModule, EncodeCore.coreEncodingModule,
      ExtractCore.hydraExpectModule, Variants.hydraVariantsModule, Lexical.hydraLexicalModule, Flows_.hydraFlowsModule]
    [Tier1.hydraCodersModule, Tier1.hydraComputeModule, Tier1.hydraGraphModule, Tier1.hydraMantleModule, Tier1.hydraModuleModule, Tier1.hydraTopologyModule] $
    Just "Utilities for reading and writing type and term annotations"
  where
   elements = [
     el aggregateAnnotationsDef,
     el debugIfDef,
     el failOnFlagDef,
     el getDebugIdDef,
     el getAttrDef,
     el getAttrWithDefaultDef,
     el getCountDef,
     el getDescriptionDef,
     el getTermAnnotationDef,
     el getTermDescriptionDef,
     el getTypeDef,
     el getTypeAnnotationDef,
     el getTypeClassesDef,
     el getTypeDescriptionDef,
     el isNativeTypeDef,
     el hasDescriptionDef,
     el hasFlagDef,
     el hasTypeDescriptionDef,
     el nextCountDef,
     el normalizeTermAnnotationsDef,
     el normalizeTypeAnnotationsDef,
     el putAttrDef,
     el putCountDef,
     el requireElementTypeDef,
     el requireTermTypeDef,
     el resetCountDef,
     el setAnnotationDef,
     el setDescriptionDef,
     el setTermAnnotationDef,
     el setTermDescriptionDef,
     el setTermTypeDef,
     el setTypeDef,
     el setTypeAnnotationDef,
     el setTypeClassesDef,
     el setTypeDescriptionDef,
     el termAnnotationInternalDef,
     el typeAnnotationInternalDef,
     el typeElementDef,
     el whenFlagDef,
     el unshadowVariablesDef,
     el withDepthDef,
     el withEmptyGraphDef]

annotationsDefinition :: String -> TTerm a -> TElement a
annotationsDefinition = definitionInModule hydraAnnotationsModule

aggregateAnnotationsDef :: TElement ((x -> Maybe y) -> (y -> x) -> (y -> M.Map Name Term) -> x -> M.Map Name Term)
aggregateAnnotationsDef = annotationsDefinition "aggregateAnnotations" $
  doc "Aggregate annotations from nested structures" $
  lambdas ["getValue", "getX", "getAnns", "t"] $ lets [
    "toPairs">: lambdas ["rest", "t"] $
      Optionals.maybe (var "rest")
        (lambda "yy" $ var "toPairs" @@
          Lists.cons (Maps.toList $ var "getAnns" @@ var "yy") (var "rest") @@
          (var "getX" @@ var "yy"))
        (var "getValue" @@ var "t")]
    $ Maps.fromList $ Lists.concat $ var "toPairs" @@ list [] @@ var "t"

debugIfDef :: TElement (String -> String -> Flow s ())
debugIfDef = annotationsDefinition "debugIf" $
  doc "Debug if the debug ID matches" $
  lambdas ["debugId", "message"] $ lets [
    "checkAndFail">: lambda "desc" $
      Logic.ifElse
        (Equality.equal (var "desc") (just $ string "debugId"))
        (Flows.fail $ var "message")
        (Flows.pure unit)]
    $ Flows.bind (ref getDebugIdDef) (var "checkAndFail")

failOnFlagDef :: TElement (Name -> String -> Flow s ())
failOnFlagDef = annotationsDefinition "failOnFlag" $
  doc "Fail if the given flag is set" $
  lambdas ["flag", "msg"] $
    withVar "val" (ref hasFlagDef @@ var "flag") $
      Logic.ifElse (var "val")
        (Flows.fail $ var "msg")
        (Flows.pure unit)

getAttrDef :: TElement (Name -> Flow s (Maybe Term))
getAttrDef = annotationsDefinition "getAttr" $
  doc "Get an attribute from the trace" $
  lambda "key" $ Compute.flow $ lambdas ["s0", "t0"] $
    Compute.flowState
      (just $ Maps.lookup (var "key") (Compute.traceOther $ var "t0"))
      (var "s0")
      (var "t0")

getAttrWithDefaultDef :: TElement (Name -> Term -> Flow s Term)
getAttrWithDefaultDef = annotationsDefinition "getAttrWithDefault" $
  doc "Get an attribute with a default value" $
  lambdas ["key", "def"] $
    Flows.map
      (lambda "mval" $ Optionals.fromMaybe (var "def") (var "mval"))
      (ref getAttrDef @@ var "key")

getCountDef :: TElement (Name -> Flow s Int)
getCountDef = annotationsDefinition "getCount" $
  doc "Get a counter value" $
  lambda "key" $ ref withEmptyGraphDef @@
    (Flows.bind
      (ref getAttrWithDefaultDef @@ var "key" @@ (Core.int32 0))
      (ref ExtractCore.int32Def))

getDebugIdDef :: TElement (Flow s (Maybe String))
getDebugIdDef = annotationsDefinition "getDebugId" $
  doc "Get the debug ID from flow state" $
  ref withEmptyGraphDef @@
    (Flows.bind
      (ref getAttrDef @@ ref Constants.key_debugIdDef)
      (lambda "desc" $ Flows.traverseOptional (ref ExtractCore.stringDef) (var "desc")))

getDescriptionDef :: TElement (M.Map Name Term -> Flow Graph (Maybe String))
getDescriptionDef = annotationsDefinition "getDescription" $
  doc "Get description from annotations map" $
  lambda "anns" $
    Optionals.maybe (Flows.pure nothing)
      (lambda "term" $ Flows.map (unaryFunction just) $ ref ExtractCore.stringDef @@ var "term")
      (Maps.lookup (Core.nameLift key_description) (var "anns"))

getTermAnnotationDef :: TElement (Name -> Term -> Maybe Term)
getTermAnnotationDef = annotationsDefinition "getTermAnnotation" $
  doc "Get a term annotation" $
  lambdas ["key", "term"] $
    Maps.lookup (var "key") (ref termAnnotationInternalDef @@ var "term")

getTermDescriptionDef :: TElement (Term -> Flow Graph (Maybe String))
getTermDescriptionDef = annotationsDefinition "getTermDescription" $
  doc "Get term description" $
  lambda "term" $ ref getDescriptionDef @@ (ref termAnnotationInternalDef @@ var "term")

getTypeDef :: TElement (M.Map Name Term -> Flow Graph (Maybe Type))
getTypeDef = annotationsDefinition "getType" $
  doc "Get type from annotations" $
  lambda "anns" $
    Optionals.maybe (Flows.pure nothing)
      (lambda "dat" $ Flows.map (unaryFunction just) (ref DecodeCore.type_Def @@ var "dat"))
      (Maps.lookup (ref Constants.key_typeDef) (var "anns"))

getTypeAnnotationDef :: TElement (Name -> Type -> Maybe Term)
getTypeAnnotationDef = annotationsDefinition "getTypeAnnotation" $
  doc "Get a type annotation" $
  lambdas ["key", "typ"] $
    Maps.lookup (var "key") (ref typeAnnotationInternalDef @@ var "typ")

getTypeClassesDef :: TElement (Term -> Flow Graph (M.Map Name (S.Set TypeClass)))
getTypeClassesDef = annotationsDefinition "getTypeClasses" $
  doc "Get type classes from term" $
  lambda "term" $ lets [
    "decodeClass">: lambda "term" $ lets [
      "byName">: Maps.fromList $ list [
        pair (Core.nameLift _TypeClass_equality) Graph.typeClassEquality,
        pair (Core.nameLift _TypeClass_ordering) Graph.typeClassOrdering]]
      $ withVar "fn" (ref ExtractCore.unitVariantDef @@ Core.nameLift _TypeClass @@ var "term") $
          Optionals.maybe
            (ref Errors.unexpectedDef @@ string "type class" @@ (Io.showTerm $ var "term"))
            (unaryFunction Flows.pure)
            (Maps.lookup (var "fn") (var "byName"))]
    $ Optionals.maybe
        (Flows.pure Maps.empty)
        (lambda "term" $ ref ExtractCore.mapDef
          @@ (ref DecodeCore.nameDef)
          @@ (ref ExtractCore.setDef @@ var "decodeClass")
          @@ (var "term"))
        (ref getTermAnnotationDef @@ ref Constants.key_classesDef @@ var "term")

getTypeDescriptionDef :: TElement (Type -> Flow Graph (Maybe String))
getTypeDescriptionDef = annotationsDefinition "getTypeDescription" $
  doc "Get type description" $
  lambda "typ" $ ref getDescriptionDef @@ (ref typeAnnotationInternalDef @@ var "typ")

isNativeTypeDef :: TElement (Element -> Bool)
isNativeTypeDef = annotationsDefinition "isNativeType" $
  doc ("For a typed term, decide whether a coder should encode it as a native type expression,"
    <> " or as a Hydra type expression.") $
  lambda "el" $ lets [
    "isFlaggedAsFirstClassType">: Optionals.fromMaybe false $
      Optionals.bind
        (ref getTermAnnotationDef @@ ref Constants.key_firstClassTypeDef @@ (Graph.elementTerm $ var "el"))
        (ref Decode.booleanDef)]
    $ Optionals.maybe
        false
        (lambda "ts" $ Logic.and
          (Equality.equal (var "ts") (Core.typeScheme (list []) (Core.typeVariable $ Core.nameLift _Type)))
          (Logic.not $ var "isFlaggedAsFirstClassType"))
        (Graph.elementType $ var "el")

hasDescriptionDef :: TElement (M.Map Name Term -> Bool)
hasDescriptionDef = annotationsDefinition "hasDescription" $
  doc "Check if annotations contain description" $
  lambda "anns" $ Optionals.isJust $ Maps.lookup (ref Constants.key_descriptionDef) (var "anns")

hasFlagDef :: TElement (Name -> Flow s Bool)
hasFlagDef = annotationsDefinition "hasFlag" $
  doc "Check if flag is set" $
  lambda "flag" $ ref withEmptyGraphDef @@
    (withVar "term" (ref getAttrWithDefaultDef @@ var "flag" @@ Core.false) $ ref ExtractCore.booleanDef @@ var "term")

hasTypeDescriptionDef :: TElement (Type -> Bool)
hasTypeDescriptionDef = annotationsDefinition "hasTypeDescription" $
  doc "Check if type has description" $
  lambda "typ" $ ref hasDescriptionDef @@ (ref typeAnnotationInternalDef @@ var "typ")

nextCountDef :: TElement (Name -> Flow s Int)
nextCountDef = annotationsDefinition "nextCount" $
  doc "Return a zero-indexed counter for the given key: 0, 1, 2, ..." $
  lambda "key" $
    withVar "count" (ref getCountDef @@ var "key") $
      Flows.map
        (constant $ var "count")
        (ref putCountDef @@ var "key" @@ Math.add (var "count") (int32 1))

normalizeTermAnnotationsDef :: TElement (Term -> Term)
normalizeTermAnnotationsDef = annotationsDefinition "normalizeTermAnnotations" $
  doc "Normalize term annotations" $
  lambda "term" $ lets [
    "anns">: ref termAnnotationInternalDef @@ var "term",
    "stripped">: ref Strip.stripTermDef @@ var "term"]
    $ Logic.ifElse (Maps.null $ var "anns")
        (var "stripped")
        (Core.termAnnotated $ Core.annotatedTerm (var "stripped") (var "anns"))

normalizeTypeAnnotationsDef :: TElement (Type -> Type)
normalizeTypeAnnotationsDef = annotationsDefinition "normalizeTypeAnnotations" $
  doc "Normalize type annotations" $
  lambda "typ" $ lets [
    "anns">: ref typeAnnotationInternalDef @@ var "typ",
    "stripped">: ref Strip.stripTypeDef @@ var "typ"]
    $ Logic.ifElse (Maps.null $ var "anns")
        (var "stripped")
        (Core.typeAnnotated $ Core.annotatedType (var "stripped") (var "anns"))

putAttrDef :: TElement (Name -> Term -> Flow s ())
putAttrDef = annotationsDefinition "putAttr" $
  doc "Set an attribute in the trace" $
  lambda "key" $ lambda "val" $ Compute.flow $ lambda "s0" $ lambda "t0" $
    Compute.flowState
      (just unit)
      (var "s0")
      (Compute.traceWithOther (var "t0") (Maps.insert (var "key") (var "val") (Compute.traceOther $ var "t0")))

putCountDef :: TElement (Name -> Int -> Flow s ())
putCountDef = annotationsDefinition "putCount" $
  doc "Set counter value" $
  lambda "key" $ lambda "count" $
    ref putAttrDef @@ var "key" @@ (Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $ var "count")

requireElementTypeDef :: TElement (Element -> Flow Graph Type)
requireElementTypeDef = annotationsDefinition "requireElementType" $
  doc "Get the annotated type of a given element, or fail if it is missing" $
  lambda "el" $ lets [
    "withType">: primitive _optionals_maybe
      @@ (Flows.fail ("missing type annotation for element " ++ (unwrap _Name @@ (project _Element _Element_name @@ var "el"))))
      @@ (lambda "t" $ Flows.pure $ var "t")] $
    var "withType" @@ (ref Rewriting.getTermTypeDef @@ (project _Element _Element_term @@ var "el"))

requireTermTypeDef :: TElement (Term -> Flow Graph Type)
requireTermTypeDef = annotationsDefinition "requireTermType" $
  doc "Get the annotated type of a given term, or fail if it is missing" $
  lets [
    "withType">: primitive _optionals_maybe
      @@ (Flows.fail "missing type annotation")
      @@ (lambda "t" $ Flows.pure $ var "t")] $
    var "withType" <.> ref Rewriting.getTermTypeDef

resetCountDef :: TElement (Name -> Flow s ())
resetCountDef = annotationsDefinition "resetCount" $
  doc "Reset counter to zero" $
  lambda "key" $ ref putAttrDef @@ var "key" @@ TTerms.int32 0

setAnnotationDef :: TElement (Name -> Maybe Term -> M.Map Name Term -> M.Map Name Term)
setAnnotationDef = annotationsDefinition "setAnnotation" $
  doc "Set annotation in map" $
  lambda "key" $ lambda "val" $ lambda "m" $
    Maps.alter (constant $ var "val") (var "key") (var "m")

setDescriptionDef :: TElement (Maybe String -> M.Map Name Term -> M.Map Name Term)
setDescriptionDef = annotationsDefinition "setDescription" $
  doc "Set description in annotations" $
  lambda "d" $ ref setAnnotationDef
    @@ ref Constants.key_descriptionDef
    @@ Optionals.map (unaryFunction Core.termLiteral <.> unaryFunction Core.literalString) (var "d")

setTermAnnotationDef :: TElement (Name -> Maybe Term -> Term -> Term)
setTermAnnotationDef = annotationsDefinition "setTermAnnotation" $
  doc "Set term annotation" $
  lambda "key" $ lambda "val" $ lambda "term" $ lets [
    "term'">: ref Strip.stripTermDef @@ var "term",
    "anns">: ref setAnnotationDef @@ var "key" @@ var "val" @@ (ref termAnnotationInternalDef @@ var "term")]
    $ Logic.ifElse (Maps.null $ var "anns")
        (var "term'")
        (Core.termAnnotated $ Core.annotatedTerm (var "term'") (var "anns"))

setTermDescriptionDef :: TElement (Maybe String -> Term -> Term)
setTermDescriptionDef = annotationsDefinition "setTermDescription" $
  doc "Set term description" $
  lambda "d" $ ref setTermAnnotationDef
    @@ ref Constants.key_descriptionDef
    @@ Optionals.map (unaryFunction Core.termLiteral <.> unaryFunction Core.literalString) (var "d")

-- TODO: temporary. Move this function out of Annotations
setTermTypeDef :: TElement (Maybe Type -> Term -> Term)
setTermTypeDef = annotationsDefinition "setTermType" $
  doc "Set term type" $
  lambda "mtyp" $ lambda "term" $ lets [
    "withoutType">: lambdas ["term"] $
      cases _Term (var "term") (Just $ var "term") [
        _Term_annotated>>: lambda "at" $ Core.termAnnotated $ Core.annotatedTerm
          (var "withoutType" @@ Core.annotatedTermSubject (var "at"))
          (Core.annotatedTermAnnotation $ var "at"),
        _Term_typed>>: lambda "tt" $ Core.typedTermTerm $ var "tt"]]
    $ Optionals.maybe
        (var "withoutType" @@ var "term")
        (lambda "typ" $ Core.termTyped $ Core.typedTerm (var "withoutType" @@ var "term") (var "typ"))
        (var "mtyp")

setTypeDef :: TElement (Maybe Type -> M.Map Name Term -> M.Map Name Term)
setTypeDef = annotationsDefinition "setType" $
  doc "Set type in annotations" $
  lambda "mt" $ ref setAnnotationDef @@ ref Constants.key_typeDef @@ Optionals.map (ref EncodeCore.coreEncodeTypeDef) (var "mt")

setTypeAnnotationDef :: TElement (Name -> Maybe Term -> Type -> Type)
setTypeAnnotationDef = annotationsDefinition "setTypeAnnotation" $
  doc "Set type annotation" $
  lambda "key" $ lambda "val" $ lambda "typ" $ lets [
    "typ'">: ref Strip.stripTypeDef @@ var "typ",
    "anns">: ref setAnnotationDef @@ var "key" @@ var "val" @@ (ref typeAnnotationInternalDef @@ var "typ")]
    $ Logic.ifElse (Maps.null (var "anns"))
        (var "typ'")
        (Core.typeAnnotated $ Core.annotatedType (var "typ'") (var "anns"))

setTypeClassesDef :: TElement (M.Map Name (S.Set TypeClass) -> Term -> Term)
setTypeClassesDef = annotationsDefinition "setTypeClasses" $
  doc "Set type classes on term" $
  lambda "m" $ lets [
    "encodeClass">: lambda "tc" $
      cases _TypeClass (var "tc") Nothing [
        _TypeClass_equality>>: constant $ TTerms.unitVariantPhantom _TypeClass _TypeClass_equality,
        _TypeClass_ordering>>: constant $ TTerms.unitVariantPhantom _TypeClass _TypeClass_ordering],
    "encodePair">: lambda "nameClasses" $ lets [
      "name">: first $ var "nameClasses",
      "classes">: second $ var "nameClasses"]
      $ pair (ref EncodeCore.coreEncodeNameDef @@ var "name")
              (Core.termSet $ Sets.fromList $ Lists.map (var "encodeClass") $ Sets.toList $ var "classes"),
    "encoded">: Logic.ifElse (Maps.null $ var "m")
        nothing
        (just $ Core.termMap $ Maps.fromList $ Lists.map (var "encodePair") $ Maps.toList $ var "m")]
    $ ref setTermAnnotationDef @@ ref Constants.key_classesDef @@ var "encoded"

setTypeDescriptionDef :: TElement (Maybe String -> Type -> Type)
setTypeDescriptionDef = annotationsDefinition "setTypeDescription" $
  doc "Set type description" $
  lambda "d" $ ref setTypeAnnotationDef
    @@ ref Constants.key_descriptionDef
    @@ Optionals.map (unaryFunction Core.termLiteral <.> unaryFunction Core.literalString) (var "d")

termAnnotationInternalDef :: TElement (Term -> M.Map Name Term)
termAnnotationInternalDef = annotationsDefinition "termAnnotationInternal" $
  doc "Get internal term annotations" $
  lets [
    "getAnn">: lambda "t" $
      cases _Term (var "t") (Just nothing) [
        _Term_annotated>>: lambda "a" $ just $ var "a",
        _Term_typed>>: lambda "tt" $ var "getAnn" @@ Core.typedTermTerm (var "tt")]]
    $ ref aggregateAnnotationsDef @@ var "getAnn" @@ (unaryFunction Core.annotatedTermSubject) @@ (unaryFunction Core.annotatedTermAnnotation)

typeAnnotationInternalDef :: TElement (Type -> M.Map Name Term)
typeAnnotationInternalDef = annotationsDefinition "typeAnnotationInternal" $
  doc "Get internal type annotations" $
  lets [
    "getAnn">: lambda "t" $
      cases _Type (var "t") (Just nothing) [
        _Type_annotated>>: lambda "a" $ just $ var "a"]]
    $ ref aggregateAnnotationsDef @@ var "getAnn" @@ (unaryFunction Core.annotatedTypeSubject) @@ (unaryFunction Core.annotatedTypeAnnotation)

typeElementDef :: TElement (Name -> Type -> Element)
typeElementDef = annotationsDefinition "typeElement" $
  doc "Create a type element with proper annotations" $
  lambda "name" $ lambda "typ" $ lets [
    "schemaTerm">: Core.termVariable (Core.nameLift _Type),
    "dataTerm">: ref normalizeTermAnnotationsDef @@ (Core.termAnnotated $ Core.annotatedTerm
      (ref EncodeCore.coreEncodeTypeDef @@ var "typ")
      (Maps.fromList $ list [pair (ref Constants.key_typeDef) (var "schemaTerm")]))]
    $ Graph.element (var "name") (var "dataTerm") (just $ Core.typeScheme (list []) (var "typ"))

whenFlagDef :: TElement (Name -> Flow s a -> Flow s a -> Flow s a)
whenFlagDef = annotationsDefinition "whenFlag" $
  doc "Execute different flows based on flag" $
  lambda "flag" $ lambda "fthen" $ lambda "felse" $
    withVar "b" (ref hasFlagDef @@ var "flag") $
      Logic.ifElse (var "b") (var "fthen") (var "felse")

-- TODO: move out of Annotations and into Rewriting
unshadowVariablesDef :: TElement (Term -> Term)
unshadowVariablesDef = annotationsDefinition "unshadowVariables" $
  doc "Unshadow variables in term" $
  lambda "term" $ lets [
    "freshName">: Flows.map (lambda "n" $ Core.name $ Strings.cat2 (string "s") (Literals.showInt32 $ var "n")) $
      ref nextCountDef @@ Core.name (string "unshadow"),
    "rewrite">: lambdas ["recurse", "term"] $ lets [
      "handleOther">: var "recurse" @@ var "term"]
      $ withVar "state" (ref Errors.getStateDef) $
        lets [
          "reserved">: first $ var "state",
          "subst">: second $ var "state"]
          $ cases _Term (var "term") (Just $ var "handleOther") [
            _Term_variable>>: lambda "v" $ Flows.pure $ Core.termVariable $
              Optionals.fromMaybe (var "v") (Maps.lookup (var "v") (var "subst")),
            _Term_function>>: lambda "f" $
              cases _Function (var "f") (Just $ var "handleOther") [
                _Function_lambda>>: lambda "l" $ lets [
                  "v">: Core.lambdaParameter $ var "l",
                  "d">: Core.lambdaDomain $ var "l",
                  "body">: Core.lambdaBody $ var "l"]
                  $ Logic.ifElse (Sets.member (var "v") (var "reserved"))
                    (withVar "v'" (var "freshName") $
                      Flows.bind (ref Errors.putStateDef @@ pair (Sets.insert (var "v'") (var "reserved")) (Maps.insert (var "v") (var "v'") (var "subst"))) $
                        constant $
                          Flows.bind (var "recurse" @@ var "body") $
                            lambda "body'" $
                              Flows.bind (ref Errors.putStateDef @@ var "state") $
                                constant $ Flows.pure $ Core.termFunction $ Core.functionLambda $ Core.lambda (var "v'") (var "d") (var "body'"))
                    (Flows.bind (ref Errors.putStateDef @@ pair (Sets.insert (var "v") (var "reserved")) (var "subst")) $
                      constant $
                        Flows.map
                          (lambda "body'" $ Core.termFunction $ Core.functionLambda $ Core.lambda (var "v") (var "d") (var "body'"))
                          (var "recurse" @@ var "body"))]]]
    $ Optionals.fromJust $ Compute.flowStateValue $ Compute.unFlow
        (ref Rewriting.rewriteTermMDef @@ var "rewrite" @@ var "term")
        (pair Sets.empty Maps.empty)
        (ref Flows_.emptyTraceDef)

withDepthDef :: TElement (Name -> (Int -> Flow s a) -> Flow s a)
withDepthDef = annotationsDefinition "withDepth" $
  doc ("Provide an one-indexed, integer-valued 'depth' to a flow, where the depth is the number of nested calls."
    <> " This is useful for generating variable names while avoiding conflicts between the variables of parents and children."
    <> " E.g. a variable in an outer case/match statement might be \"v1\", whereas the variable of another case/match statement"
    <> " inside of the first one becomes \"v2\". See also nextCount.") $
  lambdas ["key", "f"] $
    withVar "count" (ref getCountDef @@ var "key") $
      lets [
        "inc">: Math.add (var "count") (int32 1)]
        $ Flows.bind (ref putCountDef @@ var "key" @@ var "inc") $ constant $
            withVar "r" (var "f" @@ var "inc") $
              Flows.bind (ref putCountDef @@ var "key" @@ var "count") $
                constant $ Flows.pure $ var "r"

withEmptyGraphDef :: TElement (Flow Graph a -> Flow s a)
withEmptyGraphDef = annotationsDefinition "withEmptyGraph" $
  doc "Execute flow with empty graph" $
  ref Flows_.withStateDef @@ ref Lexical.emptyGraphDef
