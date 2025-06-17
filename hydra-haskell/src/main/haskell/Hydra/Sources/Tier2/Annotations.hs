{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Annotations where

-- Standard Tier-2 imports
import qualified Hydra.Dsl.Coders          as Coders
import qualified Hydra.Dsl.Compute         as Compute
import qualified Hydra.Dsl.Core            as Core
import qualified Hydra.Dsl.Graph           as Graph
import qualified Hydra.Dsl.Lib.Chars       as Chars
import qualified Hydra.Dsl.Lib.Equality    as Equality
import qualified Hydra.Dsl.Lib.Flows       as Flows
import qualified Hydra.Dsl.Lib.Io          as Io
import qualified Hydra.Dsl.Lib.Lists       as Lists
import qualified Hydra.Dsl.Lib.Literals    as Literals
import qualified Hydra.Dsl.Lib.Logic       as Logic
import qualified Hydra.Dsl.Lib.Maps        as Maps
import qualified Hydra.Dsl.Lib.Math        as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import           Hydra.Dsl.Phantoms        as Phantoms
import qualified Hydra.Dsl.Lib.Sets        as Sets
import           Hydra.Dsl.Lib.Strings     as Strings
import qualified Hydra.Dsl.Mantle          as Mantle
import qualified Hydra.Dsl.Module          as Module
import qualified Hydra.Dsl.TTerms          as TTerms
import qualified Hydra.Dsl.TTypes          as TTypes
import qualified Hydra.Dsl.Terms           as Terms
import qualified Hydra.Dsl.Types           as Types
import           Hydra.Sources.Tier1.All
import           Prelude hiding ((++))
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y

import Hydra.Sources.Tier1.Compute
import Hydra.Sources.Tier2.Variants
import Hydra.Sources.Tier2.Lexical
import Hydra.Sources.Tier2.Flows
import Hydra.Sources.Libraries


hydraAnnotationsModule :: Module
hydraAnnotationsModule = Module (Namespace "hydra.annotations") elements
    [coreEncodingModule, hydraVariantsModule, hydraLexicalModule, hydraFlowsModule]
    [hydraComputeModule, hydraGraphModule, hydraMantleModule] $
    Just "Utilities for reading and writing type and term annotations"
  where
   elements = [
     el aggregateAnnotationsDef,
--     el debugIfDef,
--     el failOnFlagDef,
--     el getDebugIdDef,
--     el getAttrDef,
--     el getAttrWithDefaultDef,
--     el getCountDef,
--     el getDescriptionDef,
--     el getTermAnnotationDef,
--     el getTermDescriptionDef,
     el getTermTypeDef,
--     el getTypeDef,
--     el getTypeAnnotationDef,
--     el getTypeClassesDef,
--     el getTypeDescriptionDef,
--     el isNativeTypeDef,
--     el hasDescriptionDef,
--     el hasFlagDef,
--     el hasTypeDescriptionDef,
--     el nextCountDef,
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
--     el whenFlagDef,
--     el unshadowVariablesDef,
--     el withDepthDef,
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

--debugIfDef :: TElement (String -> String -> Flow s ())
--debugIfDef = annotationsDefinition "debugIf" $
--  doc "Debug if the debug ID matches" $
--  lambda "debugId" $ lambda "message" $ lets [
--    "checkAndFail">: lambda "desc" $
--      Logic.ifElse (Equality.equalString (var "desc") (just $ string $ var "debugId"))
--        (Flows.fail $ var "message")
--        (Flows.pure unit)]
--    $ Flows.bind (ref getDebugIdDef) (var "checkAndFail")

--failOnFlagDef :: TElement (Name -> String -> Flow s ())
--failOnFlagDef = annotationsDefinition "failOnFlag" $
--  doc "Fail if the given flag is set" $
--  lambda "flag" $ lambda "msg" $
--    Flows.bind (ref hasFlagDef @@ var "flag") $
--      lambda "val" $ Logic.ifElse (var "val")
--        (Flows.fail $ var "msg")
--        (Flows.pure unit)

--getAttrDef :: TElement (Name -> Flow s (Maybe Term))
--getAttrDef = annotationsDefinition "getAttr" $
--  doc "Get an attribute from the trace" $
--  lambda "key" $ Flows.flow $ lambda "q" $ lambda "s0" $ lambda "t0" $
--    Compute.flowState
--      (just $ Maps.lookup (var "key") (Compute.traceOther $ var "t0"))
--      (var "s0")
--      (var "t0")
--
--getAttrWithDefaultDef :: TElement (Name -> Term -> Flow s Term)
--getAttrWithDefaultDef = annotationsDefinition "getAttrWithDefault" $
--  doc "Get an attribute with a default value" $
--  lambda "key" $ lambda "def" $
--    Flows.bind (ref getAttrDef @@ var "key") $
--      lambda "mval" $ Flows.pure $ Optionals.fromMaybe (var "def") (var "mval")

--getCountDef :: TElement (Name -> Flow s Int)
--getCountDef = annotationsDefinition "getCount" $
--  doc "Get a counter value" $
--  lambda "key" $ ref withEmptyGraphDef @@
--    (Flows.bind (ref getAttrWithDefaultDef @@ var "key" @@ Terms.int32 @@ int32 0) $
--      lambda "term" $ Expect.int32 $ var "term")

--getDebugIdDef :: TElement (Flow s (Maybe String))
--getDebugIdDef = annotationsDefinition "getDebugId" $
--  doc "Get the debug ID from flow state" $
--  ref withEmptyGraphDef @@
--    (Flows.bind (ref getAttrDef @@ Core.name key_debugId) $
--      lambda "desc" $ Optionals.traverse Expect.string (var "desc"))

--getDescriptionDef :: TElement (M.Map Name Term -> Flow Graph (Maybe String))
--getDescriptionDef = annotationsDefinition "getDescription" $
--  doc "Get description from annotations map" $
--  lambda "anns" $
--    Optionals.maybe (Flows.pure nothing)
--      (lambda "term" $
--        match _Term (Just $ Flows.fail $ Strings.cat $ list [
--          string "unexpected value for ",
--          Strings.fromShow $ Core.name key_description,
--          string ": ",
--          Strings.fromShow $ var "term"]) [
--          _Term_literal>>: lambda "lit" $
--            match _Literal (Just $ Flows.fail $ string "not a string literal") [
--              _Literal_string>>: lambda "s" $ Flows.pure $ just $ var "s"] @@ var "lit"]
--        @@ var "term")
--      (Maps.lookup (Core.name key_description) (var "anns"))

--getTermAnnotationDef :: TElement (Name -> Term -> Maybe Term)
--getTermAnnotationDef = annotationsDefinition "getTermAnnotation" $
--  doc "Get a term annotation" $
--  lambda "key" $ lambda "term" $
--    Maps.lookup (var "key") (ref termAnnotationInternalDef @@ var "term")

--getTermDescriptionDef :: TElement (Term -> Flow Graph (Maybe String))
--getTermDescriptionDef = annotationsDefinition "getTermDescription" $
--  doc "Get term description" $
--  lambda "term" $ ref getDescriptionDef @@ (ref termAnnotationInternalDef @@ var "term")

getTermTypeDef :: TElement (Term -> Maybe Type)
getTermTypeDef = annotationsDefinition "getTermType" $
  doc "Get the annotated type of a given term, if any" $
  match _Term (Just nothing) [
    "annotated">: ref getTermTypeDef <.> project _AnnotatedTerm _AnnotatedTerm_subject,
    "typed">: lambda "tt" $ just (project _TypedTerm _TypedTerm_type @@ var "tt")]

--getTypeDef :: TElement (M.Map Name Term -> Flow Graph (Maybe Type))
--getTypeDef = annotationsDefinition "getType" $
--  doc "Get type from annotations" $
--  lambda "anns" $
--    Optionals.maybe (Flows.pure nothing)
--      (lambda "dat" $ Flows.map just $ ref coreDecodeTypeDef @@ var "dat")
--      (Maps.lookup (Core.name key_type) (var "anns"))

--getTypeAnnotationDef :: TElement (Name -> Type -> Maybe Term)
--getTypeAnnotationDef = annotationsDefinition "getTypeAnnotation" $
--  doc "Get a type annotation" $
--  lambda "key" $ lambda "typ" $
--    Maps.lookup (var "key") (ref typeAnnotationInternalDef @@ var "typ")
--
--getTypeClassesDef :: TElement (Term -> Flow Graph (M.Map Name (S.Set TypeClass)))
--getTypeClassesDef = annotationsDefinition "getTypeClasses" $
--  doc "Get type classes from term" $
--  lambda "term" $ lets [
--    "decodeClass">: lambda "term" $ lets [
--      "byName">: Maps.fromList $ list [
--        pair _TypeClass_equality Core.typeClassEquality,
--        pair _TypeClass_ordering Core.typeClassOrdering]]
--      $ Flows.bind (Expect.unitVariant _TypeClass (var "term")) $
--        lambda "fn" $
--          Optionals.maybe
--            (Flows.unexpected (string "type class") (Strings.fromShow $ var "term"))
--            (Flows.pure)
--            (Maps.lookup (var "fn") (var "byName"))]
--    $ Optionals.maybe (Flows.pure Maps.empty)
--        (lambda "term" $ Expect.map (ref coreDecodeNameDef) (Expect.set $ var "decodeClass") (var "term"))
--        (ref getTermAnnotationDef @@ Core.name key_classes @@ var "term")
--
--getTypeDescriptionDef :: TElement (Type -> Flow Graph (Maybe String))
--getTypeDescriptionDef = annotationsDefinition "getTypeDescription" $
--  doc "Get type description" $
--  lambda "typ" $ ref getDescriptionDef @@ (ref typeAnnotationInternalDef @@ var "typ")

--isNativeTypeDef :: TElement (Element -> Bool)
--isNativeTypeDef = annotationsDefinition "isNativeType" $
--  doc "Check if element should be encoded as native type" $
--  lambda "el" $ lets [
--    "isFlaggedAsFirstClassType">: Optionals.fromMaybe false $
--      Flows.bind (ref getTermAnnotationDef @@ Core.name key_firstClassType @@ (Core.elementTerm $ var "el")) $
--        lambda "mterm" $ Optionals.traverse primitive _decode_boolean (var "mterm")]
--    $ Optionals.maybe false
--        (lambda "ts" $ Logic.and
--          (Logic.equals (var "ts") (Core.typeScheme (list []) (Core.typeVariable _Type)))
--          (Logic.not $ var "isFlaggedAsFirstClassType"))
--        (Graph.elementType $ var "el")

--hasDescriptionDef :: TElement (M.Map Name Term -> Bool)
--hasDescriptionDef = annotationsDefinition "hasDescription" $
--  doc "Check if annotations contain description" $
--  lambda "anns" $ Optionals.isJust $ Maps.lookup (Core.name key_description) (var "anns")
--
--hasFlagDef :: TElement (Name -> Flow s Bool)
--hasFlagDef = annotationsDefinition "hasFlag" $
--  doc "Check if flag is set" $
--  lambda "flag" $ ref withEmptyGraphDef @@
--    (Flows.bind (ref getAttrWithDefaultDef @@ var "flag" @@ Terms.boolean @@ false) $
--      lambda "term" $ Expect.boolean $ var "term")
--
--hasTypeDescriptionDef :: TElement (Type -> Bool)
--hasTypeDescriptionDef = annotationsDefinition "hasTypeDescription" $
--  doc "Check if type has description" $
--  lambda "typ" $ ref hasDescriptionDef @@ (ref typeAnnotationInternalDef @@ var "typ")
--
--nextCountDef :: TElement (Name -> Flow s Int)
--nextCountDef = annotationsDefinition "nextCount" $
--  doc "Get next counter value" $
--  lambda "key" $
--    Flows.bind (ref getCountDef @@ var "key") $
--      lambda "count" $
--        Flows.bind (ref putCountDef @@ var "key" @@ Math.add (var "count") (int32 1)) $
--          lambda "_" $ Flows.pure $ var "count"

normalizeTermAnnotationsDef :: TElement (Term -> Term)
normalizeTermAnnotationsDef = annotationsDefinition "normalizeTermAnnotations" $
  doc "Normalize term annotations" $
  lambda "term" $ lets [
    "anns">: ref termAnnotationInternalDef @@ var "term",
    "stripped">: ref stripTermDef @@ var "term"]
    $ Logic.ifElse (Maps.null $ var "anns")
        (var "stripped")
        (Core.termAnnotated $ Core.annotatedTerm (var "stripped") (var "anns"))

normalizeTypeAnnotationsDef :: TElement (Type -> Type)
normalizeTypeAnnotationsDef = annotationsDefinition "normalizeTypeAnnotations" $
  doc "Normalize type annotations" $
  lambda "typ" $ lets [
    "anns">: ref typeAnnotationInternalDef @@ var "typ",
    "stripped">: ref stripTypeDef @@ var "typ"]
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
    var "withType" @@ (ref getTermTypeDef @@ (project _Element _Element_term @@ var "el"))

requireTermTypeDef :: TElement (Term -> Flow Graph Type)
requireTermTypeDef = annotationsDefinition "requireTermType" $
  doc "Get the annotated type of a given term, or fail if it is missing" $
  lets [
    "withType">: primitive _optionals_maybe
      @@ (Flows.fail "missing type annotation")
      @@ (lambda "t" $ Flows.pure $ var "t")] $
    var "withType" <.> ref getTermTypeDef

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
    @@ Core.name key_description
    @@ Optionals.map (unaryFunction Core.termLiteral <.> unaryFunction Core.literalString) (var "d")

setTermAnnotationDef :: TElement (Name -> Maybe Term -> Term -> Term)
setTermAnnotationDef = annotationsDefinition "setTermAnnotation" $
  doc "Set term annotation" $
  lambda "key" $ lambda "val" $ lambda "term" $ lets [
    "term'">: ref stripTermDef @@ var "term",
    "anns">: ref setAnnotationDef @@ var "key" @@ var "val" @@ (ref termAnnotationInternalDef @@ var "term")]
    $ Logic.ifElse (Maps.null $ var "anns")
        (var "term'")
        (Core.termAnnotated $ Core.annotatedTerm (var "term'") (var "anns"))

setTermDescriptionDef :: TElement (Maybe String -> Term -> Term)
setTermDescriptionDef = annotationsDefinition "setTermDescription" $
  doc "Set term description" $
  lambda "d" $ ref setTermAnnotationDef
    @@ Core.name key_description
    @@ Optionals.map (unaryFunction Core.termLiteral <.> unaryFunction Core.literalString) (var "d")

-- TODO: temporary. Move this function out of Annotations
setTermTypeDef :: TElement (Maybe Type -> Term -> Term)
setTermTypeDef = annotationsDefinition "setTermType" $
  doc "Set term type" $
  lambda "mtyp" $ lambda "term" $ lets [
    "withoutType">: lambdas ["term"] $
      match _Term (Just $ var "term") [
        _Term_annotated>>: lambda "at" $ Core.termAnnotated $ Core.annotatedTerm
          (var "withoutType" @@ Core.annotatedTermSubject (var "at"))
          (Core.annotatedTermAnnotation $ var "at"),
        _Term_typed>>: lambda "tt" $ Core.typedTermTerm $ var "tt"]
      @@ var "term"]
    $ Optionals.maybe
        (var "withoutType" @@ var "term")
        (lambda "typ" $ Core.termTyped $ Core.typedTerm (var "withoutType" @@ var "term") (var "typ"))
        (var "mtyp")

setTypeDef :: TElement (Maybe Type -> M.Map Name Term -> M.Map Name Term)
setTypeDef = annotationsDefinition "setType" $
  doc "Set type in annotations" $
  lambda "mt" $ ref setAnnotationDef @@ Core.name key_type @@ Optionals.map (ref coreEncodeTypeDef) (var "mt")

setTypeAnnotationDef :: TElement (Name -> Maybe Term -> Type -> Type)
setTypeAnnotationDef = annotationsDefinition "setTypeAnnotation" $
  doc "Set type annotation" $
  lambda "key" $ lambda "val" $ lambda "typ" $ lets [
    "typ'">: ref stripTypeDef @@ var "typ",
    "anns">: ref setAnnotationDef @@ var "key" @@ var "val" @@ (ref typeAnnotationInternalDef @@ var "typ")]
    $ Logic.ifElse (Maps.null (var "anns"))
        (var "typ'")
        (Core.typeAnnotated $ Core.annotatedType (var "typ'") (var "anns"))

setTypeClassesDef :: TElement (M.Map Name (S.Set TypeClass) -> Term -> Term)
setTypeClassesDef = annotationsDefinition "setTypeClasses" $
  doc "Set type classes on term" $
  lambda "m" $ lets [
    "encodeClass">: lambda "tc" $
      match _TypeClass Nothing [
        _TypeClass_equality>>: constant $ TTerms.unitVariantPhantom _TypeClass _TypeClass_equality,
        _TypeClass_ordering>>: constant $ TTerms.unitVariantPhantom _TypeClass _TypeClass_ordering]
      @@ var "tc",
    "encodePair">: lambda "nameClasses" $ lets [
      "name">: first $ var "nameClasses",
      "classes">: second $ var "nameClasses"]
      $ pair (ref coreEncodeNameDef @@ var "name")
              (Core.termSet $ Sets.fromList $ Lists.map (var "encodeClass") $ Sets.toList $ var "classes"),
    "encoded">: Logic.ifElse (Maps.null $ var "m")
        nothing
        (just $ Core.termMap $ Maps.fromList $ Lists.map (var "encodePair") $ Maps.toList $ var "m")]
    $ ref setTermAnnotationDef @@ Core.name key_classes @@ var "encoded"

setTypeDescriptionDef :: TElement (Maybe String -> Type -> Type)
setTypeDescriptionDef = annotationsDefinition "setTypeDescription" $
  doc "Set type description" $
  lambda "d" $ ref setTypeAnnotationDef
    @@ Core.name key_description
    @@ Optionals.map (unaryFunction Core.termLiteral <.> unaryFunction Core.literalString) (var "d")

termAnnotationInternalDef :: TElement (Term -> M.Map Name Term)
termAnnotationInternalDef = annotationsDefinition "termAnnotationInternal" $
  doc "Get internal term annotations" $
  lets [
    "getAnn">: lambda "t" $
      match _Term (Just nothing) [
        _Term_annotated>>: lambda "a" $ just $ var "a",
        _Term_typed>>: lambda "tt" $ var "getAnn" @@ Core.typedTermTerm (var "tt")]
      @@ var "t"]
    $ ref aggregateAnnotationsDef @@ var "getAnn" @@ (unaryFunction Core.annotatedTermSubject) @@ (unaryFunction Core.annotatedTermAnnotation)

typeAnnotationInternalDef :: TElement (Type -> M.Map Name Term)
typeAnnotationInternalDef = annotationsDefinition "typeAnnotationInternal" $
  doc "Get internal type annotations" $
  lets [
    "getAnn">: lambda "t" $
      match _Type (Just nothing) [
        _Type_annotated>>: lambda "a" $ just $ var "a"]
      @@ var "t"]
    $ ref aggregateAnnotationsDef @@ var "getAnn" @@ (unaryFunction Core.annotatedTypeSubject) @@ (unaryFunction Core.annotatedTypeAnnotation)

typeElementDef :: TElement (Name -> Type -> Element)
typeElementDef = annotationsDefinition "typeElement" $
  doc "Create a type element with proper annotations" $
  lambda "name" $ lambda "typ" $ lets [
    "schemaTerm">: Core.termVariable (Core.name _Type),
    "dataTerm">: ref normalizeTermAnnotationsDef @@ (Core.termAnnotated $ Core.annotatedTerm
      (ref coreEncodeTypeDef @@ var "typ")
      (Maps.fromList $ list [pair (Core.name key_type) (var "schemaTerm")]))]
    $ Graph.element (var "name") (var "dataTerm") (just $ Core.typeScheme (list []) (var "typ"))

--whenFlagDef :: TElement (Name -> Flow s a -> Flow s a -> Flow s a)
--whenFlagDef = annotationsDefinition "whenFlag" $
--  doc "Execute different flows based on flag" $
--  lambda "flag" $ lambda "fthen" $ lambda "felse" $
--    Flows.bind (ref hasFlagDef @@ var "flag") $
--      lambda "b" $ Logic.ifElse (var "b") (var "fthen") (var "felse")

---- TODO: move out of Annotations and into Rewriting
--unshadowVariablesDef :: TElement (Term -> Term)
--unshadowVariablesDef = annotationsDefinition "unshadowVariables" $
--  doc "Unshadow variables in term" $
--  lambda "term" $ lets [
--    "freshName">: Flows.map (lambda "n" $ Core.name $ Strings.cat2 (string "s") (Strings.fromShow $ var "n")) $
--      ref nextCountDef @@ Core.name (string "unshadow"),
--    "rewrite">: lambda "recurse" $ lambda "term" $ lets [
--      "handleOther">: var "recurse" @@ var "term"]
--      $ Flows.bind Flows.getState $
--        lambda "state" $ lets [
--          "reserved">: first $ var "state",
--          "subst">: second $ var "state"]
--          $ match _Term (Just $ var "handleOther") [
--            _Term_variable>>: lambda "v" $ Flows.pure $ Core.termVariable $
--              Optionals.fromMaybe (var "v") (Maps.lookup (var "v") (var "subst")),
--            _Term_function>>: lambda "f" $
--              match _Function (Just $ var "handleOther") [
--                _Function_lambda>>: lambda "l" $ lets [
--                  "v">: Core.lambdaParameter $ var "l",
--                  "d">: Core.lambdaDomain $ var "l",
--                  "body">: Core.lambdaBody $ var "l"]
--                  $ Logic.ifElse (Sets.member (var "v") (var "reserved"))
--                    (Flows.bind (var "freshName") $
--                      lambda "v'" $
--                        Flows.bind (Flows.putState $ pair (Sets.insert (var "v'") (var "reserved")) (Maps.insert (var "v") (var "v'") (var "subst"))) $
--                          lambda "_" $
--                            Flows.bind (var "recurse" @@ var "body") $
--                              lambda "body'" $
--                                Flows.bind (Flows.putState $ var "state") $
--                                  lambda "_" $ Flows.pure $ Core.termFunction $ Core.functionLambda $ Core.lambda (var "v'") (var "d") (var "body'"))
--                    (Flows.bind (Flows.putState $ pair (Sets.insert (var "v") (var "reserved")) (var "subst")) $
--                      lambda "_" $
--                        Flows.bind (var "recurse" @@ var "body") $
--                          lambda "body'" $ Flows.pure $ Core.termFunction $ Core.functionLambda $ Core.lambda (var "v") (var "d") (var "body'"))]
--              @@ var "f"]
--          @@ var "term"]
--    $ Optionals.fromJust $ Compute.flowStateValue $ Compute.unFlow
--        (ref rewriteTermMDef @@ var "rewrite" @@ var "term")
--        (pair Sets.empty Maps.empty)
--        Compute.emptyTrace
--
--withDepthDef :: TElement (Name -> (Int -> Flow s a) -> Flow s a)
--withDepthDef = annotationsDefinition "withDepth" $
--  doc "Execute flow with depth counter" $
--  doc ("Provide an one-indexed, integer-valued 'depth' to a flow, where the depth is the number of nested calls."
--    ++ " This is useful for generating variable names while avoiding conflicts between the variables of parents and children."
--    ++ " E.g. a variable in an outer case/match statement might be "v1", whereas the variable of another case/match statement"
--    ++ " inside of the first one becomes \"v2\". See also nextCount.") $
--  lambda "key" $ lambda "f" $
--    Flows.bind (ref getCountDef @@ var "key") $
--      lambda "count" $ lets [
--        "inc">: Math.add (var "count") (int32 1)]
--        $ Flows.bind (ref putCountDef @@ var "key" @@ var "inc") $
--          lambda "_" $
--            Flows.bind (var "f" @@ var "inc") $
--              lambda "r" $
--                Flows.bind (ref putCountDef @@ var "key" @@ var "count") $
--                  lambda "_" $ Flows.pure $ var "r"

withEmptyGraphDef :: TElement (Flow Graph a -> Flow s a)
withEmptyGraphDef = annotationsDefinition "withEmptyGraph" $
  doc "Execute flow with empty graph" $
  ref withStateDef @@ ref emptyGraphDef
