-- Note: this is an automatically generated file. Do not edit.

-- | Utilities for reading and writing type and term annotations

module Hydra.Annotations where

import qualified Hydra.Classes as Classes
import qualified Hydra.Constants as Constants
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Encode.Core as Core__
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core___
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as Core____
import qualified Hydra.Strip as Strip
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M
import qualified Data.Set as S

-- | Aggregate annotations from nested structures
aggregateAnnotations :: Ord t2 => ((t0 -> Maybe t1) -> (t1 -> t0) -> (t1 -> M.Map t2 t3) -> t0 -> M.Map t2 t3)
aggregateAnnotations getValue getX getAnns t =

      let toPairs =
              \rest -> \t2 -> Maybes.maybe rest (\yy -> toPairs (Lists.cons (Maps.toList (getAnns yy)) rest) (getX yy)) (getValue t2)
      in (Maps.fromList (Lists.concat (toPairs [] t)))

-- | Debug if the debug ID matches (Either version)
debugIf :: Context.Context -> String -> String -> Either (Context.InContext Errors.Error) ()
debugIf cx debugId message =
    Eithers.bind (getDebugId cx) (\mid -> Logic.ifElse (Equality.equal mid (Just debugId)) (Left (Context.InContext {
      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError message)),
      Context.inContextContext = cx})) (Right ()))

-- | Fail if the given flag is set (Either version)
failOnFlag :: Context.Context -> Core.Name -> String -> Either (Context.InContext Errors.Error) ()
failOnFlag cx flag msg =
    Eithers.bind (hasFlag cx flag) (\val -> Logic.ifElse val (Left (Context.InContext {
      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError msg)),
      Context.inContextContext = cx})) (Right ()))

-- | Get an attribute from a context (pure version)
getAttr :: Core.Name -> Context.Context -> Maybe Core.Term
getAttr key cx = Maps.lookup key (Context.contextOther cx)

-- | Get an attribute with a default value from context (pure version)
getAttrWithDefault :: Core.Name -> Core.Term -> Context.Context -> Core.Term
getAttrWithDefault key def cx = Maybes.fromMaybe def (getAttr key cx)

-- | Get a counter value from context (pure version)
getCount :: Core.Name -> Context.Context -> Int
getCount key cx =
    Maybes.maybe 0 (\term -> case term of
      Core.TermLiteral v0 -> case v0 of
        Core.LiteralInteger v1 -> case v1 of
          Core.IntegerValueInt32 v2 -> v2
          _ -> 0
        _ -> 0
      _ -> 0) (Maps.lookup key (Context.contextOther cx))

-- | Get the debug ID from context (Either version)
getDebugId :: Context.Context -> Either (Context.InContext Errors.Error) (Maybe String)
getDebugId cx =
    Maybes.maybe (Right Nothing) (\term -> Eithers.map Maybes.pure (Core___.string cx (Graph.Graph {
      Graph.graphBoundTerms = Maps.empty,
      Graph.graphBoundTypes = Maps.empty,
      Graph.graphClassConstraints = Maps.empty,
      Graph.graphLambdaVariables = Sets.empty,
      Graph.graphMetadata = Maps.empty,
      Graph.graphPrimitives = Maps.empty,
      Graph.graphSchemaTypes = Maps.empty,
      Graph.graphTypeVariables = Sets.empty}) term)) (getAttr Constants.key_debugId cx)

-- | Get description from annotations map (Either version)
getDescription :: Context.Context -> Graph.Graph -> M.Map Core.Name Core.Term -> Either (Context.InContext Errors.Error) (Maybe String)
getDescription cx graph anns =
    Maybes.maybe (Right Nothing) (\term -> Eithers.map Maybes.pure (Core___.string cx graph term)) (Maps.lookup (Core.Name "description") anns)

-- | Get a term annotation
getTermAnnotation :: Core.Name -> Core.Term -> Maybe Core.Term
getTermAnnotation key term = Maps.lookup key (termAnnotationInternal term)

-- | Get term description (Either version)
getTermDescription :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Errors.Error) (Maybe String)
getTermDescription cx graph term =

      let peel =
              \t -> case t of
                Core.TermTypeLambda v0 -> peel (Core.typeLambdaBody v0)
                Core.TermTypeApplication v0 -> peel (Core.typeApplicationTermBody v0)
                _ -> t
      in (getDescription cx graph (termAnnotationInternal (peel term)))

-- | Get type from annotations
getType :: Graph.Graph -> M.Map Core.Name Core.Term -> Either Errors.DecodingError (Maybe Core.Type)
getType graph anns =
    Maybes.maybe (Right Nothing) (\dat -> Eithers.map Maybes.pure (Core_.type_ graph dat)) (Maps.lookup Constants.key_type anns)

-- | Get a type annotation
getTypeAnnotation :: Core.Name -> Core.Type -> Maybe Core.Term
getTypeAnnotation key typ = Maps.lookup key (typeAnnotationInternal typ)

-- | Get type classes from term
getTypeClasses :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Errors.Error) (M.Map Core.Name (S.Set Classes.TypeClass))
getTypeClasses cx graph term =

      let decodeClass =
              \term2 ->
                let byName =
                        Maps.fromList [
                          (Core.Name "equality", Classes.TypeClassEquality),
                          (Core.Name "ordering", Classes.TypeClassOrdering)]
                in (Eithers.bind (Core___.unitVariant cx (Core.Name "hydra.classes.TypeClass") graph term2) (\fn -> Maybes.maybe (Left (Context.InContext {
                  Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "unexpected: expected type class, got " (Core____.term term2)))),
                  Context.inContextContext = cx})) (\x -> Right x) (Maps.lookup fn byName)))
      in (Maybes.maybe (Right Maps.empty) (\term2 -> Core___.map cx (\t -> Eithers.bimap (\de -> Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError de))),
        Context.inContextContext = cx}) (\x -> x) (Core_.name graph t)) (Core___.setOf cx decodeClass graph) graph term2) (getTermAnnotation Constants.key_classes term))

-- | Get type description (Either version)
getTypeDescription :: Context.Context -> Graph.Graph -> Core.Type -> Either (Context.InContext Errors.Error) (Maybe String)
getTypeDescription cx graph typ = getDescription cx graph (typeAnnotationInternal typ)

-- | Check if annotations contain description
hasDescription :: M.Map Core.Name t0 -> Bool
hasDescription anns = Maybes.isJust (Maps.lookup Constants.key_description anns)

-- | Check if flag is set (Either version)
hasFlag :: Context.Context -> Core.Name -> Either (Context.InContext Errors.Error) Bool
hasFlag cx flag =

      let term = getAttrWithDefault flag (Core.TermLiteral (Core.LiteralBoolean False)) cx
      in (Core___.boolean cx (Graph.Graph {
        Graph.graphBoundTerms = Maps.empty,
        Graph.graphBoundTypes = Maps.empty,
        Graph.graphClassConstraints = Maps.empty,
        Graph.graphLambdaVariables = Sets.empty,
        Graph.graphMetadata = Maps.empty,
        Graph.graphPrimitives = Maps.empty,
        Graph.graphSchemaTypes = Maps.empty,
        Graph.graphTypeVariables = Sets.empty}) term)

-- | Check if type has description
hasTypeDescription :: Core.Type -> Bool
hasTypeDescription typ = hasDescription (typeAnnotationInternal typ)

-- | For a typed term, decide whether a coder should encode it as a native type expression, or as a Hydra type expression.
isNativeType :: Core.Binding -> Bool
isNativeType el =

      let isFlaggedAsFirstClassType =
              Maybes.fromMaybe False (Maybes.map (\_ -> True) (getTermAnnotation Constants.key_firstClassType (Core.bindingTerm el)))
      in (Maybes.maybe False (\ts -> Logic.and (Equality.equal ts (Core.TypeScheme {
        Core.typeSchemeVariables = [],
        Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.Type")),
        Core.typeSchemeConstraints = Nothing})) (Logic.not isFlaggedAsFirstClassType)) (Core.bindingType el))

-- | Return a zero-indexed counter for the given key and updated context (pure version)
nextCount :: Core.Name -> Context.Context -> (Int, Context.Context)
nextCount key cx =

      let count = getCount key cx
      in (count, (putCount key (Math.add count 1) cx))

-- | Normalize term annotations
normalizeTermAnnotations :: Core.Term -> Core.Term
normalizeTermAnnotations term =

      let anns = termAnnotationInternal term
          stripped = Strip.deannotateTerm term
      in (Logic.ifElse (Maps.null anns) stripped (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = stripped,
        Core.annotatedTermAnnotation = anns})))

-- | Normalize type annotations
normalizeTypeAnnotations :: Core.Type -> Core.Type
normalizeTypeAnnotations typ =

      let anns = typeAnnotationInternal typ
          stripped = Strip.deannotateType typ
      in (Logic.ifElse (Maps.null anns) stripped (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = stripped,
        Core.annotatedTypeAnnotation = anns})))

-- | Set an attribute in a context
putAttr :: Core.Name -> Core.Term -> Context.Context -> Context.Context
putAttr key val cx =
    Context.Context {
      Context.contextTrace = (Context.contextTrace cx),
      Context.contextMessages = (Context.contextMessages cx),
      Context.contextOther = (Maps.insert key val (Context.contextOther cx))}

-- | Set counter value in context
putCount :: Core.Name -> Int -> Context.Context -> Context.Context
putCount key count cx = putAttr key (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 count))) cx

-- | Reset counter to zero in context
resetCount :: Core.Name -> Context.Context -> Context.Context
resetCount key cx = putAttr key (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))) cx

-- | Set annotation in map
setAnnotation :: Ord t0 => (t0 -> Maybe t1 -> M.Map t0 t1 -> M.Map t0 t1)
setAnnotation key val m = Maps.alter (\_ -> val) key m

-- | Set description in annotations
setDescription :: Maybe String -> M.Map Core.Name Core.Term -> M.Map Core.Name Core.Term
setDescription d =
    setAnnotation Constants.key_description (Maybes.map (\arg_ -> (\x -> Core.TermLiteral x) ((\x -> Core.LiteralString x) arg_)) d)

-- | Set term annotation
setTermAnnotation :: Core.Name -> Maybe Core.Term -> Core.Term -> Core.Term
setTermAnnotation key val term =

      let term_ = Strip.deannotateTerm term
          anns = setAnnotation key val (termAnnotationInternal term)
      in (Logic.ifElse (Maps.null anns) term_ (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = term_,
        Core.annotatedTermAnnotation = anns})))

-- | Set term description
setTermDescription :: Maybe String -> Core.Term -> Core.Term
setTermDescription d =
    setTermAnnotation Constants.key_description (Maybes.map (\s -> Core.TermLiteral (Core.LiteralString s)) d)

-- | Set type in annotations
setType :: Maybe Core.Type -> M.Map Core.Name Core.Term -> M.Map Core.Name Core.Term
setType mt = setAnnotation Constants.key_type (Maybes.map Core__.type_ mt)

-- | Set type annotation
setTypeAnnotation :: Core.Name -> Maybe Core.Term -> Core.Type -> Core.Type
setTypeAnnotation key val typ =

      let typ_ = Strip.deannotateType typ
          anns = setAnnotation key val (typeAnnotationInternal typ)
      in (Logic.ifElse (Maps.null anns) typ_ (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = typ_,
        Core.annotatedTypeAnnotation = anns})))

-- | Set type classes on term
setTypeClasses :: M.Map Core.Name (S.Set Classes.TypeClass) -> Core.Term -> Core.Term
setTypeClasses m term =

      let encodeClass =
              \tc -> case tc of
                Classes.TypeClassEquality -> Core.TermUnion (Core.Injection {
                  Core.injectionTypeName = (Core.Name "hydra.classes.TypeClass"),
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "equality"),
                    Core.fieldTerm = Core.TermUnit}})
                Classes.TypeClassOrdering -> Core.TermUnion (Core.Injection {
                  Core.injectionTypeName = (Core.Name "hydra.classes.TypeClass"),
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "ordering"),
                    Core.fieldTerm = Core.TermUnit}})
          encodePair =
                  \nameClasses ->
                    let name = Pairs.first nameClasses
                        classes = Pairs.second nameClasses
                    in (Core__.name name, (Core.TermSet (Sets.fromList (Lists.map encodeClass (Sets.toList classes)))))
          encoded = Logic.ifElse (Maps.null m) Nothing (Just (Core.TermMap (Maps.fromList (Lists.map encodePair (Maps.toList m)))))
      in (setTermAnnotation Constants.key_classes encoded term)

-- | Set type description
setTypeDescription :: Maybe String -> Core.Type -> Core.Type
setTypeDescription d =
    setTypeAnnotation Constants.key_description (Maybes.map (\arg_ -> (\x -> Core.TermLiteral x) ((\x -> Core.LiteralString x) arg_)) d)

-- | Get internal term annotations
termAnnotationInternal :: Core.Term -> M.Map Core.Name Core.Term
termAnnotationInternal term =

      let getAnn =
              \t -> case t of
                Core.TermAnnotated v0 -> Just v0
                _ -> Nothing
      in (aggregateAnnotations getAnn (\at -> Core.annotatedTermBody at) (\at -> Core.annotatedTermAnnotation at) term)

-- | Get internal type annotations
typeAnnotationInternal :: Core.Type -> M.Map Core.Name Core.Term
typeAnnotationInternal typ =

      let getAnn =
              \t -> case t of
                Core.TypeAnnotated v0 -> Just v0
                _ -> Nothing
      in (aggregateAnnotations getAnn (\at -> Core.annotatedTypeBody at) (\at -> Core.annotatedTypeAnnotation at) typ)

-- | Execute different branches based on flag (Either version)
whenFlag :: Context.Context -> Core.Name -> Either (Context.InContext Errors.Error) t0 -> Either (Context.InContext Errors.Error) t0 -> Either (Context.InContext Errors.Error) t0
whenFlag cx flag ethen eelse = Eithers.bind (hasFlag cx flag) (\b -> Logic.ifElse b ethen eelse)
