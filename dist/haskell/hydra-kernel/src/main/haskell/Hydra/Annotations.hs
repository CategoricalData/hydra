-- Note: this is an automatically generated file. Do not edit.
-- | Utilities for reading and writing type and term annotations

module Hydra.Annotations where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Equality as Equality
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Maybes as Maybes
import qualified Hydra.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Show.Errors as ShowErrors
import qualified Hydra.Strip as Strip
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S
-- | Aggregate annotations from nested structures
aggregateAnnotations :: Ord t2 => ((t0 -> Maybe t1) -> (t1 -> t0) -> (t1 -> M.Map t2 t3) -> t0 -> M.Map t2 t3)
aggregateAnnotations getValue getX getAnns t =

      let toPairs =
              \rest -> \t2 -> Maybes.maybe rest (\yy -> toPairs (Lists.cons (Maps.toList (getAnns yy)) rest) (getX yy)) (getValue t2)
      in (Maps.fromList (Lists.concat (toPairs [] t)))
-- | Extract comments/description from a Binding
commentsFromBinding :: t0 -> Graph.Graph -> Core.Binding -> Either Errors.Error (Maybe String)
commentsFromBinding cx g b = getTermDescription cx g (Core.bindingTerm b)
-- | Extract comments/description from a FieldType
commentsFromFieldType :: t0 -> Graph.Graph -> Core.FieldType -> Either Errors.Error (Maybe String)
commentsFromFieldType cx g ft = getTypeDescription cx g (Core.fieldTypeType ft)
-- | Project a Map<Name, Term> out of an annotation Term. For a TermMap with TermVariable-shaped keys (or, transitionally, TermWrap-encoded Name keys), returns those (Name, value) entries; for any other shape, returns the empty map.
getAnnotationMap :: Core.Term -> M.Map Core.Name Core.Term
getAnnotationMap t =

      let extractName =
              \k -> case k of
                Core.TermVariable v0 -> Just v0
                Core.TermWrap v0 -> case (Core.wrappedTermBody v0) of
                  Core.TermLiteral v1 -> case v1 of
                    Core.LiteralString v2 -> Just (Core.Name v2)
                    _ -> Nothing
                  _ -> Nothing
                _ -> Nothing
          fromEntry =
                  \p ->
                    let k = Pairs.first p
                        v = Pairs.second p
                    in (Maybes.map (\n -> (n, v)) (extractName k))
      in case t of
        Core.TermMap v0 -> Maps.fromList (Maybes.cat (Lists.map fromEntry (Maps.toList v0)))
        _ -> Maps.empty
-- | Get description from annotations map (Either version)
getDescription :: t0 -> Graph.Graph -> M.Map Core.Name Core.Term -> Either Errors.Error (Maybe String)
getDescription cx graph anns =
    Maybes.maybe (Right Nothing) (\term -> Eithers.map Maybes.pure (ExtractCore.string graph term)) (Maps.lookup (Core.Name "description") anns)
-- | Get a term annotation
getTermAnnotation :: Core.Name -> Core.Term -> Maybe Core.Term
getTermAnnotation key term = Maps.lookup key (termAnnotationInternal term)
-- | Get term description (Either version)
getTermDescription :: t0 -> Graph.Graph -> Core.Term -> Either Errors.Error (Maybe String)
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
    Maybes.maybe (Right Nothing) (\dat -> Eithers.map Maybes.pure (DecodeCore.type_ graph dat)) (Maps.lookup Constants.keyType anns)
-- | Get a type annotation
getTypeAnnotation :: Core.Name -> Core.Type -> Maybe Core.Term
getTypeAnnotation key typ = Maps.lookup key (typeAnnotationInternal typ)
-- | Get type classes from term. Each Set Name contains bare class identifiers (#275).
getTypeClasses :: t0 -> Graph.Graph -> Core.Term -> Either Errors.Error (M.Map Core.Name (S.Set Core.Name))
getTypeClasses cx graph term =

      let decodeName = \term2 -> Eithers.bimap (\de -> Errors.ErrorDecoding de) (\x -> x) (DecodeCore.name graph term2)
      in (Maybes.maybe (Right Maps.empty) (\term2 -> ExtractCore.map decodeName (ExtractCore.setOf decodeName graph) graph term2) (getTermAnnotation Constants.keyClasses term))
-- | Get type description (Either version)
getTypeDescription :: t0 -> Graph.Graph -> Core.Type -> Either Errors.Error (Maybe String)
getTypeDescription cx graph typ = getDescription cx graph (typeAnnotationInternal typ)
-- | Check if annotations contain description
hasDescription :: M.Map Core.Name t0 -> Bool
hasDescription anns = Maybes.isJust (Maps.lookup Constants.keyDescription anns)
-- | Check if type has description
hasTypeDescription :: Core.Type -> Bool
hasTypeDescription typ = hasDescription (typeAnnotationInternal typ)
-- | For a typed term, decide whether a coder should encode it as a native type expression, or as a Hydra type expression.
isNativeType :: Core.Binding -> Bool
isNativeType el =

      let isFlaggedAsFirstClassType =
              Maybes.fromMaybe False (Maybes.map (\_ -> True) (getTermAnnotation Constants.keyFirstClassType (Core.bindingTerm el)))
      in (Maybes.maybe False (\ts -> Logic.and (Equality.equal ts (Core.TypeScheme {
        Core.typeSchemeVariables = [],
        Core.typeSchemeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
        Core.typeSchemeConstraints = Nothing})) (Logic.not isFlaggedAsFirstClassType)) (Core.bindingTypeScheme el))
-- | Normalize term annotations
normalizeTermAnnotations :: Core.Term -> Core.Term
normalizeTermAnnotations term =

      let anns = termAnnotationInternal term
          stripped = Strip.deannotateTerm term
      in (Logic.ifElse (Maps.null anns) stripped (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = stripped,
        Core.annotatedTermAnnotation = (wrapAnnotationMap anns)})))
-- | Normalize type annotations
normalizeTypeAnnotations :: Core.Type -> Core.Type
normalizeTypeAnnotations typ =

      let anns = typeAnnotationInternal typ
          stripped = Strip.deannotateType typ
      in (Logic.ifElse (Maps.null anns) stripped (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = stripped,
        Core.annotatedTypeAnnotation = (wrapAnnotationMap anns)})))
-- | Set annotation in map
setAnnotation :: Ord t0 => (t0 -> Maybe t1 -> M.Map t0 t1 -> M.Map t0 t1)
setAnnotation key val m = Maps.alter (\_ -> val) key m
-- | Set description in annotations
setDescription :: Maybe String -> M.Map Core.Name Core.Term -> M.Map Core.Name Core.Term
setDescription d =
    setAnnotation Constants.keyDescription (Maybes.map (\arg_ -> (\x -> Core.TermLiteral x) ((\x -> Core.LiteralString x) arg_)) d)
-- | Set term annotation
setTermAnnotation :: Core.Name -> Maybe Core.Term -> Core.Term -> Core.Term
setTermAnnotation key val term =

      let term_ = Strip.deannotateTerm term
          anns = setAnnotation key val (termAnnotationInternal term)
      in (Logic.ifElse (Maps.null anns) term_ (Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = term_,
        Core.annotatedTermAnnotation = (wrapAnnotationMap anns)})))
-- | Set term description
setTermDescription :: Maybe String -> Core.Term -> Core.Term
setTermDescription d =
    setTermAnnotation Constants.keyDescription (Maybes.map (\s -> Core.TermLiteral (Core.LiteralString s)) d)
-- | Set type in annotations
setType :: Maybe Core.Type -> M.Map Core.Name Core.Term -> M.Map Core.Name Core.Term
setType mt = setAnnotation Constants.keyType (Maybes.map EncodeCore.type_ mt)
-- | Set type annotation
setTypeAnnotation :: Core.Name -> Maybe Core.Term -> Core.Type -> Core.Type
setTypeAnnotation key val typ =

      let typ_ = Strip.deannotateType typ
          anns = setAnnotation key val (typeAnnotationInternal typ)
      in (Logic.ifElse (Maps.null anns) typ_ (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeBody = typ_,
        Core.annotatedTypeAnnotation = (wrapAnnotationMap anns)})))
-- | Set type classes on term. The Set Name carries bare class identifiers (#275).
setTypeClasses :: M.Map Core.Name (S.Set Core.Name) -> Core.Term -> Core.Term
setTypeClasses m term =

      let encodePair =
              \nameClasses ->
                let name = Pairs.first nameClasses
                    classes = Pairs.second nameClasses
                in (EncodeCore.name name, (Core.TermSet (Sets.fromList (Lists.map EncodeCore.name (Sets.toList classes)))))
          encoded = Logic.ifElse (Maps.null m) Nothing (Just (Core.TermMap (Maps.fromList (Lists.map encodePair (Maps.toList m)))))
      in (setTermAnnotation Constants.keyClasses encoded term)
-- | Set type description
setTypeDescription :: Maybe String -> Core.Type -> Core.Type
setTypeDescription d =
    setTypeAnnotation Constants.keyDescription (Maybes.map (\arg_ -> (\x -> Core.TermLiteral x) ((\x -> Core.LiteralString x) arg_)) d)
-- | Get internal term annotations
termAnnotationInternal :: Core.Term -> M.Map Core.Name Core.Term
termAnnotationInternal term =

      let getAnn =
              \t -> case t of
                Core.TermAnnotated v0 -> Just v0
                _ -> Nothing
      in (aggregateAnnotations getAnn (\at -> Core.annotatedTermBody at) (\at -> getAnnotationMap (Core.annotatedTermAnnotation at)) term)
-- | Get internal type annotations
typeAnnotationInternal :: Core.Type -> M.Map Core.Name Core.Term
typeAnnotationInternal typ =

      let getAnn =
              \t -> case t of
                Core.TypeAnnotated v0 -> Just v0
                _ -> Nothing
      in (aggregateAnnotations getAnn (\at -> Core.annotatedTypeBody at) (\at -> getAnnotationMap (Core.annotatedTypeAnnotation at)) typ)
-- | Wrap a Map<Name, Term> as a TermMap annotation. Each Name key becomes a TermVariable.
wrapAnnotationMap :: M.Map Core.Name Core.Term -> Core.Term
wrapAnnotationMap m =
    Core.TermMap (Maps.fromList (Lists.map (\p -> (Core.TermVariable (Pairs.first p), (Pairs.second p))) (Maps.toList m)))
