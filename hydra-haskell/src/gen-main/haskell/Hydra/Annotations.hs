-- | Utilities for reading and writing type and term annotations

module Hydra.Annotations where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.CoreEncoding as CoreEncoding
import qualified Hydra.Flows as Flows
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Flows as Flows_
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Strip as Strip
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

aggregateAnnotations :: (Ord t1) => ((t3 -> Maybe t0) -> (t0 -> t3) -> (t0 -> M.Map t1 t2) -> t3 -> M.Map t1 t2)
aggregateAnnotations getValue getX getAnns t =  
  let toPairs = (\rest -> \t -> Optionals.maybe rest (\yy -> toPairs (Lists.cons (Maps.toList (getAnns yy)) rest) (getX yy)) (getValue t))
  in (Maps.fromList (Lists.concat (toPairs [] t)))

-- | Get the annotated type of a given term, if any
getTermType :: (Core.Term -> Maybe Core.Type)
getTermType x = case x of
  Core.TermAnnotated v1 -> (getTermType (Core.annotatedTermSubject v1))
  Core.TermTyped v1 -> (Just (Core.typedTermType v1))
  _ -> Nothing

-- | Normalize term annotations
normalizeTermAnnotations :: (Core.Term -> Core.Term)
normalizeTermAnnotations term =  
  let anns = (termAnnotationInternal term) 
      stripped = (Strip.stripTerm term)
  in (Logic.ifElse (Maps.null anns) stripped (Core.TermAnnotated (Core.AnnotatedTerm {
    Core.annotatedTermSubject = stripped,
    Core.annotatedTermAnnotation = anns})))

-- | Normalize type annotations
normalizeTypeAnnotations :: (Core.Type -> Core.Type)
normalizeTypeAnnotations typ =  
  let anns = (typeAnnotationInternal typ) 
      stripped = (Strip.stripType typ)
  in (Logic.ifElse (Maps.null anns) stripped (Core.TypeAnnotated (Core.AnnotatedType {
    Core.annotatedTypeSubject = stripped,
    Core.annotatedTypeAnnotation = anns})))

putAttr :: (Core.Name -> Core.Term -> Compute.Flow t0 ())
putAttr key val = (Compute.Flow (\s0 -> \t0 -> Compute.FlowState {
  Compute.flowStateValue = (Just ()),
  Compute.flowStateState = s0,
  Compute.flowStateTrace = Compute.Trace {
    Compute.traceStack = (Compute.traceStack t0),
    Compute.traceMessages = (Compute.traceMessages t0),
    Compute.traceOther = (Maps.insert key val (Compute.traceOther t0))}}))

putCount :: (Core.Name -> Int -> Compute.Flow t0 ())
putCount key count = (putAttr key (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 count))))

requireElementType :: (Graph.Element -> Compute.Flow t0 Core.Type)
requireElementType el =  
  let withType = (Optionals.maybe (Flows_.fail (Strings.cat [
          "missing type annotation for element ",
          (Core.unName (Graph.elementName el))])) (\t -> Flows_.pure t))
  in (withType (getTermType (Graph.elementTerm el)))

requireTermType :: (Core.Term -> Compute.Flow t0 Core.Type)
requireTermType =  
  let withType = (Optionals.maybe (Flows_.fail "missing type annotation") (\t -> Flows_.pure t))
  in (\arg_ -> withType (getTermType arg_))

resetCount :: (Core.Name -> Compute.Flow t0 ())
resetCount key = (putAttr key (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))))

setAnnotation :: (Ord t0) => (t0 -> Maybe t1 -> M.Map t0 t1 -> M.Map t0 t1)
setAnnotation key val m = (Maps.alter (\_ -> val) key m)

-- | Set description in annotations
setDescription :: (Maybe String -> M.Map Core.Name Core.Term -> M.Map Core.Name Core.Term)
setDescription d = (setAnnotation (Core.Name "description") (Optionals.map (\arg_ -> (\x -> Core.TermLiteral x) ((\x -> Core.LiteralString x) arg_)) d))

-- | Set term annotation
setTermAnnotation :: (Core.Name -> Maybe Core.Term -> Core.Term -> Core.Term)
setTermAnnotation key val term =  
  let term_ = (Strip.stripTerm term) 
      anns = (setAnnotation key val (termAnnotationInternal term))
  in (Logic.ifElse (Maps.null anns) term_ (Core.TermAnnotated (Core.AnnotatedTerm {
    Core.annotatedTermSubject = term_,
    Core.annotatedTermAnnotation = anns})))

-- | Set term description
setTermDescription :: (Maybe String -> Core.Term -> Core.Term)
setTermDescription d = (setTermAnnotation (Core.Name "description") (Optionals.map (\arg_ -> (\x -> Core.TermLiteral x) ((\x -> Core.LiteralString x) arg_)) d))

-- | Set term type
setTermType :: (Maybe Core.Type -> Core.Term -> Core.Term)
setTermType mtyp term =  
  let withoutType = (\term -> (\x -> case x of
          Core.TermAnnotated v1 -> (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermSubject = (withoutType (Core.annotatedTermSubject v1)),
            Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v1)}))
          Core.TermTyped v1 -> (Core.typedTermTerm v1)
          _ -> term) term)
  in (Optionals.maybe (withoutType term) (\typ -> Core.TermTyped (Core.TypedTerm {
    Core.typedTermTerm = (withoutType term),
    Core.typedTermType = typ})) mtyp)

-- | Set type in annotations
setType :: (Maybe Core.Type -> M.Map Core.Name Core.Term -> M.Map Core.Name Core.Term)
setType mt = (setAnnotation (Core.Name "type") (Optionals.map CoreEncoding.coreEncodeType mt))

-- | Set type annotation
setTypeAnnotation :: (Core.Name -> Maybe Core.Term -> Core.Type -> Core.Type)
setTypeAnnotation key val typ =  
  let typ_ = (Strip.stripType typ) 
      anns = (setAnnotation key val (typeAnnotationInternal typ))
  in (Logic.ifElse (Maps.null anns) typ_ (Core.TypeAnnotated (Core.AnnotatedType {
    Core.annotatedTypeSubject = typ_,
    Core.annotatedTypeAnnotation = anns})))

-- | Set type classes on term
setTypeClasses :: (M.Map Core.Name (S.Set Graph.TypeClass) -> Core.Term -> Core.Term)
setTypeClasses m =  
  let encodeClass = (\tc -> (\x -> case x of
          Graph.TypeClassEquality -> (Core.TermUnion (Core.Injection {
            Core.injectionTypeName = (Core.Name "hydra.graph.TypeClass"),
            Core.injectionField = Core.Field {
              Core.fieldName = (Core.Name "equality"),
              Core.fieldTerm = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "hydra.core.Unit"),
                Core.recordFields = []}))}}))
          Graph.TypeClassOrdering -> (Core.TermUnion (Core.Injection {
            Core.injectionTypeName = (Core.Name "hydra.graph.TypeClass"),
            Core.injectionField = Core.Field {
              Core.fieldName = (Core.Name "ordering"),
              Core.fieldTerm = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "hydra.core.Unit"),
                Core.recordFields = []}))}}))) tc) 
      encodePair = (\nameClasses ->  
              let name = (fst nameClasses) 
                  classes = (snd nameClasses)
              in (CoreEncoding.coreEncodeName name, (Core.TermSet (Sets.fromList (Lists.map encodeClass (Sets.toList classes))))))
      encoded = (Logic.ifElse (Maps.null m) Nothing (Just (Core.TermMap (Maps.fromList (Lists.map encodePair (Maps.toList m))))))
  in (setTermAnnotation (Core.Name "classes") encoded)

-- | Set type description
setTypeDescription :: (Maybe String -> Core.Type -> Core.Type)
setTypeDescription d = (setTypeAnnotation (Core.Name "description") (Optionals.map (\arg_ -> (\x -> Core.TermLiteral x) ((\x -> Core.LiteralString x) arg_)) d))

-- | Get internal term annotations
termAnnotationInternal :: (Core.Term -> M.Map Core.Name Core.Term)
termAnnotationInternal = (aggregateAnnotations getAnn Core.annotatedTermSubject Core.annotatedTermAnnotation) 
  where 
    getAnn = (\t -> (\x -> case x of
      Core.TermAnnotated v1 -> (Just v1)
      Core.TermTyped v1 -> (getAnn (Core.typedTermTerm v1))
      _ -> Nothing) t)

-- | Get internal type annotations
typeAnnotationInternal :: (Core.Type -> M.Map Core.Name Core.Term)
typeAnnotationInternal = (aggregateAnnotations getAnn Core.annotatedTypeSubject Core.annotatedTypeAnnotation) 
  where 
    getAnn = (\t -> (\x -> case x of
      Core.TypeAnnotated v1 -> (Just v1)
      _ -> Nothing) t)

withEmptyGraph :: (Compute.Flow Graph.Graph t1 -> Compute.Flow t0 t1)
withEmptyGraph = (Flows.withState Lexical.emptyGraph)
