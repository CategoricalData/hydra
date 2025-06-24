-- | Utilities for reading and writing type and term annotations

module Hydra.Annotations where

import qualified Hydra.Compute as Compute
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.CoreDecoding as CoreDecoding
import qualified Hydra.CoreEncoding as CoreEncoding
import qualified Hydra.Decode as Decode
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Flows as Flows
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows_
import qualified Hydra.Lib.Io as Io
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Strip as Strip
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

aggregateAnnotations :: (Ord t1) => ((t3 -> Maybe t0) -> (t0 -> t3) -> (t0 -> M.Map t1 t2) -> t3 -> M.Map t1 t2)
aggregateAnnotations getValue getX getAnns t =  
  let toPairs = (\rest -> \t -> Optionals.maybe rest (\yy -> toPairs (Lists.cons (Maps.toList (getAnns yy)) rest) (getX yy)) (getValue t))
  in (Maps.fromList (Lists.concat (toPairs [] t)))

debugIf :: (t0 -> String -> Compute.Flow t1 ())
debugIf debugId message =  
  let checkAndFail = (\desc -> Logic.ifElse (Equality.equal desc (Just "debugId")) (Flows_.fail message) (Flows_.pure ()))
  in (Flows_.bind getDebugId checkAndFail)

failOnFlag :: (Core.Name -> String -> Compute.Flow t0 ())
failOnFlag flag msg = (Flows_.bind (hasFlag flag) (\val -> Logic.ifElse val (Flows_.fail msg) (Flows_.pure ())))

getDebugId :: (Compute.Flow t0 (Maybe String))
getDebugId = (withEmptyGraph (Flows_.bind (getAttr Constants.key_debugId) (\desc -> Flows_.traverseOptional ExtractCore.string desc)))

getAttr :: (Core.Name -> Compute.Flow t0 (Maybe Core.Term))
getAttr key = (Compute.Flow (\s0 -> \t0 -> Compute.FlowState {
  Compute.flowStateValue = (Just (Maps.lookup key (Compute.traceOther t0))),
  Compute.flowStateState = s0,
  Compute.flowStateTrace = t0}))

getAttrWithDefault :: (Core.Name -> Core.Term -> Compute.Flow t0 Core.Term)
getAttrWithDefault key def = (Flows_.map (\mval -> Optionals.fromMaybe def mval) (getAttr key))

getCount :: (Core.Name -> Compute.Flow t0 Int)
getCount key = (withEmptyGraph (Flows_.bind (getAttrWithDefault key (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))) ExtractCore.int32))

-- | Get description from annotations map
getDescription :: (M.Map Core.Name Core.Term -> Compute.Flow Graph.Graph (Maybe String))
getDescription anns = (Optionals.maybe (Flows_.pure Nothing) (\term -> Flows_.map Optionals.pure (ExtractCore.string term)) (Maps.lookup (Core.Name "description") anns))

-- | Get a term annotation
getTermAnnotation :: (Core.Name -> Core.Term -> Maybe Core.Term)
getTermAnnotation key term = (Maps.lookup key (termAnnotationInternal term))

-- | Get term description
getTermDescription :: (Core.Term -> Compute.Flow Graph.Graph (Maybe String))
getTermDescription term = (getDescription (termAnnotationInternal term))

-- | Get type from annotations
getType :: (M.Map Core.Name Core.Term -> Compute.Flow Graph.Graph (Maybe Core.Type))
getType anns = (Optionals.maybe (Flows_.pure Nothing) (\dat -> Flows_.map Optionals.pure (CoreDecoding.coreDecodeType dat)) (Maps.lookup Constants.key_type anns))

-- | Get a type annotation
getTypeAnnotation :: (Core.Name -> Core.Type -> Maybe Core.Term)
getTypeAnnotation key typ = (Maps.lookup key (typeAnnotationInternal typ))

-- | Get type classes from term
getTypeClasses :: (Core.Term -> Compute.Flow Graph.Graph (M.Map Core.Name (S.Set Graph.TypeClass)))
getTypeClasses term =  
  let decodeClass = (\term ->  
          let byName = (Maps.fromList [
                  (Core.Name "equality", Graph.TypeClassEquality),
                  (Core.Name "ordering", Graph.TypeClassOrdering)])
          in (Flows_.bind (ExtractCore.unitVariant (Core.Name "hydra.graph.TypeClass") term) (\fn -> Optionals.maybe (Errors.unexpected "type class" (Io.showTerm term)) Flows_.pure (Maps.lookup fn byName))))
  in (Optionals.maybe (Flows_.pure Maps.empty) (\term -> ExtractCore.map_ CoreDecoding.coreDecodeName (ExtractCore.set decodeClass) term) (getTermAnnotation Constants.key_classes term))

-- | Get type description
getTypeDescription :: (Core.Type -> Compute.Flow Graph.Graph (Maybe String))
getTypeDescription typ = (getDescription (typeAnnotationInternal typ))

-- | For a typed term, decide whether a coder should encode it as a native type expression, or as a Hydra type expression.
isNativeType :: (Graph.Element -> Bool)
isNativeType el =  
  let isFlaggedAsFirstClassType = (Optionals.fromMaybe False (Optionals.bind (getTermAnnotation Constants.key_firstClassType (Graph.elementTerm el)) Decode.boolean))
  in (Optionals.maybe False (\ts -> Logic.and (Equality.equal ts (Core.TypeScheme {
    Core.typeSchemeVariables = [],
    Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.Type"))})) (Logic.not isFlaggedAsFirstClassType)) (Graph.elementType el))

hasDescription :: (M.Map Core.Name t0 -> Bool)
hasDescription anns = (Optionals.isJust (Maps.lookup Constants.key_description anns))

hasFlag :: (Core.Name -> Compute.Flow t0 Bool)
hasFlag flag = (withEmptyGraph (Flows_.bind (getAttrWithDefault flag (Core.TermLiteral (Core.LiteralBoolean False))) (\term -> ExtractCore.boolean term)))

-- | Check if type has description
hasTypeDescription :: (Core.Type -> Bool)
hasTypeDescription typ = (hasDescription (typeAnnotationInternal typ))

nextCount :: (Core.Name -> Compute.Flow t0 Int)
nextCount key = (Flows_.bind (getCount key) (\count -> Flows_.map (\_ -> count) (putCount key (Math.add count 1))))

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
  in (withType (Rewriting.getTermType (Graph.elementTerm el)))

requireTermType :: (Core.Term -> Compute.Flow t0 Core.Type)
requireTermType =  
  let withType = (Optionals.maybe (Flows_.fail "missing type annotation") (\t -> Flows_.pure t))
  in (\arg_ -> withType (Rewriting.getTermType arg_))

resetCount :: (Core.Name -> Compute.Flow t0 ())
resetCount key = (putAttr key (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))))

setAnnotation :: (Ord t0) => (t0 -> Maybe t1 -> M.Map t0 t1 -> M.Map t0 t1)
setAnnotation key val m = (Maps.alter (\_ -> val) key m)

-- | Set description in annotations
setDescription :: (Maybe String -> M.Map Core.Name Core.Term -> M.Map Core.Name Core.Term)
setDescription d = (setAnnotation Constants.key_description (Optionals.map (\arg_ -> (\x -> Core.TermLiteral x) ((\x -> Core.LiteralString x) arg_)) d))

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
setTermDescription d = (setTermAnnotation Constants.key_description (Optionals.map (\arg_ -> (\x -> Core.TermLiteral x) ((\x -> Core.LiteralString x) arg_)) d))

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
setType mt = (setAnnotation Constants.key_type (Optionals.map CoreEncoding.coreEncodeType mt))

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
  in (setTermAnnotation Constants.key_classes encoded)

-- | Set type description
setTypeDescription :: (Maybe String -> Core.Type -> Core.Type)
setTypeDescription d = (setTypeAnnotation Constants.key_description (Optionals.map (\arg_ -> (\x -> Core.TermLiteral x) ((\x -> Core.LiteralString x) arg_)) d))

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

-- | Create a type element with proper annotations
typeElement :: (Core.Name -> Core.Type -> Graph.Element)
typeElement name typ =  
  let schemaTerm = (Core.TermVariable (Core.Name "hydra.core.Type")) 
      dataTerm = (normalizeTermAnnotations (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermSubject = (CoreEncoding.coreEncodeType typ),
              Core.annotatedTermAnnotation = (Maps.fromList [
                (Constants.key_type, schemaTerm)])})))
  in Graph.Element {
    Graph.elementName = name,
    Graph.elementTerm = dataTerm,
    Graph.elementType = (Just (Core.TypeScheme {
      Core.typeSchemeVariables = [],
      Core.typeSchemeType = typ}))}

whenFlag :: (Core.Name -> Compute.Flow t1 t0 -> Compute.Flow t1 t0 -> Compute.Flow t1 t0)
whenFlag flag fthen felse = (Flows_.bind (hasFlag flag) (\b -> Logic.ifElse b fthen felse))

-- | Unshadow variables in term
unshadowVariables :: (Core.Term -> Core.Term)
unshadowVariables term =  
  let freshName = (Flows_.map (\n -> Core.Name (Strings.cat2 "s" (Literals.showInt32 n))) (nextCount (Core.Name "unshadow"))) 
      rewrite = (\recurse -> \term ->  
              let handleOther = (recurse term)
              in (Flows_.bind Errors.getState (\state ->  
                let reserved = (fst state) 
                    subst = (snd state)
                in ((\x -> case x of
                  Core.TermVariable v1 -> (Flows_.pure (Core.TermVariable (Optionals.fromMaybe v1 (Maps.lookup v1 subst))))
                  Core.TermFunction v1 -> ((\x -> case x of
                    Core.FunctionLambda v2 ->  
                      let v = (Core.lambdaParameter v2) 
                          d = (Core.lambdaDomain v2)
                          body = (Core.lambdaBody v2)
                      in (Logic.ifElse (Sets.member v reserved) (Flows_.bind freshName (\v_ -> Flows_.bind (Errors.putState (Sets.insert v_ reserved, (Maps.insert v v_ subst))) (\_ -> Flows_.bind (recurse body) (\body_ -> Flows_.bind (Errors.putState state) (\_ -> Flows_.pure (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = v_,
                        Core.lambdaDomain = d,
                        Core.lambdaBody = body_})))))))) (Flows_.bind (Errors.putState (Sets.insert v reserved, subst)) (\_ -> Flows_.map (\body_ -> Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = v,
                        Core.lambdaDomain = d,
                        Core.lambdaBody = body_}))) (recurse body))))
                    _ -> handleOther) v1)
                  _ -> handleOther) term))))
  in (Optionals.fromJust (Compute.flowStateValue (Compute.unFlow (Rewriting.rewriteTermM rewrite term) (Sets.empty, Maps.empty) Flows.emptyTrace)))

withDepth :: (Core.Name -> (Int -> Compute.Flow t1 t0) -> Compute.Flow t1 t0)
withDepth key f = (Flows_.bind (getCount key) (\count ->  
  let inc = (Math.add count 1)
  in (Flows_.bind (putCount key inc) (\_ -> Flows_.bind (f inc) (\r -> Flows_.bind (putCount key count) (\_ -> Flows_.pure r))))))

withEmptyGraph :: (Compute.Flow Graph.Graph t1 -> Compute.Flow t0 t1)
withEmptyGraph = (Flows.withState Lexical.emptyGraph)
