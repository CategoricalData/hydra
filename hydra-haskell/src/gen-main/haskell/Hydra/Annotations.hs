-- Note: this is an automatically generated file. Do not edit.

-- | Utilities for reading and writing type and term annotations

module Hydra.Annotations where

import qualified Hydra.Classes as Classes
import qualified Hydra.Compute as Compute
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Encode.Core as Core__
import qualified Hydra.Extract.Core as Core___
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Monads as Monads
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Show.Core as Core____
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

aggregateAnnotations :: (Ord t2) => ((t0 -> Maybe t1) -> (t1 -> t0) -> (t1 -> M.Map t2 t3) -> t0 -> M.Map t2 t3)
aggregateAnnotations getValue getX getAnns t =  
  let toPairs = (\rest -> \t -> Maybes.maybe rest (\yy -> toPairs (Lists.cons (Maps.toList (getAnns yy)) rest) (getX yy)) (getValue t))
  in (Maps.fromList (Lists.concat (toPairs [] t)))

debugIf :: (t0 -> String -> Compute.Flow t1 ())
debugIf debugId message =  
  let checkAndFail = (\desc -> Logic.ifElse (Equality.equal desc (Just "debugId")) (Flows.fail message) (Flows.pure ()))
  in (Flows.bind getDebugId checkAndFail)

failOnFlag :: (Core.Name -> String -> Compute.Flow t0 ())
failOnFlag flag msg = (Flows.bind (hasFlag flag) (\val -> Logic.ifElse val (Flows.fail msg) (Flows.pure ())))

getDebugId :: (Compute.Flow t0 (Maybe String))
getDebugId = (Lexical.withEmptyGraph (Flows.bind (getAttr Constants.key_debugId) (\desc -> Flows.mapMaybe Core___.string desc)))

getAttr :: (Core.Name -> Compute.Flow t0 (Maybe Core.Term))
getAttr key = (Compute.Flow (\s0 -> \t0 -> Compute.FlowState {
  Compute.flowStateValue = (Just (Maps.lookup key (Compute.traceOther t0))),
  Compute.flowStateState = s0,
  Compute.flowStateTrace = t0}))

getAttrWithDefault :: (Core.Name -> Core.Term -> Compute.Flow t0 Core.Term)
getAttrWithDefault key def = (Flows.map (\mval -> Maybes.fromMaybe def mval) (getAttr key))

getCount :: (Core.Name -> Compute.Flow t0 Int)
getCount key = (Lexical.withEmptyGraph (Flows.bind (getAttrWithDefault key (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))) Core___.int32))

-- | Get description from annotations map
getDescription :: (M.Map Core.Name Core.Term -> Compute.Flow Graph.Graph (Maybe String))
getDescription anns = (Maybes.maybe (Flows.pure Nothing) (\term -> Flows.map Maybes.pure (Core___.string term)) (Maps.lookup (Core.Name "description") anns))

-- | Get a term annotation
getTermAnnotation :: (Core.Name -> Core.Term -> Maybe Core.Term)
getTermAnnotation key term = (Maps.lookup key (termAnnotationInternal term))

-- | Get term description
getTermDescription :: (Core.Term -> Compute.Flow Graph.Graph (Maybe String))
getTermDescription term = (getDescription (termAnnotationInternal term))

-- | Get type from annotations
getType :: (M.Map Core.Name Core.Term -> Compute.Flow Graph.Graph (Maybe Core.Type))
getType anns = (Maybes.maybe (Flows.pure Nothing) (\dat -> Flows.map Maybes.pure (Monads.withTrace "get type" (Core_.type_ dat))) (Maps.lookup Constants.key_type anns))

-- | Get a type annotation
getTypeAnnotation :: (Core.Name -> Core.Type -> Maybe Core.Term)
getTypeAnnotation key typ = (Maps.lookup key (typeAnnotationInternal typ))

-- | Get type classes from term
getTypeClasses :: (Core.Term -> Compute.Flow Graph.Graph (M.Map Core.Name (S.Set Classes.TypeClass)))
getTypeClasses term =  
  let decodeClass = (\term ->  
          let byName = (Maps.fromList [
                  (Core.Name "equality", Classes.TypeClassEquality),
                  (Core.Name "ordering", Classes.TypeClassOrdering)])
          in (Flows.bind (Core___.unitVariant (Core.Name "hydra.classes.TypeClass") term) (\fn -> Maybes.maybe (Monads.unexpected "type class" (Core____.term term)) Flows.pure (Maps.lookup fn byName))))
  in (Maybes.maybe (Flows.pure Maps.empty) (\term -> Core___.map Core_.name (Core___.setOf decodeClass) term) (getTermAnnotation Constants.key_classes term))

-- | Get type description
getTypeDescription :: (Core.Type -> Compute.Flow Graph.Graph (Maybe String))
getTypeDescription typ = (getDescription (typeAnnotationInternal typ))

-- | For a typed term, decide whether a coder should encode it as a native type expression, or as a Hydra type expression.
isNativeType :: (Core.Binding -> Bool)
isNativeType el =  
  let isFlaggedAsFirstClassType = (Maybes.fromMaybe False (Maybes.map (\_ -> True) (getTermAnnotation Constants.key_firstClassType (Core.bindingTerm el))))
  in (Maybes.maybe False (\ts -> Logic.and (Equality.equal ts (Core.TypeScheme {
    Core.typeSchemeVariables = [],
    Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.Type"))})) (Logic.not isFlaggedAsFirstClassType)) (Core.bindingType el))

hasDescription :: (M.Map Core.Name t0 -> Bool)
hasDescription anns = (Maybes.isJust (Maps.lookup Constants.key_description anns))

hasFlag :: (Core.Name -> Compute.Flow t0 Bool)
hasFlag flag = (Lexical.withEmptyGraph (Flows.bind (getAttrWithDefault flag (Core.TermLiteral (Core.LiteralBoolean False))) (\term -> Core___.boolean term)))

-- | Check if type has description
hasTypeDescription :: (Core.Type -> Bool)
hasTypeDescription typ = (hasDescription (typeAnnotationInternal typ))

nextCount :: (Core.Name -> Compute.Flow t0 Int)
nextCount key = (Flows.bind (getCount key) (\count -> Flows.map (\_ -> count) (putCount key (Math.add count 1))))

-- | Normalize term annotations
normalizeTermAnnotations :: (Core.Term -> Core.Term)
normalizeTermAnnotations term =  
  let anns = (termAnnotationInternal term)
  in  
    let stripped = (Rewriting.deannotateTerm term)
    in (Logic.ifElse (Maps.null anns) stripped (Core.TermAnnotated (Core.AnnotatedTerm {
      Core.annotatedTermBody = stripped,
      Core.annotatedTermAnnotation = anns})))

-- | Normalize type annotations
normalizeTypeAnnotations :: (Core.Type -> Core.Type)
normalizeTypeAnnotations typ =  
  let anns = (typeAnnotationInternal typ)
  in  
    let stripped = (Rewriting.deannotateType typ)
    in (Logic.ifElse (Maps.null anns) stripped (Core.TypeAnnotated (Core.AnnotatedType {
      Core.annotatedTypeBody = stripped,
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

resetCount :: (Core.Name -> Compute.Flow t0 ())
resetCount key = (putAttr key (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))))

setAnnotation :: (Ord t0) => (t0 -> Maybe t1 -> M.Map t0 t1 -> M.Map t0 t1)
setAnnotation key val m = (Maps.alter (\_ -> val) key m)

-- | Set description in annotations
setDescription :: (Maybe String -> M.Map Core.Name Core.Term -> M.Map Core.Name Core.Term)
setDescription d = (setAnnotation Constants.key_description (Maybes.map (\arg_ -> (\x -> Core.TermLiteral x) ((\x -> Core.LiteralString x) arg_)) d))

-- | Set term annotation
setTermAnnotation :: (Core.Name -> Maybe Core.Term -> Core.Term -> Core.Term)
setTermAnnotation key val term =  
  let term_ = (Rewriting.deannotateTerm term)
  in  
    let anns = (setAnnotation key val (termAnnotationInternal term))
    in (Logic.ifElse (Maps.null anns) term_ (Core.TermAnnotated (Core.AnnotatedTerm {
      Core.annotatedTermBody = term_,
      Core.annotatedTermAnnotation = anns})))

-- | Set term description
setTermDescription :: (Maybe String -> Core.Term -> Core.Term)
setTermDescription d = (setTermAnnotation Constants.key_description (Maybes.map (\s -> Core.TermLiteral (Core.LiteralString s)) d))

-- | Set type in annotations
setType :: (Maybe Core.Type -> M.Map Core.Name Core.Term -> M.Map Core.Name Core.Term)
setType mt = (setAnnotation Constants.key_type (Maybes.map Core__.type_ mt))

-- | Set type annotation
setTypeAnnotation :: (Core.Name -> Maybe Core.Term -> Core.Type -> Core.Type)
setTypeAnnotation key val typ =  
  let typ_ = (Rewriting.deannotateType typ)
  in  
    let anns = (setAnnotation key val (typeAnnotationInternal typ))
    in (Logic.ifElse (Maps.null anns) typ_ (Core.TypeAnnotated (Core.AnnotatedType {
      Core.annotatedTypeBody = typ_,
      Core.annotatedTypeAnnotation = anns})))

-- | Set type classes on term
setTypeClasses :: (M.Map Core.Name (S.Set Classes.TypeClass) -> Core.Term -> Core.Term)
setTypeClasses m term =  
  let encodeClass = (\tc -> (\x -> case x of
          Classes.TypeClassEquality -> (Core.TermUnion (Core.Injection {
            Core.injectionTypeName = (Core.Name "hydra.classes.TypeClass"),
            Core.injectionField = Core.Field {
              Core.fieldName = (Core.Name "equality"),
              Core.fieldTerm = Core.TermUnit}}))
          Classes.TypeClassOrdering -> (Core.TermUnion (Core.Injection {
            Core.injectionTypeName = (Core.Name "hydra.classes.TypeClass"),
            Core.injectionField = Core.Field {
              Core.fieldName = (Core.Name "ordering"),
              Core.fieldTerm = Core.TermUnit}}))) tc)
  in  
    let encodePair = (\nameClasses ->  
            let name = (fst nameClasses)
            in  
              let classes = (snd nameClasses)
              in (Core__.name name, (Core.TermSet (Sets.fromList (Lists.map encodeClass (Sets.toList classes))))))
    in  
      let encoded = (Logic.ifElse (Maps.null m) Nothing (Just (Core.TermMap (Maps.fromList (Lists.map encodePair (Maps.toList m))))))
      in (setTermAnnotation Constants.key_classes encoded term)

-- | Set type description
setTypeDescription :: (Maybe String -> Core.Type -> Core.Type)
setTypeDescription d = (setTypeAnnotation Constants.key_description (Maybes.map (\arg_ -> (\x -> Core.TermLiteral x) ((\x -> Core.LiteralString x) arg_)) d))

-- | Get internal term annotations
termAnnotationInternal :: (Core.Term -> M.Map Core.Name Core.Term)
termAnnotationInternal term =  
  let getAnn = (\t -> (\x -> case x of
          Core.TermAnnotated v1 -> (Just v1)
          _ -> Nothing) t)
  in (aggregateAnnotations getAnn (\at -> Core.annotatedTermBody at) (\at -> Core.annotatedTermAnnotation at) term)

-- | Get internal type annotations
typeAnnotationInternal :: (Core.Type -> M.Map Core.Name Core.Term)
typeAnnotationInternal typ =  
  let getAnn = (\t -> (\x -> case x of
          Core.TypeAnnotated v1 -> (Just v1)
          _ -> Nothing) t)
  in (aggregateAnnotations getAnn (\at -> Core.annotatedTypeBody at) (\at -> Core.annotatedTypeAnnotation at) typ)

-- | Create a type element with proper annotations
typeElement :: (Core.Name -> Core.Type -> Core.Binding)
typeElement name typ =  
  let schemaTerm = (Core.TermVariable (Core.Name "hydra.core.Type"))
  in  
    let dataTerm = (normalizeTermAnnotations (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core__.type_ typ),
            Core.annotatedTermAnnotation = (Maps.fromList [
              (Constants.key_type, schemaTerm)])})))
    in Core.Binding {
      Core.bindingName = name,
      Core.bindingTerm = dataTerm,
      Core.bindingType = (Just (Core.TypeScheme {
        Core.typeSchemeVariables = [],
        Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.Type"))}))}

whenFlag :: (Core.Name -> Compute.Flow t0 t1 -> Compute.Flow t0 t1 -> Compute.Flow t0 t1)
whenFlag flag fthen felse = (Flows.bind (hasFlag flag) (\b -> Logic.ifElse b fthen felse))

withDepth :: (Core.Name -> (Int -> Compute.Flow t0 t1) -> Compute.Flow t0 t1)
withDepth key f = (Flows.bind (getCount key) (\count ->  
  let inc = (Math.add count 1)
  in (Flows.bind (putCount key inc) (\_ -> Flows.bind (f inc) (\r -> Flows.bind (putCount key count) (\_ -> Flows.pure r))))))
