-- | Simple, one-way adapters for types and terms

module Hydra.Adapt.Simple where

import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Inference as Inference
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Literals as Literals_
import qualified Hydra.Module as Module
import qualified Hydra.Reduction as Reduction
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Show.Core as Core_
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Attempt to adapt a floating-point type using the given language constraints
adaptFloatType :: (Coders.LanguageConstraints -> Core.FloatType -> Maybe Core.FloatType)
adaptFloatType constraints ft =  
  let supported = (Sets.member ft (Coders.languageConstraintsFloatTypes constraints))
  in  
    let alt = (adaptFloatType constraints)
    in (Logic.ifElse supported (Just ft) ((\x -> case x of
      Core.FloatTypeBigfloat -> Nothing
      Core.FloatTypeFloat32 -> (alt Core.FloatTypeFloat64)
      Core.FloatTypeFloat64 -> (alt Core.FloatTypeBigfloat)) ft))

-- | Adapt a graph and its schema to the given language constraints, prior to inference
adaptDataGraph :: (Coders.LanguageConstraints -> Bool -> Graph.Graph -> Compute.Flow Graph.Graph Graph.Graph)
adaptDataGraph constraints doExpand graph0 =  
  let litmap = (adaptLiteralTypesMap constraints)
  in  
    let els0 = (Graph.graphElements graph0)
    in  
      let env0 = (Graph.graphEnvironment graph0)
      in  
        let body0 = (Graph.graphBody graph0)
        in  
          let prims0 = (Graph.graphPrimitives graph0)
          in  
            let schema0 = (Graph.graphSchema graph0)
            in (Flows.bind (Optionals.maybe (Flows.pure Nothing) (\sg -> Flows.bind (Schemas.graphAsTypes sg) (\tmap0 -> Flows.bind (adaptGraphSchema constraints litmap tmap0) (\tmap1 ->  
              let emap = (Schemas.typesToElements tmap1)
              in (Flows.pure (Just (Graph.Graph {
                Graph.graphElements = emap,
                Graph.graphEnvironment = (Graph.graphEnvironment sg),
                Graph.graphTypes = (Graph.graphTypes sg),
                Graph.graphBody = (Graph.graphBody sg),
                Graph.graphPrimitives = (Graph.graphPrimitives sg),
                Graph.graphSchema = (Graph.graphSchema sg)})))))) schema0) (\schema1 ->  
              let gterm0 = (Schemas.graphAsTerm graph0)
              in  
                let gterm1 = (Logic.ifElse doExpand (Reduction.expandLambdas graph0 gterm0) gterm0)
                in (Flows.bind (adaptTerm constraints litmap gterm1) (\gterm2 ->  
                  let els1 = (Schemas.termAsGraph gterm1)
                  in (Flows.pure (Graph.Graph {
                    Graph.graphElements = els1,
                    Graph.graphEnvironment = env0,
                    Graph.graphTypes = Maps.empty,
                    Graph.graphBody = Core.TermUnit,
                    Graph.graphPrimitives = prims0,
                    Graph.graphSchema = schema1}))))))

adaptGraphSchema :: (Ord t0) => (Coders.LanguageConstraints -> M.Map Core.LiteralType Core.LiteralType -> M.Map t0 Core.Type -> Compute.Flow t1 (M.Map t0 Core.Type))
adaptGraphSchema constraints litmap types0 =  
  let mapPair = (\pair ->  
          let name = (fst pair)
          in  
            let typ = (snd pair)
            in (Flows.bind (adaptType constraints litmap typ) (\typ1 -> Flows.pure (name, typ1))))
  in (Flows.bind (Flows.mapList mapPair (Maps.toList types0)) (\pairs -> Flows.pure (Maps.fromList pairs)))

-- | Attempt to adapt an integer type using the given language constraints
adaptIntegerType :: (Coders.LanguageConstraints -> Core.IntegerType -> Maybe Core.IntegerType)
adaptIntegerType constraints it =  
  let supported = (Sets.member it (Coders.languageConstraintsIntegerTypes constraints))
  in  
    let alt = (adaptIntegerType constraints)
    in (Logic.ifElse supported (Just it) ((\x -> case x of
      Core.IntegerTypeBigint -> Nothing
      Core.IntegerTypeInt8 -> (alt Core.IntegerTypeUint16)
      Core.IntegerTypeInt16 -> (alt Core.IntegerTypeUint32)
      Core.IntegerTypeInt32 -> (alt Core.IntegerTypeUint64)
      Core.IntegerTypeInt64 -> (alt Core.IntegerTypeBigint)
      Core.IntegerTypeUint8 -> (alt Core.IntegerTypeInt16)
      Core.IntegerTypeUint16 -> (alt Core.IntegerTypeInt32)
      Core.IntegerTypeUint32 -> (alt Core.IntegerTypeInt64)
      Core.IntegerTypeUint64 -> (alt Core.IntegerTypeBigint)) it))

-- | Convert a literal to a different type
adaptLiteral :: (Core.LiteralType -> Core.Literal -> Core.Literal)
adaptLiteral lt l = ((\x -> case x of
  Core.LiteralBinary v1 -> ((\x -> case x of
    Core.LiteralTypeString -> (Core.LiteralString (Literals.binaryToString v1))) lt)
  Core.LiteralBoolean v1 -> ((\x -> case x of
    Core.LiteralTypeInteger v2 -> (Core.LiteralInteger (Literals_.bigintToIntegerValue v2 (Logic.ifElse v1 1 0)))) lt)
  Core.LiteralFloat v1 -> ((\x -> case x of
    Core.LiteralTypeFloat v2 -> (Core.LiteralFloat (Literals_.bigfloatToFloatValue v2 (Literals_.floatValueToBigfloat v1)))) lt)
  Core.LiteralInteger v1 -> ((\x -> case x of
    Core.LiteralTypeInteger v2 -> (Core.LiteralInteger (Literals_.bigintToIntegerValue v2 (Literals_.integerValueToBigint v1)))) lt)) l)

-- | Attempt to adapt a literal type using the given language constraints
adaptLiteralType :: (Coders.LanguageConstraints -> Core.LiteralType -> Maybe Core.LiteralType)
adaptLiteralType constraints lt =  
  let supported = (Sets.member (Variants.literalTypeVariant lt) (Coders.languageConstraintsLiteralVariants constraints))
  in (Logic.ifElse supported (Just lt) ((\x -> case x of
    Core.LiteralTypeBinary -> (adaptLiteralType constraints Core.LiteralTypeString)
    Core.LiteralTypeBoolean -> (Optionals.map (\x -> Core.LiteralTypeInteger x) (adaptIntegerType constraints Core.IntegerTypeInt8))
    Core.LiteralTypeFloat v1 -> (Optionals.map (\x -> Core.LiteralTypeFloat x) (adaptFloatType constraints v1))
    Core.LiteralTypeInteger v1 -> (Optionals.map (\x -> Core.LiteralTypeInteger x) (adaptIntegerType constraints v1))
    Core.LiteralTypeString -> Nothing) lt))

-- | Derive a map of adapted literal types for the given language constraints
adaptLiteralTypesMap :: (Coders.LanguageConstraints -> M.Map Core.LiteralType Core.LiteralType)
adaptLiteralTypesMap constraints =  
  let tryType = (\lt -> Optionals.maybe Nothing (\lt2 -> Just (lt, lt2)) (adaptLiteralType constraints lt))
  in (Maps.fromList (Optionals.cat (Lists.map tryType Variants.literalTypes)))

-- | Adapt a literal value using the given language constraints
adaptLiteralValue :: (M.Map Core.LiteralType Core.LiteralType -> Core.Literal -> Core.Literal)
adaptLiteralValue mapping l =  
  let lt = (Variants.literalType l)
  in (Optionals.maybe (Core.LiteralString (Core_.literal l)) (\lt2 -> adaptLiteral lt2 l) (Maps.lookup lt mapping))

-- | Adapt a term using the given language constraints
adaptTerm :: (Coders.LanguageConstraints -> M.Map Core.LiteralType Core.LiteralType -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
adaptTerm constraints mapping term0 =  
  let rewrite = (\recurse -> \term -> Flows.bind (recurse term) (\term1 ->  
          let tryTerm = (\term ->  
                  let supportedVariant = (Sets.member (Variants.termVariant term) (Coders.languageConstraintsTermVariants constraints))
                  in (Logic.ifElse supportedVariant ((\x -> case x of
                    Core.TermLiteral v1 -> (Flows.pure (Just (Core.TermLiteral (adaptLiteralValue mapping v1))))
                    _ -> (Flows.pure (Just term))) term) ( 
                    let tryAlts = (\alts -> Logic.ifElse (Lists.null alts) (Flows.pure Nothing) (Flows.bind (tryTerm (Lists.head alts)) (\mterm -> Optionals.maybe (tryAlts (Lists.tail alts)) (\t -> Flows.pure (Just t)) mterm)))
                    in (Flows.bind (termAlternatives term1) (\alts -> tryAlts alts)))))
          in (Flows.bind (tryTerm term1) (\mterm -> Optionals.maybe (Flows.fail (Strings.cat [
            "no alternatives for term: ",
            (Core_.term term)])) (\term2 -> Flows.pure term2) mterm))))
  in (Rewriting.rewriteTermM rewrite term0)

adaptType :: (Coders.LanguageConstraints -> M.Map Core.LiteralType Core.LiteralType -> Core.Type -> Compute.Flow t0 Core.Type)
adaptType constraints litmap type0 =  
  let rewrite = (\recurse -> \type_ -> Flows.bind (recurse type_) (\type1 ->  
          let tryType = (\type_ ->  
                  let supportedVariant = (Sets.member (Variants.typeVariant type_) (Coders.languageConstraintsTypeVariants constraints))
                  in (Logic.ifElse supportedVariant ((\x -> case x of
                    Core.TypeLiteral v1 -> (Optionals.maybe (Just (Core.TypeLiteral Core.LiteralTypeString)) (\lt2 -> Just (Core.TypeLiteral lt2)) (Maps.lookup v1 litmap))
                    _ -> (Just type_)) type_) ( 
                    let tryAlts = (\alts -> Logic.ifElse (Lists.null alts) Nothing (Optionals.maybe (tryAlts (Lists.tail alts)) (\t -> Just t) (tryType (Lists.head alts))))
                    in  
                      let alts = (typeAlternatives type1)
                      in (tryAlts alts))))
          in (Optionals.maybe (Flows.fail (Strings.cat [
            "no alternatives for type: ",
            (Core_.type_ type_)])) (\type2 -> Flows.pure type2) (tryType type1))))
  in (Rewriting.rewriteTypeM rewrite type0)

-- | Convert a graph to a list of type or term definitions, while adapting them to the given language constraints and performing type inference
graphToDefinitions :: (Coders.LanguageConstraints -> Bool -> Graph.Graph -> S.Set Core.Name -> Compute.Flow Graph.Graph (Graph.Graph, [Module.Definition]))
graphToDefinitions constraints doExpand graph names =  
  let ellist = (Maps.elems (Graph.graphElements graph))
  in  
    let isTypeElement = (\el -> (\x -> case x of
            Core.TermUnion v1 -> (Equality.equal (Core.injectionTypeName v1) (Core.Name "hydra.core.Type"))
            _ -> False) (Rewriting.deannotateTerm (Graph.elementTerm el)))
    in  
      let isSchemaGraph = (Logic.and (Logic.not (Lists.null ellist)) (isTypeElement (Lists.head ellist)))
      in (Logic.ifElse isSchemaGraph ( 
        let litmap = (adaptLiteralTypesMap constraints)
        in (Flows.bind (Schemas.graphAsTypes graph) (\tmap0 -> Flows.bind (adaptGraphSchema constraints litmap tmap0) (\tmap1 ->  
          let toDef = (\pair -> Module.DefinitionType (Module.TypeDefinition {
                  Module.typeDefinitionName = (fst pair),
                  Module.typeDefinitionType = (snd pair)}))
          in (Flows.pure (graph, (Lists.map toDef (Lists.filter (\p -> Sets.member (fst p) names) (Maps.toList tmap1))))))))) (Flows.bind (adaptDataGraph constraints doExpand graph) (\graph1 -> Flows.bind (Inference.inferGraphTypes graph1) (\graph2 ->  
        let toDef = (\el ->  
                let ts = (Optionals.fromJust (Graph.elementType el))
                in (Module.DefinitionTerm (Module.TermDefinition {
                  Module.termDefinitionName = (Graph.elementName el),
                  Module.termDefinitionTerm = (Graph.elementTerm el),
                  Module.termDefinitionType = (Inference.typeSchemeToFType ts)})))
        in (Flows.pure (graph2, (Lists.map toDef (Lists.filter (\e -> Sets.member (Graph.elementName e) names) (Maps.elems (Graph.graphElements graph2))))))))))

-- | Find a list of alternatives for a given term, if any
termAlternatives :: (Core.Term -> Compute.Flow Graph.Graph [Core.Term])
termAlternatives term = ((\x -> case x of
  Core.TermAnnotated v1 ->  
    let term2 = (Core.annotatedTermSubject v1)
    in (Flows.pure [
      term2])
  Core.TermOptional v1 -> (Flows.pure [
    Core.TermList (Optionals.maybe [] (\term2 -> [
      term2]) v1)])
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1)
    in  
      let field = (Core.injectionField v1)
      in  
        let fname = (Core.fieldName field)
        in  
          let fterm = (Core.fieldTerm field)
          in (Flows.bind (Schemas.requireUnionType tname) (\rt -> Flows.pure [
             
              let forFieldType = (\ft ->  
                      let ftname = (Core.fieldTypeName ft)
                      in Core.Field {
                        Core.fieldName = fname,
                        Core.fieldTerm = (Core.TermOptional (Logic.ifElse (Equality.equal ftname fname) (Just fterm) Nothing))})
              in  
                let fields = (Lists.map forFieldType (Core.rowTypeFields rt))
                in (Core.TermRecord (Core.Record {
                  Core.recordTypeName = tname,
                  Core.recordFields = fields}))]))
  Core.TermUnit -> (Flows.pure [
    Core.TermLiteral (Core.LiteralBoolean True)])
  Core.TermWrap v1 ->  
    let term2 = (Core.wrappedTermObject v1)
    in (Flows.pure [
      term2])
  _ -> (Flows.pure [])) term)

-- | Find a list of alternatives for a given type, if any
typeAlternatives :: (Core.Type -> [Core.Type])
typeAlternatives type_ = ((\x -> case x of
  Core.TypeAnnotated v1 ->  
    let type2 = (Core.annotatedTypeSubject v1)
    in [
      type2]
  Core.TypeOptional v1 -> [
    Core.TypeList v1]
  Core.TypeUnion v1 ->  
    let tname = (Core.rowTypeTypeName v1)
    in  
      let fields = (Core.rowTypeFields v1)
      in [
        Core.TypeRecord (Core.RowType {
          Core.rowTypeTypeName = tname,
          Core.rowTypeFields = fields})]
  Core.TypeUnit -> [
    Core.TypeLiteral Core.LiteralTypeBoolean]
  _ -> []) type_)
