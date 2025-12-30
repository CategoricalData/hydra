-- Note: this is an automatically generated file. Do not edit.

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
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Literals as Literals_
import qualified Hydra.Module as Module
import qualified Hydra.Reduction as Reduction
import qualified Hydra.Reflect as Reflect
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Show.Core as ShowCore
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace (trace)

-- | Attempt to adapt a floating-point type using the given language constraints
adaptFloatType :: (Coders.LanguageConstraints -> Core.FloatType -> Maybe Core.FloatType)
adaptFloatType constraints ft =  
  let supported = (Sets.member ft (Coders.languageConstraintsFloatTypes constraints))
  in  
    let alt = (adaptFloatType constraints)
    in  
      let forUnsupported = (\ft -> (\x -> case x of
              Core.FloatTypeBigfloat -> (alt Core.FloatTypeFloat64)
              Core.FloatTypeFloat32 -> (alt Core.FloatTypeFloat64)
              Core.FloatTypeFloat64 -> (alt Core.FloatTypeBigfloat)) ft)
      in (Logic.ifElse supported (Just ft) (forUnsupported ft))

-- | Adapt a graph and its schema to the given language constraints, prior to inference. The doExpand flag controls eta expansion of partial applications. The doHoist flag controls hoisting of case statements to let bindings.
adaptDataGraph :: (Coders.LanguageConstraints -> Bool -> Bool -> Graph.Graph -> Compute.Flow Graph.Graph Graph.Graph)
adaptDataGraph constraints doExpand doHoist graph0 =
  let transform = (\graph -> \gterm -> Flows.bind (Schemas.graphToTypeContext graph) (\tx ->
          let gterm1 = (Rewriting.unshadowVariables gterm)
          in (Flows.bind (Logic.ifElse doExpand (Reduction.etaExpandTypedTerm tx gterm1) (Flows.pure gterm1)) (\gterm2 ->
            let gterm3 = (Logic.ifElse doHoist (Reduction.hoistCaseStatements tx gterm2) gterm2)
            in
              let gterm4 = (Rewriting.removeTypesFromTerm gterm3)
                  gterm5 = (Rewriting.liftLambdaAboveLet gterm4)
                  -- Debug: dump hydra.show.core.type after hoisting
                  debugEl = case gterm5 of
                    Core.TermLet lt ->
                      let bindings = Core.letBindings lt
                          targetBinding = L.find (\b -> Core.bindingName b == Core.Name "hydra.show.core.type") bindings
                      in case targetBinding of
                        Just b -> trace ("POST-HOIST hydra.show.core.type:\n" ++ ShowCore.term (Core.bindingTerm b)) ()
                        Nothing -> ()
                    _ -> ()
              in debugEl `seq` (Flows.pure gterm5)))))
  in  
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
              in (Flows.bind (Maybes.maybe (Flows.pure Nothing) (\sg -> Flows.bind (Schemas.graphAsTypes sg) (\tmap0 -> Flows.bind (adaptGraphSchema constraints litmap tmap0) (\tmap1 ->  
                let emap = (Schemas.typesToElements tmap1)
                in (Flows.pure (Just (Graph.Graph {
                  Graph.graphElements = emap,
                  Graph.graphEnvironment = (Graph.graphEnvironment sg),
                  Graph.graphTypes = (Graph.graphTypes sg),
                  Graph.graphBody = (Graph.graphBody sg),
                  Graph.graphPrimitives = (Graph.graphPrimitives sg),
                  Graph.graphSchema = (Graph.graphSchema sg)})))))) schema0) (\schema1 ->  
                let gterm0 = (Schemas.graphAsTerm graph0)
                in (Flows.bind (Logic.ifElse (Logic.or doExpand doHoist) (transform graph0 gterm0) (Flows.pure gterm0)) (\gterm1 -> Flows.bind (adaptTerm constraints litmap gterm1) (\gterm2 ->  
                  let els1Raw = (Schemas.termAsGraph gterm2)
                  in (Flows.bind (Flows.mapElems (adaptPrimitive constraints litmap) prims0) (\prims1 ->  
                    let originalConstraints = (Maps.fromList (Maybes.cat (Lists.map (\el -> Maybes.bind (Core.bindingType el) (\ts -> Maybes.map (\c -> (Core.bindingName el, c)) (Core.typeSchemeConstraints ts))) (Maps.elems els0))))
                    in  
                      let mergeConstraints = (\el ->  
                              let bname = (Core.bindingName el)
                              in  
                                let origConstraints = (Maps.lookup bname originalConstraints)
                                in (Maybes.maybe el (\origC -> Maybes.maybe (Core.Binding {
                                  Core.bindingName = bname,
                                  Core.bindingTerm = (Core.bindingTerm el),
                                  Core.bindingType = (Just (Core.TypeScheme {
                                    Core.typeSchemeVariables = [],
                                    Core.typeSchemeType = (Core.TypeVariable (Core.Name "a")),
                                    Core.typeSchemeConstraints = (Just origC)}))}) (\ts ->  
                                  let inferredC = (Core.typeSchemeConstraints ts)
                                  in  
                                    let mergedC = (Maybes.maybe (Just origC) (\infC -> Just (Maps.union origC infC)) inferredC)
                                    in Core.Binding {
                                      Core.bindingName = bname,
                                      Core.bindingTerm = (Core.bindingTerm el),
                                      Core.bindingType = (Just (Core.TypeScheme {
                                        Core.typeSchemeVariables = (Core.typeSchemeVariables ts),
                                        Core.typeSchemeType = (Core.typeSchemeType ts),
                                        Core.typeSchemeConstraints = mergedC}))}) (Core.bindingType el)) origConstraints))
                      in  
                        let els1 = (Maps.fromList (Lists.map (\el -> (Core.bindingName el, (mergeConstraints el))) (Maps.elems els1Raw)))
                        in (Flows.pure (Graph.Graph {
                          Graph.graphElements = els1,
                          Graph.graphEnvironment = env0,
                          Graph.graphTypes = Maps.empty,
                          Graph.graphBody = Core.TermUnit,
                          Graph.graphPrimitives = prims1,
                          Graph.graphSchema = schema1})))))))))

adaptGraphSchema :: (Ord t0) => (Coders.LanguageConstraints -> M.Map Core.LiteralType Core.LiteralType -> M.Map t0 Core.Type -> Compute.Flow t1 (M.Map t0 Core.Type))
adaptGraphSchema constraints litmap types0 =  
  let mapPair = (\pair ->  
          let name = (Pairs.first pair)
          in  
            let typ = (Pairs.second pair)
            in (Flows.bind (adaptType constraints litmap typ) (\typ1 -> Flows.pure (name, typ1))))
  in (Flows.bind (Flows.mapList mapPair (Maps.toList types0)) (\pairs -> Flows.pure (Maps.fromList pairs)))

-- | Attempt to adapt an integer type using the given language constraints
adaptIntegerType :: (Coders.LanguageConstraints -> Core.IntegerType -> Maybe Core.IntegerType)
adaptIntegerType constraints it =  
  let supported = (Sets.member it (Coders.languageConstraintsIntegerTypes constraints))
  in  
    let alt = (adaptIntegerType constraints)
    in  
      let forUnsupported = (\it -> (\x -> case x of
              Core.IntegerTypeBigint -> Nothing
              Core.IntegerTypeInt8 -> (alt Core.IntegerTypeUint16)
              Core.IntegerTypeInt16 -> (alt Core.IntegerTypeUint32)
              Core.IntegerTypeInt32 -> (alt Core.IntegerTypeUint64)
              Core.IntegerTypeInt64 -> (alt Core.IntegerTypeBigint)
              Core.IntegerTypeUint8 -> (alt Core.IntegerTypeInt16)
              Core.IntegerTypeUint16 -> (alt Core.IntegerTypeInt32)
              Core.IntegerTypeUint32 -> (alt Core.IntegerTypeInt64)
              Core.IntegerTypeUint64 -> (alt Core.IntegerTypeBigint)) it)
      in (Logic.ifElse supported (Just it) (forUnsupported it))

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
  let forUnsupported = (\lt -> (\x -> case x of
          Core.LiteralTypeBinary -> (Just Core.LiteralTypeString)
          Core.LiteralTypeBoolean -> (Maybes.map (\x -> Core.LiteralTypeInteger x) (adaptIntegerType constraints Core.IntegerTypeInt8))
          Core.LiteralTypeFloat v1 -> (Maybes.map (\x -> Core.LiteralTypeFloat x) (adaptFloatType constraints v1))
          Core.LiteralTypeInteger v1 -> (Maybes.map (\x -> Core.LiteralTypeInteger x) (adaptIntegerType constraints v1))
          _ -> Nothing) lt)
  in (Logic.ifElse (literalTypeSupported constraints lt) Nothing (forUnsupported lt))

-- | Derive a map of adapted literal types for the given language constraints
adaptLiteralTypesMap :: (Coders.LanguageConstraints -> M.Map Core.LiteralType Core.LiteralType)
adaptLiteralTypesMap constraints =  
  let tryType = (\lt -> Maybes.maybe Nothing (\lt2 -> Just (lt, lt2)) (adaptLiteralType constraints lt))
  in (Maps.fromList (Maybes.cat (Lists.map tryType Reflect.literalTypes)))

adaptLiteralValue :: (Ord t0) => (M.Map t0 Core.LiteralType -> t0 -> Core.Literal -> Core.Literal)
adaptLiteralValue litmap lt l = (Maybes.maybe (Core.LiteralString (ShowCore.literal l)) (\lt2 -> adaptLiteral lt2 l) (Maps.lookup lt litmap))

adaptPrimitive :: (Coders.LanguageConstraints -> M.Map Core.LiteralType Core.LiteralType -> Graph.Primitive -> Compute.Flow t0 Graph.Primitive)
adaptPrimitive constraints litmap prim0 =  
  let ts0 = (Graph.primitiveType prim0)
  in (Flows.bind (adaptTypeScheme constraints litmap ts0) (\ts1 -> Flows.pure (Graph.Primitive {
    Graph.primitiveName = (Graph.primitiveName prim0),
    Graph.primitiveType = ts1,
    Graph.primitiveImplementation = (Graph.primitiveImplementation prim0)})))

-- | Adapt a term using the given language constraints
adaptTerm :: (Coders.LanguageConstraints -> M.Map Core.LiteralType Core.LiteralType -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
adaptTerm constraints litmap term0 =  
  let rewrite = (\recurse -> \term0 ->  
          let forSupported = (\term -> (\x -> case x of
                  Core.TermLiteral v1 ->  
                    let lt = (Reflect.literalType v1)
                    in (Flows.pure (Just (Logic.ifElse (literalTypeSupported constraints lt) term (Core.TermLiteral (adaptLiteralValue litmap lt v1)))))
                  _ -> (Flows.pure (Just term))) term) 
              forUnsupported = (\term ->  
                      let forNonNull = (\alts -> Flows.bind (tryTerm (Lists.head alts)) (\mterm -> Maybes.maybe (tryAlts (Lists.tail alts)) (\t -> Flows.pure (Just t)) mterm)) 
                          tryAlts = (\alts -> Logic.ifElse (Lists.null alts) (Flows.pure Nothing) (forNonNull alts))
                      in (Flows.bind (termAlternatives term) (\alts -> tryAlts alts)))
              tryTerm = (\term ->  
                      let supportedVariant = (Sets.member (Reflect.termVariant term) (Coders.languageConstraintsTermVariants constraints))
                      in (Logic.ifElse supportedVariant (forSupported term) (forUnsupported term)))
          in (Flows.bind (recurse term0) (\term1 -> Flows.bind (tryTerm term1) (\mterm -> Maybes.maybe (Flows.fail (Strings.cat2 "no alternatives for term: " (ShowCore.term term1))) (\term2 -> Flows.pure term2) mterm))))
  in (Rewriting.rewriteTermM rewrite term0)

adaptType :: (Coders.LanguageConstraints -> M.Map Core.LiteralType Core.LiteralType -> Core.Type -> Compute.Flow t0 Core.Type)
adaptType constraints litmap type0 =  
  let forSupported = (\typ -> (\x -> case x of
          Core.TypeLiteral v1 -> (Logic.ifElse (literalTypeSupported constraints v1) (Just typ) (Maybes.maybe (Just (Core.TypeLiteral Core.LiteralTypeString)) (\lt2 -> Just (Core.TypeLiteral lt2)) (Maps.lookup v1 litmap)))
          _ -> (Just typ)) typ) 
      forUnsupported = (\typ ->  
              let tryAlts = (\alts -> Logic.ifElse (Lists.null alts) Nothing (Maybes.maybe (tryAlts (Lists.tail alts)) (\t -> Just t) (tryType (Lists.head alts))))
              in  
                let alts = (typeAlternatives typ)
                in (tryAlts alts))
      tryType = (\typ ->  
              let supportedVariant = (Sets.member (Reflect.typeVariant typ) (Coders.languageConstraintsTypeVariants constraints))
              in (Logic.ifElse supportedVariant (forSupported typ) (forUnsupported typ)))
  in  
    let rewrite = (\recurse -> \typ -> Flows.bind (recurse typ) (\type1 -> Maybes.maybe (Flows.fail (Strings.cat2 "no alternatives for type: " (ShowCore.type_ typ))) (\type2 -> Flows.pure type2) (tryType type1)))
    in (Rewriting.rewriteTypeM rewrite type0)

adaptTypeScheme :: (Coders.LanguageConstraints -> M.Map Core.LiteralType Core.LiteralType -> Core.TypeScheme -> Compute.Flow t0 Core.TypeScheme)
adaptTypeScheme constraints litmap ts0 =  
  let vars0 = (Core.typeSchemeVariables ts0)
  in  
    let t0 = (Core.typeSchemeType ts0)
    in (Flows.bind (adaptType constraints litmap t0) (\t1 -> Flows.pure (Core.TypeScheme {
      Core.typeSchemeVariables = vars0,
      Core.typeSchemeType = t1,
      Core.typeSchemeConstraints = (Core.typeSchemeConstraints ts0)})))

-- | Given a data graph along with language constraints and a designated list of element names, adapt the graph to the language constraints, perform inference, then return a corresponding term definition for each element name. The doExpand flag controls eta expansion; doHoist controls case statement hoisting.
dataGraphToDefinitions :: (Coders.LanguageConstraints -> Bool -> Bool -> Graph.Graph -> [[Core.Name]] -> Compute.Flow Graph.Graph (Graph.Graph, [[Module.TermDefinition]]))
dataGraphToDefinitions constraints doExpand doHoist graph nameLists = (Flows.bind (Logic.ifElse (Logic.or doExpand doHoist) (Inference.inferGraphTypes graph) (Flows.pure graph)) (\graphi -> Flows.bind (adaptDataGraph constraints doExpand doHoist graphi) (\graph1 -> Flows.bind (Inference.inferGraphTypes graph1) (\graph2 ->  
  let toDef = (\el ->  
          let ts = (Maybes.fromJust (Core.bindingType el))
          in Module.TermDefinition {
            Module.termDefinitionName = (Core.bindingName el),
            Module.termDefinitionTerm = (Core.bindingTerm el),
            Module.termDefinitionType = ts})
  in (Flows.pure (graph2, (Lists.map (\names -> Lists.map toDef (Lists.map (\n -> Maybes.fromJust (Maps.lookup n (Graph.graphElements graph2))) names)) nameLists)))))))

-- | Check if a literal type is supported by the given language constraints
literalTypeSupported :: (Coders.LanguageConstraints -> Core.LiteralType -> Bool)
literalTypeSupported constraints lt =  
  let forType = (\lt -> (\x -> case x of
          Core.LiteralTypeFloat v1 -> (Sets.member v1 (Coders.languageConstraintsFloatTypes constraints))
          Core.LiteralTypeInteger v1 -> (Sets.member v1 (Coders.languageConstraintsIntegerTypes constraints))
          _ -> True) lt)
  in (Logic.ifElse (Sets.member (Reflect.literalTypeVariant lt) (Coders.languageConstraintsLiteralVariants constraints)) (forType lt) False)

-- | Given a schema graph along with language constraints and a designated list of element names, adapt the graph to the language constraints, then return a corresponding type definition for each element name.
schemaGraphToDefinitions :: (Coders.LanguageConstraints -> Graph.Graph -> [[Core.Name]] -> Compute.Flow Graph.Graph (M.Map Core.Name Core.Type, [[Module.TypeDefinition]]))
schemaGraphToDefinitions constraints graph nameLists =  
  let litmap = (adaptLiteralTypesMap constraints)
  in (Flows.bind (Schemas.graphAsTypes graph) (\tmap0 -> Flows.bind (adaptGraphSchema constraints litmap tmap0) (\tmap1 ->  
    let toDef = (\pair -> Module.TypeDefinition {
            Module.typeDefinitionName = (Pairs.first pair),
            Module.typeDefinitionType = (Pairs.second pair)})
    in (Flows.pure (tmap1, (Lists.map (\names -> Lists.map toDef (Lists.map (\n -> (n, (Maybes.fromJust (Maps.lookup n tmap1)))) names)) nameLists))))))

-- | Find a list of alternatives for a given term, if any
termAlternatives :: (Core.Term -> Compute.Flow Graph.Graph [Core.Term])
termAlternatives term = ((\x -> case x of
  Core.TermAnnotated v1 ->  
    let term2 = (Core.annotatedTermBody v1)
    in (Flows.pure [
      term2])
  Core.TermMaybe v1 -> (Flows.pure [
    Core.TermList (Maybes.maybe [] (\term2 -> [
      term2]) v1)])
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1)
    in  
      let field = (Core.injectionField v1)
      in  
        let fname = (Core.fieldName field)
        in  
          let fterm = (Core.fieldTerm field)
          in  
            let forFieldType = (\ft ->  
                    let ftname = (Core.fieldTypeName ft)
                    in Core.Field {
                      Core.fieldName = fname,
                      Core.fieldTerm = (Core.TermMaybe (Logic.ifElse (Equality.equal ftname fname) (Just fterm) Nothing))})
            in (Flows.bind (Schemas.requireUnionType tname) (\rt -> Flows.pure [
              Core.TermRecord (Core.Record {
                Core.recordTypeName = tname,
                Core.recordFields = (Lists.map forFieldType (Core.rowTypeFields rt))})]))
  Core.TermUnit -> (Flows.pure [
    Core.TermLiteral (Core.LiteralBoolean True)])
  Core.TermWrap v1 ->  
    let term2 = (Core.wrappedTermBody v1)
    in (Flows.pure [
      term2])
  _ -> (Flows.pure [])) term)

-- | Find a list of alternatives for a given type, if any
typeAlternatives :: (Core.Type -> [Core.Type])
typeAlternatives type_ = ((\x -> case x of
  Core.TypeAnnotated v1 ->  
    let type2 = (Core.annotatedTypeBody v1)
    in [
      type2]
  Core.TypeMaybe v1 -> [
    Core.TypeList v1]
  Core.TypeUnion v1 ->  
    let tname = (Core.rowTypeTypeName v1)
    in  
      let fields = (Core.rowTypeFields v1)
      in  
        let toOptField = (\f -> Core.FieldType {
                Core.fieldTypeName = (Core.fieldTypeName f),
                Core.fieldTypeType = (Core.TypeMaybe (Core.fieldTypeType f))})
        in  
          let optFields = (Lists.map toOptField fields)
          in [
            Core.TypeRecord (Core.RowType {
              Core.rowTypeTypeName = tname,
              Core.rowTypeFields = optFields})]
  Core.TypeUnit -> [
    Core.TypeLiteral Core.LiteralTypeBoolean]
  _ -> []) type_)
