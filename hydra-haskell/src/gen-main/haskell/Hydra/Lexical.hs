-- Note: this is an automatically generated file. Do not edit.

-- | A module for lexical operations over graphs.

module Hydra.Lexical where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Monads as Monads
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Show.Core as Core_
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Build a Graph from element bindings, environment, and primitives
buildGraph :: ([Core.Binding] -> M.Map Core.Name (Maybe Core.Term) -> M.Map Core.Name Graph.Primitive -> Graph.Graph)
buildGraph elements environment primitives =  
  let elementTerms = (Maps.fromList (Lists.map (\b -> (Core.bindingName b, (Core.bindingTerm b))) elements))
  in  
    let letTerms = (Maps.map (\mt -> Maybes.fromJust mt) (Maps.filter (\mt -> Maybes.isJust mt) environment))
    in  
      let elementTypes = (Maps.fromList (Maybes.cat (Lists.map (\b -> Maybes.map (\ts -> (Core.bindingName b, ts)) (Core.bindingType b)) elements)))
      in Graph.Graph {
        Graph.graphBoundTerms = (Maps.union elementTerms letTerms),
        Graph.graphBoundTypes = elementTypes,
        Graph.graphClassConstraints = Maps.empty,
        Graph.graphLambdaVariables = (Sets.fromList (Maps.keys (Maps.filter (\mt -> Maybes.isNothing mt) environment))),
        Graph.graphMetadata = Maps.empty,
        Graph.graphPrimitives = primitives,
        Graph.graphSchemaTypes = Maps.empty,
        Graph.graphTypeVariables = Sets.empty}

chooseUniqueName :: (S.Set Core.Name -> Core.Name -> Core.Name)
chooseUniqueName reserved name =  
  let tryName = (\index ->  
          let candidate = (Logic.ifElse (Equality.equal index 1) name (Core.Name (Strings.cat2 (Core.unName name) (Literals.showInt32 index))))
          in (Logic.ifElse (Sets.member candidate reserved) (tryName (Math.add index 1)) candidate))
  in (tryName 1)

-- | Look up an element in the current graph context
dereferenceElement :: (Core.Name -> Compute.Flow Graph.Graph (Maybe Core.Binding))
dereferenceElement name = (Flows.map (\graph -> lookupElement graph name) Monads.getState)

-- | Resolve a schema type through a chain of zero or more typedefs
dereferenceSchemaType :: (Core.Name -> M.Map Core.Name Core.TypeScheme -> Maybe Core.TypeScheme)
dereferenceSchemaType name types =  
  let forType = (\t -> (\x -> case x of
          Core.TypeAnnotated v1 -> (forType (Core.annotatedTypeBody v1))
          Core.TypeForall v1 -> (Maybes.map (\ts -> Core.TypeScheme {
            Core.typeSchemeVariables = (Lists.cons (Core.forallTypeParameter v1) (Core.typeSchemeVariables ts)),
            Core.typeSchemeType = (Core.typeSchemeType ts),
            Core.typeSchemeConstraints = (Core.typeSchemeConstraints ts)}) (forType (Core.forallTypeBody v1)))
          Core.TypeVariable v1 -> (dereferenceSchemaType v1 types)
          _ -> (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = t,
            Core.typeSchemeConstraints = Nothing}))) t)
  in (Maybes.bind (Maps.lookup name types) (\ts -> Maybes.map (\ts2 -> Core.TypeScheme {
    Core.typeSchemeVariables = (Lists.concat2 (Core.typeSchemeVariables ts) (Core.typeSchemeVariables ts2)),
    Core.typeSchemeType = (Core.typeSchemeType ts2),
    Core.typeSchemeConstraints = (Core.typeSchemeConstraints ts2)}) (forType (Core.typeSchemeType ts))))

-- | Look up a binding by name in a graph, returning Either an error or the binding
dereferenceVariable :: (Graph.Graph -> Core.Name -> Either String Core.Binding)
dereferenceVariable graph name = (Maybes.maybe (Left (Strings.cat2 "no such element: " (Core.unName name))) (\right_ -> Right right_) (lookupElement graph name))

-- | Create a graph from a parent graph, schema types, and list of element bindings
elementsToGraph :: (Graph.Graph -> M.Map Core.Name Core.TypeScheme -> [Core.Binding] -> Graph.Graph)
elementsToGraph parent schemaTypes elements =  
  let prims = (Graph.graphPrimitives parent)
  in Graph.Graph {
    Graph.graphBoundTerms = (Graph.graphBoundTerms (buildGraph elements Maps.empty prims)),
    Graph.graphBoundTypes = (Graph.graphBoundTypes (buildGraph elements Maps.empty prims)),
    Graph.graphClassConstraints = (Graph.graphClassConstraints (buildGraph elements Maps.empty prims)),
    Graph.graphLambdaVariables = (Graph.graphLambdaVariables (buildGraph elements Maps.empty prims)),
    Graph.graphMetadata = (Graph.graphMetadata (buildGraph elements Maps.empty prims)),
    Graph.graphPrimitives = (Graph.graphPrimitives (buildGraph elements Maps.empty prims)),
    Graph.graphSchemaTypes = schemaTypes,
    Graph.graphTypeVariables = (Graph.graphTypeVariables (buildGraph elements Maps.empty prims))}

-- | An empty graph; no elements, no primitives, no schema.
emptyGraph :: Graph.Graph
emptyGraph = Graph.Graph {
  Graph.graphBoundTerms = Maps.empty,
  Graph.graphBoundTypes = Maps.empty,
  Graph.graphClassConstraints = Maps.empty,
  Graph.graphLambdaVariables = Sets.empty,
  Graph.graphMetadata = Maps.empty,
  Graph.graphPrimitives = Maps.empty,
  Graph.graphSchemaTypes = Maps.empty,
  Graph.graphTypeVariables = Sets.empty}

-- | Add bindings to an existing graph
extendGraphWithBindings :: ([Core.Binding] -> Graph.Graph -> Graph.Graph)
extendGraphWithBindings bindings g =  
  let newTerms = (Maps.fromList (Lists.map (\b -> (Core.bindingName b, (Core.bindingTerm b))) bindings))
  in  
    let newTypes = (Maps.fromList (Maybes.cat (Lists.map (\b -> Maybes.map (\ts -> (Core.bindingName b, ts)) (Core.bindingType b)) bindings)))
    in Graph.Graph {
      Graph.graphBoundTerms = (Maps.union newTerms (Graph.graphBoundTerms g)),
      Graph.graphBoundTypes = (Maps.union newTypes (Graph.graphBoundTypes g)),
      Graph.graphClassConstraints = (Graph.graphClassConstraints g),
      Graph.graphLambdaVariables = (Graph.graphLambdaVariables g),
      Graph.graphMetadata = (Graph.graphMetadata g),
      Graph.graphPrimitives = (Graph.graphPrimitives g),
      Graph.graphSchemaTypes = (Graph.graphSchemaTypes g),
      Graph.graphTypeVariables = (Graph.graphTypeVariables g)}

-- | Reconstruct a list of Bindings from a Graph's boundTerms and boundTypes
graphToBindings :: (Graph.Graph -> [Core.Binding])
graphToBindings g = (Lists.map (\p ->  
  let name = (Pairs.first p)
  in  
    let term = (Pairs.second p)
    in Core.Binding {
      Core.bindingName = name,
      Core.bindingTerm = term,
      Core.bindingType = (Maps.lookup name (Graph.graphBoundTypes g))}) (Maps.toList (Graph.graphBoundTerms g)))

-- | Extract the fields of a record or union type
fieldsOf :: (Core.Type -> [Core.FieldType])
fieldsOf t =  
  let stripped = (Rewriting.deannotateType t)
  in ((\x -> case x of
    Core.TypeForall v1 -> (fieldsOf (Core.forallTypeBody v1))
    Core.TypeRecord v1 -> (Core.rowTypeFields v1)
    Core.TypeUnion v1 -> (Core.rowTypeFields v1)
    _ -> []) stripped)

getField :: (M.Map Core.Name t0 -> Core.Name -> (t0 -> Compute.Flow t1 t2) -> Compute.Flow t1 t2)
getField m fname decode = (Maybes.maybe (Flows.fail (Strings.cat2 (Strings.cat2 "expected field " (Core.unName fname)) " not found")) decode (Maps.lookup fname m))

-- | Look up a binding in a graph by name
lookupElement :: (Graph.Graph -> Core.Name -> Maybe Core.Binding)
lookupElement graph name = (Maybes.map (\term -> Core.Binding {
  Core.bindingName = name,
  Core.bindingTerm = term,
  Core.bindingType = (Maps.lookup name (Graph.graphBoundTypes graph))}) (Maps.lookup name (Graph.graphBoundTerms graph)))

-- | Look up a primitive function in a graph by name
lookupPrimitive :: (Graph.Graph -> Core.Name -> Maybe Graph.Primitive)
lookupPrimitive graph name = (Maps.lookup name (Graph.graphPrimitives graph))

-- | Look up a term by name in a graph
lookupTerm :: (Graph.Graph -> Core.Name -> Maybe Core.Term)
lookupTerm graph name = (Maps.lookup name (Graph.graphBoundTerms graph))

matchEnum :: (Core.Name -> [(Core.Name, t0)] -> Core.Term -> Compute.Flow Graph.Graph t0)
matchEnum tname pairs = (matchUnion tname (Lists.map (\pair -> matchUnitField (Pairs.first pair) (Pairs.second pair)) pairs))

matchRecord :: ((M.Map Core.Name Core.Term -> Compute.Flow t0 t1) -> Core.Term -> Compute.Flow t0 t1)
matchRecord decode term =  
  let stripped = (Rewriting.deannotateAndDetypeTerm term)
  in ((\x -> case x of
    Core.TermRecord v1 -> (decode (Maps.fromList (Lists.map (\field -> (Core.fieldName field, (Core.fieldTerm field))) (Core.recordFields v1))))
    _ -> (Monads.unexpected "record" (Core_.term term))) stripped)

matchUnion :: (Core.Name -> [(Core.Name, (Core.Term -> Compute.Flow Graph.Graph t0))] -> Core.Term -> Compute.Flow Graph.Graph t0)
matchUnion tname pairs term =  
  let stripped = (Rewriting.deannotateAndDetypeTerm term)
  in  
    let mapping = (Maps.fromList pairs)
    in ((\x -> case x of
      Core.TermVariable v1 -> (Flows.bind (requireElement v1) (\el -> matchUnion tname pairs (Core.bindingTerm el)))
      Core.TermUnion v1 ->  
        let exp =  
                let fname = (Core.fieldName (Core.injectionField v1))
                in  
                  let val = (Core.fieldTerm (Core.injectionField v1))
                  in (Maybes.maybe (Flows.fail (Strings.cat2 (Strings.cat2 (Strings.cat2 "no matching case for field \"" (Core.unName fname)) "\" in union type ") (Core.unName tname))) (\f -> f val) (Maps.lookup fname mapping))
        in (Logic.ifElse (Equality.equal (Core.unName (Core.injectionTypeName v1)) (Core.unName tname)) exp (Monads.unexpected (Strings.cat2 "injection for type " (Core.unName tname)) (Core_.term term)))
      _ -> (Monads.unexpected (Strings.cat [
        "inject(",
        (Core.unName tname),
        ") with one of {",
        (Strings.intercalate ", " (Lists.map (\pair -> Core.unName (Pairs.first pair)) pairs)),
        "}"]) (Core_.term stripped))) stripped)

matchUnitField :: (t0 -> t1 -> (t0, (t2 -> Compute.Flow t3 t1)))
matchUnitField fname x = (fname, (\ignored -> Flows.pure x))

requireElement :: (Core.Name -> Compute.Flow Graph.Graph Core.Binding)
requireElement name =  
  let showAll = False
  in  
    let ellipsis = (\strings -> Logic.ifElse (Logic.and (Equality.gt (Lists.length strings) 3) (Logic.not showAll)) (Lists.concat2 (Lists.take 3 strings) [
            "..."]) strings)
    in  
      let err = (\graph -> Flows.fail (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 "no such element: " (Core.unName name)) ". Available elements: {") (Strings.intercalate ", " (ellipsis (Lists.map Core.unName (Maps.keys (Graph.graphBoundTerms graph)))))) "}"))
      in (Flows.bind (dereferenceElement name) (\mel -> Maybes.maybe (Flows.bind Monads.getState (\graph -> err graph)) Flows.pure mel))

requirePrimitive :: (Core.Name -> Compute.Flow Graph.Graph Graph.Primitive)
requirePrimitive name = (Flows.bind Monads.getState (\graph -> Maybes.maybe (Flows.fail (Strings.cat2 "no such primitive function: " (Core.unName name))) Flows.pure (lookupPrimitive graph name)))

requirePrimitiveType :: (Graph.Graph -> Core.Name -> Compute.Flow t0 Core.TypeScheme)
requirePrimitiveType tx name =  
  let mts = (Maps.lookup name (Maps.fromList (Lists.map (\_gpt_p -> (Graph.primitiveName _gpt_p, (Graph.primitiveType _gpt_p))) (Maps.elems (Graph.graphPrimitives tx)))))
  in (Maybes.maybe (Flows.fail (Strings.cat2 "no such primitive function: " (Core.unName name))) (\ts -> Flows.pure ts) mts)

requireTerm :: (Core.Name -> Compute.Flow Graph.Graph Core.Term)
requireTerm name = (Flows.bind (resolveTerm name) (\mt -> Maybes.maybe (Flows.fail (Strings.cat2 "no such element: " (Core.unName name))) Flows.pure mt))

-- | TODO: distinguish between lambda-bound and let-bound variables
resolveTerm :: (Core.Name -> Compute.Flow Graph.Graph (Maybe Core.Term))
resolveTerm name =  
  let recurse = (\term ->  
          let stripped = (Rewriting.deannotateTerm term)
          in ((\x -> case x of
            Core.TermVariable v1 -> (resolveTerm v1)
            _ -> (Flows.pure (Just term))) stripped))
  in (Flows.bind Monads.getState (\graph -> Maybes.maybe (Flows.pure Nothing) recurse (lookupTerm graph name)))

stripAndDereferenceTerm :: (Core.Term -> Compute.Flow Graph.Graph Core.Term)
stripAndDereferenceTerm term =  
  let stripped = (Rewriting.deannotateAndDetypeTerm term)
  in ((\x -> case x of
    Core.TermVariable v1 -> (Flows.bind (requireTerm v1) (\t -> stripAndDereferenceTerm t))
    _ -> (Flows.pure stripped)) stripped)

-- | Strip annotations and dereference variables, returning Either an error or the resolved term
stripAndDereferenceTermEither :: (Graph.Graph -> Core.Term -> Either String Core.Term)
stripAndDereferenceTermEither graph term =  
  let stripped = (Rewriting.deannotateAndDetypeTerm term)
  in ((\x -> case x of
    Core.TermVariable v1 -> (Eithers.either (\left_ -> Left left_) (\binding -> stripAndDereferenceTermEither graph (Core.bindingTerm binding)) (dereferenceVariable graph v1))
    _ -> (Right stripped)) stripped)

-- | Execute flow with empty graph
withEmptyGraph :: (Compute.Flow Graph.Graph t0 -> Compute.Flow t1 t0)
withEmptyGraph = (Monads.withState (Graph.Graph {
  Graph.graphBoundTerms = Maps.empty,
  Graph.graphBoundTypes = Maps.empty,
  Graph.graphClassConstraints = Maps.empty,
  Graph.graphLambdaVariables = Sets.empty,
  Graph.graphMetadata = Maps.empty,
  Graph.graphPrimitives = Maps.empty,
  Graph.graphSchemaTypes = Maps.empty,
  Graph.graphTypeVariables = Sets.empty}))
