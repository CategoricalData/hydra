-- Note: this is an automatically generated file. Do not edit.

-- | A module for lexical operations over graphs.

module Hydra.Lexical where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as Core_
import qualified Hydra.Strip as Strip
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M
import qualified Data.Set as S

-- | Build a Graph from element bindings, environment, and primitives
buildGraph :: [Core.Binding] -> M.Map Core.Name (Maybe Core.Term) -> M.Map Core.Name Graph.Primitive -> Graph.Graph
buildGraph elements environment primitives =

      let elementTerms = Maps.fromList (Lists.map (\b -> (Core.bindingName b, (Core.bindingTerm b))) elements)
          letTerms = Maps.map (\mt -> Maybes.fromJust mt) (Maps.filter (\mt -> Maybes.isJust mt) environment)
          elementTypes =
                  Maps.fromList (Maybes.cat (Lists.map (\b -> Maybes.map (\ts -> (Core.bindingName b, ts)) (Core.bindingType b)) elements))
      in Graph.Graph {
        Graph.graphBoundTerms = (Maps.union elementTerms letTerms),
        Graph.graphBoundTypes = elementTypes,
        Graph.graphClassConstraints = Maps.empty,
        Graph.graphLambdaVariables = (Sets.fromList (Maps.keys (Maps.filter (\mt -> Maybes.isNothing mt) environment))),
        Graph.graphMetadata = Maps.empty,
        Graph.graphPrimitives = primitives,
        Graph.graphSchemaTypes = Maps.empty,
        Graph.graphTypeVariables = Sets.empty}

chooseUniqueName :: S.Set Core.Name -> Core.Name -> Core.Name
chooseUniqueName reserved name =

      let tryName =
              \index ->
                let candidate = Logic.ifElse (Equality.equal index 1) name (Core.Name (Strings.cat2 (Core.unName name) (Literals.showInt32 index)))
                in (Logic.ifElse (Sets.member candidate reserved) (tryName (Math.add index 1)) candidate)
      in (tryName 1)

-- | Look up an element in a graph
dereferenceElement :: Graph.Graph -> Core.Name -> Maybe Core.Binding
dereferenceElement graph name = lookupElement graph name

-- | Resolve a schema type through a chain of zero or more typedefs
dereferenceSchemaType :: Core.Name -> M.Map Core.Name Core.TypeScheme -> Maybe Core.TypeScheme
dereferenceSchemaType name types =

      let forType =
              \t -> case t of
                Core.TypeAnnotated v0 -> forType (Core.annotatedTypeBody v0)
                Core.TypeForall v0 -> Maybes.map (\ts -> Core.TypeScheme {
                  Core.typeSchemeVariables = (Lists.cons (Core.forallTypeParameter v0) (Core.typeSchemeVariables ts)),
                  Core.typeSchemeType = (Core.typeSchemeType ts),
                  Core.typeSchemeConstraints = (Core.typeSchemeConstraints ts)}) (forType (Core.forallTypeBody v0))
                Core.TypeVariable v0 -> dereferenceSchemaType v0 types
                _ -> Just (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = t,
                  Core.typeSchemeConstraints = Nothing})
      in (Maybes.bind (Maps.lookup name types) (\ts -> Maybes.map (\ts2 -> Core.TypeScheme {
        Core.typeSchemeVariables = (Lists.concat2 (Core.typeSchemeVariables ts) (Core.typeSchemeVariables ts2)),
        Core.typeSchemeType = (Core.typeSchemeType ts2),
        Core.typeSchemeConstraints = (Core.typeSchemeConstraints ts2)}) (forType (Core.typeSchemeType ts))))

-- | Look up a binding by name in a graph, returning Either an error or the binding
dereferenceVariable :: Graph.Graph -> Core.Name -> Either String Core.Binding
dereferenceVariable graph name =
    Maybes.maybe (Left (Strings.cat2 "no such element: " (Core.unName name))) (\right_ -> Right right_) (lookupElement graph name)

-- | Create a graph from a parent graph, schema types, and list of element bindings
elementsToGraph :: Graph.Graph -> M.Map Core.Name Core.TypeScheme -> [Core.Binding] -> Graph.Graph
elementsToGraph parent schemaTypes elements =

      let prims = Graph.graphPrimitives parent
          g = buildGraph elements Maps.empty prims
      in Graph.Graph {
        Graph.graphBoundTerms = (Graph.graphBoundTerms g),
        Graph.graphBoundTypes = (Graph.graphBoundTypes g),
        Graph.graphClassConstraints = (Graph.graphClassConstraints g),
        Graph.graphLambdaVariables = (Graph.graphLambdaVariables g),
        Graph.graphMetadata = (Graph.graphMetadata g),
        Graph.graphPrimitives = (Graph.graphPrimitives g),
        Graph.graphSchemaTypes = schemaTypes,
        Graph.graphTypeVariables = (Graph.graphTypeVariables g)}

-- | An empty context; no trace, no messages, no other data.
emptyContext :: Context.Context
emptyContext =
    Context.Context {
      Context.contextTrace = [],
      Context.contextMessages = [],
      Context.contextOther = Maps.empty}

-- | An empty graph; no elements, no primitives, no schema.
emptyGraph :: Graph.Graph
emptyGraph =
    Graph.Graph {
      Graph.graphBoundTerms = Maps.empty,
      Graph.graphBoundTypes = Maps.empty,
      Graph.graphClassConstraints = Maps.empty,
      Graph.graphLambdaVariables = Sets.empty,
      Graph.graphMetadata = Maps.empty,
      Graph.graphPrimitives = Maps.empty,
      Graph.graphSchemaTypes = Maps.empty,
      Graph.graphTypeVariables = Sets.empty}

-- | Extract the fields of a record or union type
fieldsOf :: Core.Type -> [Core.FieldType]
fieldsOf t =

      let stripped = Strip.deannotateType t
      in case stripped of
        Core.TypeForall v0 -> fieldsOf (Core.forallTypeBody v0)
        Core.TypeRecord v0 -> v0
        Core.TypeUnion v0 -> v0
        _ -> []

getField :: Context.Context -> M.Map Core.Name t0 -> Core.Name -> (t0 -> Either (Context.InContext Errors.Error) t1) -> Either (Context.InContext Errors.Error) t1
getField cx m fname decode =
    Maybes.maybe (Left (Context.InContext {
      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 "expected field " (Core.unName fname)) " not found"))),
      Context.inContextContext = cx})) decode (Maps.lookup fname m)

-- | Reconstruct a list of Bindings from a Graph's boundTerms and boundTypes
graphToBindings :: Graph.Graph -> [Core.Binding]
graphToBindings g =
    Lists.map (\p ->
      let name = Pairs.first p
          term = Pairs.second p
      in Core.Binding {
        Core.bindingName = name,
        Core.bindingTerm = term,
        Core.bindingType = (Maps.lookup name (Graph.graphBoundTypes g))}) (Maps.toList (Graph.graphBoundTerms g))

-- | Look up a binding in a graph by name
lookupElement :: Graph.Graph -> Core.Name -> Maybe Core.Binding
lookupElement graph name =
    Maybes.map (\term -> Core.Binding {
      Core.bindingName = name,
      Core.bindingTerm = term,
      Core.bindingType = (Maps.lookup name (Graph.graphBoundTypes graph))}) (Maps.lookup name (Graph.graphBoundTerms graph))

-- | Look up a primitive function in a graph by name
lookupPrimitive :: Graph.Graph -> Core.Name -> Maybe Graph.Primitive
lookupPrimitive graph name = Maps.lookup name (Graph.graphPrimitives graph)

-- | Look up a term by name in a graph
lookupTerm :: Graph.Graph -> Core.Name -> Maybe Core.Term
lookupTerm graph name = Maps.lookup name (Graph.graphBoundTerms graph)

matchEnum :: Context.Context -> Graph.Graph -> Core.Name -> [(Core.Name, t0)] -> Core.Term -> Either (Context.InContext Errors.Error) t0
matchEnum cx graph tname pairs =
    matchUnion cx graph tname (Lists.map (\pair -> matchUnitField (Pairs.first pair) (Pairs.second pair)) pairs)

matchRecord :: Context.Context -> t0 -> (M.Map Core.Name Core.Term -> Either (Context.InContext Errors.Error) t1) -> Core.Term -> Either (Context.InContext Errors.Error) t1
matchRecord cx graph decode term =

      let stripped = Strip.deannotateAndDetypeTerm term
      in case stripped of
        Core.TermRecord v0 -> decode (Maps.fromList (Lists.map (\field -> (Core.fieldName field, (Core.fieldTerm field))) (Core.recordFields v0)))
        _ -> Left (Context.InContext {
          Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "expected a record, got " (Core_.term term)))),
          Context.inContextContext = cx})

matchUnion :: Context.Context -> Graph.Graph -> Core.Name -> [(Core.Name, (Core.Term -> Either (Context.InContext Errors.Error) t0))] -> Core.Term -> Either (Context.InContext Errors.Error) t0
matchUnion cx graph tname pairs term =

      let stripped = Strip.deannotateAndDetypeTerm term
          mapping = Maps.fromList pairs
      in case stripped of
        Core.TermVariable v0 -> Eithers.bind (requireElement cx graph v0) (\el -> matchUnion cx graph tname pairs (Core.bindingTerm el))
        Core.TermUnion v0 ->
          let exp =

                    let fname = Core.fieldName (Core.injectionField v0)
                        val = Core.fieldTerm (Core.injectionField v0)
                    in (Maybes.maybe (Left (Context.InContext {
                      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "no matching case for field \"" (Core.unName fname)) "\" in union type ") (Core.unName tname)))),
                      Context.inContextContext = cx})) (\f -> f val) (Maps.lookup fname mapping))
          in (Logic.ifElse (Equality.equal (Core.unName (Core.injectionTypeName v0)) (Core.unName tname)) exp (Left (Context.InContext {
            Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected injection for type " (Core.unName tname)) ", got ") (Core_.term term)))),
            Context.inContextContext = cx})))
        _ -> Left (Context.InContext {
          Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat [
            "expected inject(",
            (Core.unName tname),
            ") with one of {",
            (Strings.intercalate ", " (Lists.map (\pair -> Core.unName (Pairs.first pair)) pairs)),
            "}, got ",
            (Core_.term stripped)]))),
          Context.inContextContext = cx})

matchUnitField :: t0 -> t1 -> (t0, (t2 -> Either t3 t1))
matchUnitField fname x = (fname, (\ignored -> Right x))

requireElement :: Context.Context -> Graph.Graph -> Core.Name -> Either (Context.InContext Errors.Error) Core.Binding
requireElement cx graph name =

      let showAll = False
          ellipsis =
                  \strings -> Logic.ifElse (Logic.and (Equality.gt (Lists.length strings) 3) (Logic.not showAll)) (Lists.concat2 (Lists.take 3 strings) [
                    "..."]) strings
          errMsg =
                  Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 "no such element: " (Core.unName name)) ". Available elements: {") (Strings.intercalate ", " (ellipsis (Lists.map Core.unName (Maps.keys (Graph.graphBoundTerms graph)))))) "}"
      in (Maybes.maybe (Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError errMsg)),
        Context.inContextContext = cx})) (\x -> Right x) (dereferenceElement graph name))

requirePrimitive :: Context.Context -> Graph.Graph -> Core.Name -> Either (Context.InContext Errors.Error) Graph.Primitive
requirePrimitive cx graph name =
    Maybes.maybe (Left (Context.InContext {
      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "no such primitive function: " (Core.unName name)))),
      Context.inContextContext = cx})) (\x -> Right x) (lookupPrimitive graph name)

requirePrimitiveType :: Context.Context -> Graph.Graph -> Core.Name -> Either (Context.InContext Errors.Error) Core.TypeScheme
requirePrimitiveType cx tx name =

      let mts = Maybes.map (\_p -> Graph.primitiveType _p) (Maps.lookup name (Graph.graphPrimitives tx))
      in (Maybes.maybe (Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "no such primitive function: " (Core.unName name)))),
        Context.inContextContext = cx})) (\ts -> Right ts) mts)

requireTerm :: Context.Context -> Graph.Graph -> Core.Name -> Either (Context.InContext Errors.Error) Core.Term
requireTerm cx graph name =
    Maybes.maybe (Left (Context.InContext {
      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "no such element: " (Core.unName name)))),
      Context.inContextContext = cx})) (\x -> Right x) (resolveTerm graph name)

-- | TODO: distinguish between lambda-bound and let-bound variables
resolveTerm :: Graph.Graph -> Core.Name -> Maybe Core.Term
resolveTerm graph name =

      let recurse =
              \term ->
                let stripped = Strip.deannotateTerm term
                in case stripped of
                  Core.TermVariable v0 -> resolveTerm graph v0
                  _ -> Just term
      in (Maybes.maybe Nothing recurse (lookupTerm graph name))

stripAndDereferenceTerm :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Errors.Error) Core.Term
stripAndDereferenceTerm cx graph term =

      let stripped = Strip.deannotateAndDetypeTerm term
      in case stripped of
        Core.TermVariable v0 -> Eithers.bind (requireTerm cx graph v0) (\t -> stripAndDereferenceTerm cx graph t)
        _ -> Right stripped

-- | Strip annotations and dereference variables, returning Either an error or the resolved term
stripAndDereferenceTermEither :: Graph.Graph -> Core.Term -> Either String Core.Term
stripAndDereferenceTermEither graph term =

      let stripped = Strip.deannotateAndDetypeTerm term
      in case stripped of
        Core.TermVariable v0 -> Eithers.either (\left_ -> Left left_) (\binding -> stripAndDereferenceTermEither graph (Core.bindingTerm binding)) (dereferenceVariable graph v0)
        _ -> Right stripped
