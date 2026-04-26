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
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Strip as Strip
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S
-- | Build a Graph from element bindings, environment, and primitives
buildGraph :: [Core.Binding] -> M.Map Core.Name (Maybe Core.Term) -> M.Map Core.Name Graph.Primitive -> Graph.Graph
buildGraph elements environment primitives =

      let elementTerms = Maps.fromList (Lists.map (\b -> (Core.bindingName b, (Core.bindingTerm b))) elements)
          letTerms =
                  Maps.fromList (Maybes.mapMaybe (\kv -> Maybes.map (\t -> (Pairs.first kv, t)) (Pairs.second kv)) (Maps.toList environment))
          mergedTerms = Maps.union elementTerms letTerms
          filteredTerms = Maps.filterWithKey (\k -> \_v -> Logic.not (Maps.member k primitives)) mergedTerms
          elementTypes =
                  Maps.fromList (Maybes.cat (Lists.map (\b -> Maybes.map (\ts -> (Core.bindingName b, ts)) (Core.bindingType b)) elements))
          filteredTypes = Maps.filterWithKey (\k -> \_v -> Logic.not (Maps.member k primitives)) elementTypes
      in Graph.Graph {
        Graph.graphBoundTerms = filteredTerms,
        Graph.graphBoundTypes = filteredTypes,
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
dereferenceVariable :: Graph.Graph -> Core.Name -> Either Errors.Error Core.Binding
dereferenceVariable graph name =
    Maybes.maybe (Left (Errors.ErrorResolution (Errors.ResolutionErrorNoSuchBinding (Errors.NoSuchBindingError {
      Errors.noSuchBindingErrorName = name})))) (\right_ -> Right right_) (lookupBinding graph name)
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
getField :: M.Map Core.Name t0 -> Core.Name -> (t0 -> Either Errors.Error t1) -> Either Errors.Error t1
getField m fname decode =
    Maybes.maybe (Left (Errors.ErrorResolution (Errors.ResolutionErrorNoMatchingField (Errors.NoMatchingFieldError {
      Errors.noMatchingFieldErrorFieldName = fname})))) decode (Maps.lookup fname m)
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
-- | Build a graph with primitives assembled from built-in and user-provided lists. User-provided primitives shadow built-in ones.
graphWithPrimitives :: [Graph.Primitive] -> [Graph.Primitive] -> Graph.Graph
graphWithPrimitives builtIn userProvided =

      let toMap = \ps -> Maps.fromList (Lists.map (\p -> (Graph.primitiveName p, p)) ps)
          prims = Maps.union (toMap userProvided) (toMap builtIn)
      in (buildGraph [] Maps.empty prims)
-- | Look up a binding in a graph by name
lookupBinding :: Graph.Graph -> Core.Name -> Maybe Core.Binding
lookupBinding graph name =
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
matchEnum :: Graph.Graph -> Core.Name -> [(Core.Name, t0)] -> Core.Term -> Either Errors.Error t0
matchEnum graph tname pairs =
    matchUnion graph tname (Lists.map (\pair -> matchUnitField (Pairs.first pair) (Pairs.second pair)) pairs)
matchRecord :: t0 -> (M.Map Core.Name Core.Term -> Either Errors.Error t1) -> Core.Term -> Either Errors.Error t1
matchRecord graph decode term =

      let stripped = Strip.deannotateAndDetypeTerm term
      in case stripped of
        Core.TermRecord v0 -> decode (Maps.fromList (Lists.map (\field -> (Core.fieldName field, (Core.fieldTerm field))) (Core.recordFields v0)))
        _ -> Left (Errors.ErrorResolution (Errors.ResolutionErrorUnexpectedShape (Errors.UnexpectedShapeError {
          Errors.unexpectedShapeErrorExpected = "record",
          Errors.unexpectedShapeErrorActual = (ShowCore.term term)})))
matchUnion :: Graph.Graph -> Core.Name -> [(Core.Name, (Core.Term -> Either Errors.Error t0))] -> Core.Term -> Either Errors.Error t0
matchUnion graph tname pairs term =

      let stripped = Strip.deannotateAndDetypeTerm term
          mapping = Maps.fromList pairs
      in case stripped of
        Core.TermVariable v0 -> Eithers.bind (requireBinding graph v0) (\el -> matchUnion graph tname pairs (Core.bindingTerm el))
        Core.TermInject v0 ->
          let exp =

                    let fname = Core.fieldName (Core.injectionField v0)
                        val = Core.fieldTerm (Core.injectionField v0)
                    in (Maybes.maybe (Left (Errors.ErrorResolution (Errors.ResolutionErrorNoMatchingField (Errors.NoMatchingFieldError {
                      Errors.noMatchingFieldErrorFieldName = fname})))) (\f -> f val) (Maps.lookup fname mapping))
          in (Logic.ifElse (Equality.equal (Core.unName (Core.injectionTypeName v0)) (Core.unName tname)) exp (Left (Errors.ErrorResolution (Errors.ResolutionErrorUnexpectedShape (Errors.UnexpectedShapeError {
            Errors.unexpectedShapeErrorExpected = (Strings.cat2 "injection for type " (Core.unName tname)),
            Errors.unexpectedShapeErrorActual = (ShowCore.term term)})))))
        _ -> Left (Errors.ErrorResolution (Errors.ResolutionErrorUnexpectedShape (Errors.UnexpectedShapeError {
          Errors.unexpectedShapeErrorExpected = (Strings.cat2 "injection for type " (Core.unName tname)),
          Errors.unexpectedShapeErrorActual = (ShowCore.term stripped)})))
matchUnitField :: t0 -> t1 -> (t0, (t2 -> Either t3 t1))
matchUnitField fname x = (fname, (\ignored -> Right x))
requireBinding :: Graph.Graph -> Core.Name -> Either Errors.Error Core.Binding
requireBinding graph name =

      let showAll = False
          ellipsis =
                  \strings -> Logic.ifElse (Logic.and (Equality.gt (Lists.length strings) 3) (Logic.not showAll)) (Lists.concat2 (Lists.take 3 strings) [
                    "..."]) strings
          errMsg =
                  Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 "no such element: " (Core.unName name)) ". Available elements: {") (Strings.intercalate ", " (ellipsis (Lists.map Core.unName (Maps.keys (Graph.graphBoundTerms graph)))))) "}"
      in (Maybes.maybe (Left (Errors.ErrorResolution (Errors.ResolutionErrorOther (Errors.OtherResolutionError errMsg)))) (\x -> Right x) (lookupBinding graph name))
requirePrimitive :: Graph.Graph -> Core.Name -> Either Errors.Error Graph.Primitive
requirePrimitive graph name =
    Maybes.maybe (Left (Errors.ErrorResolution (Errors.ResolutionErrorNoSuchPrimitive (Errors.NoSuchPrimitiveError {
      Errors.noSuchPrimitiveErrorName = name})))) (\x -> Right x) (lookupPrimitive graph name)
requirePrimitiveType :: Graph.Graph -> Core.Name -> Either Errors.Error Core.TypeScheme
requirePrimitiveType tx name =

      let mts = Maybes.map (\_p -> Graph.primitiveType _p) (Maps.lookup name (Graph.graphPrimitives tx))
      in (Maybes.maybe (Left (Errors.ErrorResolution (Errors.ResolutionErrorNoSuchPrimitive (Errors.NoSuchPrimitiveError {
        Errors.noSuchPrimitiveErrorName = name})))) (\ts -> Right ts) mts)
requireTerm :: Graph.Graph -> Core.Name -> Either Errors.Error Core.Term
requireTerm graph name =
    Maybes.maybe (Left (Errors.ErrorResolution (Errors.ResolutionErrorNoSuchBinding (Errors.NoSuchBindingError {
      Errors.noSuchBindingErrorName = name})))) (\x -> Right x) (resolveTerm graph name)
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
stripAndDereferenceTerm :: Graph.Graph -> Core.Term -> Either Errors.Error Core.Term
stripAndDereferenceTerm graph term =

      let stripped = Strip.deannotateAndDetypeTerm term
      in case stripped of
        Core.TermVariable v0 -> Eithers.bind (requireTerm graph v0) (\t -> stripAndDereferenceTerm graph t)
        _ -> Right stripped
-- | Strip annotations and dereference variables, returning Either an error or the resolved term
stripAndDereferenceTermEither :: Graph.Graph -> Core.Term -> Either Errors.Error Core.Term
stripAndDereferenceTermEither graph term =

      let stripped = Strip.deannotateAndDetypeTerm term
      in case stripped of
        Core.TermVariable v0 -> Eithers.either (\left_ -> Left left_) (\binding -> stripAndDereferenceTermEither graph (Core.bindingTerm binding)) (dereferenceVariable graph v0)
        _ -> Right stripped
