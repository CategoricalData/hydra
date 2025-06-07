-- | A module for lexical operations over graphs.

module Hydra.Lexical where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Flows as Flows
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows_
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Strip as Strip
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

dereferenceElement :: (Core.Name -> Compute.Flow Graph.Graph (Maybe Graph.Element))
dereferenceElement name = (Flows_.map (\g -> lookupElement g name) Errors.getState)

elementsToGraph :: (Graph.Graph -> Maybe Graph.Graph -> [Graph.Element] -> Graph.Graph)
elementsToGraph parent schema elements =  
  let toPair = (\el -> (Graph.elementName el, el))
  in Graph.Graph {
    Graph.graphElements = (Maps.fromList (Lists.map toPair elements)),
    Graph.graphEnvironment = (Graph.graphEnvironment parent),
    Graph.graphTypes = (Graph.graphTypes parent),
    Graph.graphBody = (Graph.graphBody parent),
    Graph.graphPrimitives = (Graph.graphPrimitives parent),
    Graph.graphSchema = schema}

-- | An empty graph; no elements, no primitives, no schema, and an arbitrary body.
emptyGraph :: Graph.Graph
emptyGraph = Graph.Graph {
  Graph.graphElements = Maps.empty,
  Graph.graphEnvironment = Maps.empty,
  Graph.graphTypes = Maps.empty,
  Graph.graphBody = (Core.TermLiteral (Core.LiteralString "empty graph")),
  Graph.graphPrimitives = Maps.empty,
  Graph.graphSchema = Nothing}

extendGraphWithBindings :: ([Core.LetBinding] -> Graph.Graph -> Graph.Graph)
extendGraphWithBindings bindings g =  
  let newEls = (Maps.fromList (Lists.map toEl bindings)) 
      toEl = (\binding ->  
              let name = (Core.letBindingName binding) 
                  term = (Core.letBindingTerm binding)
                  mts = (Core.letBindingType binding)
              in (name, Graph.Element {
                Graph.elementName = name,
                Graph.elementTerm = term,
                Graph.elementType = mts}))
  in Graph.Graph {
    Graph.graphElements = (Maps.union newEls (Graph.graphElements g)),
    Graph.graphEnvironment = (Graph.graphEnvironment g),
    Graph.graphTypes = (Graph.graphTypes g),
    Graph.graphBody = (Graph.graphBody g),
    Graph.graphPrimitives = (Graph.graphPrimitives g),
    Graph.graphSchema = (Graph.graphSchema g)}

fieldsOf :: (Core.Type -> [Core.FieldType])
fieldsOf t =  
  let stripped = (Strip.stripType t)
  in ((\x -> case x of
    Core.TypeForall v1 -> (fieldsOf (Core.forallTypeBody v1))
    Core.TypeRecord v1 -> (Core.rowTypeFields v1)
    Core.TypeUnion v1 -> (Core.rowTypeFields v1)
    _ -> []) stripped)

lookupElement :: (Graph.Graph -> Core.Name -> Maybe Graph.Element)
lookupElement g name = (Maps.lookup name (Graph.graphElements g))

lookupPrimitive :: (Graph.Graph -> Core.Name -> Maybe Graph.Primitive)
lookupPrimitive g name = (Maps.lookup name (Graph.graphPrimitives g))

requireElement :: (Core.Name -> Compute.Flow Graph.Graph Graph.Element)
requireElement name =  
  let showAll = False 
      ellipsis = (\strings -> Logic.ifElse (Logic.and (Equality.gtInt32 (Lists.length strings) 3) (Logic.not showAll)) (Lists.concat2 (Lists.take 3 strings) [
              "..."]) strings)
      err = (\g -> Flows_.fail (Strings.cat [
              Strings.cat [
                Strings.cat [
                  Strings.cat [
                    "no such element: ",
                    (Core.unName name)],
                  ". Available elements: {"],
                (Strings.intercalate ", " (ellipsis (Lists.map (\el -> Core.unName (Graph.elementName el)) (Maps.elems (Graph.graphElements g)))))],
              "}"]))
  in (Flows_.bind (dereferenceElement name) (\mel -> Optionals.maybe (Flows_.bind Errors.getState err) Flows_.pure mel))

requirePrimitive :: (Core.Name -> Compute.Flow Graph.Graph Graph.Primitive)
requirePrimitive name = (Flows_.bind Errors.getState (\g -> Optionals.maybe (Flows_.fail (Strings.cat [
  "no such primitive function: ",
  (Core.unName name)])) Flows_.pure (lookupPrimitive g name)))

requireTerm :: (Core.Name -> Compute.Flow Graph.Graph Core.Term)
requireTerm name = (Flows_.bind (resolveTerm name) (\mt -> Optionals.maybe (Flows_.fail (Strings.cat [
  "no such element: ",
  (Core.unName name)])) Flows_.pure mt))

-- | TODO: distinguish between lambda-bound and let-bound variables
resolveTerm :: (Core.Name -> Compute.Flow Graph.Graph (Maybe Core.Term))
resolveTerm name =  
  let recurse = (\el ->  
          let stripped = (Strip.fullyStripTerm (Graph.elementTerm el))
          in ((\x -> case x of
            Core.TermVariable v1 -> (resolveTerm v1)
            _ -> (Flows_.pure (Just (Graph.elementTerm el)))) stripped))
  in (Flows_.bind Errors.getState (\g -> Optionals.maybe (Flows_.pure Nothing) recurse (Maps.lookup name (Graph.graphElements g))))

-- | Note: assuming for now that primitive functions are the same in the schema graph
schemaContext :: (Graph.Graph -> Graph.Graph)
schemaContext g = (Optionals.fromMaybe g (Graph.graphSchema g))

stripAndDereferenceTerm :: (Core.Term -> Compute.Flow Graph.Graph Core.Term)
stripAndDereferenceTerm term =  
  let stripped = (Strip.fullyStripTerm term)
  in ((\x -> case x of
    Core.TermVariable v1 -> (Flows_.bind (requireTerm v1) (\t -> stripAndDereferenceTerm t))
    _ -> (Flows_.pure stripped)) stripped)

typeOfPrimitive :: (Core.Name -> Compute.Flow Graph.Graph Core.TypeScheme)
typeOfPrimitive name = (Flows_.map Graph.primitiveType (requirePrimitive name))

withSchemaContext :: (Compute.Flow Graph.Graph t0 -> Compute.Flow Graph.Graph t0)
withSchemaContext f = (Flows_.bind Errors.getState (\g -> Flows.withState (schemaContext g) f))
