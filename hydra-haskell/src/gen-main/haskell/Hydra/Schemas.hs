-- | Various functions for dereferencing and decoding schema types.

module Hydra.Schemas where

import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Encode.Core as Core__
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Module as Module
import qualified Hydra.Monads as Monads
import qualified Hydra.Names as Names
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Show.Core as Core___
import qualified Hydra.Sorting as Sorting
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Get dependency namespaces from definitions
definitionDependencyNamespaces :: ([Module.Definition] -> S.Set Module.Namespace)
definitionDependencyNamespaces defs =  
  let defNames = (\def -> (\x -> case x of
          Module.DefinitionType v1 -> (Rewriting.typeDependencyNames True (Module.typeDefinitionType v1))
          Module.DefinitionTerm v1 -> (Rewriting.termDependencyNames True True True (Module.termDefinitionTerm v1))) def) 
      allNames = (Sets.unions (Lists.map defNames defs))
  in (Sets.fromList (Optionals.cat (Lists.map Names.namespaceOf (Sets.toList allNames))))

-- | Find dependency namespaces in all of a set of terms
dependencyNamespaces :: (Bool -> Bool -> Bool -> Bool -> [Graph.Element] -> Compute.Flow Graph.Graph (S.Set Module.Namespace))
dependencyNamespaces binds withPrims withNoms withSchema els =  
  let depNames = (\el ->  
          let term = (Graph.elementTerm el) 
              dataNames = (Rewriting.termDependencyNames binds withPrims withNoms term)
              schemaNames = (Logic.ifElse withSchema (Optionals.maybe Sets.empty (\ts -> Rewriting.typeDependencyNames True (Core.typeSchemeType ts)) (Graph.elementType el)) Sets.empty)
          in (Logic.ifElse (Core__.isEncodedType (Rewriting.deannotateTerm term)) (Flows.bind (Core_.type_ term) (\typ -> Flows.pure (Sets.unions [
            dataNames,
            schemaNames,
            (Rewriting.typeDependencyNames True typ)]))) (Flows.pure (Sets.unions [
            dataNames,
            schemaNames]))))
  in (Flows.bind (Flows.mapList depNames els) (\namesList -> Flows.pure (Sets.fromList (Optionals.cat (Lists.map Names.namespaceOf (Sets.toList (Sets.delete Constants.placeholderName (Sets.unions namesList))))))))

-- | Dereference a type name to get the actual type
dereferenceType :: (Core.Name -> Compute.Flow Graph.Graph (Maybe Core.Type))
dereferenceType name = (Flows.bind (Lexical.dereferenceElement name) (\mel -> Optionals.maybe (Flows.pure Nothing) (\el -> Flows.map Optionals.pure (Core_.type_ (Graph.elementTerm el))) mel))

elementAsTypedTerm :: (Graph.Element -> Compute.Flow t0 Core.TypedTerm)
elementAsTypedTerm el = (Optionals.maybe (Flows.fail "missing element type") (\ts -> Flows.pure (Core.TypedTerm {
  Core.typedTermTerm = (Graph.elementTerm el),
  Core.typedTermType = (Core.typeSchemeType ts)})) (Graph.elementType el))

-- | Get elements with their dependencies
elementsWithDependencies :: ([Graph.Element] -> Compute.Flow Graph.Graph [Graph.Element])
elementsWithDependencies original =  
  let depNames = (\el -> Sets.toList (Rewriting.termDependencyNames True False False (Graph.elementTerm el))) 
      allDepNames = (Lists.nub (Lists.concat2 (Lists.map Graph.elementName original) (Lists.concat (Lists.map depNames original))))
  in (Flows.mapList Lexical.requireElement allDepNames)

fieldMap :: ([Core.Field] -> M.Map Core.Name Core.Term)
fieldMap fields = (Maps.fromList (Lists.map toPair fields)) 
  where 
    toPair = (\f -> (Core.fieldName f, (Core.fieldTerm f)))

fieldTypeMap :: ([Core.FieldType] -> M.Map Core.Name Core.Type)
fieldTypeMap fields = (Maps.fromList (Lists.map toPair fields)) 
  where 
    toPair = (\f -> (Core.fieldTypeName f, (Core.fieldTypeType f)))

findFieldType :: (Core.Name -> [Core.FieldType] -> Compute.Flow t0 Core.Type)
findFieldType fname fields =  
  let matchingFields = (Lists.filter (\ft -> Equality.equal (Core.unName (Core.fieldTypeName ft)) (Core.unName fname)) fields)
  in (Logic.ifElse (Lists.null matchingFields) (Flows.fail (Strings.cat2 "No such field: " (Core.unName fname))) (Logic.ifElse (Equality.equal (Lists.length matchingFields) 1) (Flows.pure (Core.fieldTypeType (Lists.head matchingFields))) (Flows.fail (Strings.cat2 "Multiple fields named " (Core.unName fname)))))

-- | Get field types from a record or union type
fieldTypes :: (Core.Type -> Compute.Flow Graph.Graph (M.Map Core.Name Core.Type))
fieldTypes t =  
  let toMap = (\fields -> Maps.fromList (Lists.map (\ft -> (Core.fieldTypeName ft, (Core.fieldTypeType ft))) fields))
  in ((\x -> case x of
    Core.TypeForall v1 -> (fieldTypes (Core.forallTypeBody v1))
    Core.TypeRecord v1 -> (Flows.pure (toMap (Core.rowTypeFields v1)))
    Core.TypeUnion v1 -> (Flows.pure (toMap (Core.rowTypeFields v1)))
    Core.TypeVariable v1 -> (Monads.withTrace (Strings.cat2 "field types of " (Core.unName v1)) (Flows.bind (Lexical.requireElement v1) (\el -> Flows.bind (Core_.type_ (Graph.elementTerm el)) fieldTypes)))
    _ -> (Monads.unexpected "record or union type" (Core___.type_ t))) (Rewriting.deannotateType t))

-- | Fully strip a type of forall quantifiers
fullyStripType :: (Core.Type -> Core.Type)
fullyStripType typ = ((\x -> case x of
  Core.TypeForall v1 -> (fullyStripType (Core.forallTypeBody v1))
  _ -> typ) (Rewriting.deannotateType typ))

-- | Check if a row type represents an enum (all fields are unit-typed)
isEnumRowType :: (Core.RowType -> Bool)
isEnumRowType rt = (Lists.foldl Logic.and True (Lists.map (\f -> Core__.isUnitType (Core.fieldTypeType f)) (Core.rowTypeFields rt)))

-- | Check if a type is an enum type
isEnumType :: (Core.Type -> Bool)
isEnumType typ = ((\x -> case x of
  Core.TypeUnion v1 -> (isEnumRowType v1)
  _ -> False) (Rewriting.deannotateType typ))

-- | Check if an element is serializable (no function types in dependencies)
isSerializable :: (Graph.Element -> Compute.Flow Graph.Graph Bool)
isSerializable el =  
  let variants = (\typ -> Lists.map Variants.typeVariant (Rewriting.foldOverType Coders.TraversalOrderPre (\m -> \t -> Lists.cons t m) [] typ))
  in (Flows.map (\deps ->  
    let allVariants = (Sets.fromList (Lists.concat (Lists.map variants (Maps.elems deps))))
    in (Logic.not (Sets.member Mantle.TypeVariantFunction allVariants))) (typeDependencies False Equality.identity (Graph.elementName el)))

-- | Find dependency namespaces in all elements of a module, excluding the module's own namespace
moduleDependencyNamespaces :: (Bool -> Bool -> Bool -> Bool -> Module.Module -> Compute.Flow Graph.Graph (S.Set Module.Namespace))
moduleDependencyNamespaces binds withPrims withNoms withSchema mod = (Flows.bind (dependencyNamespaces binds withPrims withNoms withSchema (Module.moduleElements mod)) (\deps -> Flows.pure (Sets.delete (Module.moduleNamespace mod) deps)))

namespacesForDefinitions :: ((Module.Namespace -> t0) -> Module.Namespace -> [Module.Definition] -> Module.Namespaces t0)
namespacesForDefinitions encodeNamespace focusNs defs =  
  let nss = (Sets.delete focusNs (definitionDependencyNamespaces defs)) 
      toPair = (\ns -> (ns, (encodeNamespace ns)))
  in Module.Namespaces {
    Module.namespacesFocus = (toPair focusNs),
    Module.namespacesMapping = (Maps.fromList (Lists.map toPair (Sets.toList nss)))}

-- | Require a name to resolve to a record type
requireRecordType :: (Core.Name -> Compute.Flow Graph.Graph Core.RowType)
requireRecordType = (requireRowType "record type" (\t -> (\x -> case x of
  Core.TypeRecord v1 -> (Just v1)
  _ -> Nothing) t))

requireRowType :: (String -> (Core.Type -> Maybe t0) -> Core.Name -> Compute.Flow Graph.Graph t0)
requireRowType label getter name =  
  let rawType = (\t -> (\x -> case x of
          Core.TypeAnnotated v1 -> (rawType (Core.annotatedTypeSubject v1))
          Core.TypeForall v1 -> (rawType (Core.forallTypeBody v1))
          _ -> t) t)
  in (Flows.bind (requireType name) (\t -> Optionals.maybe (Flows.fail (Strings.cat [
    Core.unName name,
    " does not resolve to a ",
    label,
    " type: ",
    (Core___.type_ t)])) Flows.pure (getter (rawType t))))

-- | Require a type by name
requireType :: (Core.Name -> Compute.Flow Graph.Graph Core.Type)
requireType name = (Monads.withTrace (Strings.cat2 "require type " (Core.unName name)) (Flows.bind (Lexical.withSchemaContext (Lexical.requireElement name)) (\el -> Core_.type_ (Graph.elementTerm el))))

-- | Require a name to resolve to a union type
requireUnionType :: (Core.Name -> Compute.Flow Graph.Graph Core.RowType)
requireUnionType = (requireRowType "union" (\t -> (\x -> case x of
  Core.TypeUnion v1 -> (Just v1)
  _ -> Nothing) t))

-- | Resolve a type, dereferencing type variables
resolveType :: (Core.Type -> Compute.Flow Graph.Graph (Maybe Core.Type))
resolveType typ = ((\x -> case x of
  Core.TypeVariable v1 -> (Lexical.withSchemaContext (Flows.bind (Lexical.resolveTerm v1) (\mterm -> Optionals.maybe (Flows.pure Nothing) (\t -> Flows.map Optionals.pure (Core_.type_ t)) mterm)))
  _ -> (Flows.pure (Just typ))) (Rewriting.deannotateType typ))

schemaGraphToTypingEnvironment :: (Graph.Graph -> Compute.Flow t0 (M.Map Core.Name Core.TypeScheme))
schemaGraphToTypingEnvironment g =  
  let toTypeScheme = (\vars -> \typ -> (\x -> case x of
          Core.TypeForall v1 -> (toTypeScheme (Lists.cons (Core.forallTypeParameter v1) vars) (Core.forallTypeBody v1))
          _ -> Core.TypeScheme {
            Core.typeSchemeVariables = (Lists.reverse vars),
            Core.typeSchemeType = typ}) (Rewriting.deannotateType typ)) 
      toPair = (\el -> Flows.map (\mts -> Optionals.map (\ts -> (Graph.elementName el, ts)) mts) (Optionals.maybe (Flows.pure Nothing) (\ts -> Logic.ifElse (Equality.equal ts (Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.TypeScheme"))})) (Flows.map Optionals.pure (Core_.typeScheme (Graph.elementTerm el))) (Logic.ifElse (Equality.equal ts (Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.Type"))})) (Flows.map (\decoded -> Just (toTypeScheme [] decoded)) (Core_.type_ (Graph.elementTerm el))) ((\x -> case x of
              Core.TermRecord v1 -> (Logic.ifElse (Equality.equal (Core.recordTypeName v1) (Core.Name "hydra.core.TypeScheme")) (Flows.map Optionals.pure (Core_.typeScheme (Graph.elementTerm el))) (Flows.pure Nothing))
              Core.TermUnion v1 -> (Logic.ifElse (Equality.equal (Core.injectionTypeName v1) (Core.Name "hydra.core.Type")) (Flows.map (\decoded -> Just (toTypeScheme [] decoded)) (Core_.type_ (Graph.elementTerm el))) (Flows.pure Nothing))
              _ -> (Flows.pure Nothing)) (Rewriting.deannotateTerm (Graph.elementTerm el))))) (Graph.elementType el)))
  in (Monads.withState g (Flows.bind (Flows.mapList toPair (Maps.elems (Graph.graphElements g))) (\mpairs -> Flows.pure (Maps.fromList (Optionals.cat mpairs)))))

-- | Topologically sort type definitions by dependencies
topologicalSortTypeDefinitions :: ([Module.TypeDefinition] -> [[Module.TypeDefinition]])
topologicalSortTypeDefinitions defs =  
  let toPair = (\def -> (Module.typeDefinitionName def, (Sets.toList (Rewriting.typeDependencyNames False (Module.typeDefinitionType def))))) 
      nameToDef = (Maps.fromList (Lists.map (\d -> (Module.typeDefinitionName d, d)) defs))
      sorted = (Sorting.topologicalSortComponents (Lists.map toPair defs))
  in (Lists.map (\names -> Optionals.cat (Lists.map (\n -> Maps.lookup n nameToDef) names)) sorted)

-- | Get all type dependencies for a given type name
typeDependencies :: (Bool -> (Core.Type -> Core.Type) -> Core.Name -> Compute.Flow Graph.Graph (M.Map Core.Name Core.Type))
typeDependencies withSchema transform name =  
  let requireType = (\name -> Monads.withTrace (Strings.cat2 "type dependencies of " (Core.unName name)) (Flows.bind (Lexical.requireElement name) (\el -> Core_.type_ (Graph.elementTerm el)))) 
      toPair = (\name -> Flows.bind (requireType name) (\typ -> Flows.pure (name, (transform typ))))
      deps = (\seeds -> \names -> Logic.ifElse (Sets.null seeds) (Flows.pure names) (Flows.bind (Flows.mapList toPair (Sets.toList seeds)) (\pairs ->  
              let newNames = (Maps.union names (Maps.fromList pairs)) 
                  refs = (Lists.foldl Sets.union Sets.empty (Lists.map (\pair -> Rewriting.typeDependencyNames withSchema (snd pair)) pairs))
                  visited = (Sets.fromList (Maps.keys names))
                  newSeeds = (Sets.difference refs visited)
              in (deps newSeeds newNames))))
  in (deps (Sets.singleton name) Maps.empty)
