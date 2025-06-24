-- | Various functions for dereferencing and decoding schema types.

module Hydra.Schemas where

import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.CoreEncoding as CoreEncoding
import qualified Hydra.Errors as Errors
import qualified Hydra.Flows as Flows
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows_
import qualified Hydra.Lib.Io as Io
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Module as Module
import qualified Hydra.Qnames as Qnames
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Sorting as Sorting
import qualified Hydra.Strip as Strip
import qualified Hydra.Variants as Variants
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Get dependency namespaces from definitions
definitionDependencyNamespaces :: (Bool -> [Module.Definition] -> S.Set Module.Namespace)
definitionDependencyNamespaces excludeUnit defs =  
  let defNames = (\def -> (\x -> case x of
          Module.DefinitionType v1 -> (Rewriting.typeDependencyNames True excludeUnit (Module.typeDefinitionType v1))
          Module.DefinitionTerm v1 -> (Rewriting.termDependencyNames True True True (Module.termDefinitionTerm v1))) def) 
      allNames = (Sets.unions (Lists.map defNames defs))
  in (Sets.fromList (Optionals.cat (Lists.map Qnames.namespaceOf (Sets.toList allNames))))

-- | Find dependency namespaces in all of a set of terms
dependencyNamespaces :: (Bool -> Bool -> Bool -> Bool -> [Graph.Element] -> Compute.Flow Graph.Graph (S.Set Module.Namespace))
dependencyNamespaces withVars withPrims withNoms withSchema els =  
  let depNames = (\el ->  
          let term = (Graph.elementTerm el) 
              dataNames = (Rewriting.termDependencyNames withVars withPrims withNoms term)
              schemaNames = (Logic.ifElse withSchema (Optionals.maybe Sets.empty (\ts -> Rewriting.typeDependencyNames True True (Core.typeSchemeType ts)) (Graph.elementType el)) Sets.empty)
          in (Logic.ifElse (CoreEncoding.isEncodedType (Strip.fullyStripTerm term)) (Flows_.bind (DecodeCore.coreDecodeType term) (\typ -> Flows_.pure (Sets.unions [
            dataNames,
            schemaNames,
            (Rewriting.typeDependencyNames True True typ)]))) (Flows_.pure (Sets.unions [
            dataNames,
            schemaNames]))))
  in (Flows_.bind (Flows_.mapList depNames els) (\namesList -> Flows_.pure (Sets.fromList (Optionals.cat (Lists.map Qnames.namespaceOf (Sets.toList (Sets.delete Constants.placeholderName (Sets.unions namesList))))))))

-- | Dereference a type name to get the actual type
dereferenceType :: (Core.Name -> Compute.Flow Graph.Graph (Maybe Core.Type))
dereferenceType name = (Flows_.bind (Lexical.dereferenceElement name) (\mel -> Optionals.maybe (Flows_.pure Nothing) (\el -> Flows_.map Optionals.pure (DecodeCore.coreDecodeType (Graph.elementTerm el))) mel))

elementAsTypedTerm :: (Graph.Element -> Compute.Flow t0 Core.TypedTerm)
elementAsTypedTerm el = (Optionals.maybe (Flows_.fail "missing element type") (\ts -> Flows_.pure (Core.TypedTerm {
  Core.typedTermTerm = (Graph.elementTerm el),
  Core.typedTermType = (Core.typeSchemeType ts)})) (Graph.elementType el))

findFieldType :: (Core.Name -> [Core.FieldType] -> Compute.Flow t0 Core.Type)
findFieldType fname fields =  
  let matchingFields = (Lists.filter (\ft -> Equality.equalString (Core.unName (Core.fieldTypeName ft)) (Core.unName fname)) fields)
  in (Logic.ifElse (Lists.null matchingFields) (Flows_.fail (Strings.cat2 "No such field: " (Core.unName fname))) (Logic.ifElse (Equality.equalInt32 (Lists.length matchingFields) 1) (Flows_.pure (Core.fieldTypeType (Lists.head matchingFields))) (Flows_.fail (Strings.cat2 "Multiple fields named " (Core.unName fname)))))

-- | Get field types from a record or union type
fieldTypes :: (Core.Type -> Compute.Flow Graph.Graph (M.Map Core.Name Core.Type))
fieldTypes t =  
  let toMap = (\fields -> Maps.fromList (Lists.map (\ft -> (Core.fieldTypeName ft, (Core.fieldTypeType ft))) fields))
  in ((\x -> case x of
    Core.TypeForall v1 -> (fieldTypes (Core.forallTypeBody v1))
    Core.TypeRecord v1 -> (Flows_.pure (toMap (Core.rowTypeFields v1)))
    Core.TypeUnion v1 -> (Flows_.pure (toMap (Core.rowTypeFields v1)))
    Core.TypeVariable v1 -> (Flows.withTrace (Strings.cat2 "field types of " (Core.unName v1)) (Flows_.bind (Lexical.requireElement v1) (\el -> Flows_.bind (DecodeCore.coreDecodeType (Graph.elementTerm el)) fieldTypes)))
    _ -> (Errors.unexpected "record or union type" (Io.showType t))) (Strip.stripType t))

-- | Fully strip a type of forall quantifiers
fullyStripType :: (Core.Type -> Core.Type)
fullyStripType typ = ((\x -> case x of
  Core.TypeForall v1 -> (fullyStripType (Core.forallTypeBody v1))
  _ -> typ) (Strip.stripType typ))

-- | Check if a row type represents an enum (all fields are unit-typed)
isEnumRowType :: (Core.RowType -> Bool)
isEnumRowType rt = (Lists.foldl Logic.and True (Lists.map (\f -> CoreEncoding.isUnitType (Core.fieldTypeType f)) (Core.rowTypeFields rt)))

-- | Check if a type is an enum type
isEnumType :: (Core.Type -> Bool)
isEnumType typ = ((\x -> case x of
  Core.TypeUnion v1 -> (isEnumRowType v1)
  _ -> False) (Strip.stripType typ))

-- | Check if an element is serializable (no function types in dependencies)
isSerializable :: (Graph.Element -> Compute.Flow Graph.Graph Bool)
isSerializable el =  
  let variants = (\typ -> Lists.map Variants.typeVariant (Rewriting.foldOverType Coders.TraversalOrderPre (\m -> \t -> Lists.cons t m) [] typ))
  in (Flows_.map (\deps ->  
    let allVariants = (Sets.fromList (Lists.concat (Lists.map variants (Maps.elems deps))))
    in (Logic.not (Sets.member Mantle.TypeVariantFunction allVariants))) (typeDependencies False Equality.identity (Graph.elementName el)))

-- | Find dependency namespaces in all elements of a module, excluding the module's own namespace
moduleDependencyNamespaces :: (Bool -> Bool -> Bool -> Bool -> Module.Module -> Compute.Flow Graph.Graph (S.Set Module.Namespace))
moduleDependencyNamespaces withVars withPrims withNoms withSchema mod_ = (Flows_.bind (dependencyNamespaces withVars withPrims withNoms withSchema (Module.moduleElements mod_)) (\deps -> Flows_.pure (Sets.delete (Module.moduleNamespace mod_) deps)))

namespacesForDefinitions :: (Bool -> (Module.Namespace -> t0) -> Module.Namespace -> [Module.Definition] -> Module.Namespaces t0)
namespacesForDefinitions excludeUnit encodeNamespace focusNs defs =  
  let nss = (Sets.delete focusNs (definitionDependencyNamespaces excludeUnit defs)) 
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
  in (Flows_.bind (requireType name) (\t -> Optionals.maybe (Flows_.fail (Strings.cat [
    Core.unName name,
    " does not resolve to a ",
    label,
    " type: ",
    (Io.showType t)])) Flows_.pure (getter (rawType t))))

-- | Require a type by name
requireType :: (Core.Name -> Compute.Flow Graph.Graph Core.Type)
requireType name = (Flows.withTrace (Strings.cat2 "require type " (Core.unName name)) (Flows_.bind (Lexical.withSchemaContext (Lexical.requireElement name)) (\el -> DecodeCore.coreDecodeType (Graph.elementTerm el))))

-- | Require a name to resolve to a union type
requireUnionType :: (Core.Name -> Compute.Flow Graph.Graph Core.RowType)
requireUnionType = (requireRowType "union" (\t -> (\x -> case x of
  Core.TypeUnion v1 -> (Just v1)
  _ -> Nothing) t))

-- | Resolve a type, dereferencing type variables
resolveType :: (Core.Type -> Compute.Flow Graph.Graph (Maybe Core.Type))
resolveType typ = ((\x -> case x of
  Core.TypeVariable v1 -> (Lexical.withSchemaContext (Flows_.bind (Lexical.resolveTerm v1) (\mterm -> Optionals.maybe (Flows_.pure Nothing) (\t -> Flows_.map Optionals.pure (DecodeCore.coreDecodeType t)) mterm)))
  _ -> (Flows_.pure (Just typ))) (Strip.stripType typ))

schemaGraphToTypingEnvironment :: (Graph.Graph -> Compute.Flow t0 (M.Map Core.Name Core.TypeScheme))
schemaGraphToTypingEnvironment g =  
  let toTypeScheme = (\vars -> \typ -> (\x -> case x of
          Core.TypeForall v1 -> (toTypeScheme (Lists.cons (Core.forallTypeParameter v1) vars) (Core.forallTypeBody v1))
          _ -> Core.TypeScheme {
            Core.typeSchemeVariables = (Lists.reverse vars),
            Core.typeSchemeType = typ}) (Strip.stripType typ)) 
      toPair = (\el -> Flows_.map (\mts -> Optionals.map (\ts -> (Graph.elementName el, ts)) mts) (Optionals.maybe (Flows_.pure Nothing) (\ts -> Logic.ifElse (Equality.equal ts (Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.TypeScheme"))})) (Flows_.map Optionals.pure (DecodeCore.coreDecodeTypeScheme (Graph.elementTerm el))) (Logic.ifElse (Equality.equal ts (Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.Type"))})) (Flows_.map (\decoded -> Just (toTypeScheme [] decoded)) (DecodeCore.coreDecodeType (Graph.elementTerm el))) ((\x -> case x of
              Core.TermRecord v1 -> (Logic.ifElse (Equality.equal (Core.recordTypeName v1) (Core.Name "hydra.core.TypeScheme")) (Flows_.map Optionals.pure (DecodeCore.coreDecodeTypeScheme (Graph.elementTerm el))) (Flows_.pure Nothing))
              Core.TermUnion v1 -> (Logic.ifElse (Equality.equal (Core.injectionTypeName v1) (Core.Name "hydra.core.Type")) (Flows_.map (\decoded -> Just (toTypeScheme [] decoded)) (DecodeCore.coreDecodeType (Graph.elementTerm el))) (Flows_.pure Nothing))
              _ -> (Flows_.pure Nothing)) (Strip.fullyStripTerm (Graph.elementTerm el))))) (Graph.elementType el)))
  in (Flows.withState g (Flows_.bind (Flows_.mapList toPair (Maps.elems (Graph.graphElements g))) (\mpairs -> Flows_.pure (Maps.fromList (Optionals.cat mpairs)))))

-- | Topologically sort type definitions by dependencies
topologicalSortTypeDefinitions :: ([Module.TypeDefinition] -> [[Module.TypeDefinition]])
topologicalSortTypeDefinitions defs =  
  let toPair = (\def -> (Module.typeDefinitionName def, (Sets.toList (Rewriting.typeDependencyNames False True (Module.typeDefinitionType def))))) 
      nameToDef = (Maps.fromList (Lists.map (\d -> (Module.typeDefinitionName d, d)) defs))
      sorted = (Sorting.topologicalSortComponents (Lists.map toPair defs))
  in (Lists.map (\names -> Optionals.cat (Lists.map (\n -> Maps.lookup n nameToDef) names)) sorted)

-- | Get all type dependencies for a given type name
typeDependencies :: (Bool -> (Core.Type -> Core.Type) -> Core.Name -> Compute.Flow Graph.Graph (M.Map Core.Name Core.Type))
typeDependencies withSchema transform name =  
  let requireType = (\name -> Flows.withTrace (Strings.cat2 "type dependencies of " (Core.unName name)) (Flows_.bind (Lexical.requireElement name) (\el -> DecodeCore.coreDecodeType (Graph.elementTerm el)))) 
      toPair = (\name -> Flows_.bind (requireType name) (\typ -> Flows_.pure (name, (transform typ))))
      deps = (\seeds -> \names -> Logic.ifElse (Sets.null seeds) (Flows_.pure names) (Flows_.bind (Flows_.mapList toPair (Sets.toList seeds)) (\pairs ->  
              let newNames = (Maps.union names (Maps.fromList pairs)) 
                  refs = (Lists.foldl Sets.union Sets.empty (Lists.map (\pair -> Rewriting.typeDependencyNames withSchema True (snd pair)) pairs))
                  visited = (Sets.fromList (Maps.keys names))
                  newSeeds = (Sets.difference refs visited)
              in (deps newSeeds newNames))))
  in (deps (Sets.singleton name) Maps.empty)
