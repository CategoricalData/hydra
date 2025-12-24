-- Note: this is an automatically generated file. Do not edit.

-- | Various functions for dereferencing and decoding schema types.

module Hydra.Schemas where

import qualified Hydra.Annotations as Annotations
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
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Monads as Monads
import qualified Hydra.Names as Names
import qualified Hydra.Reflect as Reflect
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Show.Core as Core___
import qualified Hydra.Sorting as Sorting
import qualified Hydra.Substitution as Substitution
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

addNamesToNamespaces :: ((Module.Namespace -> t0) -> S.Set Core.Name -> Module.Namespaces t0 -> Module.Namespaces t0)
addNamesToNamespaces encodeNamespace names ns0 =  
  let nss = (Sets.fromList (Maybes.cat (Lists.map Names.namespaceOf (Sets.toList names))))
  in  
    let toPair = (\ns -> (ns, (encodeNamespace ns)))
    in Module.Namespaces {
      Module.namespacesFocus = (Module.namespacesFocus ns0),
      Module.namespacesMapping = (Maps.union (Module.namespacesMapping ns0) (Maps.fromList (Lists.map toPair (Sets.toList nss))))}

-- | Get dependency namespaces from definitions
definitionDependencyNamespaces :: ([Module.Definition] -> S.Set Module.Namespace)
definitionDependencyNamespaces defs =  
  let defNames = (\def -> (\x -> case x of
          Module.DefinitionType v1 -> (Rewriting.typeDependencyNames True (Module.typeDefinitionType v1))
          Module.DefinitionTerm v1 -> (Rewriting.termDependencyNames True True True (Module.termDefinitionTerm v1))) def)
  in  
    let allNames = (Sets.unions (Lists.map defNames defs))
    in (Sets.fromList (Maybes.cat (Lists.map Names.namespaceOf (Sets.toList allNames))))

-- | Find dependency namespaces in all of a set of terms
dependencyNamespaces :: (Bool -> Bool -> Bool -> Bool -> [Core.Binding] -> Compute.Flow Graph.Graph (S.Set Module.Namespace))
dependencyNamespaces binds withPrims withNoms withSchema els = (Flows.bind Monads.getState (\cx ->  
  let depNames = (\el ->  
          let term = (Core.bindingTerm el)
          in  
            let dataNames = (Rewriting.termDependencyNames binds withPrims withNoms term)
            in  
              let schemaNames = (Logic.ifElse withSchema (Maybes.maybe Sets.empty (\ts -> Rewriting.typeDependencyNames True (Core.typeSchemeType ts)) (Core.bindingType el)) Sets.empty)
              in (Logic.ifElse (isEncodedType (Rewriting.deannotateTerm term)) (Flows.bind (Monads.withTrace "dependency namespace" (Monads.eitherToFlow Util.unDecodingError (Core_.type_ cx term))) (\typ -> Flows.pure (Sets.unions [
                dataNames,
                schemaNames,
                (Rewriting.typeDependencyNames True typ)]))) (Flows.pure (Sets.unions [
                dataNames,
                schemaNames]))))
  in (Flows.bind (Flows.mapList depNames els) (\namesList -> Flows.pure (Sets.fromList (Maybes.cat (Lists.map Names.namespaceOf (Sets.toList (Sets.delete Constants.placeholderName (Sets.unions namesList))))))))))

-- | Dereference a type name to get the actual type
dereferenceType :: (Core.Name -> Compute.Flow Graph.Graph (Maybe Core.Type))
dereferenceType name = (Flows.bind Monads.getState (\cx -> Flows.bind (Lexical.dereferenceElement name) (\mel -> Maybes.maybe (Flows.pure Nothing) (\el -> Flows.map Maybes.pure (Monads.withTrace "dereference type" (Monads.eitherToFlow Util.unDecodingError (Core_.type_ cx (Core.bindingTerm el))))) mel)))

elementAsTypeApplicationTerm :: (Core.Binding -> Compute.Flow t0 Core.TypeApplicationTerm)
elementAsTypeApplicationTerm el = (Maybes.maybe (Flows.fail "missing element type") (\ts -> Flows.pure (Core.TypeApplicationTerm {
  Core.typeApplicationTermBody = (Core.bindingTerm el),
  Core.typeApplicationTermType = (Core.typeSchemeType ts)})) (Core.bindingType el))

-- | Get elements with their dependencies
elementsWithDependencies :: ([Core.Binding] -> Compute.Flow Graph.Graph [Core.Binding])
elementsWithDependencies original =  
  let depNames = (\el -> Sets.toList (Rewriting.termDependencyNames True False False (Core.bindingTerm el)))
  in  
    let allDepNames = (Lists.nub (Lists.concat2 (Lists.map Core.bindingName original) (Lists.concat (Lists.map depNames original))))
    in (Flows.mapList Lexical.requireElement allDepNames)

-- | Extend a type context by descending into a System F lambda body
extendTypeContextForLambda :: (Typing.TypeContext -> Core.Lambda -> Typing.TypeContext)
extendTypeContextForLambda tcontext lam =  
  let var = (Core.lambdaParameter lam)
  in  
    let dom = (Maybes.fromJust (Core.lambdaDomain lam))
    in Typing.TypeContext {
      Typing.typeContextTypes = (Maps.insert var dom (Typing.typeContextTypes tcontext)),
      Typing.typeContextMetadata = (Maps.delete var (Typing.typeContextMetadata tcontext)),
      Typing.typeContextTypeVariables = (Typing.typeContextTypeVariables tcontext),
      Typing.typeContextLambdaVariables = (Sets.insert var (Typing.typeContextLambdaVariables tcontext)),
      Typing.typeContextInferenceContext = (Typing.typeContextInferenceContext tcontext)}

-- | Extend a type context by descending into a let body
extendTypeContextForLet :: ((Typing.TypeContext -> Core.Binding -> Maybe Core.Term) -> Typing.TypeContext -> Core.Let -> Typing.TypeContext)
extendTypeContextForLet forBinding tcontext letrec =  
  let bindings = (Core.letBindings letrec)
  in Typing.TypeContext {
    Typing.typeContextTypes = (Maps.union (Typing.typeContextTypes tcontext) (Maps.fromList (Lists.map (\b -> (Core.bindingName b, (typeSchemeToFType (Maybes.fromJust (Core.bindingType b))))) bindings))),
    Typing.typeContextMetadata = (Lists.foldl (\m -> \b -> Maybes.maybe (Maps.delete (Core.bindingName b) m) (\t -> Maps.insert (Core.bindingName b) t m) (forBinding tcontext b)) (Typing.typeContextMetadata tcontext) bindings),
    Typing.typeContextTypeVariables = (Typing.typeContextTypeVariables tcontext),
    Typing.typeContextLambdaVariables = (Lists.foldl (\s -> \b -> Sets.delete (Core.bindingName b) s) (Typing.typeContextLambdaVariables tcontext) bindings),
    Typing.typeContextInferenceContext = (Typing.typeContextInferenceContext tcontext)}

-- | Extend a type context by descending into a System F type lambda body
extendTypeContextForTypeLambda :: (Typing.TypeContext -> Core.TypeLambda -> Typing.TypeContext)
extendTypeContextForTypeLambda tcontext tlam =  
  let name = (Core.typeLambdaParameter tlam)
  in Typing.TypeContext {
    Typing.typeContextTypes = (Typing.typeContextTypes tcontext),
    Typing.typeContextMetadata = (Typing.typeContextMetadata tcontext),
    Typing.typeContextTypeVariables = (Sets.insert name (Typing.typeContextTypeVariables tcontext)),
    Typing.typeContextLambdaVariables = (Typing.typeContextLambdaVariables tcontext),
    Typing.typeContextInferenceContext = (Typing.typeContextInferenceContext tcontext)}

fieldMap :: ([Core.Field] -> M.Map Core.Name Core.Term)
fieldMap fields =  
  let toPair = (\f -> (Core.fieldName f, (Core.fieldTerm f)))
  in (Maps.fromList (Lists.map toPair fields))

fieldTypeMap :: ([Core.FieldType] -> M.Map Core.Name Core.Type)
fieldTypeMap fields =  
  let toPair = (\f -> (Core.fieldTypeName f, (Core.fieldTypeType f)))
  in (Maps.fromList (Lists.map toPair fields))

-- | Get field types from a record or union type
fieldTypes :: (Core.Type -> Compute.Flow Graph.Graph (M.Map Core.Name Core.Type))
fieldTypes t = (Flows.bind Monads.getState (\cx ->  
  let toMap = (\fields -> Maps.fromList (Lists.map (\ft -> (Core.fieldTypeName ft, (Core.fieldTypeType ft))) fields))
  in ((\x -> case x of
    Core.TypeForall v1 -> (fieldTypes (Core.forallTypeBody v1))
    Core.TypeRecord v1 -> (Flows.pure (toMap (Core.rowTypeFields v1)))
    Core.TypeUnion v1 -> (Flows.pure (toMap (Core.rowTypeFields v1)))
    Core.TypeVariable v1 -> (Monads.withTrace (Strings.cat2 "field types of " (Core.unName v1)) (Flows.bind (Lexical.requireElement v1) (\el -> Flows.bind (Monads.withTrace "field types" (Monads.eitherToFlow Util.unDecodingError (Core_.type_ cx (Core.bindingTerm el)))) fieldTypes)))
    _ -> (Monads.unexpected "record or union type" (Core___.type_ t))) (Rewriting.deannotateType t))))

findFieldType :: (Core.Name -> [Core.FieldType] -> Compute.Flow t0 Core.Type)
findFieldType fname fields =  
  let matchingFields = (Lists.filter (\ft -> Equality.equal (Core.unName (Core.fieldTypeName ft)) (Core.unName fname)) fields)
  in (Logic.ifElse (Lists.null matchingFields) (Flows.fail (Strings.cat2 "No such field: " (Core.unName fname))) (Logic.ifElse (Equality.equal (Lists.length matchingFields) 1) (Flows.pure (Core.fieldTypeType (Lists.head matchingFields))) (Flows.fail (Strings.cat2 "Multiple fields named " (Core.unName fname)))))

freshName :: (Compute.Flow t0 Core.Name)
freshName = (Flows.map normalTypeVariable (Annotations.nextCount Constants.key_freshTypeVariableCount))

freshNames :: (Int -> Compute.Flow t0 [Core.Name])
freshNames n = (Flows.sequence (Lists.replicate n freshName))

-- | Convert a forall type to a type scheme
fTypeToTypeScheme :: (Core.Type -> Core.TypeScheme)
fTypeToTypeScheme typ =  
  let gatherForall = (\vars -> \typ -> (\x -> case x of
          Core.TypeForall v1 -> (gatherForall (Lists.cons (Core.forallTypeParameter v1) vars) (Core.forallTypeBody v1))
          _ -> Core.TypeScheme {
            Core.typeSchemeVariables = (Lists.reverse vars),
            Core.typeSchemeType = typ,
            Core.typeSchemeConstraints = Nothing}) (Rewriting.deannotateType typ))
  in (gatherForall [] typ)

-- | Fully strip a type of forall quantifiers
fullyStripType :: (Core.Type -> Core.Type)
fullyStripType typ = ((\x -> case x of
  Core.TypeForall v1 -> (fullyStripType (Core.forallTypeBody v1))
  _ -> typ) (Rewriting.deannotateType typ))

-- | Convert a graph to a term, taking advantage of the built-in duality between graphs and terms
graphAsTerm :: (Graph.Graph -> Core.Term)
graphAsTerm g =  
  let toBinding = (\el ->  
          let name = (Core.bindingName el)
          in  
            let term = (Core.bindingTerm el)
            in  
              let mts = (Core.bindingType el)
              in Core.Binding {
                Core.bindingName = name,
                Core.bindingTerm = term,
                Core.bindingType = mts})
  in (Core.TermLet (Core.Let {
    Core.letBindings = (Lists.map toBinding (Maps.elems (Graph.graphElements g))),
    Core.letBody = (Graph.graphBody g)}))

-- | Decode a schema graph which encodes a set of named types
graphAsTypes :: (Graph.Graph -> Compute.Flow Graph.Graph (M.Map Core.Name Core.Type))
graphAsTypes sg = (Flows.bind Monads.getState (\cx ->  
  let els = (Maps.elems (Graph.graphElements sg))
  in  
    let toPair = (\el -> Flows.bind (Monads.withTrace (Strings.cat2 "graph as types: " (Core.unName (Core.bindingName el))) (Monads.eitherToFlow Util.unDecodingError (Core_.type_ cx (Core.bindingTerm el)))) (\typ -> Flows.pure (Core.bindingName el, typ)))
    in (Flows.bind (Flows.mapList toPair els) (\pairs -> Flows.pure (Maps.fromList pairs)))))

graphToInferenceContext :: (Graph.Graph -> Compute.Flow t0 Typing.InferenceContext)
graphToInferenceContext graph =  
  let schema = (Maybes.fromMaybe graph (Graph.graphSchema graph))
  in  
    let primTypes = (Maps.fromList (Lists.map (\p -> (Graph.primitiveName p, (Graph.primitiveType p))) (Maps.elems (Graph.graphPrimitives graph))))
    in  
      let varTypes = (Maps.fromList (Maybes.cat (Lists.map (\b -> Maybes.map (\ts -> (Core.bindingName b, ts)) (Core.bindingType b)) (Maps.elems (Graph.graphElements graph)))))
      in (Flows.bind (schemaGraphToTypingEnvironment schema) (\schemaTypes -> Flows.pure (Typing.InferenceContext {
        Typing.inferenceContextSchemaTypes = schemaTypes,
        Typing.inferenceContextPrimitiveTypes = primTypes,
        Typing.inferenceContextDataTypes = varTypes,
        Typing.inferenceContextClassConstraints = Maps.empty,
        Typing.inferenceContextDebug = False})))

graphToTypeContext :: (Graph.Graph -> Compute.Flow t0 Typing.TypeContext)
graphToTypeContext graph = (Flows.bind (graphToInferenceContext graph) (\ix -> Flows.pure (Typing.TypeContext {
  Typing.typeContextTypes = Maps.empty,
  Typing.typeContextMetadata = Maps.empty,
  Typing.typeContextTypeVariables = Sets.empty,
  Typing.typeContextLambdaVariables = Sets.empty,
  Typing.typeContextInferenceContext = ix})))

instantiateType :: (Core.Type -> Compute.Flow t0 Core.Type)
instantiateType typ = (Flows.bind (instantiateTypeScheme (typeToTypeScheme typ)) (\ts -> Flows.pure (typeSchemeToFType ts)))

instantiateTypeScheme :: (Core.TypeScheme -> Compute.Flow t0 Core.TypeScheme)
instantiateTypeScheme scheme =  
  let oldVars = (Core.typeSchemeVariables scheme)
  in (Flows.bind (freshNames (Lists.length oldVars)) (\newVars ->  
    let subst = (Typing.TypeSubst (Maps.fromList (Lists.zip oldVars (Lists.map (\x -> Core.TypeVariable x) newVars))))
    in  
      let nameSubst = (Maps.fromList (Lists.zip oldVars newVars))
      in  
        let renamedConstraints = (Maybes.map (\oldConstraints -> Maps.fromList (Lists.map (\kv -> (Maybes.fromMaybe (Pairs.first kv) (Maps.lookup (Pairs.first kv) nameSubst), (Pairs.second kv))) (Maps.toList oldConstraints))) (Core.typeSchemeConstraints scheme))
        in (Flows.pure (Core.TypeScheme {
          Core.typeSchemeVariables = newVars,
          Core.typeSchemeType = (Substitution.substInType subst (Core.typeSchemeType scheme)),
          Core.typeSchemeConstraints = renamedConstraints}))))

-- | Determines whether a given term is an encoded type
isEncodedType :: (Core.Term -> Bool)
isEncodedType t = ((\x -> case x of
  Core.TermApplication v1 -> (isEncodedType (Core.applicationFunction v1))
  Core.TermUnion v1 -> (Equality.equal "hydra.core.Type" (Core.unName (Core.injectionTypeName v1)))
  _ -> False) (Rewriting.deannotateTerm t))

-- | Check if a row type represents an enum (all fields are unit-typed)
isEnumRowType :: (Core.RowType -> Bool)
isEnumRowType rt = (Lists.foldl Logic.and True (Lists.map (\f -> isUnitType (Rewriting.deannotateType (Core.fieldTypeType f))) (Core.rowTypeFields rt)))

-- | Check if a type is an enum type
isEnumType :: (Core.Type -> Bool)
isEnumType typ = ((\x -> case x of
  Core.TypeUnion v1 -> (isEnumRowType v1)
  _ -> False) (Rewriting.deannotateType typ))

-- | Check if an element is serializable (no function types in dependencies)
isSerializable :: (Core.Binding -> Compute.Flow Graph.Graph Bool)
isSerializable el =  
  let variants = (\typ -> Lists.map Reflect.typeVariant (Rewriting.foldOverType Coders.TraversalOrderPre (\m -> \t -> Lists.cons t m) [] typ))
  in (Flows.map (\deps ->  
    let allVariants = (Sets.fromList (Lists.concat (Lists.map variants (Maps.elems deps))))
    in (Logic.not (Sets.member Variants.TypeVariantFunction allVariants))) (typeDependencies False Equality.identity (Core.bindingName el)))

-- | Check if a type is serializable (no function types in the type itself)
isSerializableType :: (Core.Type -> Bool)
isSerializableType typ =  
  let allVariants = (Sets.fromList (Lists.map Reflect.typeVariant (Rewriting.foldOverType Coders.TraversalOrderPre (\m -> \t -> Lists.cons t m) [] typ)))
  in (Logic.not (Sets.member Variants.TypeVariantFunction allVariants))

-- | Check if a type (by name) is serializable, resolving all type dependencies
isSerializableByName :: (Core.Name -> Compute.Flow Graph.Graph Bool)
isSerializableByName name =  
  let variants = (\typ -> Lists.map Reflect.typeVariant (Rewriting.foldOverType Coders.TraversalOrderPre (\m -> \t -> Lists.cons t m) [] typ))
  in (Flows.map (\deps ->  
    let allVariants = (Sets.fromList (Lists.concat (Lists.map variants (Maps.elems deps))))
    in (Logic.not (Sets.member Variants.TypeVariantFunction allVariants))) (typeDependencies False Equality.identity name))

-- | Check whether a type is a type (always true for non-encoded types)
isType :: (Core.Type -> Bool)
isType t = ((\x -> case x of
  Core.TypeApplication v1 -> (isType (Core.applicationTypeFunction v1))
  Core.TypeForall v1 -> (isType (Core.forallTypeBody v1))
  Core.TypeUnion v1 -> (Equality.equal "hydra.core.Type" (Core.unName (Core.rowTypeTypeName v1)))
  Core.TypeVariable v1 -> (Equality.equal v1 (Core.Name "hydra.core.Type"))
  _ -> False) (Rewriting.deannotateType t))

-- | Check whether a term is the unit term
isUnitTerm :: (Core.Term -> Bool)
isUnitTerm x = case x of
  Core.TermUnit -> True
  _ -> False

-- | Check whether a type is the unit type
isUnitType :: (Core.Type -> Bool)
isUnitType x = case x of
  Core.TypeUnit -> True
  _ -> False

-- | Find dependency namespaces in all elements of a module, excluding the module's own namespace
moduleDependencyNamespaces :: (Bool -> Bool -> Bool -> Bool -> Module.Module -> Compute.Flow Graph.Graph (S.Set Module.Namespace))
moduleDependencyNamespaces binds withPrims withNoms withSchema mod = (Flows.bind (dependencyNamespaces binds withPrims withNoms withSchema (Module.moduleElements mod)) (\deps -> Flows.pure (Sets.delete (Module.moduleNamespace mod) deps)))

namespacesForDefinitions :: ((Module.Namespace -> t0) -> Module.Namespace -> [Module.Definition] -> Module.Namespaces t0)
namespacesForDefinitions encodeNamespace focusNs defs =  
  let nss = (Sets.delete focusNs (definitionDependencyNamespaces defs))
  in  
    let toPair = (\ns -> (ns, (encodeNamespace ns)))
    in Module.Namespaces {
      Module.namespacesFocus = (toPair focusNs),
      Module.namespacesMapping = (Maps.fromList (Lists.map toPair (Sets.toList nss)))}

-- | Apply type arguments to a nominal type
nominalApplication :: (Core.Name -> [Core.Type] -> Core.Type)
nominalApplication tname args = (Lists.foldl (\t -> \a -> Core.TypeApplication (Core.ApplicationType {
  Core.applicationTypeFunction = t,
  Core.applicationTypeArgument = a})) (Core.TypeVariable tname) args)

-- | Type variable naming convention follows Haskell: t0, t1, etc.
normalTypeVariable :: (Int -> Core.Name)
normalTypeVariable i = (Core.Name (Strings.cat2 "t" (Literals.showInt32 i)))

-- | Partition a list of definitions into type definitions and term definitions
partitionDefinitions :: ([Module.Definition] -> ([Module.TypeDefinition], [Module.TermDefinition]))
partitionDefinitions defs =  
  let getType = (\def -> (\x -> case x of
          Module.DefinitionType v1 -> (Just v1)
          Module.DefinitionTerm _ -> Nothing) def)
  in  
    let getTerm = (\def -> (\x -> case x of
            Module.DefinitionType _ -> Nothing
            Module.DefinitionTerm v1 -> (Just v1)) def)
    in (Maybes.cat (Lists.map getType defs), (Maybes.cat (Lists.map getTerm defs)))

-- | Require a name to resolve to a record type
requireRecordType :: (Core.Name -> Compute.Flow Graph.Graph Core.RowType)
requireRecordType name =  
  let toRecord = (\t -> (\x -> case x of
          Core.TypeRecord v1 -> (Just v1)
          _ -> Nothing) t)
  in (requireRowType "record type" toRecord name)

requireRowType :: (String -> (Core.Type -> Maybe t0) -> Core.Name -> Compute.Flow Graph.Graph t0)
requireRowType label getter name =  
  let rawType = (\t -> (\x -> case x of
          Core.TypeAnnotated v1 -> (rawType (Core.annotatedTypeBody v1))
          Core.TypeForall v1 -> (rawType (Core.forallTypeBody v1))
          _ -> t) t)
  in (Flows.bind (requireType name) (\t -> Maybes.maybe (Flows.fail (Strings.cat [
    Core.unName name,
    " does not resolve to a ",
    label,
    " type: ",
    (Core___.type_ t)])) Flows.pure (getter (rawType t))))

requireSchemaType :: (Typing.InferenceContext -> Core.Name -> Compute.Flow t0 Core.TypeScheme)
requireSchemaType cx tname =  
  let types = (Typing.inferenceContextSchemaTypes cx)
  in (Maybes.maybe (Flows.fail (Strings.cat [
    "No such schema type: ",
    Core.unName tname,
    ". Available types are: ",
    (Strings.intercalate ", " (Lists.map Core.unName (Maps.keys types)))])) (\ts -> instantiateTypeScheme (Rewriting.deannotateTypeSchemeRecursive ts)) (Maps.lookup tname types))

-- | Require a type by name
requireType :: (Core.Name -> Compute.Flow Graph.Graph Core.Type)
requireType name = (Flows.bind Monads.getState (\cx -> Monads.withTrace (Strings.cat2 "require type " (Core.unName name)) (Flows.bind (Lexical.withSchemaContext (Lexical.requireElement name)) (\el -> Monads.eitherToFlow Util.unDecodingError (Core_.type_ cx (Core.bindingTerm el))))))

-- | Require a field type from a union type
requireUnionField :: (Core.Name -> Core.Name -> Compute.Flow Graph.Graph Core.Type)
requireUnionField tname fname =  
  let withRowType = (\rt ->  
          let matches = (Lists.filter (\ft -> Equality.equal (Core.fieldTypeName ft) fname) (Core.rowTypeFields rt))
          in (Logic.ifElse (Lists.null matches) (Flows.fail (Strings.cat [
            "no field \"",
            Core.unName fname,
            "\" in union type \"",
            (Core.unName tname)])) (Flows.pure (Core.fieldTypeType (Lists.head matches)))))
  in (Flows.bind (requireUnionType tname) withRowType)

-- | Require a name to resolve to a union type
requireUnionType :: (Core.Name -> Compute.Flow Graph.Graph Core.RowType)
requireUnionType name =  
  let toUnion = (\t -> (\x -> case x of
          Core.TypeUnion v1 -> (Just v1)
          _ -> Nothing) t)
  in (requireRowType "union" toUnion name)

-- | Resolve a type, dereferencing type variables
resolveType :: (Core.Type -> Compute.Flow Graph.Graph (Maybe Core.Type))
resolveType typ = (Flows.bind Monads.getState (\cx -> (\x -> case x of
  Core.TypeVariable v1 -> (Lexical.withSchemaContext (Flows.bind (Lexical.resolveTerm v1) (\mterm -> Maybes.maybe (Flows.pure Nothing) (\t -> Flows.map Maybes.pure (Monads.eitherToFlow Util.unDecodingError (Core_.type_ cx t))) mterm)))
  _ -> (Flows.pure (Just typ))) (Rewriting.deannotateType typ)))

schemaGraphToTypingEnvironment :: (Graph.Graph -> Compute.Flow t0 (M.Map Core.Name Core.TypeScheme))
schemaGraphToTypingEnvironment g =  
  let toTypeScheme = (\vars -> \typ -> (\x -> case x of
          Core.TypeForall v1 -> (toTypeScheme (Lists.cons (Core.forallTypeParameter v1) vars) (Core.forallTypeBody v1))
          _ -> Core.TypeScheme {
            Core.typeSchemeVariables = (Lists.reverse vars),
            Core.typeSchemeType = typ,
            Core.typeSchemeConstraints = Nothing}) (Rewriting.deannotateType typ))
  in  
    let toPair = (\el -> Flows.bind Monads.getState (\cx ->  
            let forTerm = (\term -> (\x -> case x of
                    Core.TermRecord v1 -> (Logic.ifElse (Equality.equal (Core.recordTypeName v1) (Core.Name "hydra.core.TypeScheme")) (Flows.map Maybes.pure (Monads.eitherToFlow Util.unDecodingError (Core_.typeScheme cx (Core.bindingTerm el)))) (Flows.pure Nothing))
                    Core.TermUnion v1 -> (Logic.ifElse (Equality.equal (Core.injectionTypeName v1) (Core.Name "hydra.core.Type")) (Flows.map (\decoded -> Just (toTypeScheme [] decoded)) (Monads.eitherToFlow Util.unDecodingError (Core_.type_ cx (Core.bindingTerm el)))) (Flows.pure Nothing))
                    _ -> (Flows.pure Nothing)) term)
            in (Flows.bind (Maybes.maybe (Flows.map (\typ -> Just (fTypeToTypeScheme typ)) (Monads.eitherToFlow Util.unDecodingError (Core_.type_ cx (Core.bindingTerm el)))) (\ts -> Logic.ifElse (Equality.equal ts (Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.TypeScheme")),
              Core.typeSchemeConstraints = Nothing})) (Flows.map Maybes.pure (Monads.eitherToFlow Util.unDecodingError (Core_.typeScheme cx (Core.bindingTerm el)))) (Logic.ifElse (Equality.equal ts (Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.Type")),
              Core.typeSchemeConstraints = Nothing})) (Flows.map (\decoded -> Just (toTypeScheme [] decoded)) (Monads.eitherToFlow Util.unDecodingError (Core_.type_ cx (Core.bindingTerm el)))) (forTerm (Rewriting.deannotateTerm (Core.bindingTerm el))))) (Core.bindingType el)) (\mts -> Flows.pure (Maybes.map (\ts -> (Core.bindingName el, ts)) mts)))))
    in (Monads.withTrace "schema graph to typing environment" (Monads.withState g (Flows.bind (Flows.mapList toPair (Maps.elems (Graph.graphElements g))) (\mpairs -> Flows.pure (Maps.fromList (Maybes.cat mpairs))))))

-- | Find the equivalent graph representation of a term
termAsGraph :: (Core.Term -> M.Map Core.Name Core.Binding)
termAsGraph term = ((\x -> case x of
  Core.TermLet v1 ->  
    let bindings = (Core.letBindings v1)
    in  
      let fromBinding = (\b ->  
              let name = (Core.bindingName b)
              in  
                let term = (Core.bindingTerm b)
                in  
                  let ts = (Core.bindingType b)
                  in (name, Core.Binding {
                    Core.bindingName = name,
                    Core.bindingTerm = term,
                    Core.bindingType = ts}))
      in (Maps.fromList (Lists.map fromBinding bindings))
  _ -> Maps.empty) (Rewriting.deannotateTerm term))

-- | Topologically sort type definitions by dependencies
topologicalSortTypeDefinitions :: ([Module.TypeDefinition] -> [[Module.TypeDefinition]])
topologicalSortTypeDefinitions defs =  
  let toPair = (\def -> (Module.typeDefinitionName def, (Sets.toList (Rewriting.typeDependencyNames False (Module.typeDefinitionType def)))))
  in  
    let nameToDef = (Maps.fromList (Lists.map (\d -> (Module.typeDefinitionName d, d)) defs))
    in  
      let sorted = (Sorting.topologicalSortComponents (Lists.map toPair defs))
      in (Lists.map (\names -> Maybes.cat (Lists.map (\n -> Maps.lookup n nameToDef) names)) sorted)

-- | Get all type dependencies for a given type name
typeDependencies :: (Bool -> (Core.Type -> Core.Type) -> Core.Name -> Compute.Flow Graph.Graph (M.Map Core.Name Core.Type))
typeDependencies withSchema transform name = (Flows.bind Monads.getState (\cx ->  
  let requireType = (\name -> Monads.withTrace (Strings.cat2 "type dependencies of " (Core.unName name)) (Flows.bind (Lexical.requireElement name) (\el -> Monads.eitherToFlow Util.unDecodingError (Core_.type_ cx (Core.bindingTerm el)))))
  in  
    let toPair = (\name -> Flows.bind (requireType name) (\typ -> Flows.pure (name, (transform typ))))
    in  
      let deps = (\seeds -> \names -> Logic.ifElse (Sets.null seeds) (Flows.pure names) (Flows.bind (Flows.mapList toPair (Sets.toList seeds)) (\pairs ->  
              let newNames = (Maps.union names (Maps.fromList pairs))
              in  
                let refs = (Lists.foldl Sets.union Sets.empty (Lists.map (\pair -> Rewriting.typeDependencyNames withSchema (Pairs.second pair)) pairs))
                in  
                  let visited = (Sets.fromList (Maps.keys names))
                  in  
                    let newSeeds = (Sets.difference refs visited)
                    in (deps newSeeds newNames))))
      in (Monads.withTrace "type dependencies" (deps (Sets.singleton name) Maps.empty))))

-- | Convert a type scheme to a forall type
typeSchemeToFType :: (Core.TypeScheme -> Core.Type)
typeSchemeToFType ts =  
  let vars = (Core.typeSchemeVariables ts)
  in  
    let body = (Core.typeSchemeType ts)
    in (Lists.foldl (\t -> \v -> Core.TypeForall (Core.ForallType {
      Core.forallTypeParameter = v,
      Core.forallTypeBody = t})) body (Lists.reverse vars))

-- | Convert a (System F -style) type to a type scheme
typeToTypeScheme :: (Core.Type -> Core.TypeScheme)
typeToTypeScheme t0 =  
  let helper = (\vars -> \t -> (\x -> case x of
          Core.TypeForall v1 -> (helper (Lists.cons (Core.forallTypeParameter v1) vars) (Core.forallTypeBody v1))
          _ -> Core.TypeScheme {
            Core.typeSchemeVariables = (Lists.reverse vars),
            Core.typeSchemeType = t,
            Core.typeSchemeConstraints = Nothing}) (Rewriting.deannotateType t))
  in (helper [] t0)

-- | Encode a map of named types to a map of elements
typesToElements :: (M.Map Core.Name Core.Type -> M.Map Core.Name Core.Binding)
typesToElements typeMap =  
  let toElement = (\pair ->  
          let name = (Pairs.first pair)
          in (name, Core.Binding {
            Core.bindingName = name,
            Core.bindingTerm = (Core__.type_ (Pairs.second pair)),
            Core.bindingType = Nothing}))
  in (Maps.fromList (Lists.map toElement (Maps.toList typeMap)))

withLambdaContext :: ((t0 -> Typing.TypeContext) -> (Typing.TypeContext -> t0 -> t1) -> t0 -> Core.Lambda -> (t1 -> t2) -> t2)
withLambdaContext getContext setContext env lam body =  
  let newContext = (extendTypeContextForLambda (getContext env) lam)
  in (body (setContext newContext env))

withLetContext :: ((t0 -> Typing.TypeContext) -> (Typing.TypeContext -> t0 -> t1) -> (Typing.TypeContext -> Core.Binding -> Maybe Core.Term) -> t0 -> Core.Let -> (t1 -> t2) -> t2)
withLetContext getContext setContext forBinding env letrec body =  
  let newContext = (extendTypeContextForLet forBinding (getContext env) letrec)
  in (body (setContext newContext env))

withTypeLambdaContext :: ((t0 -> Typing.TypeContext) -> (Typing.TypeContext -> t0 -> t1) -> t0 -> Core.TypeLambda -> (t1 -> t2) -> t2)
withTypeLambdaContext getContext setContext env tlam body =  
  let newContext = (extendTypeContextForTypeLambda (getContext env) tlam)
  in (body (setContext newContext env))
