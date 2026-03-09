-- Note: this is an automatically generated file. Do not edit.

-- | Various functions for dereferencing and decoding schema types.

module Hydra.Schemas where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Encode.Core as Core__
import qualified Hydra.Error as Error
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
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
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import qualified Hydra.Reflect as Reflect
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Show.Core as Core___
import qualified Hydra.Sorting as Sorting
import qualified Hydra.Substitution as Substitution
import qualified Hydra.Typing as Typing
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Add names to existing namespaces mapping
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
          Module.DefinitionType v0 -> (Rewriting.typeDependencyNames True (Module.typeDefinitionType v0))
          Module.DefinitionTerm v0 -> (Rewriting.termDependencyNames True True True (Module.termDefinitionTerm v0))) def)
  in  
    let allNames = (Sets.unions (Lists.map defNames defs))
    in (Sets.fromList (Maybes.cat (Lists.map Names.namespaceOf (Sets.toList allNames))))

-- | Find dependency namespaces in all of a set of terms (Either version)
dependencyNamespaces :: (Context.Context -> Graph.Graph -> Bool -> Bool -> Bool -> Bool -> [Core.Binding] -> Either (Context.InContext Error.OtherError) (S.Set Module.Namespace))
dependencyNamespaces cx graph binds withPrims withNoms withSchema els =  
  let depNames = (\el ->  
          let term = (Core.bindingTerm el)
          in  
            let deannotatedTerm = (Rewriting.deannotateTerm term)
            in  
              let dataNames = (Rewriting.termDependencyNames binds withPrims withNoms term)
              in  
                let schemaNames = (Logic.ifElse withSchema (Maybes.maybe Sets.empty (\ts -> Rewriting.typeDependencyNames True (Core.typeSchemeType ts)) (Core.bindingType el)) Sets.empty)
                in (Logic.ifElse (isEncodedType deannotatedTerm) (Eithers.map (\typ -> Sets.unions [
                  dataNames,
                  schemaNames,
                  (Rewriting.typeDependencyNames True typ)]) (Eithers.bimap (\_wc_e -> Context.InContext {
                  Context.inContextObject = _wc_e,
                  Context.inContextContext = Context.Context {
                    Context.contextTrace = (Lists.cons "dependency namespace (type)" (Context.contextTrace cx)),
                    Context.contextMessages = (Context.contextMessages cx),
                    Context.contextOther = (Context.contextOther cx)}}) (\_wc_a -> _wc_a) (Eithers.bimap (\_e -> Error.OtherError (Error.unDecodingError _e)) (\_a -> _a) (Core_.type_ graph term)))) (Logic.ifElse (isEncodedTerm deannotatedTerm) (Eithers.map (\decodedTerm -> Sets.unions [
                  dataNames,
                  schemaNames,
                  (Rewriting.termDependencyNames binds withPrims withNoms decodedTerm)]) (Eithers.bimap (\_wc_e -> Context.InContext {
                  Context.inContextObject = _wc_e,
                  Context.inContextContext = Context.Context {
                    Context.contextTrace = (Lists.cons "dependency namespace (term)" (Context.contextTrace cx)),
                    Context.contextMessages = (Context.contextMessages cx),
                    Context.contextOther = (Context.contextOther cx)}}) (\_wc_a -> _wc_a) (Eithers.bimap (\_e -> Error.OtherError (Error.unDecodingError _e)) (\_a -> _a) (Core_.term graph term)))) (Right (Sets.unions [
                  dataNames,
                  schemaNames])))))
  in (Eithers.map (\namesList -> Sets.fromList (Maybes.cat (Lists.map Names.namespaceOf (Sets.toList (Sets.delete Constants.placeholderName (Sets.unions namesList)))))) (Eithers.mapList depNames els))

-- | Dereference a type name to get the actual type (Either version)
dereferenceType :: (Context.Context -> Graph.Graph -> Core.Name -> Either (Context.InContext Error.OtherError) (Maybe Core.Type))
dereferenceType cx graph name =  
  let mel = (Lexical.dereferenceElement graph name)
  in (Maybes.maybe (Right Nothing) (\el -> Eithers.map Maybes.pure (Eithers.bimap (\_wc_e -> Context.InContext {
    Context.inContextObject = _wc_e,
    Context.inContextContext = cx}) (\_wc_a -> _wc_a) (Eithers.bimap (\_e -> Error.OtherError (Error.unDecodingError _e)) (\_a -> _a) (Core_.type_ graph (Core.bindingTerm el))))) mel)

-- | Convert an element to a typed term
elementAsTypeApplicationTerm :: (Context.Context -> Core.Binding -> Either (Context.InContext Error.OtherError) Core.TypeApplicationTerm)
elementAsTypeApplicationTerm cx el = (Maybes.maybe (Left (Context.InContext {
  Context.inContextObject = (Error.OtherError "missing element type"),
  Context.inContextContext = cx})) (\ts -> Right (Core.TypeApplicationTerm {
  Core.typeApplicationTermBody = (Core.bindingTerm el),
  Core.typeApplicationTermType = (Core.typeSchemeType ts)})) (Core.bindingType el))

-- | Get elements with their dependencies
elementsWithDependencies :: (Context.Context -> Graph.Graph -> [Core.Binding] -> Either (Context.InContext Error.OtherError) [Core.Binding])
elementsWithDependencies cx graph original =  
  let depNames = (\el -> Sets.toList (Rewriting.termDependencyNames True False False (Core.bindingTerm el)))
  in  
    let allDepNames = (Lists.nub (Lists.concat2 (Lists.map Core.bindingName original) (Lists.concat (Lists.map depNames original))))
    in (Eithers.mapList (\name -> Lexical.requireElement cx graph name) allDepNames)

-- | Extend a graph by descending into a lambda body
extendGraphForLambda :: (Graph.Graph -> Core.Lambda -> Graph.Graph)
extendGraphForLambda g lam =  
  let var = (Core.lambdaParameter lam)
  in Graph.Graph {
    Graph.graphBoundTerms = (Graph.graphBoundTerms g),
    Graph.graphBoundTypes = (Maybes.maybe (Graph.graphBoundTypes g) (\dom -> Maps.insert var (Rewriting.fTypeToTypeScheme dom) (Graph.graphBoundTypes g)) (Core.lambdaDomain lam)),
    Graph.graphClassConstraints = (Graph.graphClassConstraints g),
    Graph.graphLambdaVariables = (Sets.insert var (Graph.graphLambdaVariables g)),
    Graph.graphMetadata = (Maps.delete var (Graph.graphMetadata g)),
    Graph.graphPrimitives = (Graph.graphPrimitives g),
    Graph.graphSchemaTypes = (Graph.graphSchemaTypes g),
    Graph.graphTypeVariables = (Graph.graphTypeVariables g)}

-- | Extend a graph by descending into a let body
extendGraphForLet :: ((Graph.Graph -> Core.Binding -> Maybe Core.Term) -> Graph.Graph -> Core.Let -> Graph.Graph)
extendGraphForLet forBinding g letrec =  
  let bindings = (Core.letBindings letrec)
  in  
    let g2 = (Lexical.extendGraphWithBindings bindings g)
    in Graph.Graph {
      Graph.graphBoundTerms = (Maps.union (Maps.fromList (Lists.map (\b -> (Core.bindingName b, (Core.bindingTerm b))) bindings)) (Graph.graphBoundTerms g)),
      Graph.graphBoundTypes = (Maps.union (Maps.fromList (Maybes.cat (Lists.map (\b -> Maybes.map (\ts -> (Core.bindingName b, ts)) (Core.bindingType b)) bindings))) (Graph.graphBoundTypes g)),
      Graph.graphClassConstraints = (Graph.graphClassConstraints g),
      Graph.graphLambdaVariables = (Lists.foldl (\s -> \b -> Sets.delete (Core.bindingName b) s) (Graph.graphLambdaVariables g) bindings),
      Graph.graphMetadata = (Graph.graphMetadata (Lists.foldl (\gAcc -> \b ->  
        let m = (Graph.graphMetadata gAcc)
        in  
          let newMeta = (Maybes.maybe (Maps.delete (Core.bindingName b) m) (\t -> Maps.insert (Core.bindingName b) t m) (forBinding gAcc b))
          in Graph.Graph {
            Graph.graphBoundTerms = (Graph.graphBoundTerms gAcc),
            Graph.graphBoundTypes = (Graph.graphBoundTypes gAcc),
            Graph.graphClassConstraints = (Graph.graphClassConstraints gAcc),
            Graph.graphLambdaVariables = (Graph.graphLambdaVariables gAcc),
            Graph.graphMetadata = newMeta,
            Graph.graphPrimitives = (Graph.graphPrimitives gAcc),
            Graph.graphSchemaTypes = (Graph.graphSchemaTypes gAcc),
            Graph.graphTypeVariables = (Graph.graphTypeVariables gAcc)}) g2 bindings)),
      Graph.graphPrimitives = (Graph.graphPrimitives g),
      Graph.graphSchemaTypes = (Graph.graphSchemaTypes g),
      Graph.graphTypeVariables = (Graph.graphTypeVariables g)}

-- | Extend a graph by descending into a type lambda body
extendGraphForTypeLambda :: (Graph.Graph -> Core.TypeLambda -> Graph.Graph)
extendGraphForTypeLambda g tlam =  
  let name = (Core.typeLambdaParameter tlam)
  in Graph.Graph {
    Graph.graphBoundTerms = (Graph.graphBoundTerms g),
    Graph.graphBoundTypes = (Graph.graphBoundTypes g),
    Graph.graphClassConstraints = (Graph.graphClassConstraints g),
    Graph.graphLambdaVariables = (Graph.graphLambdaVariables g),
    Graph.graphMetadata = (Graph.graphMetadata g),
    Graph.graphPrimitives = (Graph.graphPrimitives g),
    Graph.graphSchemaTypes = (Graph.graphSchemaTypes g),
    Graph.graphTypeVariables = (Sets.insert name (Graph.graphTypeVariables g))}

fieldMap :: ([Core.Field] -> M.Map Core.Name Core.Term)
fieldMap fields =  
  let toPair = (\f -> (Core.fieldName f, (Core.fieldTerm f)))
  in (Maps.fromList (Lists.map toPair fields))

fieldTypeMap :: ([Core.FieldType] -> M.Map Core.Name Core.Type)
fieldTypeMap fields =  
  let toPair = (\f -> (Core.fieldTypeName f, (Core.fieldTypeType f)))
  in (Maps.fromList (Lists.map toPair fields))

-- | Get field types from a record or union type (Either version)
fieldTypes :: (Context.Context -> Graph.Graph -> Core.Type -> Either (Context.InContext Error.OtherError) (M.Map Core.Name Core.Type))
fieldTypes cx graph t =  
  let toMap = (\fields -> Maps.fromList (Lists.map (\ft -> (Core.fieldTypeName ft, (Core.fieldTypeType ft))) fields))
  in ((\x -> case x of
    Core.TypeForall v0 -> (fieldTypes cx graph (Core.forallTypeBody v0))
    Core.TypeRecord v0 -> (Right (toMap (Core.rowTypeFields v0)))
    Core.TypeUnion v0 -> (Right (toMap (Core.rowTypeFields v0)))
    Core.TypeVariable v0 -> (Eithers.bind (Lexical.requireElement cx graph v0) (\el -> Eithers.bind (Eithers.bimap (\_wc_e -> Context.InContext {
      Context.inContextObject = _wc_e,
      Context.inContextContext = cx}) (\_wc_a -> _wc_a) (Eithers.bimap (\_e -> Error.OtherError (Error.unDecodingError _e)) (\_a -> _a) (Core_.type_ graph (Core.bindingTerm el)))) (\decodedType -> fieldTypes cx graph decodedType)))
    _ -> (Left (Context.InContext {
      Context.inContextObject = (Error.OtherError (Strings.cat [
        "expected record or union type but found ",
        (Core___.type_ t)])),
      Context.inContextContext = cx}))) (Rewriting.deannotateType t))

-- | Find a field type by name in a list of field types
findFieldType :: (Context.Context -> Core.Name -> [Core.FieldType] -> Either (Context.InContext Error.OtherError) Core.Type)
findFieldType cx fname fields =  
  let matchingFields = (Lists.filter (\ft -> Equality.equal (Core.unName (Core.fieldTypeName ft)) (Core.unName fname)) fields)
  in (Logic.ifElse (Lists.null matchingFields) (Left (Context.InContext {
    Context.inContextObject = (Error.OtherError (Strings.cat2 "No such field: " (Core.unName fname))),
    Context.inContextContext = cx})) (Logic.ifElse (Equality.equal (Lists.length matchingFields) 1) (Right (Core.fieldTypeType (Lists.head matchingFields))) (Left (Context.InContext {
    Context.inContextObject = (Error.OtherError (Strings.cat2 "Multiple fields named " (Core.unName fname))),
    Context.inContextContext = cx}))))

-- | Generate a fresh type variable name, threading Context
freshName :: (Context.Context -> (Core.Name, Context.Context))
freshName cx =  
  let count = (Annotations.getCount Constants.key_freshTypeVariableCount cx)
  in (normalTypeVariable count, (Annotations.putCount Constants.key_freshTypeVariableCount (Math.add count 1) cx))

-- | Generate multiple fresh type variable names, threading Context
freshNames :: (Int -> Context.Context -> ([Core.Name], Context.Context))
freshNames n cx =  
  let go = (\acc -> \_ ->  
          let names = (Pairs.first acc)
          in  
            let cx0 = (Pairs.second acc)
            in  
              let result = (freshName cx0)
              in  
                let name = (Pairs.first result)
                in  
                  let cx1 = (Pairs.second result)
                  in (Lists.concat2 names (Lists.pure name), cx1))
  in (Lists.foldl go ([], cx) (Lists.replicate n ()))

-- | Test whether a given System F type is polymorphic (i.e., a forall type)
fTypeIsPolymorphic :: (Core.Type -> Bool)
fTypeIsPolymorphic typ = ((\x -> case x of
  Core.TypeAnnotated v0 -> (fTypeIsPolymorphic (Core.annotatedTypeBody v0))
  Core.TypeForall _ -> True
  _ -> False) typ)

-- | Fully strip a type of forall quantifiers, normalizing bound variable names for alpha-equivalence comparison
fullyStripAndNormalizeType :: (Core.Type -> Core.Type)
fullyStripAndNormalizeType typ =  
  let go = (\depth -> \subst -> \t -> (\x -> case x of
          Core.TypeForall v0 ->  
            let oldVar = (Core.forallTypeParameter v0)
            in  
              let newVar = (Core.Name (Strings.cat2 "_" (Literals.showInt32 depth)))
              in (go (Math.add depth 1) (Maps.insert oldVar newVar subst) (Core.forallTypeBody v0))
          _ -> (subst, t)) (Rewriting.deannotateType t))
  in  
    let result = (go 0 Maps.empty typ)
    in  
      let subst = (Pairs.first result)
      in  
        let body = (Pairs.second result)
        in (Rewriting.substituteTypeVariables subst body)

-- | Fully strip a type of forall quantifiers
fullyStripType :: (Core.Type -> Core.Type)
fullyStripType typ = ((\x -> case x of
  Core.TypeForall v0 -> (fullyStripType (Core.forallTypeBody v0))
  _ -> typ) (Rewriting.deannotateType typ))

-- | Convert bindings and a body to a let expression
graphAsLet :: ([Core.Binding] -> Core.Term -> Core.Let)
graphAsLet bindings body = Core.Let {
  Core.letBindings = bindings,
  Core.letBody = body}

-- | Convert bindings and a body to a term, using let-term duality
graphAsTerm :: ([Core.Binding] -> Core.Term -> Core.Term)
graphAsTerm bindings body = (Core.TermLet (graphAsLet bindings body))

-- | Decode a list of type-encoding bindings into a map of named types
graphAsTypes :: (Context.Context -> Graph.Graph -> [Core.Binding] -> Either (Context.InContext Error.DecodingError) (M.Map Core.Name Core.Type))
graphAsTypes cx graph els =  
  let toPair = (\el -> Eithers.map (\typ -> (Core.bindingName el, typ)) (Eithers.bimap (\_wc_e -> Context.InContext {
          Context.inContextObject = _wc_e,
          Context.inContextContext = cx}) (\_wc_a -> _wc_a) (Core_.type_ graph (Core.bindingTerm el))))
  in (Eithers.map Maps.fromList (Eithers.mapList toPair els))

-- | Instantiate a type by replacing all forall-bound type variables with fresh variables, threading Context
instantiateType :: (Context.Context -> Core.Type -> (Core.Type, Context.Context))
instantiateType cx typ =  
  let result = (instantiateTypeScheme cx (typeToTypeScheme typ))
  in (Rewriting.typeSchemeToFType (Pairs.first result), (Pairs.second result))

-- | Instantiate a type scheme with fresh variables, threading Context
instantiateTypeScheme :: (Context.Context -> Core.TypeScheme -> (Core.TypeScheme, Context.Context))
instantiateTypeScheme cx scheme =  
  let oldVars = (Core.typeSchemeVariables scheme)
  in  
    let result = (freshNames (Lists.length oldVars) cx)
    in  
      let newVars = (Pairs.first result)
      in  
        let cx2 = (Pairs.second result)
        in  
          let subst = (Typing.TypeSubst (Maps.fromList (Lists.zip oldVars (Lists.map (\x -> Core.TypeVariable x) newVars))))
          in  
            let nameSubst = (Maps.fromList (Lists.zip oldVars newVars))
            in  
              let renamedConstraints = (Maybes.map (\oldConstraints -> Maps.fromList (Lists.map (\kv -> (Maybes.fromMaybe (Pairs.first kv) (Maps.lookup (Pairs.first kv) nameSubst), (Pairs.second kv))) (Maps.toList oldConstraints))) (Core.typeSchemeConstraints scheme))
              in (Core.TypeScheme {
                Core.typeSchemeVariables = newVars,
                Core.typeSchemeType = (Substitution.substInType subst (Core.typeSchemeType scheme)),
                Core.typeSchemeConstraints = renamedConstraints}, cx2)

-- | Determines whether a given term is an encoded term (meta-level term)
isEncodedTerm :: (Core.Term -> Bool)
isEncodedTerm t = ((\x -> case x of
  Core.TermApplication v0 -> (isEncodedTerm (Core.applicationFunction v0))
  Core.TermUnion v0 -> (Equality.equal "hydra.core.Term" (Core.unName (Core.injectionTypeName v0)))
  _ -> False) (Rewriting.deannotateTerm t))

-- | Determines whether a given term is an encoded type
isEncodedType :: (Core.Term -> Bool)
isEncodedType t = ((\x -> case x of
  Core.TermApplication v0 -> (isEncodedType (Core.applicationFunction v0))
  Core.TermUnion v0 -> (Equality.equal "hydra.core.Type" (Core.unName (Core.injectionTypeName v0)))
  _ -> False) (Rewriting.deannotateTerm t))

-- | Check if a row type represents an enum (all fields are unit-typed)
isEnumRowType :: (Core.RowType -> Bool)
isEnumRowType rt = (Lists.foldl Logic.and True (Lists.map (\f -> isUnitType (Rewriting.deannotateType (Core.fieldTypeType f))) (Core.rowTypeFields rt)))

-- | Check if a type is an enum type
isEnumType :: (Core.Type -> Bool)
isEnumType typ = ((\x -> case x of
  Core.TypeUnion v0 -> (isEnumRowType v0)
  _ -> False) (Rewriting.deannotateType typ))

-- | Check if an element is serializable (no function types in dependencies) (Either version)
isSerializable :: (Context.Context -> Graph.Graph -> Core.Binding -> Either (Context.InContext Error.OtherError) Bool)
isSerializable cx graph el =  
  let variants = (\typ -> Lists.map Reflect.typeVariant (Rewriting.foldOverType Coders.TraversalOrderPre (\m -> \t -> Lists.cons t m) [] typ))
  in (Eithers.map (\deps ->  
    let allVariants = (Sets.fromList (Lists.concat (Lists.map variants (Maps.elems deps))))
    in (Logic.not (Sets.member Variants.TypeVariantFunction allVariants))) (typeDependencies cx graph False Equality.identity (Core.bindingName el)))

-- | Check if a type is serializable (no function types in the type itself)
isSerializableType :: (Core.Type -> Bool)
isSerializableType typ =  
  let allVariants = (Sets.fromList (Lists.map Reflect.typeVariant (Rewriting.foldOverType Coders.TraversalOrderPre (\m -> \t -> Lists.cons t m) [] typ)))
  in (Logic.not (Sets.member Variants.TypeVariantFunction allVariants))

-- | Check if a type (by name) is serializable, resolving all type dependencies (Either version)
isSerializableByName :: (Context.Context -> Graph.Graph -> Core.Name -> Either (Context.InContext Error.OtherError) Bool)
isSerializableByName cx graph name =  
  let variants = (\typ -> Lists.map Reflect.typeVariant (Rewriting.foldOverType Coders.TraversalOrderPre (\m -> \t -> Lists.cons t m) [] typ))
  in (Eithers.map (\deps ->  
    let allVariants = (Sets.fromList (Lists.concat (Lists.map variants (Maps.elems deps))))
    in (Logic.not (Sets.member Variants.TypeVariantFunction allVariants))) (typeDependencies cx graph False Equality.identity name))

-- | Check whether a type is a type (always true for non-encoded types)
isType :: (Core.Type -> Bool)
isType t = ((\x -> case x of
  Core.TypeApplication v0 -> (isType (Core.applicationTypeFunction v0))
  Core.TypeForall v0 -> (isType (Core.forallTypeBody v0))
  Core.TypeUnion v0 -> (Equality.equal "hydra.core.Type" (Core.unName (Core.rowTypeTypeName v0)))
  Core.TypeVariable v0 -> (Equality.equal v0 (Core.Name "hydra.core.Type"))
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

-- | Check whether a module contains any binary literal values
moduleContainsBinaryLiterals :: (Module.Module -> Bool)
moduleContainsBinaryLiterals mod =  
  let checkTerm = (\found -> \term -> Logic.or found ((\x -> case x of
          Core.TermLiteral v0 -> ((\x -> case x of
            Core.LiteralBinary _ -> True
            _ -> False) v0)
          _ -> False) term))
  in  
    let termContainsBinary = (\term -> Rewriting.foldOverTerm Coders.TraversalOrderPre checkTerm False term)
    in (Lists.foldl (\acc -> \el -> Logic.or acc (termContainsBinary (Core.bindingTerm el))) False (Module.moduleElements mod))

-- | Find dependency namespaces in all elements of a module, excluding the module's own namespace (Either version)
moduleDependencyNamespaces :: (Context.Context -> Graph.Graph -> Bool -> Bool -> Bool -> Bool -> Module.Module -> Either (Context.InContext Error.OtherError) (S.Set Module.Namespace))
moduleDependencyNamespaces cx graph binds withPrims withNoms withSchema mod = (Eithers.map (\deps -> Sets.delete (Module.moduleNamespace mod) deps) (dependencyNamespaces cx graph binds withPrims withNoms withSchema (Module.moduleElements mod)))

-- | Create namespaces mapping for definitions
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
          Module.DefinitionType v0 -> (Just v0)
          Module.DefinitionTerm _ -> Nothing) def)
  in  
    let getTerm = (\def -> (\x -> case x of
            Module.DefinitionType _ -> Nothing
            Module.DefinitionTerm v0 -> (Just v0)) def)
    in (Maybes.cat (Lists.map getType defs), (Maybes.cat (Lists.map getTerm defs)))

-- | Require a name to resolve to a record type
requireRecordType :: (Context.Context -> Graph.Graph -> Core.Name -> Either (Context.InContext Error.OtherError) Core.RowType)
requireRecordType cx graph name =  
  let toRecord = (\t -> (\x -> case x of
          Core.TypeRecord v0 -> (Just v0)
          _ -> Nothing) t)
  in (requireRowType cx "record type" toRecord graph name)

-- | Require a name to resolve to a row type
requireRowType :: (Context.Context -> String -> (Core.Type -> Maybe t0) -> Graph.Graph -> Core.Name -> Either (Context.InContext Error.OtherError) t0)
requireRowType cx label getter graph name =  
  let rawType = (\t -> (\x -> case x of
          Core.TypeAnnotated v0 -> (rawType (Core.annotatedTypeBody v0))
          Core.TypeForall v0 -> (rawType (Core.forallTypeBody v0))
          _ -> t) t)
  in (Eithers.bind (requireType cx graph name) (\t -> Maybes.maybe (Left (Context.InContext {
    Context.inContextObject = (Error.OtherError (Strings.cat [
      Core.unName name,
      " does not resolve to a ",
      label,
      " type: ",
      (Core___.type_ t)])),
    Context.inContextContext = cx})) (\x -> Right x) (getter (rawType t))))

-- | Look up a schema type and instantiate it, threading Context
requireSchemaType :: (Context.Context -> M.Map Core.Name Core.TypeScheme -> Core.Name -> Either (Context.InContext Error.OtherError) (Core.TypeScheme, Context.Context))
requireSchemaType cx types tname = (Maybes.maybe (Left (Context.InContext {
  Context.inContextObject = (Error.OtherError (Strings.cat [
    "No such schema type: ",
    (Core.unName tname),
    ". Available types are: ",
    (Strings.intercalate ", " (Lists.map Core.unName (Maps.keys types)))])),
  Context.inContextContext = cx})) (\ts -> Right (instantiateTypeScheme cx (Rewriting.deannotateTypeSchemeRecursive ts))) (Maps.lookup tname types))

-- | Require a type by name
requireType :: (Context.Context -> Graph.Graph -> Core.Name -> Either (Context.InContext Error.OtherError) Core.Type)
requireType cx graph name = (Maybes.maybe (Maybes.maybe (Left (Context.InContext {
  Context.inContextObject = (Error.OtherError (Strings.cat2 "no such type: " (Core.unName name))),
  Context.inContextContext = cx})) (\ts -> Right (Rewriting.typeSchemeToFType ts)) (Maps.lookup name (Graph.graphBoundTypes graph))) (\ts -> Right (Rewriting.typeSchemeToFType ts)) (Maps.lookup name (Graph.graphSchemaTypes graph)))

-- | Require a field type from a union type
requireUnionField :: (Context.Context -> Graph.Graph -> Core.Name -> Core.Name -> Either (Context.InContext Error.OtherError) Core.Type)
requireUnionField cx graph tname fname =  
  let withRowType = (\rt ->  
          let matches = (Lists.filter (\ft -> Equality.equal (Core.fieldTypeName ft) fname) (Core.rowTypeFields rt))
          in (Logic.ifElse (Lists.null matches) (Left (Context.InContext {
            Context.inContextObject = (Error.OtherError (Strings.cat [
              "no field \"",
              (Core.unName fname),
              "\" in union type \"",
              (Core.unName tname)])),
            Context.inContextContext = cx})) (Right (Core.fieldTypeType (Lists.head matches)))))
  in (Eithers.bind (requireUnionType cx graph tname) withRowType)

-- | Require a name to resolve to a union type
requireUnionType :: (Context.Context -> Graph.Graph -> Core.Name -> Either (Context.InContext Error.OtherError) Core.RowType)
requireUnionType cx graph name =  
  let toUnion = (\t -> (\x -> case x of
          Core.TypeUnion v0 -> (Just v0)
          _ -> Nothing) t)
  in (requireRowType cx "union" toUnion graph name)

-- | Resolve a type, dereferencing type variables
resolveType :: (Graph.Graph -> Core.Type -> Maybe Core.Type)
resolveType graph typ = ((\x -> case x of
  Core.TypeVariable v0 -> (Maybes.maybe (Maybes.map (\ts -> Rewriting.typeSchemeToFType ts) (Maps.lookup v0 (Graph.graphBoundTypes graph))) (\ts -> Just (Rewriting.typeSchemeToFType ts)) (Maps.lookup v0 (Graph.graphSchemaTypes graph)))
  _ -> (Just typ)) (Rewriting.deannotateType typ))

-- | Convert a schema graph to a typing environment (Either version)
schemaGraphToTypingEnvironment :: (Context.Context -> Graph.Graph -> Either (Context.InContext Error.OtherError) (M.Map Core.Name Core.TypeScheme))
schemaGraphToTypingEnvironment cx g =  
  let toTypeScheme = (\vars -> \typ -> (\x -> case x of
          Core.TypeForall v0 -> (toTypeScheme (Lists.cons (Core.forallTypeParameter v0) vars) (Core.forallTypeBody v0))
          _ -> Core.TypeScheme {
            Core.typeSchemeVariables = (Lists.reverse vars),
            Core.typeSchemeType = typ,
            Core.typeSchemeConstraints = Nothing}) (Rewriting.deannotateType typ))
  in  
    let decodeType = (\term -> Eithers.bimap (\_wc_e -> Context.InContext {
            Context.inContextObject = _wc_e,
            Context.inContextContext = cx}) (\_wc_a -> _wc_a) (Eithers.bimap (\_e -> Error.OtherError (Error.unDecodingError _e)) (\_a -> _a) (Core_.type_ g term)))
    in  
      let decodeTypeScheme = (\term -> Eithers.bimap (\_wc_e -> Context.InContext {
              Context.inContextObject = _wc_e,
              Context.inContextContext = cx}) (\_wc_a -> _wc_a) (Eithers.bimap (\_e -> Error.OtherError (Error.unDecodingError _e)) (\_a -> _a) (Core_.typeScheme g term)))
      in  
        let toPair = (\el ->  
                let forTerm = (\term -> (\x -> case x of
                        Core.TermRecord v0 -> (Logic.ifElse (Equality.equal (Core.recordTypeName v0) (Core.Name "hydra.core.TypeScheme")) (Eithers.map Maybes.pure (decodeTypeScheme (Core.bindingTerm el))) (Right Nothing))
                        Core.TermUnion v0 -> (Logic.ifElse (Equality.equal (Core.injectionTypeName v0) (Core.Name "hydra.core.Type")) (Eithers.map (\decoded -> Just (toTypeScheme [] decoded)) (decodeType (Core.bindingTerm el))) (Right Nothing))
                        _ -> (Right Nothing)) term)
                in (Eithers.bind (Maybes.maybe (Eithers.map (\typ -> Just (Rewriting.fTypeToTypeScheme typ)) (decodeType (Core.bindingTerm el))) (\ts -> Logic.ifElse (Equality.equal ts (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.TypeScheme")),
                  Core.typeSchemeConstraints = Nothing})) (Eithers.map Maybes.pure (decodeTypeScheme (Core.bindingTerm el))) (Logic.ifElse (Equality.equal ts (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                  Core.typeSchemeConstraints = Nothing})) (Eithers.map (\decoded -> Just (toTypeScheme [] decoded)) (decodeType (Core.bindingTerm el))) (forTerm (Rewriting.deannotateTerm (Core.bindingTerm el))))) (Core.bindingType el)) (\mts -> Right (Maybes.map (\ts -> (Core.bindingName el, ts)) mts))))
        in (Eithers.map (\mpairs -> Maps.fromList (Maybes.cat mpairs)) (Eithers.mapList toPair (Lexical.graphToBindings g)))

-- | Extract the bindings from a let term, or return an empty list for other terms
termAsBindings :: (Core.Term -> [Core.Binding])
termAsBindings term = ((\x -> case x of
  Core.TermLet v0 -> (Core.letBindings v0)
  _ -> []) (Rewriting.deannotateTerm term))

-- | Topologically sort type definitions by dependencies
topologicalSortTypeDefinitions :: ([Module.TypeDefinition] -> [[Module.TypeDefinition]])
topologicalSortTypeDefinitions defs =  
  let toPair = (\def -> (Module.typeDefinitionName def, (Sets.toList (Rewriting.typeDependencyNames False (Module.typeDefinitionType def)))))
  in  
    let nameToDef = (Maps.fromList (Lists.map (\d -> (Module.typeDefinitionName d, d)) defs))
    in  
      let sorted = (Sorting.topologicalSortComponents (Lists.map toPair defs))
      in (Lists.map (\names -> Maybes.cat (Lists.map (\n -> Maps.lookup n nameToDef) names)) sorted)

-- | Get all type dependencies for a given type name (Either version)
typeDependencies :: (Context.Context -> Graph.Graph -> Bool -> (Core.Type -> Core.Type) -> Core.Name -> Either (Context.InContext Error.OtherError) (M.Map Core.Name Core.Type))
typeDependencies cx graph withSchema transform name =  
  let requireType = (\name ->  
          let cx1 = Context.Context {
                  Context.contextTrace = (Lists.cons (Strings.cat2 "type dependencies of " (Core.unName name)) (Context.contextTrace cx)),
                  Context.contextMessages = (Context.contextMessages cx),
                  Context.contextOther = (Context.contextOther cx)}
          in (Eithers.bind (Lexical.requireElement cx1 graph name) (\el -> Eithers.bimap (\_wc_e -> Context.InContext {
            Context.inContextObject = _wc_e,
            Context.inContextContext = cx1}) (\_wc_a -> _wc_a) (Eithers.bimap (\_e -> Error.OtherError (Error.unDecodingError _e)) (\_a -> _a) (Core_.type_ graph (Core.bindingTerm el))))))
  in  
    let toPair = (\name -> Eithers.map (\typ -> (name, (transform typ))) (requireType name))
    in  
      let deps = (\seeds -> \names -> Logic.ifElse (Sets.null seeds) (Right names) (Eithers.bind (Eithers.mapList toPair (Sets.toList seeds)) (\pairs ->  
              let newNames = (Maps.union names (Maps.fromList pairs))
              in  
                let refs = (Lists.foldl Sets.union Sets.empty (Lists.map (\pair -> Rewriting.typeDependencyNames withSchema (Pairs.second pair)) pairs))
                in  
                  let visited = (Sets.fromList (Maps.keys names))
                  in  
                    let newSeeds = (Sets.difference refs visited)
                    in (deps newSeeds newNames))))
      in (deps (Sets.singleton name) Maps.empty)

-- | Convert a (System F -style) type to a type scheme
typeToTypeScheme :: (Core.Type -> Core.TypeScheme)
typeToTypeScheme t0 =  
  let helper = (\vars -> \t -> (\x -> case x of
          Core.TypeForall v0 -> (helper (Lists.cons (Core.forallTypeParameter v0) vars) (Core.forallTypeBody v0))
          _ -> Core.TypeScheme {
            Core.typeSchemeVariables = (Lists.reverse vars),
            Core.typeSchemeType = t,
            Core.typeSchemeConstraints = Nothing}) (Rewriting.deannotateType t))
  in (helper [] t0)

-- | Encode a map of named types to a list of elements
typesToElements :: (M.Map Core.Name Core.Type -> [Core.Binding])
typesToElements typeMap =  
  let toElement = (\pair ->  
          let name = (Pairs.first pair)
          in Core.Binding {
            Core.bindingName = name,
            Core.bindingTerm = (Core__.type_ (Pairs.second pair)),
            Core.bindingType = Nothing})
  in (Lists.map toElement (Maps.toList typeMap))

-- | Execute a computation in the context of a lambda body, extending the type context with the lambda parameter
withLambdaContext :: ((t0 -> Graph.Graph) -> (Graph.Graph -> t0 -> t1) -> t0 -> Core.Lambda -> (t1 -> t2) -> t2)
withLambdaContext getContext setContext env lam body =  
  let newContext = (extendGraphForLambda (getContext env) lam)
  in (body (setContext newContext env))

-- | Execute a computation in the context of a let body, extending the type context with the let bindings
withLetContext :: ((t0 -> Graph.Graph) -> (Graph.Graph -> t0 -> t1) -> (Graph.Graph -> Core.Binding -> Maybe Core.Term) -> t0 -> Core.Let -> (t1 -> t2) -> t2)
withLetContext getContext setContext forBinding env letrec body =  
  let newContext = (extendGraphForLet forBinding (getContext env) letrec)
  in (body (setContext newContext env))

-- | Execute a computation in the context of a type lambda body, extending the type context with the type parameter
withTypeLambdaContext :: ((t0 -> Graph.Graph) -> (Graph.Graph -> t0 -> t1) -> t0 -> Core.TypeLambda -> (t1 -> t2) -> t2)
withTypeLambdaContext getContext setContext env tlam body =  
  let newContext = (extendGraphForTypeLambda (getContext env) tlam)
  in (body (setContext newContext env))
