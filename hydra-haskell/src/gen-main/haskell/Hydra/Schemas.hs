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
import qualified Hydra.Errors as Errors
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
addNamesToNamespaces :: (Module.Namespace -> t0) -> S.Set Core.Name -> Module.Namespaces t0 -> Module.Namespaces t0
addNamesToNamespaces encodeNamespace names ns0 =

      let nss = Sets.fromList (Maybes.cat (Lists.map Names.namespaceOf (Sets.toList names)))
          toPair = \ns -> (ns, (encodeNamespace ns))
      in Module.Namespaces {
        Module.namespacesFocus = (Module.namespacesFocus ns0),
        Module.namespacesMapping = (Maps.union (Module.namespacesMapping ns0) (Maps.fromList (Lists.map toPair (Sets.toList nss))))}

-- | Get dependency namespaces from definitions
definitionDependencyNamespaces :: [Module.Definition] -> S.Set Module.Namespace
definitionDependencyNamespaces defs =

      let defNames =
              \def -> case def of
                Module.DefinitionType v0 -> Rewriting.typeDependencyNames True (Module.typeDefinitionType v0)
                Module.DefinitionTerm v0 -> Rewriting.termDependencyNames True True True (Module.termDefinitionTerm v0)
          allNames = Sets.unions (Lists.map defNames defs)
      in (Sets.fromList (Maybes.cat (Lists.map Names.namespaceOf (Sets.toList allNames))))

-- | Find dependency namespaces in all of a set of terms (Either version)
dependencyNamespaces :: Context.Context -> Graph.Graph -> Bool -> Bool -> Bool -> Bool -> [Core.Binding] -> Either (Context.InContext Errors.Error) (S.Set Module.Namespace)
dependencyNamespaces cx graph binds withPrims withNoms withSchema els =

      let depNames =
              \el ->
                let term = Core.bindingTerm el
                    deannotatedTerm = Rewriting.deannotateTerm term
                    dataNames = Rewriting.termDependencyNames binds withPrims withNoms term
                    schemaNames =
                            Logic.ifElse withSchema (Maybes.maybe Sets.empty (\ts -> Rewriting.typeDependencyNames True (Core.typeSchemeType ts)) (Core.bindingType el)) Sets.empty
                in (Logic.ifElse (isEncodedType deannotatedTerm) (Eithers.map (\typ -> Sets.unions [
                  dataNames,
                  schemaNames,
                  (Rewriting.typeDependencyNames True typ)]) (Eithers.bimap (\_wc_e -> Context.InContext {
                  Context.inContextObject = _wc_e,
                  Context.inContextContext = Context.Context {
                    Context.contextTrace = (Lists.cons "dependency namespace (type)" (Context.contextTrace cx)),
                    Context.contextMessages = (Context.contextMessages cx),
                    Context.contextOther = (Context.contextOther cx)}}) (\_wc_a -> _wc_a) (Eithers.bimap (\_e -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _e))) (\_a -> _a) (Core_.type_ graph term)))) (Logic.ifElse (isEncodedTerm deannotatedTerm) (Eithers.map (\decodedTerm -> Sets.unions [
                  dataNames,
                  schemaNames,
                  (Rewriting.termDependencyNames binds withPrims withNoms decodedTerm)]) (Eithers.bimap (\_wc_e -> Context.InContext {
                  Context.inContextObject = _wc_e,
                  Context.inContextContext = Context.Context {
                    Context.contextTrace = (Lists.cons "dependency namespace (term)" (Context.contextTrace cx)),
                    Context.contextMessages = (Context.contextMessages cx),
                    Context.contextOther = (Context.contextOther cx)}}) (\_wc_a -> _wc_a) (Eithers.bimap (\_e -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _e))) (\_a -> _a) (Core_.term graph term)))) (Right (Sets.unions [
                  dataNames,
                  schemaNames]))))
      in (Eithers.map (\namesList -> Sets.fromList (Maybes.cat (Lists.map Names.namespaceOf (Sets.toList (Sets.unions namesList))))) (Eithers.mapList depNames els))

-- | Dereference a type name to get the actual type (Either version)
dereferenceType :: Context.Context -> Graph.Graph -> Core.Name -> Either (Context.InContext Errors.Error) (Maybe Core.Type)
dereferenceType cx graph name =

      let mel = Lexical.dereferenceElement graph name
      in (Maybes.maybe (Right Nothing) (\el -> Eithers.map Maybes.pure (Eithers.bimap (\_wc_e -> Context.InContext {
        Context.inContextObject = _wc_e,
        Context.inContextContext = cx}) (\_wc_a -> _wc_a) (Eithers.bimap (\_e -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _e))) (\_a -> _a) (Core_.type_ graph (Core.bindingTerm el))))) mel)

-- | Convert an element to a typed term
elementAsTypeApplicationTerm :: Context.Context -> Core.Binding -> Either (Context.InContext Errors.Error) Core.TypeApplicationTerm
elementAsTypeApplicationTerm cx el =
    Maybes.maybe (Left (Context.InContext {
      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError "missing element type")),
      Context.inContextContext = cx})) (\ts -> Right (Core.TypeApplicationTerm {
      Core.typeApplicationTermBody = (Core.bindingTerm el),
      Core.typeApplicationTermType = (Core.typeSchemeType ts)})) (Core.bindingType el)

-- | Get elements with their dependencies
elementsWithDependencies :: Context.Context -> Graph.Graph -> [Core.Binding] -> Either (Context.InContext Errors.Error) [Core.Binding]
elementsWithDependencies cx graph original =

      let depNames = \el -> Sets.toList (Rewriting.termDependencyNames True False False (Core.bindingTerm el))
          allDepNames = Lists.nub (Lists.concat2 (Lists.map Core.bindingName original) (Lists.concat (Lists.map depNames original)))
      in (Eithers.mapList (\name -> Lexical.requireElement cx graph name) allDepNames)

fieldMap :: [Core.Field] -> M.Map Core.Name Core.Term
fieldMap fields =

      let toPair = \f -> (Core.fieldName f, (Core.fieldTerm f))
      in (Maps.fromList (Lists.map toPair fields))

fieldTypeMap :: [Core.FieldType] -> M.Map Core.Name Core.Type
fieldTypeMap fields =

      let toPair = \f -> (Core.fieldTypeName f, (Core.fieldTypeType f))
      in (Maps.fromList (Lists.map toPair fields))

-- | Get field types from a record or union type (Either version)
fieldTypes :: Context.Context -> Graph.Graph -> Core.Type -> Either (Context.InContext Errors.Error) (M.Map Core.Name Core.Type)
fieldTypes cx graph t =

      let toMap = \fields -> Maps.fromList (Lists.map (\ft -> (Core.fieldTypeName ft, (Core.fieldTypeType ft))) fields)
      in case (Rewriting.deannotateType t) of
        Core.TypeForall v0 -> fieldTypes cx graph (Core.forallTypeBody v0)
        Core.TypeRecord v0 -> Right (toMap v0)
        Core.TypeUnion v0 -> Right (toMap v0)
        Core.TypeVariable v0 -> Eithers.bind (Lexical.requireElement cx graph v0) (\el -> Eithers.bind (Eithers.bimap (\_wc_e -> Context.InContext {
          Context.inContextObject = _wc_e,
          Context.inContextContext = cx}) (\_wc_a -> _wc_a) (Eithers.bimap (\_e -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _e))) (\_a -> _a) (Core_.type_ graph (Core.bindingTerm el)))) (\decodedType -> fieldTypes cx graph decodedType))
        _ -> Left (Context.InContext {
          Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat [
            "expected record or union type but found ",
            (Core___.type_ t)]))),
          Context.inContextContext = cx})

-- | Find a field type by name in a list of field types
findFieldType :: Context.Context -> Core.Name -> [Core.FieldType] -> Either (Context.InContext Errors.Error) Core.Type
findFieldType cx fname fields =

      let matchingFields = Lists.filter (\ft -> Equality.equal (Core.unName (Core.fieldTypeName ft)) (Core.unName fname)) fields
      in (Logic.ifElse (Lists.null matchingFields) (Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "No such field: " (Core.unName fname)))),
        Context.inContextContext = cx})) (Logic.ifElse (Equality.equal (Lists.length matchingFields) 1) (Right (Core.fieldTypeType (Lists.head matchingFields))) (Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "Multiple fields named " (Core.unName fname)))),
        Context.inContextContext = cx}))))

-- | Generate a fresh type variable name, threading Context
freshName :: Context.Context -> (Core.Name, Context.Context)
freshName cx =

      let count = Annotations.getCount Constants.key_freshTypeVariableCount cx
      in (normalTypeVariable count, (Annotations.putCount Constants.key_freshTypeVariableCount (Math.add count 1) cx))

-- | Generate multiple fresh type variable names, threading Context
freshNames :: Int -> Context.Context -> ([Core.Name], Context.Context)
freshNames n cx =

      let go =
              \acc -> \_ ->
                let names = Pairs.first acc
                    cx0 = Pairs.second acc
                    result = freshName cx0
                    name = Pairs.first result
                    cx1 = Pairs.second result
                in (Lists.concat2 names (Lists.pure name), cx1)
      in (Lists.foldl go ([], cx) (Lists.replicate n ()))

-- | Test whether a given System F type is polymorphic (i.e., a forall type)
fTypeIsPolymorphic :: Core.Type -> Bool
fTypeIsPolymorphic typ =
    case typ of
      Core.TypeAnnotated v0 -> fTypeIsPolymorphic (Core.annotatedTypeBody v0)
      Core.TypeForall _ -> True
      _ -> False

-- | Fully strip a type of forall quantifiers, normalizing bound variable names for alpha-equivalence comparison
fullyStripAndNormalizeType :: Core.Type -> Core.Type
fullyStripAndNormalizeType typ =

      let go =
              \depth -> \subst -> \t -> case (Rewriting.deannotateType t) of
                Core.TypeForall v0 ->
                  let oldVar = Core.forallTypeParameter v0
                      newVar = Core.Name (Strings.cat2 "_" (Literals.showInt32 depth))
                  in (go (Math.add depth 1) (Maps.insert oldVar newVar subst) (Core.forallTypeBody v0))
                _ -> (subst, t)
          result = go 0 Maps.empty typ
          subst = Pairs.first result
          body = Pairs.second result
      in (Rewriting.substituteTypeVariables subst body)

-- | Fully strip a type of forall quantifiers
fullyStripType :: Core.Type -> Core.Type
fullyStripType typ =
    case (Rewriting.deannotateType typ) of
      Core.TypeForall v0 -> fullyStripType (Core.forallTypeBody v0)
      _ -> typ

-- | Convert bindings and a body to a let expression
graphAsLet :: [Core.Binding] -> Core.Term -> Core.Let
graphAsLet bindings body =
    Core.Let {
      Core.letBindings = bindings,
      Core.letBody = body}

-- | Convert bindings and a body to a term, using let-term duality
graphAsTerm :: [Core.Binding] -> Core.Term -> Core.Term
graphAsTerm bindings body = Core.TermLet (graphAsLet bindings body)

-- | Decode a list of type-encoding bindings into a map of named types
graphAsTypes :: Context.Context -> Graph.Graph -> [Core.Binding] -> Either (Context.InContext Errors.DecodingError) (M.Map Core.Name Core.Type)
graphAsTypes cx graph els =

      let toPair =
              \el -> Eithers.map (\typ -> (Core.bindingName el, typ)) (Eithers.bimap (\_wc_e -> Context.InContext {
                Context.inContextObject = _wc_e,
                Context.inContextContext = cx}) (\_wc_a -> _wc_a) (Core_.type_ graph (Core.bindingTerm el)))
      in (Eithers.map Maps.fromList (Eithers.mapList toPair els))

-- | Instantiate a type by replacing all forall-bound type variables with fresh variables, threading Context
instantiateType :: Context.Context -> Core.Type -> (Core.Type, Context.Context)
instantiateType cx typ =

      let result = instantiateTypeScheme cx (typeToTypeScheme typ)
      in (Rewriting.typeSchemeToFType (Pairs.first result), (Pairs.second result))

-- | Instantiate a type scheme with fresh variables, threading Context
instantiateTypeScheme :: Context.Context -> Core.TypeScheme -> (Core.TypeScheme, Context.Context)
instantiateTypeScheme cx scheme =

      let oldVars = Core.typeSchemeVariables scheme
          result = freshNames (Lists.length oldVars) cx
          newVars = Pairs.first result
          cx2 = Pairs.second result
          subst = Typing.TypeSubst (Maps.fromList (Lists.zip oldVars (Lists.map (\x -> Core.TypeVariable x) newVars)))
          nameSubst = Maps.fromList (Lists.zip oldVars newVars)
          renamedConstraints =
                  Maybes.map (\oldConstraints -> Maps.fromList (Lists.map (\kv -> (Maybes.fromMaybe (Pairs.first kv) (Maps.lookup (Pairs.first kv) nameSubst), (Pairs.second kv))) (Maps.toList oldConstraints))) (Core.typeSchemeConstraints scheme)
      in (Core.TypeScheme {
        Core.typeSchemeVariables = newVars,
        Core.typeSchemeType = (Substitution.substInType subst (Core.typeSchemeType scheme)),
        Core.typeSchemeConstraints = renamedConstraints}, cx2)

-- | Determines whether a given term is an encoded term (meta-level term)
isEncodedTerm :: Core.Term -> Bool
isEncodedTerm t =
    case (Rewriting.deannotateTerm t) of
      Core.TermApplication v0 -> isEncodedTerm (Core.applicationFunction v0)
      Core.TermUnion v0 -> Equality.equal "hydra.core.Term" (Core.unName (Core.injectionTypeName v0))
      _ -> False

-- | Determines whether a given term is an encoded type
isEncodedType :: Core.Term -> Bool
isEncodedType t =
    case (Rewriting.deannotateTerm t) of
      Core.TermApplication v0 -> isEncodedType (Core.applicationFunction v0)
      Core.TermUnion v0 -> Equality.equal "hydra.core.Type" (Core.unName (Core.injectionTypeName v0))
      _ -> False

-- | Check if a row type represents an enum (all fields are unit-typed)
isEnumRowType :: [Core.FieldType] -> Bool
isEnumRowType rt =
    Lists.foldl Logic.and True (Lists.map (\f -> isUnitType (Rewriting.deannotateType (Core.fieldTypeType f))) rt)

-- | Check if a type is an enum type
isEnumType :: Core.Type -> Bool
isEnumType typ =
    case (Rewriting.deannotateType typ) of
      Core.TypeUnion v0 -> isEnumRowType v0
      _ -> False

-- | Check if an element is serializable (no function types in dependencies) (Either version)
isSerializable :: Context.Context -> Graph.Graph -> Core.Binding -> Either (Context.InContext Errors.Error) Bool
isSerializable cx graph el =

      let variants =
              \typ -> Lists.map Reflect.typeVariant (Rewriting.foldOverType Coders.TraversalOrderPre (\m -> \t -> Lists.cons t m) [] typ)
      in (Eithers.map (\deps ->
        let allVariants = Sets.fromList (Lists.concat (Lists.map variants (Maps.elems deps)))
        in (Logic.not (Sets.member Variants.TypeVariantFunction allVariants))) (typeDependencies cx graph False Equality.identity (Core.bindingName el)))

-- | Check if a type is serializable (no function types in the type itself)
isSerializableType :: Core.Type -> Bool
isSerializableType typ =

      let allVariants =
              Sets.fromList (Lists.map Reflect.typeVariant (Rewriting.foldOverType Coders.TraversalOrderPre (\m -> \t -> Lists.cons t m) [] typ))
      in (Logic.not (Sets.member Variants.TypeVariantFunction allVariants))

-- | Check if a type (by name) is serializable, resolving all type dependencies (Either version)
isSerializableByName :: Context.Context -> Graph.Graph -> Core.Name -> Either (Context.InContext Errors.Error) Bool
isSerializableByName cx graph name =

      let variants =
              \typ -> Lists.map Reflect.typeVariant (Rewriting.foldOverType Coders.TraversalOrderPre (\m -> \t -> Lists.cons t m) [] typ)
      in (Eithers.map (\deps ->
        let allVariants = Sets.fromList (Lists.concat (Lists.map variants (Maps.elems deps)))
        in (Logic.not (Sets.member Variants.TypeVariantFunction allVariants))) (typeDependencies cx graph False Equality.identity name))

isNominalType :: Core.Type -> Bool
isNominalType typ =
    case (Rewriting.deannotateType typ) of
      Core.TypeRecord _ -> True
      Core.TypeUnion _ -> True
      Core.TypeWrap _ -> True
      Core.TypeForall v0 -> isNominalType (Core.forallTypeBody v0)
      _ -> False

-- | Check whether a type is a type (always true for non-encoded types)
isType :: Core.Type -> Bool
isType t =
    case (Rewriting.deannotateType t) of
      Core.TypeApplication v0 -> isType (Core.applicationTypeFunction v0)
      Core.TypeForall v0 -> isType (Core.forallTypeBody v0)
      Core.TypeUnion _ -> False
      Core.TypeVariable v0 -> Equality.equal v0 (Core.Name "hydra.core.Type")
      _ -> False

-- | Check whether a term is the unit term
isUnitTerm :: Core.Term -> Bool
isUnitTerm x =
    case x of
      Core.TermUnit -> True
      _ -> False

-- | Check whether a type is the unit type
isUnitType :: Core.Type -> Bool
isUnitType x =
    case x of
      Core.TypeUnit -> True
      _ -> False

-- | Check whether a module contains any binary literal values
moduleContainsBinaryLiterals :: Module.Module -> Bool
moduleContainsBinaryLiterals mod =

      let checkTerm =
              \found -> \term -> Logic.or found (case term of
                Core.TermLiteral v0 -> case v0 of
                  Core.LiteralBinary _ -> True
                  _ -> False
                _ -> False)
          termContainsBinary = \term -> Rewriting.foldOverTerm Coders.TraversalOrderPre checkTerm False term
          defTerms =
                  Maybes.cat (Lists.map (\d -> case d of
                    Module.DefinitionTerm v0 -> Just (Module.termDefinitionTerm v0)
                    _ -> Nothing) (Module.moduleDefinitions mod))
      in (Lists.foldl (\acc -> \t -> Logic.or acc (termContainsBinary t)) False defTerms)

-- | Find dependency namespaces in all elements of a module, excluding the module's own namespace (Either version)
moduleDependencyNamespaces :: Context.Context -> Graph.Graph -> Bool -> Bool -> Bool -> Bool -> Module.Module -> Either (Context.InContext Errors.Error) (S.Set Module.Namespace)
moduleDependencyNamespaces cx graph binds withPrims withNoms withSchema mod =

      let allBindings =
              Maybes.cat (Lists.map (\d -> case d of
                Module.DefinitionType v0 -> Just (Annotations.typeElement (Module.typeDefinitionName v0) (Module.typeDefinitionType v0))
                Module.DefinitionTerm v0 -> Just (Core.Binding {
                  Core.bindingName = (Module.termDefinitionName v0),
                  Core.bindingTerm = (Module.termDefinitionTerm v0),
                  Core.bindingType = (Module.termDefinitionType v0)})
                _ -> Nothing) (Module.moduleDefinitions mod))
      in (Eithers.map (\deps -> Sets.delete (Module.moduleNamespace mod) deps) (dependencyNamespaces cx graph binds withPrims withNoms withSchema allBindings))

-- | Create namespaces mapping for definitions
namespacesForDefinitions :: (Module.Namespace -> t0) -> Module.Namespace -> [Module.Definition] -> Module.Namespaces t0
namespacesForDefinitions encodeNamespace focusNs defs =

      let nss = Sets.delete focusNs (definitionDependencyNamespaces defs)
          toPair = \ns -> (ns, (encodeNamespace ns))
      in Module.Namespaces {
        Module.namespacesFocus = (toPair focusNs),
        Module.namespacesMapping = (Maps.fromList (Lists.map toPair (Sets.toList nss)))}

-- | Apply type arguments to a nominal type
nominalApplication :: Core.Name -> [Core.Type] -> Core.Type
nominalApplication tname args =
    Lists.foldl (\t -> \a -> Core.TypeApplication (Core.ApplicationType {
      Core.applicationTypeFunction = t,
      Core.applicationTypeArgument = a})) (Core.TypeVariable tname) args

-- | Type variable naming convention follows Haskell: t0, t1, etc.
normalTypeVariable :: Int -> Core.Name
normalTypeVariable i = Core.Name (Strings.cat2 "t" (Literals.showInt32 i))

-- | Partition a list of definitions into type definitions and term definitions
partitionDefinitions :: [Module.Definition] -> ([Module.TypeDefinition], [Module.TermDefinition])
partitionDefinitions defs =

      let getType =
              \def -> case def of
                Module.DefinitionType v0 -> Just v0
                Module.DefinitionTerm _ -> Nothing
          getTerm =
                  \def -> case def of
                    Module.DefinitionType _ -> Nothing
                    Module.DefinitionTerm v0 -> Just v0
      in (Maybes.cat (Lists.map getType defs), (Maybes.cat (Lists.map getTerm defs)))

-- | Require a name to resolve to a record type
requireRecordType :: Context.Context -> Graph.Graph -> Core.Name -> Either (Context.InContext Errors.Error) [Core.FieldType]
requireRecordType cx graph name =

      let toRecord =
              \t -> case t of
                Core.TypeRecord v0 -> Just v0
                _ -> Nothing
      in (requireRowType cx "record type" toRecord graph name)

-- | Require a name to resolve to a row type
requireRowType :: Context.Context -> String -> (Core.Type -> Maybe t0) -> Graph.Graph -> Core.Name -> Either (Context.InContext Errors.Error) t0
requireRowType cx label getter graph name =

      let rawType =
              \t -> case t of
                Core.TypeAnnotated v0 -> rawType (Core.annotatedTypeBody v0)
                Core.TypeForall v0 -> rawType (Core.forallTypeBody v0)
                _ -> t
      in (Eithers.bind (requireType cx graph name) (\t -> Maybes.maybe (Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat [
          Core.unName name,
          " does not resolve to a ",
          label,
          " type: ",
          (Core___.type_ t)]))),
        Context.inContextContext = cx})) (\x -> Right x) (getter (rawType t))))

-- | Look up a schema type and instantiate it, threading Context
requireSchemaType :: Context.Context -> M.Map Core.Name Core.TypeScheme -> Core.Name -> Either (Context.InContext Errors.Error) (Core.TypeScheme, Context.Context)
requireSchemaType cx types tname =
    Maybes.maybe (Left (Context.InContext {
      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat [
        "No such schema type: ",
        (Core.unName tname),
        ". Available types are: ",
        (Strings.intercalate ", " (Lists.map Core.unName (Maps.keys types)))]))),
      Context.inContextContext = cx})) (\ts -> Right (instantiateTypeScheme cx (Rewriting.deannotateTypeSchemeRecursive ts))) (Maps.lookup tname types)

-- | Require a type by name
requireType :: Context.Context -> Graph.Graph -> Core.Name -> Either (Context.InContext Errors.Error) Core.Type
requireType cx graph name =
    Maybes.maybe (Maybes.maybe (Left (Context.InContext {
      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "no such type: " (Core.unName name)))),
      Context.inContextContext = cx})) (\ts -> Right (Rewriting.typeSchemeToFType ts)) (Maps.lookup name (Graph.graphBoundTypes graph))) (\ts -> Right (Rewriting.typeSchemeToFType ts)) (Maps.lookup name (Graph.graphSchemaTypes graph))

-- | Require a field type from a union type
requireUnionField :: Context.Context -> Graph.Graph -> Core.Name -> Core.Name -> Either (Context.InContext Errors.Error) Core.Type
requireUnionField cx graph tname fname =

      let withRowType =
              \rt ->
                let matches = Lists.filter (\ft -> Equality.equal (Core.fieldTypeName ft) fname) rt
                in (Logic.ifElse (Lists.null matches) (Left (Context.InContext {
                  Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                    "no field \"",
                    (Core.unName fname),
                    "\" in union type \"",
                    (Core.unName tname)]))),
                  Context.inContextContext = cx})) (Right (Core.fieldTypeType (Lists.head matches))))
      in (Eithers.bind (requireUnionType cx graph tname) withRowType)

-- | Require a name to resolve to a union type
requireUnionType :: Context.Context -> Graph.Graph -> Core.Name -> Either (Context.InContext Errors.Error) [Core.FieldType]
requireUnionType cx graph name =

      let toUnion =
              \t -> case t of
                Core.TypeUnion v0 -> Just v0
                _ -> Nothing
      in (requireRowType cx "union" toUnion graph name)

-- | Resolve a type, dereferencing type variables
resolveType :: Graph.Graph -> Core.Type -> Maybe Core.Type
resolveType graph typ =
    case (Rewriting.deannotateType typ) of
      Core.TypeVariable v0 -> Maybes.maybe (Maybes.map (\ts -> Rewriting.typeSchemeToFType ts) (Maps.lookup v0 (Graph.graphBoundTypes graph))) (\ts -> Just (Rewriting.typeSchemeToFType ts)) (Maps.lookup v0 (Graph.graphSchemaTypes graph))
      _ -> Just typ

-- | Convert a schema graph to a typing environment (Either version)
schemaGraphToTypingEnvironment :: Context.Context -> Graph.Graph -> Either (Context.InContext Errors.Error) (M.Map Core.Name Core.TypeScheme)
schemaGraphToTypingEnvironment cx g =

      let toTypeScheme =
              \vars -> \typ -> case (Rewriting.deannotateType typ) of
                Core.TypeForall v0 -> toTypeScheme (Lists.cons (Core.forallTypeParameter v0) vars) (Core.forallTypeBody v0)
                _ -> Core.TypeScheme {
                  Core.typeSchemeVariables = (Lists.reverse vars),
                  Core.typeSchemeType = typ,
                  Core.typeSchemeConstraints = Nothing}
          decodeType =
                  \term -> Eithers.bimap (\_wc_e -> Context.InContext {
                    Context.inContextObject = _wc_e,
                    Context.inContextContext = cx}) (\_wc_a -> _wc_a) (Eithers.bimap (\_e -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _e))) (\_a -> _a) (Core_.type_ g term))
          decodeTypeScheme =
                  \term -> Eithers.bimap (\_wc_e -> Context.InContext {
                    Context.inContextObject = _wc_e,
                    Context.inContextContext = cx}) (\_wc_a -> _wc_a) (Eithers.bimap (\_e -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _e))) (\_a -> _a) (Core_.typeScheme g term))
          toPair =
                  \el ->
                    let forTerm =
                            \term -> case term of
                              Core.TermRecord v0 -> Logic.ifElse (Equality.equal (Core.recordTypeName v0) (Core.Name "hydra.core.TypeScheme")) (Eithers.map Maybes.pure (decodeTypeScheme (Core.bindingTerm el))) (Right Nothing)
                              Core.TermUnion v0 -> Logic.ifElse (Equality.equal (Core.injectionTypeName v0) (Core.Name "hydra.core.Type")) (Eithers.map (\decoded -> Just (toTypeScheme [] decoded)) (decodeType (Core.bindingTerm el))) (Right Nothing)
                              _ -> Right Nothing
                    in (Eithers.bind (Maybes.maybe (Eithers.map (\typ -> Just (Rewriting.fTypeToTypeScheme typ)) (decodeType (Core.bindingTerm el))) (\ts -> Logic.ifElse (Equality.equal ts (Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.TypeScheme")),
                      Core.typeSchemeConstraints = Nothing})) (Eithers.map Maybes.pure (decodeTypeScheme (Core.bindingTerm el))) (Logic.ifElse (Equality.equal ts (Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                      Core.typeSchemeConstraints = Nothing})) (Eithers.map (\decoded -> Just (toTypeScheme [] decoded)) (decodeType (Core.bindingTerm el))) (forTerm (Rewriting.deannotateTerm (Core.bindingTerm el))))) (Core.bindingType el)) (\mts -> Right (Maybes.map (\ts -> (Core.bindingName el, ts)) mts)))
      in (Eithers.map (\mpairs -> Maps.fromList (Maybes.cat mpairs)) (Eithers.mapList toPair (Lexical.graphToBindings g)))

-- | Extract the bindings from a let term, or return an empty list for other terms
termAsBindings :: Core.Term -> [Core.Binding]
termAsBindings term =
    case (Rewriting.deannotateTerm term) of
      Core.TermLet v0 -> Core.letBindings v0
      _ -> []

-- | Topologically sort type definitions by dependencies
topologicalSortTypeDefinitions :: [Module.TypeDefinition] -> [[Module.TypeDefinition]]
topologicalSortTypeDefinitions defs =

      let toPair =
              \def -> (Module.typeDefinitionName def, (Sets.toList (Rewriting.typeDependencyNames False (Module.typeDefinitionType def))))
          nameToDef = Maps.fromList (Lists.map (\d -> (Module.typeDefinitionName d, d)) defs)
          sorted = Sorting.topologicalSortComponents (Lists.map toPair defs)
      in (Lists.map (\names -> Maybes.cat (Lists.map (\n -> Maps.lookup n nameToDef) names)) sorted)

-- | Get all type dependencies for a given type name (Either version)
typeDependencies :: Context.Context -> Graph.Graph -> Bool -> (Core.Type -> Core.Type) -> Core.Name -> Either (Context.InContext Errors.Error) (M.Map Core.Name Core.Type)
typeDependencies cx graph withSchema transform name =

      let requireType =
              \name ->
                let cx1 =
                        Context.Context {
                          Context.contextTrace = (Lists.cons (Strings.cat2 "type dependencies of " (Core.unName name)) (Context.contextTrace cx)),
                          Context.contextMessages = (Context.contextMessages cx),
                          Context.contextOther = (Context.contextOther cx)}
                in (Eithers.bind (Lexical.requireElement cx1 graph name) (\el -> Eithers.bimap (\_wc_e -> Context.InContext {
                  Context.inContextObject = _wc_e,
                  Context.inContextContext = cx1}) (\_wc_a -> _wc_a) (Eithers.bimap (\_e -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _e))) (\_a -> _a) (Core_.type_ graph (Core.bindingTerm el)))))
          toPair = \name -> Eithers.map (\typ -> (name, (transform typ))) (requireType name)
          deps =
                  \seeds -> \names -> Logic.ifElse (Sets.null seeds) (Right names) (Eithers.bind (Eithers.mapList toPair (Sets.toList seeds)) (\pairs ->
                    let newNames = Maps.union names (Maps.fromList pairs)
                        refs =
                                Lists.foldl Sets.union Sets.empty (Lists.map (\pair -> Rewriting.typeDependencyNames withSchema (Pairs.second pair)) pairs)
                        visited = Sets.fromList (Maps.keys names)
                        newSeeds = Sets.difference refs visited
                    in (deps newSeeds newNames)))
      in (deps (Sets.singleton name) Maps.empty)

-- | Convert a (System F -style) type to a type scheme
typeToTypeScheme :: Core.Type -> Core.TypeScheme
typeToTypeScheme t0 =

      let helper =
              \vars -> \t -> case (Rewriting.deannotateType t) of
                Core.TypeForall v0 -> helper (Lists.cons (Core.forallTypeParameter v0) vars) (Core.forallTypeBody v0)
                _ -> Core.TypeScheme {
                  Core.typeSchemeVariables = (Lists.reverse vars),
                  Core.typeSchemeType = t,
                  Core.typeSchemeConstraints = Nothing}
      in (helper [] t0)

-- | Encode a map of named types to a list of elements
typesToElements :: M.Map Core.Name Core.Type -> [Core.Binding]
typesToElements typeMap =

      let toElement =
              \pair ->
                let name = Pairs.first pair
                in Core.Binding {
                  Core.bindingName = name,
                  Core.bindingTerm = (Core__.type_ (Pairs.second pair)),
                  Core.bindingType = Nothing}
      in (Lists.map toElement (Maps.toList typeMap))

-- | Execute a computation in the context of a lambda body, extending the type context with the lambda parameter
withLambdaContext :: (t0 -> Graph.Graph) -> (Graph.Graph -> t0 -> t1) -> t0 -> Core.Lambda -> (t1 -> t2) -> t2
withLambdaContext getContext setContext env lam body =

      let newContext = Rewriting.extendGraphForLambda (getContext env) lam
      in (body (setContext newContext env))

-- | Execute a computation in the context of a let body, extending the type context with the let bindings
withLetContext :: (t0 -> Graph.Graph) -> (Graph.Graph -> t0 -> t1) -> (Graph.Graph -> Core.Binding -> Maybe Core.Term) -> t0 -> Core.Let -> (t1 -> t2) -> t2
withLetContext getContext setContext forBinding env letrec body =

      let newContext = Rewriting.extendGraphForLet forBinding (getContext env) letrec
      in (body (setContext newContext env))

-- | Execute a computation in the context of a type lambda body, extending the type context with the type parameter
withTypeLambdaContext :: (t0 -> Graph.Graph) -> (Graph.Graph -> t0 -> t1) -> t0 -> Core.TypeLambda -> (t1 -> t2) -> t2
withTypeLambdaContext getContext setContext env tlam body =

      let newContext = Rewriting.extendGraphForTypeLambda (getContext env) tlam
      in (body (setContext newContext env))
