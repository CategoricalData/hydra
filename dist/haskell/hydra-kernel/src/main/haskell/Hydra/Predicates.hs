-- Note: this is an automatically generated file. Do not edit.

-- | Type and term classification predicates

module Hydra.Predicates where

import qualified Hydra.Arity as Arity
import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Dependencies as Dependencies
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Reflect as Reflect
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Strip as Strip
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

-- | Check if a binding needs to be treated as a function
isComplexBinding :: Graph.Graph -> Core.Binding -> Bool
isComplexBinding tc b =

      let term = Core.bindingTerm b
          mts = Core.bindingType b
      in (Maybes.cases mts (isComplexTerm tc term) (\ts ->
        let isPolymorphic = Logic.not (Lists.null (Core.typeSchemeVariables ts))
            isNonNullary = Equality.gt (Arity.typeArity (Core.typeSchemeType ts)) 0
            isComplex = isComplexTerm tc term
        in (Logic.or (Logic.or isPolymorphic isNonNullary) isComplex)))

-- | Check if a term needs to be treated as a function rather than a simple value
isComplexTerm :: Graph.Graph -> Core.Term -> Bool
isComplexTerm tc t =
    case t of
      Core.TermLet _ -> True
      Core.TermTypeApplication _ -> True
      Core.TermTypeLambda _ -> True
      Core.TermVariable v0 -> isComplexVariable tc v0
      _ -> Lists.foldl (\b -> \sub -> Logic.or b (isComplexTerm tc sub)) False (Rewriting.subterms t)

-- | Check if a variable is bound to a complex term
isComplexVariable :: Graph.Graph -> Core.Name -> Bool
isComplexVariable tc name =

      let metaLookup = Maps.lookup name (Graph.graphMetadata tc)
      in (Logic.ifElse (Maybes.isJust metaLookup) True (Logic.ifElse (Sets.member name (Graph.graphLambdaVariables tc)) True (
        let typeLookup = Maps.lookup name (Graph.graphBoundTypes tc)
        in (Maybes.maybe (
          let primLookup = Maps.lookup name (Graph.graphPrimitives tc)
          in (Maybes.maybe True (\prim -> Equality.gt (Arity.typeSchemeArity (Graph.primitiveType prim)) 0) primLookup)) (\ts -> Equality.gt (Arity.typeSchemeArity ts) 0) typeLookup))))

-- | Determines whether a given term is an encoded term (meta-level term)
isEncodedTerm :: Core.Term -> Bool
isEncodedTerm t =
    case (Strip.deannotateTerm t) of
      Core.TermApplication v0 -> isEncodedTerm (Core.applicationFunction v0)
      Core.TermInject v0 -> Equality.equal "hydra.core.Term" (Core.unName (Core.injectionTypeName v0))
      _ -> False

-- | Determines whether a given term is an encoded type
isEncodedType :: Core.Term -> Bool
isEncodedType t =
    case (Strip.deannotateTerm t) of
      Core.TermApplication v0 -> isEncodedType (Core.applicationFunction v0)
      Core.TermInject v0 -> Equality.equal "hydra.core.Type" (Core.unName (Core.injectionTypeName v0))
      _ -> False

-- | Check if a row type represents an enum (all fields are unit-typed)
isEnumRowType :: [Core.FieldType] -> Bool
isEnumRowType rt =
    Lists.foldl Logic.and True (Lists.map (\f -> isUnitType (Strip.deannotateType (Core.fieldTypeType f))) rt)

-- | Check if a type is an enum type
isEnumType :: Core.Type -> Bool
isEnumType typ =
    case (Strip.deannotateType typ) of
      Core.TypeUnion v0 -> isEnumRowType v0
      _ -> False

isNominalType :: Core.Type -> Bool
isNominalType typ =
    case (Strip.deannotateType typ) of
      Core.TypeRecord _ -> True
      Core.TypeUnion _ -> True
      Core.TypeWrap _ -> True
      Core.TypeForall v0 -> isNominalType (Core.forallTypeBody v0)
      _ -> False

-- | Check if an element is serializable (no function types in dependencies) (Either version)
isSerializable :: Context.Context -> Graph.Graph -> Core.Binding -> Either Errors.Error Bool
isSerializable cx graph el =

      let variants =
              \typ -> Lists.map Reflect.typeVariant (Rewriting.foldOverType Coders.TraversalOrderPre (\m -> \t -> Lists.cons t m) [] typ)
      in (Eithers.map (\deps ->
        let allVariants = Sets.fromList (Lists.concat (Lists.map variants (Maps.elems deps)))
        in (Logic.not (Sets.member Variants.TypeVariantFunction allVariants))) (typeDependencies cx graph False Equality.identity (Core.bindingName el)))

-- | Check if a type (by name) is serializable, resolving all type dependencies (Either version)
isSerializableByName :: Context.Context -> Graph.Graph -> Core.Name -> Either Errors.Error Bool
isSerializableByName cx graph name =

      let variants =
              \typ -> Lists.map Reflect.typeVariant (Rewriting.foldOverType Coders.TraversalOrderPre (\m -> \t -> Lists.cons t m) [] typ)
      in (Eithers.map (\deps ->
        let allVariants = Sets.fromList (Lists.concat (Lists.map variants (Maps.elems deps)))
        in (Logic.not (Sets.member Variants.TypeVariantFunction allVariants))) (typeDependencies cx graph False Equality.identity name))

-- | Check if a type is serializable (no function types in the type itself)
isSerializableType :: Core.Type -> Bool
isSerializableType typ =

      let allVariants =
              Sets.fromList (Lists.map Reflect.typeVariant (Rewriting.foldOverType Coders.TraversalOrderPre (\m -> \t -> Lists.cons t m) [] typ))
      in (Logic.not (Sets.member Variants.TypeVariantFunction allVariants))

-- | Check if a term is trivially cheap (no thunking needed)
isTrivialTerm :: Core.Term -> Bool
isTrivialTerm t =
    case (Strip.deannotateTerm t) of
      Core.TermLiteral _ -> True
      Core.TermVariable v0 -> Equality.equal (Lists.length (Strings.splitOn "." (Core.unName v0))) 1
      Core.TermUnit -> True
      Core.TermApplication v0 ->
        let fun = Core.applicationFunction v0
            arg = Core.applicationArgument v0
        in case fun of
          Core.TermProject _ -> isTrivialTerm arg
          Core.TermUnwrap _ -> isTrivialTerm arg
          _ -> False
      Core.TermMaybe v0 -> Maybes.maybe True (\inner -> isTrivialTerm inner) v0
      Core.TermRecord v0 -> Lists.foldl (\acc -> \fld -> Logic.and acc (isTrivialTerm (Core.fieldTerm fld))) True (Core.recordFields v0)
      Core.TermWrap v0 -> isTrivialTerm (Core.wrappedTermBody v0)
      Core.TermTypeApplication v0 -> isTrivialTerm (Core.typeApplicationTermBody v0)
      Core.TermTypeLambda v0 -> isTrivialTerm (Core.typeLambdaBody v0)
      _ -> False

-- | Check whether a type is a type (always true for non-encoded types)
isType :: Core.Type -> Bool
isType t =
    case (Strip.deannotateType t) of
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

-- | Get all type dependencies for a given type name (Either version)
typeDependencies :: Context.Context -> Graph.Graph -> Bool -> (Core.Type -> Core.Type) -> Core.Name -> Either Errors.Error (M.Map Core.Name Core.Type)
typeDependencies cx graph withSchema transform name =

      let requireType =
              \name2 ->
                let cx1 =
                        Context.Context {
                          Context.contextTrace = (Lists.cons (Strings.cat2 "type dependencies of " (Core.unName name2)) (Context.contextTrace cx)),
                          Context.contextMessages = (Context.contextMessages cx),
                          Context.contextOther = (Context.contextOther cx)}
                in (Eithers.bind (Lexical.requireBinding graph name2) (\el -> Eithers.bimap (\_e -> Errors.ErrorDecoding _e) (\_a -> _a) (DecodeCore.type_ graph (Core.bindingTerm el))))
          toPair = \name2 -> Eithers.map (\typ -> (name2, (transform typ))) (requireType name2)
          deps =
                  \seeds -> \names -> Logic.ifElse (Sets.null seeds) (Right names) (Eithers.bind (Eithers.mapList toPair (Sets.toList seeds)) (\pairs ->
                    let newNames = Maps.union names (Maps.fromList pairs)
                        refs =
                                Lists.foldl Sets.union Sets.empty (Lists.map (\pair -> Dependencies.typeDependencyNames withSchema (Pairs.second pair)) pairs)
                        visited = Sets.fromList (Maps.keys names)
                        newSeeds = Sets.difference refs visited
                    in (deps newSeeds newNames)))
      in (deps (Sets.singleton name) Maps.empty)
