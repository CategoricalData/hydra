-- Note: this is an automatically generated file. Do not edit.

-- | Type dereference, lookup, requirements, and instantiation

module Hydra.Resolution where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
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
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Names as Names
import qualified Hydra.Scoping as Scoping
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Strip as Strip
import qualified Hydra.Substitution as Substitution
import qualified Hydra.Typing as Typing
import qualified Hydra.Variables as Variables
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M

-- | Dereference a type name to get the actual type (Either version)
dereferenceType :: t0 -> Graph.Graph -> Core.Name -> Either Errors.Error (Maybe Core.Type)
dereferenceType cx graph name =

      let mel = Lexical.lookupBinding graph name
      in (Maybes.maybe (Right Nothing) (\el -> Eithers.map Maybes.pure (Eithers.bimap (\_e -> Errors.ErrorResolution (Errors.ResolutionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "type",
        Errors.unexpectedShapeErrorActual = (Errors.unDecodingError _e)}))) (\_a -> _a) (DecodeCore.type_ graph (Core.bindingTerm el)))) mel)

-- | Test whether a given System F type is polymorphic (i.e., a forall type)
fTypeIsPolymorphic :: Core.Type -> Bool
fTypeIsPolymorphic typ =
    case typ of
      Core.TypeAnnotated v0 -> fTypeIsPolymorphic (Core.annotatedTypeBody v0)
      Core.TypeForall _ -> True
      _ -> False

fieldMap :: [Core.Field] -> M.Map Core.Name Core.Term
fieldMap fields =

      let toPair = \f -> (Core.fieldName f, (Core.fieldTerm f))
      in (Maps.fromList (Lists.map toPair fields))

fieldTypeMap :: [Core.FieldType] -> M.Map Core.Name Core.Type
fieldTypeMap fields =

      let toPair = \f -> (Core.fieldTypeName f, (Core.fieldTypeType f))
      in (Maps.fromList (Lists.map toPair fields))

-- | Get field types from a record or union type (Either version)
fieldTypes :: t0 -> Graph.Graph -> Core.Type -> Either Errors.Error (M.Map Core.Name Core.Type)
fieldTypes cx graph t =

      let toMap = \fields -> Maps.fromList (Lists.map (\ft -> (Core.fieldTypeName ft, (Core.fieldTypeType ft))) fields)
      in case (Strip.deannotateType t) of
        Core.TypeForall v0 -> fieldTypes cx graph (Core.forallTypeBody v0)
        Core.TypeRecord v0 -> Right (toMap v0)
        Core.TypeUnion v0 -> Right (toMap v0)
        Core.TypeVariable v0 -> Maybes.maybe (Eithers.bind (Lexical.requireBinding graph v0) (\el -> Eithers.bind (Eithers.bimap (\_e -> Errors.ErrorResolution (Errors.ResolutionErrorUnexpectedShape (Errors.UnexpectedShapeError {
          Errors.unexpectedShapeErrorExpected = "type",
          Errors.unexpectedShapeErrorActual = (Errors.unDecodingError _e)}))) (\_a -> _a) (DecodeCore.type_ graph (Core.bindingTerm el))) (\decodedType -> fieldTypes cx graph decodedType))) (\ts -> fieldTypes cx graph (Core.typeSchemeType ts)) (Maps.lookup v0 (Graph.graphSchemaTypes graph))
        _ -> Left (Errors.ErrorResolution (Errors.ResolutionErrorUnexpectedShape (Errors.UnexpectedShapeError {
          Errors.unexpectedShapeErrorExpected = "record or union type",
          Errors.unexpectedShapeErrorActual = (ShowCore.type_ t)})))

-- | Find a field type by name in a list of field types
findFieldType :: t0 -> Core.Name -> [Core.FieldType] -> Either Errors.Error Core.Type
findFieldType cx fname fields =

      let matchingFields = Lists.filter (\ft -> Equality.equal (Core.unName (Core.fieldTypeName ft)) (Core.unName fname)) fields
      in (Logic.ifElse (Lists.null matchingFields) (Left (Errors.ErrorResolution (Errors.ResolutionErrorNoMatchingField (Errors.NoMatchingFieldError {
        Errors.noMatchingFieldErrorFieldName = fname})))) (Logic.ifElse (Equality.equal (Lists.length matchingFields) 1) (Right (Core.fieldTypeType (Lists.head matchingFields))) (Left (Errors.ErrorExtraction (Errors.ExtractionErrorMultipleFields (Errors.MultipleFieldsError {
        Errors.multipleFieldsErrorFieldName = fname}))))))

-- | Fully strip a type of forall quantifiers, normalizing bound variable names for alpha-equivalence comparison
fullyStripAndNormalizeType :: Core.Type -> Core.Type
fullyStripAndNormalizeType typ =

      let go =
              \depth -> \subst -> \t -> case (Strip.deannotateType t) of
                Core.TypeForall v0 ->
                  let oldVar = Core.forallTypeParameter v0
                      newVar = Core.Name (Strings.cat2 "_" (Literals.showInt32 depth))
                  in (go (Math.add depth 1) (Maps.insert oldVar newVar subst) (Core.forallTypeBody v0))
                _ -> (subst, t)
          result = go 0 Maps.empty typ
          subst = Pairs.first result
          body = Pairs.second result
      in (Variables.substituteTypeVariables subst body)

-- | Fully strip a type of forall quantifiers
fullyStripType :: Core.Type -> Core.Type
fullyStripType typ =
    case (Strip.deannotateType typ) of
      Core.TypeForall v0 -> fullyStripType (Core.forallTypeBody v0)
      _ -> typ

-- | Instantiate a type by replacing all forall-bound type variables with fresh variables, threading Context
instantiateType :: Context.Context -> Core.Type -> (Core.Type, Context.Context)
instantiateType cx typ =

      let result = instantiateTypeScheme cx (typeToTypeScheme typ)
      in (Scoping.typeSchemeToFType (Pairs.first result), (Pairs.second result))

-- | Instantiate a type scheme with fresh variables, threading Context
instantiateTypeScheme :: Context.Context -> Core.TypeScheme -> (Core.TypeScheme, Context.Context)
instantiateTypeScheme cx scheme =

      let oldVars = Core.typeSchemeVariables scheme
          result = Names.freshNames (Lists.length oldVars) cx
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

-- | Apply type arguments to a nominal type
nominalApplication :: Core.Name -> [Core.Type] -> Core.Type
nominalApplication tname args =
    Lists.foldl (\t -> \a -> Core.TypeApplication (Core.ApplicationType {
      Core.applicationTypeFunction = t,
      Core.applicationTypeArgument = a})) (Core.TypeVariable tname) args

-- | Require a name to resolve to a record type
requireRecordType :: t0 -> Graph.Graph -> Core.Name -> Either Errors.Error [Core.FieldType]
requireRecordType cx graph name =

      let toRecord =
              \t -> case t of
                Core.TypeRecord v0 -> Just v0
                _ -> Nothing
      in (requireRowType cx "record type" toRecord graph name)

-- | Require a name to resolve to a row type
requireRowType :: t0 -> String -> (Core.Type -> Maybe t1) -> Graph.Graph -> Core.Name -> Either Errors.Error t1
requireRowType cx label getter graph name =

      let rawType =
              \t -> case t of
                Core.TypeAnnotated v0 -> rawType (Core.annotatedTypeBody v0)
                Core.TypeForall v0 -> rawType (Core.forallTypeBody v0)
                _ -> t
      in (Eithers.bind (requireType cx graph name) (\t -> Maybes.maybe (Left (Errors.ErrorResolution (Errors.ResolutionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = (Strings.cat2 label " type"),
        Errors.unexpectedShapeErrorActual = (Strings.cat2 (Core.unName name) (Strings.cat2 ": " (ShowCore.type_ t)))})))) (\x -> Right x) (getter (rawType t))))

-- | Look up a schema type and instantiate it, threading Context
requireSchemaType :: Context.Context -> M.Map Core.Name Core.TypeScheme -> Core.Name -> Either Errors.Error (Core.TypeScheme, Context.Context)
requireSchemaType cx types tname =
    Maybes.maybe (Left (Errors.ErrorResolution (Errors.ResolutionErrorNoSuchBinding (Errors.NoSuchBindingError {
      Errors.noSuchBindingErrorName = tname})))) (\ts -> Right (instantiateTypeScheme cx (Strip.deannotateTypeSchemeRecursive ts))) (Maps.lookup tname types)

-- | Require a type by name
requireType :: t0 -> Graph.Graph -> Core.Name -> Either Errors.Error Core.Type
requireType cx graph name =
    Maybes.maybe (Maybes.maybe (Left (Errors.ErrorResolution (Errors.ResolutionErrorNoSuchBinding (Errors.NoSuchBindingError {
      Errors.noSuchBindingErrorName = name})))) (\ts -> Right (Scoping.typeSchemeToFType ts)) (Maps.lookup name (Graph.graphBoundTypes graph))) (\ts -> Right (Scoping.typeSchemeToFType ts)) (Maps.lookup name (Graph.graphSchemaTypes graph))

-- | Require a field type from a union type
requireUnionField :: t0 -> Graph.Graph -> Core.Name -> Core.Name -> Either Errors.Error Core.Type
requireUnionField cx graph tname fname =

      let withRowType =
              \rt ->
                let matches = Lists.filter (\ft -> Equality.equal (Core.fieldTypeName ft) fname) rt
                in (Logic.ifElse (Lists.null matches) (Left (Errors.ErrorResolution (Errors.ResolutionErrorNoMatchingField (Errors.NoMatchingFieldError {
                  Errors.noMatchingFieldErrorFieldName = fname})))) (Right (Core.fieldTypeType (Lists.head matches))))
      in (Eithers.bind (requireUnionType cx graph tname) withRowType)

-- | Require a name to resolve to a union type
requireUnionType :: t0 -> Graph.Graph -> Core.Name -> Either Errors.Error [Core.FieldType]
requireUnionType cx graph name =

      let toUnion =
              \t -> case t of
                Core.TypeUnion v0 -> Just v0
                _ -> Nothing
      in (requireRowType cx "union" toUnion graph name)

-- | Resolve a type, dereferencing type variables
resolveType :: Graph.Graph -> Core.Type -> Maybe Core.Type
resolveType graph typ =
    case (Strip.deannotateType typ) of
      Core.TypeVariable v0 -> Maybes.maybe (Maybes.map (\ts -> Scoping.typeSchemeToFType ts) (Maps.lookup v0 (Graph.graphBoundTypes graph))) (\ts -> Just (Scoping.typeSchemeToFType ts)) (Maps.lookup v0 (Graph.graphSchemaTypes graph))
      _ -> Just typ

-- | Convert a (System F -style) type to a type scheme
typeToTypeScheme :: Core.Type -> Core.TypeScheme
typeToTypeScheme t0 =

      let helper =
              \vars -> \t -> case (Strip.deannotateType t) of
                Core.TypeForall v0 -> helper (Lists.cons (Core.forallTypeParameter v0) vars) (Core.forallTypeBody v0)
                _ -> Core.TypeScheme {
                  Core.typeSchemeVariables = (Lists.reverse vars),
                  Core.typeSchemeType = t,
                  Core.typeSchemeConstraints = Nothing}
      in (helper [] t0)
