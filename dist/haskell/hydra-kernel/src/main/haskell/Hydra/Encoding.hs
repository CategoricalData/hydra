-- Note: this is an automatically generated file. Do not edit.

-- | Functions for generating term encoders from type modules

module Hydra.Encoding where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Constants as Constants
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Errors as Errors
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Predicates as Predicates
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | Transform a type binding into an encoder binding
encodeBinding :: t0 -> Graph.Graph -> Core.Binding -> Either Errors.DecodingError Core.Binding
encodeBinding cx graph b =
    Eithers.bind (DecodeCore.type_ graph (Core.bindingTerm b)) (\typ -> Right (Core.Binding {
      Core.bindingName = (encodeBindingName (Core.bindingName b)),
      Core.bindingTerm = (encodeTypeNamed (Core.bindingName b) typ),
      Core.bindingType = (Just (encoderTypeSchemeNamed (Core.bindingName b) typ))}))

-- | Generate a binding name for an encoder function from a type name
encodeBindingName :: Core.Name -> Core.Name
encodeBindingName n =
    Logic.ifElse (Logic.not (Lists.null (Lists.tail (Strings.splitOn "." (Core.unName n))))) (Core.Name (Strings.intercalate "." (Lists.concat2 [
      "hydra",
      "encode"] (Lists.concat2 (Lists.tail (Lists.init (Strings.splitOn "." (Core.unName n)))) [
      Formatting.decapitalize (Names.localNameOf n)])))) (Core.Name (Formatting.decapitalize (Names.localNameOf n)))

-- | Generate an encoder for an Either type
encodeEitherType :: Core.EitherType -> Core.Term
encodeEitherType et =
    Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "e"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "either"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bimap")),
                Core.applicationArgument = (encodeType (Core.eitherTypeLeft et))})),
              Core.applicationArgument = (encodeType (Core.eitherTypeRight et))})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "e"))}))}}))})

-- | Generate the encoder for a field's value
encodeFieldValue :: Core.Name -> Core.Name -> Core.Type -> Core.Term
encodeFieldValue typeName fieldName fieldType =
    Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "y"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "inject"),
          Core.fieldTerm = (encodeInjection typeName fieldName (Core.TermApplication (Core.Application {
            Core.applicationFunction = (encodeType fieldType),
            Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})))}}))})

-- | Encode a float value based on its float type
encodeFloatValue :: Core.FloatType -> Core.Term -> Core.Term
encodeFloatValue floatType valTerm =
    Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.FloatValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = case floatType of
          Core.FloatTypeBigfloat -> Core.Name "bigfloat"
          Core.FloatTypeFloat32 -> Core.Name "float32"
          Core.FloatTypeFloat64 -> Core.Name "float64",
        Core.fieldTerm = valTerm}})

-- | Generate an encoder for a polymorphic (forall) type
encodeForallType :: Core.ForallType -> Core.Term
encodeForallType ft =
    Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (encodeBindingName (Core.forallTypeParameter ft)),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (encodeType (Core.forallTypeBody ft))})

-- | Encode an Injection as a term
encodeInjection :: Core.Name -> Core.Name -> Core.Term -> Core.Term
encodeInjection typeName fieldName fieldTerm =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.core.Injection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeName"),
          Core.fieldTerm = (encodeName typeName)},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = ((\fname -> \fterm -> Core.TermRecord (Core.Record {
            Core.recordTypeName = (Core.Name "hydra.core.Field"),
            Core.recordFields = [
              Core.Field {
                Core.fieldName = (Core.Name "name"),
                Core.fieldTerm = (encodeName fname)},
              Core.Field {
                Core.fieldName = (Core.Name "term"),
                Core.fieldTerm = fterm}]})) fieldName fieldTerm)}]})

-- | Encode an integer value based on its integer type
encodeIntegerValue :: Core.IntegerType -> Core.Term -> Core.Term
encodeIntegerValue intType valTerm =
    Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = case intType of
          Core.IntegerTypeBigint -> Core.Name "bigint"
          Core.IntegerTypeInt8 -> Core.Name "int8"
          Core.IntegerTypeInt16 -> Core.Name "int16"
          Core.IntegerTypeInt32 -> Core.Name "int32"
          Core.IntegerTypeInt64 -> Core.Name "int64"
          Core.IntegerTypeUint8 -> Core.Name "uint8"
          Core.IntegerTypeUint16 -> Core.Name "uint16"
          Core.IntegerTypeUint32 -> Core.Name "uint32"
          Core.IntegerTypeUint64 -> Core.Name "uint64",
        Core.fieldTerm = valTerm}})

-- | Generate an encoder for a list type
encodeListType :: Core.Type -> Core.Term
encodeListType elemType =
    Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "xs"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "list"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
              Core.applicationArgument = (encodeType elemType)})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})

-- | Generate an encoder for a literal type
encodeLiteralType :: Core.LiteralType -> Core.Term
encodeLiteralType x =
    case x of
      Core.LiteralTypeBinary -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "x"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermInject (Core.Injection {
          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
          Core.injectionField = Core.Field {
            Core.fieldName = (Core.Name "literal"),
            Core.fieldTerm = (Core.TermInject (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "binary"),
                Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))})
      Core.LiteralTypeBoolean -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "x"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermInject (Core.Injection {
          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
          Core.injectionField = Core.Field {
            Core.fieldName = (Core.Name "literal"),
            Core.fieldTerm = (Core.TermInject (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "boolean"),
                Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))})
      Core.LiteralTypeString -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "x"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermInject (Core.Injection {
          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
          Core.injectionField = Core.Field {
            Core.fieldName = (Core.Name "literal"),
            Core.fieldTerm = (Core.TermInject (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "string"),
                Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))})
      Core.LiteralTypeInteger v0 -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "x"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermInject (Core.Injection {
          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
          Core.injectionField = Core.Field {
            Core.fieldName = (Core.Name "literal"),
            Core.fieldTerm = (Core.TermInject (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "integer"),
                Core.fieldTerm = (encodeIntegerValue v0 (Core.TermVariable (Core.Name "x")))}}))}}))})
      Core.LiteralTypeFloat v0 -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "x"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermInject (Core.Injection {
          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
          Core.injectionField = Core.Field {
            Core.fieldName = (Core.Name "literal"),
            Core.fieldTerm = (Core.TermInject (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "float"),
                Core.fieldTerm = (encodeFloatValue v0 (Core.TermVariable (Core.Name "x")))}}))}}))})
      _ -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "x"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})

-- | Generate an encoder for a map type
encodeMapType :: Core.MapType -> Core.Term
encodeMapType mt =
    Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "m"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "map"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.bimap")),
                Core.applicationArgument = (encodeType (Core.mapTypeKeys mt))})),
              Core.applicationArgument = (encodeType (Core.mapTypeValues mt))})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))})

-- | Transform a type module into an encoder module
encodeModule :: Context.Context -> Graph.Graph -> Packaging.Module -> Either Errors.Error (Maybe Packaging.Module)
encodeModule cx graph mod =
    Eithers.bind (filterTypeBindings cx graph (Maybes.cat (Lists.map (\d -> case d of
      Packaging.DefinitionType v0 -> Just ((\name -> \typ ->
        let schemaTerm = Core.TermVariable (Core.Name "hydra.core.Type")
            dataTerm =
                    Annotations.normalizeTermAnnotations (Core.TermAnnotated (Core.AnnotatedTerm {
                      Core.annotatedTermBody = (EncodeCore.type_ typ),
                      Core.annotatedTermAnnotation = (Maps.fromList [
                        (Constants.key_type, schemaTerm)])}))
        in Core.Binding {
          Core.bindingName = name,
          Core.bindingTerm = dataTerm,
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.Type")),
            Core.typeSchemeConstraints = Nothing}))}) (Packaging.typeDefinitionName v0) (Core.typeSchemeType (Packaging.typeDefinitionType v0)))
      _ -> Nothing) (Packaging.moduleDefinitions mod)))) (\typeBindings -> Logic.ifElse (Lists.null typeBindings) (Right Nothing) (Eithers.bind (Eithers.mapList (\b -> Eithers.bimap (\_e -> Errors.ErrorDecoding _e) (\x -> x) (encodeBinding cx graph b)) typeBindings) (\encodedBindings -> Right (Just (Packaging.Module {
      Packaging.moduleNamespace = (encodeNamespace (Packaging.moduleNamespace mod)),
      Packaging.moduleDefinitions = (Lists.map (\b -> Packaging.DefinitionTerm (Packaging.TermDefinition {
        Packaging.termDefinitionName = (Core.bindingName b),
        Packaging.termDefinitionTerm = (Core.bindingTerm b),
        Packaging.termDefinitionType = (Core.bindingType b)})) encodedBindings),
      Packaging.moduleTermDependencies = (Lists.nub (Lists.concat2 (Lists.map encodeNamespace (Packaging.moduleTypeDependencies mod)) (Lists.map encodeNamespace (Packaging.moduleTermDependencies mod)))),
      Packaging.moduleTypeDependencies = [
        Packaging.moduleNamespace mod],
      Packaging.moduleDescription = (Just (Strings.cat [
        "Term encoders for ",
        (Packaging.unNamespace (Packaging.moduleNamespace mod))]))})))))

-- | Encode a Name as a term
encodeName :: Core.Name -> Core.Term
encodeName n =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName n)))})

-- | Generate an encoder module namespace from a source module namespace
encodeNamespace :: Packaging.Namespace -> Packaging.Namespace
encodeNamespace ns =
    Packaging.Namespace (Strings.cat [
      "hydra.encode.",
      (Strings.intercalate "." (Lists.tail (Strings.splitOn "." (Packaging.unNamespace ns))))])

-- | Generate an encoder for a Maybe type
encodeOptionalType :: Core.Type -> Core.Term
encodeOptionalType elemType =
    Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "opt"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "maybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.map")),
              Core.applicationArgument = (encodeType elemType)})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "opt"))}))}}))})

-- | Generate an encoder for a pair type
encodePairType :: Core.PairType -> Core.Term
encodePairType pt =
    Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "p"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "pair"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.bimap")),
                Core.applicationArgument = (encodeType (Core.pairTypeFirst pt))})),
              Core.applicationArgument = (encodeType (Core.pairTypeSecond pt))})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "p"))}))}}))})

-- | Generate an encoder for a record type (unnamed — should not be called directly)
encodeRecordType :: [Core.FieldType] -> Core.Term
encodeRecordType rt = encodeRecordTypeNamed (Core.Name "unknown") rt

-- | Generate an encoder for a record type with the given element name
encodeRecordTypeNamed :: Core.Name -> [Core.FieldType] -> Core.Term
encodeRecordTypeNamed ename rt =
    Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "x"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "record"),
          Core.fieldTerm = (Core.TermRecord (Core.Record {
            Core.recordTypeName = (Core.Name "hydra.core.Record"),
            Core.recordFields = [
              Core.Field {
                Core.fieldName = (Core.Name "typeName"),
                Core.fieldTerm = (encodeName ename)},
              Core.Field {
                Core.fieldName = (Core.Name "fields"),
                Core.fieldTerm = (Core.TermList (Lists.map ((\tname -> \recType -> \ft -> Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "hydra.core.Field"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "name"),
                      Core.fieldTerm = (encodeName (Core.fieldTypeName ft))},
                    Core.Field {
                      Core.fieldName = (Core.Name "term"),
                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (encodeType (Core.fieldTypeType ft)),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermProject (Core.Projection {
                            Core.projectionTypeName = tname,
                            Core.projectionField = (Core.fieldTypeName ft)})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})) ename rt) rt))}]}))}}))})

-- | Generate an encoder for a set type
encodeSetType :: Core.Type -> Core.Term
encodeSetType elemType =
    Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "s"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "set"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.map")),
              Core.applicationArgument = (encodeType elemType)})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))}}))})

-- | Generate an encoder term for a Type
encodeType :: Core.Type -> Core.Term
encodeType x =
    case x of
      Core.TypeAnnotated v0 -> encodeType (Core.annotatedTypeBody v0)
      Core.TypeApplication v0 -> Core.TermApplication (Core.Application {
        Core.applicationFunction = (encodeType (Core.applicationTypeFunction v0)),
        Core.applicationArgument = (encodeType (Core.applicationTypeArgument v0))})
      Core.TypeEither v0 -> encodeEitherType v0
      Core.TypeForall v0 -> encodeForallType v0
      Core.TypeFunction _ -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "x"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})
      Core.TypeList v0 -> encodeListType v0
      Core.TypeLiteral v0 -> encodeLiteralType v0
      Core.TypeMap v0 -> encodeMapType v0
      Core.TypeMaybe v0 -> encodeOptionalType v0
      Core.TypePair v0 -> encodePairType v0
      Core.TypeRecord v0 -> encodeRecordType v0
      Core.TypeSet v0 -> encodeSetType v0
      Core.TypeUnion v0 -> encodeUnionType v0
      Core.TypeWrap v0 -> encodeWrappedType v0
      Core.TypeUnit -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "_"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermInject (Core.Injection {
          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
          Core.injectionField = Core.Field {
            Core.fieldName = (Core.Name "unit"),
            Core.fieldTerm = Core.TermUnit}}))})
      Core.TypeVoid -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "_"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermInject (Core.Injection {
          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
          Core.injectionField = Core.Field {
            Core.fieldName = (Core.Name "unit"),
            Core.fieldTerm = Core.TermUnit}}))})
      Core.TypeVariable v0 -> Core.TermVariable (encodeBindingName v0)
      _ -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "x"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})

-- | Generate an encoder term for a Type, with the element name for nominal types
encodeTypeNamed :: Core.Name -> Core.Type -> Core.Term
encodeTypeNamed ename typ =
    case typ of
      Core.TypeAnnotated v0 -> encodeTypeNamed ename (Core.annotatedTypeBody v0)
      Core.TypeApplication v0 -> Core.TermApplication (Core.Application {
        Core.applicationFunction = (encodeType (Core.applicationTypeFunction v0)),
        Core.applicationArgument = (encodeType (Core.applicationTypeArgument v0))})
      Core.TypeEither v0 -> encodeEitherType v0
      Core.TypeForall v0 -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (encodeBindingName (Core.forallTypeParameter v0)),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (encodeTypeNamed ename (Core.forallTypeBody v0))})
      Core.TypeFunction _ -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "x"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})
      Core.TypeList v0 -> encodeListType v0
      Core.TypeLiteral v0 -> encodeLiteralType v0
      Core.TypeMap v0 -> encodeMapType v0
      Core.TypeMaybe v0 -> encodeOptionalType v0
      Core.TypePair v0 -> encodePairType v0
      Core.TypeRecord v0 -> encodeRecordTypeNamed ename v0
      Core.TypeSet v0 -> encodeSetType v0
      Core.TypeUnion v0 -> encodeUnionTypeNamed ename v0
      Core.TypeWrap v0 -> encodeWrappedTypeNamed ename v0
      Core.TypeUnit -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "_"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermInject (Core.Injection {
          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
          Core.injectionField = Core.Field {
            Core.fieldName = (Core.Name "unit"),
            Core.fieldTerm = Core.TermUnit}}))})
      Core.TypeVoid -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "_"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermInject (Core.Injection {
          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
          Core.injectionField = Core.Field {
            Core.fieldName = (Core.Name "unit"),
            Core.fieldTerm = Core.TermUnit}}))})
      Core.TypeVariable v0 -> Core.TermVariable (encodeBindingName v0)
      _ -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "x"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})

-- | Generate an encoder for a union type (placeholder name)
encodeUnionType :: [Core.FieldType] -> Core.Term
encodeUnionType rt = encodeUnionTypeNamed (Core.Name "unknown") rt

-- | Generate an encoder for a union type with the given element name
encodeUnionTypeNamed :: Core.Name -> [Core.FieldType] -> Core.Term
encodeUnionTypeNamed ename rt =
    Core.TermCases (Core.CaseStatement {
      Core.caseStatementTypeName = ename,
      Core.caseStatementDefault = Nothing,
      Core.caseStatementCases = (Lists.map (\ft -> Core.Field {
        Core.fieldName = (Core.fieldTypeName ft),
        Core.fieldTerm = (encodeFieldValue ename (Core.fieldTypeName ft) (Core.fieldTypeType ft))}) rt)})

-- | Generate an encoder for a wrapped type (placeholder name)
encodeWrappedType :: Core.Type -> Core.Term
encodeWrappedType wt = encodeWrappedTypeNamed (Core.Name "unknown") wt

-- | Generate an encoder for a wrapped type with the given element name
encodeWrappedTypeNamed :: Core.Name -> Core.Type -> Core.Term
encodeWrappedTypeNamed ename wt =
    Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "x"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "wrap"),
          Core.fieldTerm = (Core.TermRecord (Core.Record {
            Core.recordTypeName = (Core.Name "hydra.core.WrappedTerm"),
            Core.recordFields = [
              Core.Field {
                Core.fieldName = (Core.Name "typeName"),
                Core.fieldTerm = (encodeName ename)},
              Core.Field {
                Core.fieldName = (Core.Name "body"),
                Core.fieldTerm = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (encodeType wt),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermUnwrap ename),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))})

-- | Collect forall type variable names from a type
encoderCollectForallVariables :: Core.Type -> [Core.Name]
encoderCollectForallVariables typ =
    case typ of
      Core.TypeAnnotated v0 -> encoderCollectForallVariables (Core.annotatedTypeBody v0)
      Core.TypeForall v0 -> Lists.cons (Core.forallTypeParameter v0) (encoderCollectForallVariables (Core.forallTypeBody v0))
      _ -> []

-- | Collect type variables needing Ord constraints
encoderCollectOrdVars :: Core.Type -> [Core.Name]
encoderCollectOrdVars typ =
    case typ of
      Core.TypeAnnotated v0 -> encoderCollectOrdVars (Core.annotatedTypeBody v0)
      Core.TypeApplication v0 -> Lists.concat2 (encoderCollectOrdVars (Core.applicationTypeFunction v0)) (encoderCollectOrdVars (Core.applicationTypeArgument v0))
      Core.TypeEither v0 -> Lists.concat2 (encoderCollectOrdVars (Core.eitherTypeLeft v0)) (encoderCollectOrdVars (Core.eitherTypeRight v0))
      Core.TypeForall v0 -> encoderCollectOrdVars (Core.forallTypeBody v0)
      Core.TypeList v0 -> encoderCollectOrdVars v0
      Core.TypeMap v0 -> Lists.concat [
        encoderCollectTypeVarsFromType (Core.mapTypeKeys v0),
        (encoderCollectOrdVars (Core.mapTypeKeys v0)),
        (encoderCollectOrdVars (Core.mapTypeValues v0))]
      Core.TypeMaybe v0 -> encoderCollectOrdVars v0
      Core.TypePair v0 -> Lists.concat2 (encoderCollectOrdVars (Core.pairTypeFirst v0)) (encoderCollectOrdVars (Core.pairTypeSecond v0))
      Core.TypeRecord v0 -> Lists.concat (Lists.map (\ft -> encoderCollectOrdVars (Core.fieldTypeType ft)) v0)
      Core.TypeSet v0 -> Lists.concat2 (encoderCollectTypeVarsFromType v0) (encoderCollectOrdVars v0)
      Core.TypeUnion v0 -> Lists.concat (Lists.map (\ft -> encoderCollectOrdVars (Core.fieldTypeType ft)) v0)
      Core.TypeWrap v0 -> encoderCollectOrdVars v0
      _ -> []

-- | Collect all type variable names from a type expression
encoderCollectTypeVarsFromType :: Core.Type -> [Core.Name]
encoderCollectTypeVarsFromType typ =
    case typ of
      Core.TypeAnnotated v0 -> encoderCollectTypeVarsFromType (Core.annotatedTypeBody v0)
      Core.TypeApplication v0 -> Lists.concat2 (encoderCollectTypeVarsFromType (Core.applicationTypeFunction v0)) (encoderCollectTypeVarsFromType (Core.applicationTypeArgument v0))
      Core.TypeForall v0 -> encoderCollectTypeVarsFromType (Core.forallTypeBody v0)
      Core.TypeList v0 -> encoderCollectTypeVarsFromType v0
      Core.TypeMap v0 -> Lists.concat2 (encoderCollectTypeVarsFromType (Core.mapTypeKeys v0)) (encoderCollectTypeVarsFromType (Core.mapTypeValues v0))
      Core.TypeMaybe v0 -> encoderCollectTypeVarsFromType v0
      Core.TypePair v0 -> Lists.concat2 (encoderCollectTypeVarsFromType (Core.pairTypeFirst v0)) (encoderCollectTypeVarsFromType (Core.pairTypeSecond v0))
      Core.TypeRecord v0 -> Lists.concat (Lists.map (\ft -> encoderCollectTypeVarsFromType (Core.fieldTypeType ft)) v0)
      Core.TypeSet v0 -> encoderCollectTypeVarsFromType v0
      Core.TypeUnion v0 -> Lists.concat (Lists.map (\ft -> encoderCollectTypeVarsFromType (Core.fieldTypeType ft)) v0)
      Core.TypeVariable v0 -> [
        v0]
      Core.TypeWrap v0 -> encoderCollectTypeVarsFromType v0
      _ -> []

-- | Get full result type for encoder input
encoderFullResultType :: Core.Type -> Core.Type
encoderFullResultType typ =
    case typ of
      Core.TypeAnnotated v0 -> encoderFullResultType (Core.annotatedTypeBody v0)
      Core.TypeApplication v0 -> Core.TypeApplication (Core.ApplicationType {
        Core.applicationTypeFunction = (encoderFullResultType (Core.applicationTypeFunction v0)),
        Core.applicationTypeArgument = (Core.applicationTypeArgument v0)})
      Core.TypeEither v0 -> Core.TypeEither (Core.EitherType {
        Core.eitherTypeLeft = (encoderFullResultType (Core.eitherTypeLeft v0)),
        Core.eitherTypeRight = (encoderFullResultType (Core.eitherTypeRight v0))})
      Core.TypeForall v0 -> Core.TypeApplication (Core.ApplicationType {
        Core.applicationTypeFunction = (encoderFullResultType (Core.forallTypeBody v0)),
        Core.applicationTypeArgument = (Core.TypeVariable (Core.forallTypeParameter v0))})
      Core.TypeList v0 -> Core.TypeList (encoderFullResultType v0)
      Core.TypeLiteral _ -> Core.TypeVariable (Core.Name "hydra.core.Literal")
      Core.TypeMap v0 -> Core.TypeMap (Core.MapType {
        Core.mapTypeKeys = (encoderFullResultType (Core.mapTypeKeys v0)),
        Core.mapTypeValues = (encoderFullResultType (Core.mapTypeValues v0))})
      Core.TypeMaybe v0 -> Core.TypeMaybe (encoderFullResultType v0)
      Core.TypePair v0 -> Core.TypePair (Core.PairType {
        Core.pairTypeFirst = (encoderFullResultType (Core.pairTypeFirst v0)),
        Core.pairTypeSecond = (encoderFullResultType (Core.pairTypeSecond v0))})
      Core.TypeRecord _ -> Core.TypeVariable (Core.Name "hydra.core.Term")
      Core.TypeSet v0 -> Core.TypeSet (encoderFullResultType v0)
      Core.TypeUnion _ -> Core.TypeVariable (Core.Name "hydra.core.Term")
      Core.TypeUnit -> Core.TypeUnit
      Core.TypeVariable v0 -> Core.TypeVariable v0
      Core.TypeVoid -> Core.TypeVoid
      Core.TypeWrap _ -> Core.TypeVariable (Core.Name "hydra.core.Term")
      _ -> Core.TypeVariable (Core.Name "hydra.core.Term")

-- | Get full result type for encoder input, using element name for nominal types
encoderFullResultTypeNamed :: Core.Name -> Core.Type -> Core.Type
encoderFullResultTypeNamed ename typ =
    case typ of
      Core.TypeAnnotated v0 -> encoderFullResultTypeNamed ename (Core.annotatedTypeBody v0)
      Core.TypeApplication v0 -> Core.TypeApplication (Core.ApplicationType {
        Core.applicationTypeFunction = (encoderFullResultType (Core.applicationTypeFunction v0)),
        Core.applicationTypeArgument = (Core.applicationTypeArgument v0)})
      Core.TypeEither v0 -> Core.TypeEither (Core.EitherType {
        Core.eitherTypeLeft = (encoderFullResultType (Core.eitherTypeLeft v0)),
        Core.eitherTypeRight = (encoderFullResultType (Core.eitherTypeRight v0))})
      Core.TypeForall v0 -> Core.TypeApplication (Core.ApplicationType {
        Core.applicationTypeFunction = (encoderFullResultTypeNamed ename (Core.forallTypeBody v0)),
        Core.applicationTypeArgument = (Core.TypeVariable (Core.forallTypeParameter v0))})
      Core.TypeList v0 -> Core.TypeList (encoderFullResultType v0)
      Core.TypeLiteral _ -> Core.TypeVariable (Core.Name "hydra.core.Literal")
      Core.TypeMap v0 -> Core.TypeMap (Core.MapType {
        Core.mapTypeKeys = (encoderFullResultType (Core.mapTypeKeys v0)),
        Core.mapTypeValues = (encoderFullResultType (Core.mapTypeValues v0))})
      Core.TypeMaybe v0 -> Core.TypeMaybe (encoderFullResultType v0)
      Core.TypePair v0 -> Core.TypePair (Core.PairType {
        Core.pairTypeFirst = (encoderFullResultType (Core.pairTypeFirst v0)),
        Core.pairTypeSecond = (encoderFullResultType (Core.pairTypeSecond v0))})
      Core.TypeRecord _ -> Core.TypeVariable ename
      Core.TypeSet v0 -> Core.TypeSet (encoderFullResultType v0)
      Core.TypeUnion _ -> Core.TypeVariable ename
      Core.TypeUnit -> Core.TypeUnit
      Core.TypeVariable v0 -> Core.TypeVariable v0
      Core.TypeVoid -> Core.TypeVoid
      Core.TypeWrap _ -> Core.TypeVariable ename
      _ -> Core.TypeVariable (Core.Name "hydra.core.Term")

-- | Build encoder function type
encoderType :: Core.Type -> Core.Type
encoderType typ =

      let resultType = encoderFullResultType typ
          baseType =
                  Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = resultType,
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})
      in (prependForallEncoders baseType typ)

-- | Build encoder function type with element name for nominal types
encoderTypeNamed :: Core.Name -> Core.Type -> Core.Type
encoderTypeNamed ename typ =

      let resultType = encoderFullResultTypeNamed ename typ
          baseType =
                  Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = resultType,
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})
      in (prependForallEncoders baseType typ)

-- | Construct a TypeScheme for an encoder function from a source type
encoderTypeScheme :: Core.Type -> Core.TypeScheme
encoderTypeScheme typ =

      let typeVars = encoderCollectForallVariables typ
          encoderFunType = encoderType typ
          allOrdVars = encoderCollectOrdVars typ
          ordVars = Lists.filter (\v -> Lists.elem v typeVars) allOrdVars
          constraints =
                  Logic.ifElse (Lists.null ordVars) Nothing (Just (Maps.fromList (Lists.map (\v -> (v, Core.TypeVariableMetadata {
                    Core.typeVariableMetadataClasses = (Sets.singleton (Core.Name "ordering"))})) ordVars)))
      in Core.TypeScheme {
        Core.typeSchemeVariables = typeVars,
        Core.typeSchemeType = encoderFunType,
        Core.typeSchemeConstraints = constraints}

-- | Construct a TypeScheme for an encoder function, with element name for nominal types
encoderTypeSchemeNamed :: Core.Name -> Core.Type -> Core.TypeScheme
encoderTypeSchemeNamed ename typ =

      let typeVars = encoderCollectForallVariables typ
          encoderFunType = encoderTypeNamed ename typ
          allOrdVars = encoderCollectOrdVars typ
          ordVars = Lists.filter (\v -> Lists.elem v typeVars) allOrdVars
          constraints =
                  Logic.ifElse (Lists.null ordVars) Nothing (Just (Maps.fromList (Lists.map (\v -> (v, Core.TypeVariableMetadata {
                    Core.typeVariableMetadataClasses = (Sets.singleton (Core.Name "ordering"))})) ordVars)))
      in Core.TypeScheme {
        Core.typeSchemeVariables = typeVars,
        Core.typeSchemeType = encoderFunType,
        Core.typeSchemeConstraints = constraints}

-- | Filter bindings to only encodable type definitions
filterTypeBindings :: Context.Context -> Graph.Graph -> [Core.Binding] -> Either Errors.Error [Core.Binding]
filterTypeBindings cx graph bindings =
    Eithers.map Maybes.cat (Eithers.mapList (isEncodableBinding cx graph) (Lists.filter Annotations.isNativeType bindings))

-- | Check if a binding is encodable (serializable type)
isEncodableBinding :: Context.Context -> Graph.Graph -> Core.Binding -> Either Errors.Error (Maybe Core.Binding)
isEncodableBinding cx graph b =
    Eithers.bind (Predicates.isSerializableByName cx graph (Core.bindingName b)) (\serializable -> Right (Logic.ifElse serializable (Just b) Nothing))

-- | Check whether a type is the unit type
isUnitType :: Core.Type -> Bool
isUnitType x =
    case x of
      Core.TypeUnit -> True
      _ -> False

-- | Prepend encoder types for forall parameters to base type
prependForallEncoders :: Core.Type -> Core.Type -> Core.Type
prependForallEncoders baseType typ =
    case typ of
      Core.TypeAnnotated v0 -> prependForallEncoders baseType (Core.annotatedTypeBody v0)
      Core.TypeForall v0 -> Core.TypeFunction (Core.FunctionType {
        Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
          Core.functionTypeDomain = (Core.TypeVariable (Core.forallTypeParameter v0)),
          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
        Core.functionTypeCodomain = (prependForallEncoders baseType (Core.forallTypeBody v0))})
      _ -> baseType
