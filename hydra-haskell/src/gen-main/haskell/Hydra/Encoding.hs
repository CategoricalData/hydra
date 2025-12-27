-- Note: this is an automatically generated file. Do not edit.

-- | Functions for generating term encoders from type modules

module Hydra.Encoding where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Monads as Monads
import qualified Hydra.Names as Names
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Transform a type binding into an encoder binding
encodeBinding :: (Core.Binding -> Compute.Flow Graph.Graph Core.Binding)
encodeBinding b = (Flows.bind Monads.getState (\cx -> Flows.bind (Monads.eitherToFlow Util.unDecodingError (Core_.type_ cx (Core.bindingTerm b))) (\typ -> Flows.pure (Core.Binding {
  Core.bindingName = (encodeBindingName (Core.bindingName b)),
  Core.bindingTerm = (encodeType typ),
  Core.bindingType = Nothing}))))

-- | Generate a binding name for an encoder function from a type name
encodeBindingName :: (Core.Name -> Core.Name)
encodeBindingName n = (Logic.ifElse (Logic.not (Lists.null (Lists.tail (Strings.splitOn "." (Core.unName n))))) (Core.Name (Strings.intercalate "." (Lists.concat2 [
  "hydra",
  "encode"] (Lists.concat2 (Lists.tail (Lists.init (Strings.splitOn "." (Core.unName n)))) [
  Formatting.decapitalize (Names.localNameOf n)])))) (Core.Name (Formatting.decapitalize (Names.localNameOf n))))

-- | Generate the encoder for a field's value
encodeFieldValue :: (Core.Name -> Core.Name -> Core.Type -> Core.Term)
encodeFieldValue typeName fieldName fieldType = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
  Core.lambdaParameter = (Core.Name "v"),
  Core.lambdaDomain = Nothing,
  Core.lambdaBody = (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "union"),
      Core.fieldTerm = (encodeInjection typeName fieldName (Core.TermApplication (Core.Application {
        Core.applicationFunction = (encodeType fieldType),
        Core.applicationArgument = (Core.TermVariable (Core.Name "v"))})))}}))})))

-- | Encode a float value based on its float type
encodeFloatValue :: (Core.FloatType -> Core.Term -> Core.Term)
encodeFloatValue floatType valTerm = (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.core.FloatValue"),
  Core.injectionField = Core.Field {
    Core.fieldName = ((\x -> case x of
      Core.FloatTypeBigfloat -> (Core.Name "bigfloat")
      Core.FloatTypeFloat32 -> (Core.Name "float32")
      Core.FloatTypeFloat64 -> (Core.Name "float64")) floatType),
    Core.fieldTerm = valTerm}}))

-- | Encode an Injection as a term
encodeInjection :: (Core.Name -> Core.Name -> Core.Term -> Core.Term)
encodeInjection typeName fieldName fieldTerm = (Core.TermRecord (Core.Record {
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
            Core.fieldTerm = fterm}]})) fieldName fieldTerm)}]}))

-- | Encode an integer value based on its integer type
encodeIntegerValue :: (Core.IntegerType -> Core.Term -> Core.Term)
encodeIntegerValue intType valTerm = (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
  Core.injectionField = Core.Field {
    Core.fieldName = ((\x -> case x of
      Core.IntegerTypeBigint -> (Core.Name "bigint")
      Core.IntegerTypeInt8 -> (Core.Name "int8")
      Core.IntegerTypeInt16 -> (Core.Name "int16")
      Core.IntegerTypeInt32 -> (Core.Name "int32")
      Core.IntegerTypeInt64 -> (Core.Name "int64")
      Core.IntegerTypeUint8 -> (Core.Name "uint8")
      Core.IntegerTypeUint16 -> (Core.Name "uint16")
      Core.IntegerTypeUint32 -> (Core.Name "uint32")
      Core.IntegerTypeUint64 -> (Core.Name "uint64")) intType),
    Core.fieldTerm = valTerm}}))

-- | Generate an encoder for a list type
encodeListType :: (Core.Type -> Core.Term)
encodeListType elemType = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
  Core.lambdaParameter = (Core.Name "xs"),
  Core.lambdaDomain = Nothing,
  Core.lambdaBody = (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "list"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.map"))),
          Core.applicationArgument = (encodeType elemType)})),
        Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})))

-- | Generate an encoder for a literal type
encodeLiteralType :: (Core.LiteralType -> Core.Term)
encodeLiteralType x = case x of
  Core.LiteralTypeBinary -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
    Core.lambdaParameter = (Core.Name "x"),
    Core.lambdaDomain = Nothing,
    Core.lambdaBody = (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Core.TermUnion (Core.Injection {
          Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
          Core.injectionField = Core.Field {
            Core.fieldName = (Core.Name "binary"),
            Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))})))
  Core.LiteralTypeBoolean -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
    Core.lambdaParameter = (Core.Name "x"),
    Core.lambdaDomain = Nothing,
    Core.lambdaBody = (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Core.TermUnion (Core.Injection {
          Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
          Core.injectionField = Core.Field {
            Core.fieldName = (Core.Name "boolean"),
            Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))})))
  Core.LiteralTypeString -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
    Core.lambdaParameter = (Core.Name "x"),
    Core.lambdaDomain = Nothing,
    Core.lambdaBody = (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Core.TermUnion (Core.Injection {
          Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
          Core.injectionField = Core.Field {
            Core.fieldName = (Core.Name "string"),
            Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))})))
  Core.LiteralTypeInteger v1 -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
    Core.lambdaParameter = (Core.Name "x"),
    Core.lambdaDomain = Nothing,
    Core.lambdaBody = (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Core.TermUnion (Core.Injection {
          Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
          Core.injectionField = Core.Field {
            Core.fieldName = (Core.Name "integer"),
            Core.fieldTerm = (encodeIntegerValue v1 (Core.TermVariable (Core.Name "x")))}}))}}))})))
  Core.LiteralTypeFloat v1 -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
    Core.lambdaParameter = (Core.Name "x"),
    Core.lambdaDomain = Nothing,
    Core.lambdaBody = (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Core.TermUnion (Core.Injection {
          Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
          Core.injectionField = Core.Field {
            Core.fieldName = (Core.Name "float"),
            Core.fieldTerm = (encodeFloatValue v1 (Core.TermVariable (Core.Name "x")))}}))}}))})))
  _ -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
    Core.lambdaParameter = (Core.Name "x"),
    Core.lambdaDomain = Nothing,
    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))

-- | Generate an encoder for an Either type
encodeEitherType :: (Core.EitherType -> Core.Term)
encodeEitherType et = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
  Core.lambdaParameter = (Core.Name "e"),
  Core.lambdaDomain = Nothing,
  Core.lambdaBody = (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "either"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.bimap"))),
            Core.applicationArgument = (encodeType (Core.eitherTypeLeft et))})),
          Core.applicationArgument = (encodeType (Core.eitherTypeRight et))})),
        Core.applicationArgument = (Core.TermVariable (Core.Name "e"))}))}}))})))

-- | Generate an encoder for a polymorphic (forall) type
encodeForallType :: (Core.ForallType -> Core.Term)
encodeForallType ft = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
  Core.lambdaParameter = (encodeBindingName (Core.forallTypeParameter ft)),
  Core.lambdaDomain = Nothing,
  Core.lambdaBody = (encodeType (Core.forallTypeBody ft))})))

-- | Generate an encoder for a map type
encodeMapType :: (Core.MapType -> Core.Term)
encodeMapType mt = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
  Core.lambdaParameter = (Core.Name "m"),
  Core.lambdaDomain = Nothing,
  Core.lambdaBody = (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "map"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.bimap"))),
            Core.applicationArgument = (encodeType (Core.mapTypeKeys mt))})),
          Core.applicationArgument = (encodeType (Core.mapTypeValues mt))})),
        Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))})))

-- | Generate an encoder for an optional type
encodeOptionalType :: (Core.Type -> Core.Term)
encodeOptionalType elemType = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
  Core.lambdaParameter = (Core.Name "opt"),
  Core.lambdaDomain = Nothing,
  Core.lambdaBody = (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "maybe"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maybes.map"))),
          Core.applicationArgument = (encodeType elemType)})),
        Core.applicationArgument = (Core.TermVariable (Core.Name "opt"))}))}}))})))

-- | Generate an encoder for a pair type
encodePairType :: (Core.PairType -> Core.Term)
encodePairType pt = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
  Core.lambdaParameter = (Core.Name "p"),
  Core.lambdaDomain = Nothing,
  Core.lambdaBody = (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "pair"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.pairs.bimap"))),
            Core.applicationArgument = (encodeType (Core.pairTypeFirst pt))})),
          Core.applicationArgument = (encodeType (Core.pairTypeSecond pt))})),
        Core.applicationArgument = (Core.TermVariable (Core.Name "p"))}))}}))})))

-- | Transform a type module into an encoder module
encodeModule :: (Module.Module -> Compute.Flow Graph.Graph (Maybe Module.Module))
encodeModule mod = (Flows.bind (filterTypeBindings (Module.moduleElements mod)) (\typeBindings -> Logic.ifElse (Lists.null typeBindings) (Flows.pure Nothing) (Flows.bind (Flows.mapList encodeBinding typeBindings) (\encodedBindings -> Flows.pure (Just (Module.Module {
  Module.moduleNamespace = (encodeNamespace (Module.moduleNamespace mod)),
  Module.moduleElements = encodedBindings,
  Module.moduleTermDependencies = (Lists.map encodeNamespace (Module.moduleTypeDependencies mod)),
  Module.moduleTypeDependencies = [
    Module.moduleNamespace mod],
  Module.moduleDescription = (Just (Strings.cat [
    "Term encoders for ",
    (Module.unNamespace (Module.moduleNamespace mod))]))}))))))

-- | Encode a Name as a term
encodeName :: (Core.Name -> Core.Term)
encodeName n = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName n)))}))

-- | Generate an encoder module namespace from a source module namespace
encodeNamespace :: (Module.Namespace -> Module.Namespace)
encodeNamespace ns = (Module.Namespace (Strings.cat [
  "hydra.encode.",
  (Strings.intercalate "." (Lists.tail (Strings.splitOn "." (Module.unNamespace ns))))]))

-- | Generate an encoder for a record type
encodeRecordType :: (Core.RowType -> Core.Term)
encodeRecordType rt = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
  Core.lambdaParameter = (Core.Name "x"),
  Core.lambdaDomain = Nothing,
  Core.lambdaBody = (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "record"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra.core.Record"),
        Core.recordFields = [
          Core.Field {
            Core.fieldName = (Core.Name "typeName"),
            Core.fieldTerm = (encodeName (Core.rowTypeTypeName rt))},
          Core.Field {
            Core.fieldName = (Core.Name "fields"),
            Core.fieldTerm = (Core.TermList (Lists.map ((\recType -> \ft -> Core.TermRecord (Core.Record {
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
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                        Core.projectionTypeName = (Core.rowTypeTypeName recType),
                        Core.projectionField = (Core.fieldTypeName ft)})))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})) rt) (Core.rowTypeFields rt)))}]}))}}))})))

-- | Generate an encoder for a set type
encodeSetType :: (Core.Type -> Core.Term)
encodeSetType elemType = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
  Core.lambdaParameter = (Core.Name "s"),
  Core.lambdaDomain = Nothing,
  Core.lambdaBody = (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "set"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.map"))),
          Core.applicationArgument = (encodeType elemType)})),
        Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))}}))})))

-- | Generate an encoder term for a Type
encodeType :: (Core.Type -> Core.Term)
encodeType x = case x of
  Core.TypeAnnotated v1 -> (encodeType (Core.annotatedTypeBody v1))
  Core.TypeApplication v1 -> (Core.TermApplication (Core.Application {
    Core.applicationFunction = (encodeType (Core.applicationTypeFunction v1)),
    Core.applicationArgument = (encodeType (Core.applicationTypeArgument v1))}))
  Core.TypeEither v1 -> (encodeEitherType v1)
  Core.TypeForall v1 -> (encodeForallType v1)
  Core.TypeFunction _ -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
    Core.lambdaParameter = (Core.Name "x"),
    Core.lambdaDomain = Nothing,
    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))
  Core.TypeList v1 -> (encodeListType v1)
  Core.TypeLiteral v1 -> (encodeLiteralType v1)
  Core.TypeMap v1 -> (encodeMapType v1)
  Core.TypeMaybe v1 -> (encodeOptionalType v1)
  Core.TypePair v1 -> (encodePairType v1)
  Core.TypeRecord v1 -> (encodeRecordType v1)
  Core.TypeSet v1 -> (encodeSetType v1)
  Core.TypeUnion v1 -> (encodeUnionType v1)
  Core.TypeWrap v1 -> (encodeWrappedType v1)
  Core.TypeUnit -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
    Core.lambdaParameter = (Core.Name "_"),
    Core.lambdaDomain = Nothing,
    Core.lambdaBody = (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unit"),
        Core.fieldTerm = Core.TermUnit}}))})))
  Core.TypeVariable v1 -> (Core.TermVariable (encodeBindingName v1))
  _ -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
    Core.lambdaParameter = (Core.Name "x"),
    Core.lambdaDomain = Nothing,
    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))

-- | Generate an encoder for a union type
encodeUnionType :: (Core.RowType -> Core.Term)
encodeUnionType rt = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
  Core.caseStatementTypeName = (Core.rowTypeTypeName rt),
  Core.caseStatementDefault = Nothing,
  Core.caseStatementCases = (Lists.map (\ft -> Core.Field {
    Core.fieldName = (Core.fieldTypeName ft),
    Core.fieldTerm = (encodeFieldValue (Core.rowTypeTypeName rt) (Core.fieldTypeName ft) (Core.fieldTypeType ft))}) (Core.rowTypeFields rt))}))))

-- | Generate an encoder for a wrapped type
encodeWrappedType :: (Core.WrappedType -> Core.Term)
encodeWrappedType wt = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
  Core.lambdaParameter = (Core.Name "x"),
  Core.lambdaDomain = Nothing,
  Core.lambdaBody = (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "wrap"),
      Core.fieldTerm = (Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.Name "hydra.core.WrappedTerm"),
        Core.recordFields = [
          Core.Field {
            Core.fieldName = (Core.Name "typeName"),
            Core.fieldTerm = (encodeName (Core.wrappedTypeTypeName wt))},
          Core.Field {
            Core.fieldName = (Core.Name "body"),
            Core.fieldTerm = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (encodeType (Core.wrappedTypeBody wt)),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.wrappedTypeTypeName wt)))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))})))

-- | Filter bindings to only encodable type definitions
filterTypeBindings :: ([Core.Binding] -> Compute.Flow Graph.Graph [Core.Binding])
filterTypeBindings bindings = (Flows.map Maybes.cat (Flows.mapList isEncodableBinding (Lists.filter Annotations.isNativeType bindings)))

-- | Check if a binding is encodable (serializable type)
isEncodableBinding :: (Core.Binding -> Compute.Flow Graph.Graph (Maybe Core.Binding))
isEncodableBinding b = (Flows.map (\serializable -> Logic.ifElse serializable (Just b) Nothing) (Schemas.isSerializableByName (Core.bindingName b)))

-- | Check whether a type is the unit type
isUnitType :: (Core.Type -> Bool)
isUnitType x = case x of
  Core.TypeUnit -> True
  _ -> False
