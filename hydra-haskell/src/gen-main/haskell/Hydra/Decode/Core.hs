-- Note: this is an automatically generated file. Do not edit.

-- | Decode hydra.core types from the hydra.core.Term type

module Hydra.Decode.Core where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Monads as Monads
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Show.Core as Core__
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Decode an annotated type from a term
annotatedType :: (Core.Term -> Compute.Flow Graph.Graph Core.AnnotatedType)
annotatedType = (Lexical.matchRecord (\m -> Flows.bind (Lexical.getField m (Core.Name "body") type_) (\body -> Flows.bind (Lexical.getField m (Core.Name "annotation") (\mapTerm -> (\x -> case x of
  Core.TermMap v1 -> (Flows.map Maps.fromList (Flows.mapList (\entry -> Flows.bind (name (Pairs.first entry)) (\k -> Flows.map (\v -> (k, v)) ((\t -> Lexical.matchUnion (Core.Name "hydra.core.Term") [
    (Core.Name "literal", (\lit -> Flows.map (\x -> Core.TermLiteral x) (Lexical.matchUnion (Core.Name "hydra.core.Literal") [
      (Core.Name "boolean", (\b -> Flows.map (\x -> Core.LiteralBoolean x) (Core_.boolean b))),
      (Core.Name "float", (\fv -> Flows.map (\x -> Core.LiteralFloat x) (Lexical.matchUnion (Core.Name "hydra.core.FloatValue") [
        (Core.Name "bigfloat", (\v -> Flows.map (\x -> Core.FloatValueBigfloat x) (Core_.bigfloat v))),
        (Core.Name "float32", (\v -> Flows.map (\x -> Core.FloatValueFloat32 x) (Core_.float32 v))),
        (Core.Name "float64", (\v -> Flows.map (\x -> Core.FloatValueFloat64 x) (Core_.float64 v)))] fv))),
      (Core.Name "integer", (\iv -> Flows.map (\x -> Core.LiteralInteger x) (Lexical.matchUnion (Core.Name "hydra.core.IntegerValue") [
        (Core.Name "bigint", (\v -> Flows.map (\x -> Core.IntegerValueBigint x) (Core_.bigint v))),
        (Core.Name "int8", (\v -> Flows.map (\x -> Core.IntegerValueInt8 x) (Core_.int8 v))),
        (Core.Name "int16", (\v -> Flows.map (\x -> Core.IntegerValueInt16 x) (Core_.int16 v))),
        (Core.Name "int32", (\v -> Flows.map (\x -> Core.IntegerValueInt32 x) (Core_.int32 v))),
        (Core.Name "int64", (\v -> Flows.map (\x -> Core.IntegerValueInt64 x) (Core_.int64 v))),
        (Core.Name "uint8", (\v -> Flows.map (\x -> Core.IntegerValueUint8 x) (Core_.uint8 v))),
        (Core.Name "uint16", (\v -> Flows.map (\x -> Core.IntegerValueUint16 x) (Core_.uint16 v))),
        (Core.Name "uint32", (\v -> Flows.map (\x -> Core.IntegerValueUint32 x) (Core_.uint32 v))),
        (Core.Name "uint64", (\v -> Flows.map (\x -> Core.IntegerValueUint64 x) (Core_.uint64 v)))] iv))),
      (Core.Name "string", (\s -> Flows.map (\x -> Core.LiteralString x) (Core_.string s)))] lit))),
    (Core.Name "variable", (\n -> Flows.map (\x -> Core.TermVariable x) (name n)))] t) (Pairs.second entry)))) (Maps.toList v1)))
  _ -> (Monads.unexpected "map" (Core__.term mapTerm))) mapTerm)) (\annotation -> Flows.pure (Core.AnnotatedType {
  Core.annotatedTypeBody = body,
  Core.annotatedTypeAnnotation = annotation})))))

-- | Decode an application type from a term
applicationType :: (Core.Term -> Compute.Flow Graph.Graph Core.ApplicationType)
applicationType = (Lexical.matchRecord (\m -> Flows.bind (Lexical.getField m (Core.Name "function") type_) (\function -> Flows.bind (Lexical.getField m (Core.Name "argument") type_) (\argument -> Flows.pure (Core.ApplicationType {
  Core.applicationTypeFunction = function,
  Core.applicationTypeArgument = argument})))))

-- | Decode an either type from a term
eitherType :: (Core.Term -> Compute.Flow Graph.Graph Core.EitherType)
eitherType = (Lexical.matchRecord (\m -> Flows.bind (Lexical.getField m (Core.Name "left") type_) (\left -> Flows.bind (Lexical.getField m (Core.Name "right") type_) (\right -> Flows.pure (Core.EitherType {
  Core.eitherTypeLeft = left,
  Core.eitherTypeRight = right})))))

-- | Decode a field type from a term
fieldType :: (Core.Term -> Compute.Flow Graph.Graph Core.FieldType)
fieldType = (Lexical.matchRecord (\m -> Flows.bind (Lexical.getField m (Core.Name "name") name) (\name -> Flows.bind (Lexical.getField m (Core.Name "type") type_) (\typ -> Flows.pure (Core.FieldType {
  Core.fieldTypeName = name,
  Core.fieldTypeType = typ})))))

-- | Decode a list of field types from a term
fieldTypes :: (Core.Term -> Compute.Flow Graph.Graph [Core.FieldType])
fieldTypes term =  
  let stripped = (Rewriting.deannotateAndDetypeTerm term)
  in ((\x -> case x of
    Core.TermList v1 -> (Flows.mapList fieldType v1)
    _ -> (Monads.unexpected "list" (Core__.term term))) stripped)

-- | Decode a floating-point type from a term
floatType :: (Core.Term -> Compute.Flow Graph.Graph Core.FloatType)
floatType term0 = (Monads.withTrace "dbg 1" (Lexical.matchEnum (Core.Name "hydra.core.FloatType") [
  (Core.Name "bigfloat", Core.FloatTypeBigfloat),
  (Core.Name "float32", Core.FloatTypeFloat32),
  (Core.Name "float64", Core.FloatTypeFloat64)] term0))

-- | Decode a forall type from a term
forallType :: (Core.Term -> Compute.Flow Graph.Graph Core.ForallType)
forallType = (Lexical.matchRecord (\m -> Flows.bind (Lexical.getField m (Core.Name "parameter") name) (\parameter -> Flows.bind (Lexical.getField m (Core.Name "body") type_) (\body -> Flows.pure (Core.ForallType {
  Core.forallTypeParameter = parameter,
  Core.forallTypeBody = body})))))

-- | Decode a function type from a term
functionType :: (Core.Term -> Compute.Flow Graph.Graph Core.FunctionType)
functionType = (Lexical.matchRecord (\m -> Flows.bind (Lexical.getField m (Core.Name "domain") type_) (\domain -> Flows.bind (Lexical.getField m (Core.Name "codomain") type_) (\codomain -> Flows.pure (Core.FunctionType {
  Core.functionTypeDomain = domain,
  Core.functionTypeCodomain = codomain})))))

-- | Decode an integer type from a term
integerType :: (Core.Term -> Compute.Flow Graph.Graph Core.IntegerType)
integerType term0 = (Monads.withTrace "dbg 1" (Lexical.matchEnum (Core.Name "hydra.core.IntegerType") [
  (Core.Name "bigint", Core.IntegerTypeBigint),
  (Core.Name "int8", Core.IntegerTypeInt8),
  (Core.Name "int16", Core.IntegerTypeInt16),
  (Core.Name "int32", Core.IntegerTypeInt32),
  (Core.Name "int64", Core.IntegerTypeInt64),
  (Core.Name "uint8", Core.IntegerTypeUint8),
  (Core.Name "uint16", Core.IntegerTypeUint16),
  (Core.Name "uint32", Core.IntegerTypeUint32),
  (Core.Name "uint64", Core.IntegerTypeUint64)] term0))

-- | Decode a literal type from a term
literalType :: (Core.Term -> Compute.Flow Graph.Graph Core.LiteralType)
literalType term0 = (Monads.withTrace "dbg 3" (Lexical.matchUnion (Core.Name "hydra.core.LiteralType") [
  Lexical.matchUnitField (Core.Name "binary") Core.LiteralTypeBinary,
  Lexical.matchUnitField (Core.Name "boolean") Core.LiteralTypeBoolean,
  (Core.Name "float", (\ft -> Flows.map (\x -> Core.LiteralTypeFloat x) (floatType ft))),
  (Core.Name "integer", (\it -> Flows.map (\x -> Core.LiteralTypeInteger x) (integerType it))),
  (Lexical.matchUnitField (Core.Name "string") Core.LiteralTypeString)] term0))

-- | Decode a map type from a term
mapType :: (Core.Term -> Compute.Flow Graph.Graph Core.MapType)
mapType = (Lexical.matchRecord (\m -> Flows.bind (Lexical.getField m (Core.Name "keys") type_) (\keys -> Flows.bind (Lexical.getField m (Core.Name "values") type_) (\values -> Flows.pure (Core.MapType {
  Core.mapTypeKeys = keys,
  Core.mapTypeValues = values})))))

-- | Decode a name from a term
name :: (Core.Term -> Compute.Flow Graph.Graph Core.Name)
name term = (Flows.map (\x -> Core.Name x) (Flows.bind (Core_.wrap (Core.Name "hydra.core.Name") term) Core_.string))

-- | Decode a pair type from a term
pairType :: (Core.Term -> Compute.Flow Graph.Graph Core.PairType)
pairType = (Lexical.matchRecord (\m -> Flows.bind (Lexical.getField m (Core.Name "first") type_) (\first -> Flows.bind (Lexical.getField m (Core.Name "second") type_) (\second -> Flows.pure (Core.PairType {
  Core.pairTypeFirst = first,
  Core.pairTypeSecond = second})))))

-- | Decode a row type from a term
rowType :: (Core.Term -> Compute.Flow Graph.Graph Core.RowType)
rowType = (Lexical.matchRecord (\m -> Flows.bind (Lexical.getField m (Core.Name "typeName") name) (\typeName -> Flows.bind (Lexical.getField m (Core.Name "fields") fieldTypes) (\fields -> Flows.pure (Core.RowType {
  Core.rowTypeTypeName = typeName,
  Core.rowTypeFields = fields})))))

-- | Decode a string from a term
string :: (Core.Term -> Compute.Flow Graph.Graph String)
string term = (Core_.string (Rewriting.deannotateAndDetypeTerm term))

-- | Decode a type from a term
type_ :: (Core.Term -> Compute.Flow Graph.Graph Core.Type)
type_ dat = ((\x -> case x of
  Core.TermAnnotated v1 -> (Flows.map (\t -> Core.TypeAnnotated (Core.AnnotatedType {
    Core.annotatedTypeBody = t,
    Core.annotatedTypeAnnotation = (Core.annotatedTermAnnotation v1)})) (type_ (Core.annotatedTermBody v1)))
  _ -> (Monads.withTrace "dbg 4" (Lexical.matchUnion (Core.Name "hydra.core.Type") [
    (Core.Name "annotated", (\at -> Flows.map (\x -> Core.TypeAnnotated x) (annotatedType at))),
    (Core.Name "application", (\at -> Flows.map (\x -> Core.TypeApplication x) (applicationType at))),
    (Core.Name "either", (\et -> Flows.map (\x -> Core.TypeEither x) (eitherType et))),
    (Core.Name "forall", (\ft -> Flows.map (\x -> Core.TypeForall x) (forallType ft))),
    (Core.Name "function", (\ft -> Flows.map (\x -> Core.TypeFunction x) (functionType ft))),
    (Core.Name "list", (\et -> Flows.map (\x -> Core.TypeList x) (type_ et))),
    (Core.Name "literal", (\lt -> Flows.map (\x -> Core.TypeLiteral x) (literalType lt))),
    (Core.Name "map", (\mt -> Flows.map (\x -> Core.TypeMap x) (mapType mt))),
    (Core.Name "maybe", (\et -> Flows.map (\x -> Core.TypeMaybe x) (type_ et))),
    (Core.Name "pair", (\pt -> Flows.map (\x -> Core.TypePair x) (pairType pt))),
    (Core.Name "record", (\rt -> Flows.map (\x -> Core.TypeRecord x) (rowType rt))),
    (Core.Name "set", (\et -> Flows.map (\x -> Core.TypeSet x) (type_ et))),
    (Core.Name "union", (\rt -> Flows.map (\x -> Core.TypeUnion x) (rowType rt))),
    (Core.Name "unit", (\_ -> Flows.pure Core.TypeUnit)),
    (Core.Name "variable", (\n -> Flows.map (\x -> Core.TypeVariable x) (name n))),
    (Core.Name "wrap", (\wt -> Flows.map (\x -> Core.TypeWrap x) (wrappedType wt)))] dat))) dat)

-- | Decode a type scheme from a term
typeScheme :: (Core.Term -> Compute.Flow Graph.Graph Core.TypeScheme)
typeScheme = (Lexical.matchRecord (\m -> Flows.bind (Lexical.getField m (Core.Name "variables") (Core_.listOf name)) (\vars -> Flows.bind (Lexical.getField m (Core.Name "type") type_) (\body -> Flows.pure (Core.TypeScheme {
  Core.typeSchemeVariables = vars,
  Core.typeSchemeType = body})))))

-- | Decode a wrapped type from a term
wrappedType :: (Core.Term -> Compute.Flow Graph.Graph Core.WrappedType)
wrappedType term = (Flows.bind (Core_.record (Core.Name "hydra.core.WrappedType") term) (\fields -> Flows.bind (Core_.field (Core.Name "typeName") name fields) (\name -> Flows.bind (Core_.field (Core.Name "body") type_ fields) (\obj -> Flows.pure (Core.WrappedType {
  Core.wrappedTypeTypeName = name,
  Core.wrappedTypeBody = obj})))))
