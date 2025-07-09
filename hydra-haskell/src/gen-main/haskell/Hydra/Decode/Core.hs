-- | Decode hydra.core types from the hydra.core.Term type

module Hydra.Decode.Core where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Monads as Monads
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Show.Core as Core__
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

applicationType :: (Core.Term -> Compute.Flow Graph.Graph Core.ApplicationType)
applicationType = (Lexical.matchRecord (\m -> Monads.map2 (Lexical.getField m (Core.Name "function") type_) (Lexical.getField m (Core.Name "argument") type_) (\function -> \argument -> Core.ApplicationType {
  Core.applicationTypeFunction = function,
  Core.applicationTypeArgument = argument})))

fieldType :: (Core.Term -> Compute.Flow Graph.Graph Core.FieldType)
fieldType = (Lexical.matchRecord (\m -> Monads.map2 (Lexical.getField m (Core.Name "name") name) (Lexical.getField m (Core.Name "type") type_) (\name -> \typ -> Core.FieldType {
  Core.fieldTypeName = name,
  Core.fieldTypeType = typ})))

fieldTypes :: (Core.Term -> Compute.Flow Graph.Graph [Core.FieldType])
fieldTypes term =  
  let stripped = (Rewriting.deannotateAndDetypeTerm term)
  in ((\x -> case x of
    Core.TermList v1 -> (Flows.mapList fieldType v1)
    _ -> (Monads.unexpected "list--" (Core__.term term))) stripped)

floatType :: (Core.Term -> Compute.Flow Graph.Graph Core.FloatType)
floatType = (Lexical.matchEnum (Core.Name "hydra.core.FloatType") [
  (Core.Name "bigfloat", Core.FloatTypeBigfloat),
  (Core.Name "float32", Core.FloatTypeFloat32),
  (Core.Name "float64", Core.FloatTypeFloat64)])

forallType :: (Core.Term -> Compute.Flow Graph.Graph Core.ForallType)
forallType = (Lexical.matchRecord (\m -> Monads.map2 (Lexical.getField m (Core.Name "parameter") name) (Lexical.getField m (Core.Name "body") type_) (\parameter -> \body -> Core.ForallType {
  Core.forallTypeParameter = parameter,
  Core.forallTypeBody = body})))

functionType :: (Core.Term -> Compute.Flow Graph.Graph Core.FunctionType)
functionType = (Lexical.matchRecord (\m -> Monads.map2 (Lexical.getField m (Core.Name "domain") type_) (Lexical.getField m (Core.Name "codomain") type_) (\domain -> \codomain -> Core.FunctionType {
  Core.functionTypeDomain = domain,
  Core.functionTypeCodomain = codomain})))

integerType :: (Core.Term -> Compute.Flow Graph.Graph Core.IntegerType)
integerType = (Lexical.matchEnum (Core.Name "hydra.core.IntegerType") [
  (Core.Name "bigint", Core.IntegerTypeBigint),
  (Core.Name "int8", Core.IntegerTypeInt8),
  (Core.Name "int16", Core.IntegerTypeInt16),
  (Core.Name "int32", Core.IntegerTypeInt32),
  (Core.Name "int64", Core.IntegerTypeInt64),
  (Core.Name "uint8", Core.IntegerTypeUint8),
  (Core.Name "uint16", Core.IntegerTypeUint16),
  (Core.Name "uint32", Core.IntegerTypeUint32),
  (Core.Name "uint64", Core.IntegerTypeUint64)])

literalType :: (Core.Term -> Compute.Flow Graph.Graph Core.LiteralType)
literalType = (Lexical.matchUnion (Core.Name "hydra.core.LiteralType") [
  Lexical.matchUnitField (Core.Name "binary") Core.LiteralTypeBinary,
  Lexical.matchUnitField (Core.Name "boolean") Core.LiteralTypeBoolean,
  (Core.Name "float", (\ft -> Flows.map (\x -> Core.LiteralTypeFloat x) (floatType ft))),
  (Core.Name "integer", (\it -> Flows.map (\x -> Core.LiteralTypeInteger x) (integerType it))),
  (Lexical.matchUnitField (Core.Name "string") Core.LiteralTypeString)])

mapType :: (Core.Term -> Compute.Flow Graph.Graph Core.MapType)
mapType = (Lexical.matchRecord (\m -> Monads.map2 (Lexical.getField m (Core.Name "keys") type_) (Lexical.getField m (Core.Name "values") type_) (\keys -> \values -> Core.MapType {
  Core.mapTypeKeys = keys,
  Core.mapTypeValues = values})))

name :: (Core.Term -> Compute.Flow Graph.Graph Core.Name)
name term = (Flows.map (\x -> Core.Name x) (Flows.bind (Core_.wrap (Core.Name "hydra.core.Name") term) Core_.string))

rowType :: (Core.Term -> Compute.Flow Graph.Graph Core.RowType)
rowType = (Lexical.matchRecord (\m -> Monads.map2 (Lexical.getField m (Core.Name "typeName") name) (Lexical.getField m (Core.Name "fields") fieldTypes) (\typeName -> \fields -> Core.RowType {
  Core.rowTypeTypeName = typeName,
  Core.rowTypeFields = fields})))

string :: (Core.Term -> Compute.Flow Graph.Graph String)
string term = (Core_.string (Rewriting.deannotateAndDetypeTerm term))

type_ :: (Core.Term -> Compute.Flow Graph.Graph Core.Type)
type_ dat = ((\x -> case x of
  Core.TermAnnotated v1 -> (Flows.map (\t -> Core.TypeAnnotated (Core.AnnotatedType {
    Core.annotatedTypeSubject = t,
    Core.annotatedTypeAnnotation = (Core.annotatedTermAnnotation v1)})) (type_ (Core.annotatedTermSubject v1)))
  _ -> (Lexical.matchUnion (Core.Name "hydra.core.Type") [
    (Core.Name "application", (\at -> Flows.map (\x -> Core.TypeApplication x) (applicationType at))),
    (Core.Name "forall", (\ft -> Flows.map (\x -> Core.TypeForall x) (forallType ft))),
    (Core.Name "function", (\ft -> Flows.map (\x -> Core.TypeFunction x) (functionType ft))),
    (Core.Name "list", (\et -> Flows.map (\x -> Core.TypeList x) (type_ et))),
    (Core.Name "literal", (\lt -> Flows.map (\x -> Core.TypeLiteral x) (literalType lt))),
    (Core.Name "map", (\mt -> Flows.map (\x -> Core.TypeMap x) (mapType mt))),
    (Core.Name "optional", (\et -> Flows.map (\x -> Core.TypeOptional x) (type_ et))),
    (Core.Name "product", (\types -> Flows.map (\x -> Core.TypeProduct x) (Core_.list type_ types))),
    (Core.Name "record", (\rt -> Flows.map (\x -> Core.TypeRecord x) (rowType rt))),
    (Core.Name "set", (\et -> Flows.map (\x -> Core.TypeSet x) (type_ et))),
    (Core.Name "sum", (\types -> Flows.map (\x -> Core.TypeSum x) (Core_.list type_ types))),
    (Core.Name "union", (\rt -> Flows.map (\x -> Core.TypeUnion x) (rowType rt))),
    (Core.Name "unit", (\_ -> Flows.pure Core.TypeUnit)),
    (Core.Name "variable", (\n -> Flows.map (\x -> Core.TypeVariable x) (name n))),
    (Core.Name "wrap", (\wt -> Flows.map (\x -> Core.TypeWrap x) (wrappedType wt)))] dat)) dat)

typeScheme :: (Core.Term -> Compute.Flow Graph.Graph Core.TypeScheme)
typeScheme = (Lexical.matchRecord (\m -> Monads.map2 (Lexical.getField m (Core.Name "variables") (Core_.list name)) (Lexical.getField m (Core.Name "type") type_) (\vars -> \body -> Core.TypeScheme {
  Core.typeSchemeVariables = vars,
  Core.typeSchemeType = body})))

wrappedType :: (Core.Term -> Compute.Flow Graph.Graph Core.WrappedType)
wrappedType term = (Flows.bind (Core_.record (Core.Name "hydra.core.WrappedType") term) (\fields -> Monads.map2 (Core_.field (Core.Name "typeName") name fields) (Core_.field (Core.Name "object") type_ fields) (\name -> \obj -> Core.WrappedType {
  Core.wrappedTypeTypeName = name,
  Core.wrappedTypeObject = obj})))
