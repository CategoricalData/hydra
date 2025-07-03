-- | Decoding of encoded types (as terms) back to types according to LambdaGraph's epsilon encoding.

module Hydra.Decode.Core where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Monads as Monads
import qualified Hydra.Show.Core as Core__
import qualified Hydra.Strip as Strip
import Prelude hiding  (Enum, Ordering, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

applicationType :: (Core.Term -> Compute.Flow Graph.Graph Core.ApplicationType)
applicationType = (matchRecord (\m -> Monads.map2 (getField m (Core.Name "function") type_) (getField m (Core.Name "argument") type_) (\function -> \argument -> Core.ApplicationType {
  Core.applicationTypeFunction = function,
  Core.applicationTypeArgument = argument})))

fieldType :: (Core.Term -> Compute.Flow Graph.Graph Core.FieldType)
fieldType = (matchRecord (\m -> Monads.map2 (getField m (Core.Name "name") name) (getField m (Core.Name "type") type_) (\name -> \typ -> Core.FieldType {
  Core.fieldTypeName = name,
  Core.fieldTypeType = typ})))

fieldTypes :: (Core.Term -> Compute.Flow Graph.Graph [Core.FieldType])
fieldTypes term =  
  let stripped = (Strip.fullyStripTerm term)
  in ((\x -> case x of
    Core.TermList v1 -> (Flows.mapList fieldType v1)
    _ -> (Errors.unexpected "list" (Core__.term term))) stripped)

floatType :: (Core.Term -> Compute.Flow Graph.Graph Core.FloatType)
floatType = (matchEnum (Core.Name "hydra.core.FloatType") [
  (Core.Name "bigfloat", Core.FloatTypeBigfloat),
  (Core.Name "float32", Core.FloatTypeFloat32),
  (Core.Name "float64", Core.FloatTypeFloat64)])

forallType :: (Core.Term -> Compute.Flow Graph.Graph Core.ForallType)
forallType = (matchRecord (\m -> Monads.map2 (getField m (Core.Name "parameter") name) (getField m (Core.Name "body") type_) (\parameter -> \body -> Core.ForallType {
  Core.forallTypeParameter = parameter,
  Core.forallTypeBody = body})))

functionType :: (Core.Term -> Compute.Flow Graph.Graph Core.FunctionType)
functionType = (matchRecord (\m -> Monads.map2 (getField m (Core.Name "domain") type_) (getField m (Core.Name "codomain") type_) (\domain -> \codomain -> Core.FunctionType {
  Core.functionTypeDomain = domain,
  Core.functionTypeCodomain = codomain})))

integerType :: (Core.Term -> Compute.Flow Graph.Graph Core.IntegerType)
integerType = (matchEnum (Core.Name "hydra.core.IntegerType") [
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
literalType = (matchUnion (Core.Name "hydra.core.LiteralType") [
  matchUnitField (Core.Name "binary") Core.LiteralTypeBinary,
  matchUnitField (Core.Name "boolean") Core.LiteralTypeBoolean,
  (Core.Name "float", (\ft -> Flows.map (\x -> Core.LiteralTypeFloat x) (floatType ft))),
  (Core.Name "integer", (\it -> Flows.map (\x -> Core.LiteralTypeInteger x) (integerType it))),
  (matchUnitField (Core.Name "string") Core.LiteralTypeString)])

mapType :: (Core.Term -> Compute.Flow Graph.Graph Core.MapType)
mapType = (matchRecord (\m -> Monads.map2 (getField m (Core.Name "keys") type_) (getField m (Core.Name "values") type_) (\keys -> \values -> Core.MapType {
  Core.mapTypeKeys = keys,
  Core.mapTypeValues = values})))

name :: (Core.Term -> Compute.Flow Graph.Graph Core.Name)
name term = (Flows.map (\x -> Core.Name x) (Flows.bind (Core_.wrap (Core.Name "hydra.core.Name") term) Core_.string))

rowType :: (Core.Term -> Compute.Flow Graph.Graph Core.RowType)
rowType = (matchRecord (\m -> Monads.map2 (getField m (Core.Name "typeName") name) (getField m (Core.Name "fields") fieldTypes) (\typeName -> \fields -> Core.RowType {
  Core.rowTypeTypeName = typeName,
  Core.rowTypeFields = fields})))

string :: (Core.Term -> Compute.Flow Graph.Graph String)
string term = (Core_.string (Strip.fullyStripTerm term))

type_ :: (Core.Term -> Compute.Flow Graph.Graph Core.Type)
type_ dat = ((\x -> case x of
  Core.TermAnnotated v1 -> (Flows.map (\t -> Core.TypeAnnotated (Core.AnnotatedType {
    Core.annotatedTypeSubject = t,
    Core.annotatedTypeAnnotation = (Core.annotatedTermAnnotation v1)})) (type_ (Core.annotatedTermSubject v1)))
  _ -> (matchUnion (Core.Name "hydra.core.Type") [
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
    (Core.Name "variable", (\n -> Flows.map (\x -> Core.TypeVariable x) (name n))),
    (Core.Name "wrap", (\wt -> Flows.map (\x -> Core.TypeWrap x) (wrappedType wt)))] dat)) dat)

typeScheme :: (Core.Term -> Compute.Flow Graph.Graph Core.TypeScheme)
typeScheme = (matchRecord (\m -> Monads.map2 (getField m (Core.Name "variables") (Core_.list name)) (getField m (Core.Name "type") type_) (\vars -> \body -> Core.TypeScheme {
  Core.typeSchemeVariables = vars,
  Core.typeSchemeType = body})))

wrappedType :: (Core.Term -> Compute.Flow Graph.Graph Core.WrappedType)
wrappedType term = (Flows.bind (Core_.record (Core.Name "hydra.core.WrappedType") term) (\fields -> Monads.map2 (Core_.field (Core.Name "typeName") name fields) (Core_.field (Core.Name "object") type_ fields) (\name -> \obj -> Core.WrappedType {
  Core.wrappedTypeTypeName = name,
  Core.wrappedTypeObject = obj})))

getField :: (M.Map Core.Name t2 -> Core.Name -> (t2 -> Compute.Flow t0 t1) -> Compute.Flow t0 t1)
getField m fname decode = (Optionals.maybe (Flows.fail (Strings.cat [
  Strings.cat [
    "expected field ",
    (Core.unName fname)],
  " not found"])) decode (Maps.lookup fname m))

matchEnum :: (Core.Name -> [(Core.Name, t0)] -> Core.Term -> Compute.Flow Graph.Graph t0)
matchEnum tname pairs = (matchUnion tname (Lists.map (\pair -> matchUnitField (fst pair) (snd pair)) pairs))

matchRecord :: ((M.Map Core.Name Core.Term -> Compute.Flow t0 t1) -> Core.Term -> Compute.Flow t0 t1)
matchRecord decode term =  
  let stripped = (Strip.fullyStripTerm term)
  in ((\x -> case x of
    Core.TermRecord v1 -> (decode (Maps.fromList (Lists.map (\field -> (Core.fieldName field, (Core.fieldTerm field))) (Core.recordFields v1))))
    _ -> (Errors.unexpected "record" (Core__.term term))) stripped)

matchUnion :: (Core.Name -> [(Core.Name, (Core.Term -> Compute.Flow Graph.Graph t0))] -> Core.Term -> Compute.Flow Graph.Graph t0)
matchUnion tname pairs term =  
  let stripped = (Strip.fullyStripTerm term) 
      mapping = (Maps.fromList pairs)
  in ((\x -> case x of
    Core.TermVariable v1 -> (Flows.bind (Lexical.requireElement v1) (\el -> matchUnion tname pairs (Graph.elementTerm el)))
    Core.TermUnion v1 -> (Logic.ifElse (Equality.equalString (Core.unName (Core.injectionTypeName v1)) (Core.unName tname)) ( 
      let fname = (Core.fieldName (Core.injectionField v1)) 
          val = (Core.fieldTerm (Core.injectionField v1))
      in (Optionals.maybe (Flows.fail (Strings.cat [
        "no matching case for field ",
        (Core.unName fname)])) (\f -> f val) (Maps.lookup fname mapping))) (Errors.unexpected (Strings.cat [
      "injection for type ",
      (Core.unName tname)]) (Core__.term term)))
    _ -> (Errors.unexpected (Strings.cat [
      Strings.cat [
        "union with one of {",
        (Strings.intercalate ", " (Lists.map (\pair -> Core.unName (fst pair)) pairs))],
      "}"]) (Core__.term stripped))) stripped)

matchUnitField :: (t0 -> t1 -> (t0, (t2 -> Compute.Flow t3 t1)))
matchUnitField fname x = (fname, (\ignored -> Flows.pure x))
