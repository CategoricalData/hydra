-- | Decoding of encoded types (as terms) back to types according to LambdaGraph's epsilon encoding.

module Hydra.CoreDecoding where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Expect as Expect
import qualified Hydra.Flows as Flows
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows_
import qualified Hydra.Lib.Io as Io
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Strip as Strip
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

coreDecodeApplicationType :: (Core.Term -> Compute.Flow Graph.Graph Core.ApplicationType)
coreDecodeApplicationType = (matchRecord (\m -> Flows.map2 (getField m (Core.Name "function") coreDecodeType) (getField m (Core.Name "argument") coreDecodeType) (\function -> \argument -> Core.ApplicationType {
  Core.applicationTypeFunction = function,
  Core.applicationTypeArgument = argument})))

coreDecodeFieldType :: (Core.Term -> Compute.Flow Graph.Graph Core.FieldType)
coreDecodeFieldType = (matchRecord (\m -> Flows.map2 (getField m (Core.Name "name") coreDecodeName) (getField m (Core.Name "type") coreDecodeType) (\name -> \typ -> Core.FieldType {
  Core.fieldTypeName = name,
  Core.fieldTypeType = typ})))

coreDecodeFieldTypes :: (Core.Term -> Compute.Flow Graph.Graph [Core.FieldType])
coreDecodeFieldTypes term =  
  let stripped = (Strip.fullyStripTerm term)
  in ((\x -> case x of
    Core.TermList v1 -> (Flows_.mapList coreDecodeFieldType v1)
    _ -> (Errors.unexpected "list" (Io.showTerm term))) stripped)

coreDecodeFloatType :: (Core.Term -> Compute.Flow Graph.Graph Core.FloatType)
coreDecodeFloatType = (matchEnum (Core.Name "hydra.core.FloatType") [
  (Core.Name "bigfloat", Core.FloatTypeBigfloat),
  (Core.Name "float32", Core.FloatTypeFloat32),
  (Core.Name "float64", Core.FloatTypeFloat64)])

coreDecodeForallType :: (Core.Term -> Compute.Flow Graph.Graph Core.ForallType)
coreDecodeForallType = (matchRecord (\m -> Flows.map2 (getField m (Core.Name "parameter") coreDecodeName) (getField m (Core.Name "body") coreDecodeType) (\parameter -> \body -> Core.ForallType {
  Core.forallTypeParameter = parameter,
  Core.forallTypeBody = body})))

coreDecodeFunctionType :: (Core.Term -> Compute.Flow Graph.Graph Core.FunctionType)
coreDecodeFunctionType = (matchRecord (\m -> Flows.map2 (getField m (Core.Name "domain") coreDecodeType) (getField m (Core.Name "codomain") coreDecodeType) (\domain -> \codomain -> Core.FunctionType {
  Core.functionTypeDomain = domain,
  Core.functionTypeCodomain = codomain})))

coreDecodeIntegerType :: (Core.Term -> Compute.Flow Graph.Graph Core.IntegerType)
coreDecodeIntegerType = (matchEnum (Core.Name "hydra.core.IntegerType") [
  (Core.Name "bigint", Core.IntegerTypeBigint),
  (Core.Name "int8", Core.IntegerTypeInt8),
  (Core.Name "int16", Core.IntegerTypeInt16),
  (Core.Name "int32", Core.IntegerTypeInt32),
  (Core.Name "int64", Core.IntegerTypeInt64),
  (Core.Name "uint8", Core.IntegerTypeUint8),
  (Core.Name "uint16", Core.IntegerTypeUint16),
  (Core.Name "uint32", Core.IntegerTypeUint32),
  (Core.Name "uint64", Core.IntegerTypeUint64)])

coreDecodeLiteralType :: (Core.Term -> Compute.Flow Graph.Graph Core.LiteralType)
coreDecodeLiteralType = (matchUnion (Core.Name "hydra.core.LiteralType") [
  matchUnitField (Core.Name "binary") Core.LiteralTypeBinary,
  matchUnitField (Core.Name "boolean") Core.LiteralTypeBoolean,
  (Core.Name "float", (\ft -> Flows_.map (\x -> Core.LiteralTypeFloat x) (coreDecodeFloatType ft))),
  (Core.Name "integer", (\it -> Flows_.map (\x -> Core.LiteralTypeInteger x) (coreDecodeIntegerType it))),
  (matchUnitField (Core.Name "string") Core.LiteralTypeString)])

coreDecodeMapType :: (Core.Term -> Compute.Flow Graph.Graph Core.MapType)
coreDecodeMapType = (matchRecord (\m -> Flows.map2 (getField m (Core.Name "keys") coreDecodeType) (getField m (Core.Name "values") coreDecodeType) (\keys -> \values -> Core.MapType {
  Core.mapTypeKeys = keys,
  Core.mapTypeValues = values})))

coreDecodeName :: (Core.Term -> Compute.Flow Graph.Graph Core.Name)
coreDecodeName term = (Flows_.map (\x -> Core.Name x) (Flows_.bind (Expect.wrap (Core.Name "hydra.core.Name") term) Expect.string))

coreDecodeRowType :: (Core.Term -> Compute.Flow Graph.Graph Core.RowType)
coreDecodeRowType = (matchRecord (\m -> Flows.map2 (getField m (Core.Name "typeName") coreDecodeName) (getField m (Core.Name "fields") coreDecodeFieldTypes) (\typeName -> \fields -> Core.RowType {
  Core.rowTypeTypeName = typeName,
  Core.rowTypeFields = fields})))

coreDecodeString :: (Core.Term -> Compute.Flow Graph.Graph String)
coreDecodeString term = (Expect.string (Strip.fullyStripTerm term))

coreDecodeType :: (Core.Term -> Compute.Flow Graph.Graph Core.Type)
coreDecodeType dat = ((\x -> case x of
  Core.TermAnnotated v1 -> (Flows_.map (\t -> Core.TypeAnnotated (Core.AnnotatedType {
    Core.annotatedTypeSubject = t,
    Core.annotatedTypeAnnotation = (Core.annotatedTermAnnotation v1)})) (coreDecodeType (Core.annotatedTermSubject v1)))
  Core.TermTyped v1 -> (coreDecodeType (Core.typedTermTerm v1))
  _ -> (matchUnion (Core.Name "hydra.core.Type") [
    (Core.Name "application", (\at -> Flows_.map (\x -> Core.TypeApplication x) (coreDecodeApplicationType at))),
    (Core.Name "forall", (\ft -> Flows_.map (\x -> Core.TypeForall x) (coreDecodeForallType ft))),
    (Core.Name "function", (\ft -> Flows_.map (\x -> Core.TypeFunction x) (coreDecodeFunctionType ft))),
    (Core.Name "list", (\et -> Flows_.map (\x -> Core.TypeList x) (coreDecodeType et))),
    (Core.Name "literal", (\lt -> Flows_.map (\x -> Core.TypeLiteral x) (coreDecodeLiteralType lt))),
    (Core.Name "map", (\mt -> Flows_.map (\x -> Core.TypeMap x) (coreDecodeMapType mt))),
    (Core.Name "optional", (\et -> Flows_.map (\x -> Core.TypeOptional x) (coreDecodeType et))),
    (Core.Name "product", (\types -> Flows_.map (\x -> Core.TypeProduct x) (Expect.list coreDecodeType types))),
    (Core.Name "record", (\rt -> Flows_.map (\x -> Core.TypeRecord x) (coreDecodeRowType rt))),
    (Core.Name "set", (\et -> Flows_.map (\x -> Core.TypeSet x) (coreDecodeType et))),
    (Core.Name "sum", (\types -> Flows_.map (\x -> Core.TypeSum x) (Expect.list coreDecodeType types))),
    (Core.Name "union", (\rt -> Flows_.map (\x -> Core.TypeUnion x) (coreDecodeRowType rt))),
    (Core.Name "variable", (\n -> Flows_.map (\x -> Core.TypeVariable x) (coreDecodeName n))),
    (Core.Name "wrap", (\wt -> Flows_.map (\x -> Core.TypeWrap x) (coreDecodeWrappedType wt)))] dat)) dat)

coreDecodeTypeScheme :: (Core.Term -> Compute.Flow Graph.Graph Core.TypeScheme)
coreDecodeTypeScheme = (matchRecord (\m -> Flows.map2 (getField m (Core.Name "variables") (Expect.list coreDecodeName)) (getField m (Core.Name "type") coreDecodeType) (\vars -> \body -> Core.TypeScheme {
  Core.typeSchemeVariables = vars,
  Core.typeSchemeType = body})))

coreDecodeWrappedType :: (Core.Term -> Compute.Flow Graph.Graph Core.WrappedType)
coreDecodeWrappedType term = (Flows_.bind (Expect.record (Core.Name "hydra.core.WrappedType") term) (\fields -> Flows.map2 (Expect.field (Core.Name "typeName") coreDecodeName fields) (Expect.field (Core.Name "object") coreDecodeType fields) (\name -> \obj -> Core.WrappedType {
  Core.wrappedTypeTypeName = name,
  Core.wrappedTypeObject = obj})))

getField :: (M.Map Core.Name t2 -> Core.Name -> (t2 -> Compute.Flow t0 t1) -> Compute.Flow t0 t1)
getField m fname decode = (Optionals.maybe (Flows_.fail (Strings.cat [
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
    _ -> (Errors.unexpected "record" (Io.showTerm term))) stripped)

matchUnion :: (Core.Name -> [(Core.Name, (Core.Term -> Compute.Flow Graph.Graph t0))] -> Core.Term -> Compute.Flow Graph.Graph t0)
matchUnion tname pairs term =  
  let stripped = (Strip.fullyStripTerm term) 
      mapping = (Maps.fromList pairs)
  in ((\x -> case x of
    Core.TermVariable v1 -> (Flows_.bind (Lexical.requireElement v1) (\el -> matchUnion tname pairs (Graph.elementTerm el)))
    Core.TermUnion v1 -> (Logic.ifElse (Equality.equalString (Core.unName (Core.injectionTypeName v1)) (Core.unName tname)) ( 
      let fname = (Core.fieldName (Core.injectionField v1)) 
          val = (Core.fieldTerm (Core.injectionField v1))
      in (Optionals.maybe (Flows_.fail (Strings.cat [
        "no matching case for field ",
        (Core.unName fname)])) (\f -> f val) (Maps.lookup fname mapping))) (Errors.unexpected (Strings.cat [
      "injection for type ",
      (Core.unName tname)]) (Io.showTerm term)))
    _ -> (Errors.unexpected (Strings.cat [
      Strings.cat [
        "union with one of {",
        (Strings.intercalate ", " (Lists.map (\pair -> Core.unName (fst pair)) pairs))],
      "}"]) (Io.showTerm stripped))) stripped)

matchUnitField :: (t0 -> t1 -> (t0, (t2 -> Compute.Flow t3 t1)))
matchUnitField fname x = (fname, (\ignored -> Flows_.pure x))
