-- | A DSL for decoding and validating Hydra terms at runtime. This module provides functions to extract typed values from Hydra terms with appropriate error handling.

module Hydra.Expect where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Io as Io
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Strip as Strip
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Extract an arbitrary-precision floating-point value from a term
bigfloat :: (Core.Term -> Compute.Flow Graph.Graph Double)
bigfloat t = (Flows.bind (literal t) (\l -> Flows.bind (floatLiteral l) (\f -> bigfloatValue f)))

bigfloatValue :: (Core.FloatValue -> Compute.Flow t0 Double)
bigfloatValue v = ((\x -> case x of
  Core.FloatValueBigfloat v1 -> (Flows.pure v1)
  _ -> (Errors.unexpected "bigfloat" (Io.showFloat v))) v)

-- | Extract an arbitrary-precision integer value from a term
bigint :: (Core.Term -> Compute.Flow Graph.Graph Integer)
bigint t = (Flows.bind (literal t) (\l -> Flows.bind (integerLiteral l) (\i -> bigintValue i)))

bigintValue :: (Core.IntegerValue -> Compute.Flow t0 Integer)
bigintValue v = ((\x -> case x of
  Core.IntegerValueBigint v1 -> (Flows.pure v1)
  _ -> (Errors.unexpected "bigint" (Io.showInteger v))) v)

-- | Extract a binary data value from a term
binary :: (Core.Term -> Compute.Flow Graph.Graph String)
binary t = (Flows.bind (literal t) binaryLiteral)

binaryLiteral :: (Core.Literal -> Compute.Flow t0 String)
binaryLiteral v = ((\x -> case x of
  Core.LiteralBinary v1 -> (Flows.pure v1)
  _ -> (Errors.unexpected "binary" (Io.showLiteral v))) v)

-- | Extract a boolean value from a term
boolean :: (Core.Term -> Compute.Flow Graph.Graph Bool)
boolean t = (Flows.bind (literal t) booleanLiteral)

booleanLiteral :: (Core.Literal -> Compute.Flow t0 Bool)
booleanLiteral v = ((\x -> case x of
  Core.LiteralBoolean v1 -> (Flows.pure v1)
  _ -> (Errors.unexpected "boolean" (Io.showLiteral v))) v)

-- | Extract a specific case handler from a case statement term
caseField :: (Core.Name -> String -> Core.Term -> Compute.Flow Graph.Graph Core.Field)
caseField name n term =  
  let fieldName = (Core.Name n)
  in (Flows.bind (cases name term) (\cs ->  
    let matching = (Lists.filter (\f -> Equality.equalString (Core.unName (Core.fieldName f)) (Core.unName fieldName)) (Core.caseStatementCases cs))
    in (Logic.ifElse (Lists.null matching) (Flows.fail "not enough cases") (Flows.pure (Lists.head matching)))))

-- | Extract case statement from a term
cases :: (Core.Name -> Core.Term -> Compute.Flow Graph.Graph Core.CaseStatement)
cases name term0 = (Flows.bind (Lexical.stripAndDereferenceTerm term0) (\term -> (\x -> case x of
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionElimination v2 -> ((\x -> case x of
      Core.EliminationUnion v3 -> (Logic.ifElse (Equality.equalString (Core.unName (Core.caseStatementTypeName v3)) (Core.unName name)) (Flows.pure v3) (Errors.unexpected (Strings.cat [
        "case statement for type ",
        (Core.unName name)]) (Io.showTerm term)))
      _ -> (Errors.unexpected "case statement" (Io.showTerm term))) v2)
    _ -> (Errors.unexpected "case statement" (Io.showTerm term))) v1)
  _ -> (Errors.unexpected "case statement" (Io.showTerm term))) term))

-- | Extract a comparison from a term
comparison :: (Core.Term -> Compute.Flow Graph.Graph Graph.Comparison)
comparison term = (Flows.bind (unitVariant (Core.Name "hydra.graph.Comparison") term) (\fname -> Logic.ifElse (Equality.equalString (Core.unName fname) "equalTo") (Flows.pure Graph.ComparisonEqualTo) (Logic.ifElse (Equality.equalString (Core.unName fname) "lessThan") (Flows.pure Graph.ComparisonLessThan) (Logic.ifElse (Equality.equalString (Core.unName fname) "greaterThan") (Flows.pure Graph.ComparisonGreaterThan) (Errors.unexpected "comparison" (Core.unName fname))))))

field :: (Core.Name -> (Core.Term -> Compute.Flow Graph.Graph t0) -> [Core.Field] -> Compute.Flow Graph.Graph t0)
field fname mapping fields =  
  let matchingFields = (Lists.filter (\f -> Equality.equalString (Core.unName (Core.fieldName f)) (Core.unName fname)) fields)
  in (Logic.ifElse (Lists.null matchingFields) (Flows.fail (Strings.cat [
    Strings.cat [
      "field ",
      (Core.unName fname)],
    " not found"])) (Logic.ifElse (Equality.equalInt32 (Lists.length matchingFields) 1) (Flows.bind (Lexical.stripAndDereferenceTerm (Core.fieldTerm (Lists.head matchingFields))) mapping) (Flows.fail (Strings.cat [
    "multiple fields named ",
    (Core.unName fname)]))))

-- | Extract a 32-bit floating-point value from a term
float32 :: (Core.Term -> Compute.Flow Graph.Graph Float)
float32 t = (Flows.bind (literal t) (\l -> Flows.bind (floatLiteral l) (\f -> float32Value f)))

float32Value :: (Core.FloatValue -> Compute.Flow t0 Float)
float32Value v = ((\x -> case x of
  Core.FloatValueFloat32 v1 -> (Flows.pure v1)
  _ -> (Errors.unexpected "float32" (Io.showFloat v))) v)

-- | Extract a 64-bit floating-point value from a term
float64 :: (Core.Term -> Compute.Flow Graph.Graph Double)
float64 t = (Flows.bind (literal t) (\l -> Flows.bind (floatLiteral l) (\f -> float64Value f)))

float64Value :: (Core.FloatValue -> Compute.Flow t0 Double)
float64Value v = ((\x -> case x of
  Core.FloatValueFloat64 v1 -> (Flows.pure v1)
  _ -> (Errors.unexpected "float64" (Io.showFloat v))) v)

floatLiteral :: (Core.Literal -> Compute.Flow t0 Core.FloatValue)
floatLiteral lit = ((\x -> case x of
  Core.LiteralFloat v1 -> (Flows.pure v1)
  _ -> (Errors.unexpected "floating-point value" (Io.showLiteral lit))) lit)

-- | Extract a float value from a term
floatValue :: (Core.Term -> Compute.Flow Graph.Graph Core.FloatValue)
floatValue t = (Flows.bind (literal t) floatLiteral)

functionType :: (Core.Type -> Compute.Flow t0 Core.FunctionType)
functionType typ =  
  let stripped = (Strip.stripType typ)
  in ((\x -> case x of
    Core.TypeFunction v1 -> (Flows.pure v1)
    _ -> (Errors.unexpected "function type" (Io.showType typ))) stripped)

-- | Extract a field from a union term
injection :: (Core.Name -> Core.Term -> Compute.Flow Graph.Graph Core.Field)
injection expected term0 = (Flows.bind (Lexical.stripAndDereferenceTerm term0) (\term -> (\x -> case x of
  Core.TermUnion v1 -> (Logic.ifElse (Equality.equalString (Core.unName (Core.injectionTypeName v1)) (Core.unName expected)) (Flows.pure (Core.injectionField v1)) (Errors.unexpected (Strings.cat [
    "injection of type ",
    (Core.unName expected)]) (Core.unName (Core.injectionTypeName v1))))
  _ -> (Errors.unexpected "injection" (Io.showTerm term))) term))

-- | Extract a 16-bit signed integer value from a term
int16 :: (Core.Term -> Compute.Flow Graph.Graph I.Int16)
int16 t = (Flows.bind (literal t) (\l -> Flows.bind (integerLiteral l) (\i -> int16Value i)))

int16Value :: (Core.IntegerValue -> Compute.Flow t0 I.Int16)
int16Value v = ((\x -> case x of
  Core.IntegerValueInt16 v1 -> (Flows.pure v1)
  _ -> (Errors.unexpected "int16" (Io.showInteger v))) v)

-- | Extract a 32-bit signed integer value from a term
int32 :: (Core.Term -> Compute.Flow Graph.Graph Int)
int32 t = (Flows.bind (literal t) (\l -> Flows.bind (integerLiteral l) (\i -> int32Value i)))

int32Value :: (Core.IntegerValue -> Compute.Flow t0 Int)
int32Value v = ((\x -> case x of
  Core.IntegerValueInt32 v1 -> (Flows.pure v1)
  _ -> (Errors.unexpected "int32" (Io.showInteger v))) v)

-- | Extract a 64-bit signed integer value from a term
int64 :: (Core.Term -> Compute.Flow Graph.Graph I.Int64)
int64 t = (Flows.bind (literal t) (\l -> Flows.bind (integerLiteral l) (\i -> int64Value i)))

int64Value :: (Core.IntegerValue -> Compute.Flow t0 I.Int64)
int64Value v = ((\x -> case x of
  Core.IntegerValueInt64 v1 -> (Flows.pure v1)
  _ -> (Errors.unexpected "int64" (Io.showInteger v))) v)

-- | Extract an 8-bit signed integer value from a term
int8 :: (Core.Term -> Compute.Flow Graph.Graph I.Int8)
int8 t = (Flows.bind (literal t) (\l -> Flows.bind (integerLiteral l) (\i -> int8Value i)))

int8Value :: (Core.IntegerValue -> Compute.Flow t0 I.Int8)
int8Value v = ((\x -> case x of
  Core.IntegerValueInt8 v1 -> (Flows.pure v1)
  _ -> (Errors.unexpected "int8" (Io.showInteger v))) v)

integerLiteral :: (Core.Literal -> Compute.Flow t0 Core.IntegerValue)
integerLiteral lit = ((\x -> case x of
  Core.LiteralInteger v1 -> (Flows.pure v1)
  _ -> (Errors.unexpected "integer value" (Io.showLiteral lit))) lit)

-- | Extract an integer value from a term
integerValue :: (Core.Term -> Compute.Flow Graph.Graph Core.IntegerValue)
integerValue t = (Flows.bind (literal t) integerLiteral)

-- | Extract the body of a lambda term
lambdaBody :: (Core.Term -> Compute.Flow Graph.Graph Core.Term)
lambdaBody term = (Flows.map Core.lambdaBody (lambda term))

-- | Extract a lambda from a term
lambda :: (Core.Term -> Compute.Flow Graph.Graph Core.Lambda)
lambda term0 = (Flows.bind (Lexical.stripAndDereferenceTerm term0) (\term -> (\x -> case x of
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionLambda v2 -> (Flows.pure v2)
    _ -> (Errors.unexpected "lambda" (Io.showTerm term))) v1)
  _ -> (Errors.unexpected "lambda" (Io.showTerm term))) term))

-- | Extract a binding with the given name from a let term
letBinding :: (String -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
letBinding n term =  
  let name = (Core.Name n)
  in (Flows.bind (letTerm term) (\letExpr ->  
    let matchingBindings = (Lists.filter (\b -> Equality.equalString (Core.unName (Core.letBindingName b)) (Core.unName name)) (Core.letBindings letExpr))
    in (Logic.ifElse (Lists.null matchingBindings) (Flows.fail (Strings.cat [
      "no such binding: ",
      n])) (Logic.ifElse (Equality.equalInt32 (Lists.length matchingBindings) 1) (Flows.pure (Core.letBindingTerm (Lists.head matchingBindings))) (Flows.fail (Strings.cat [
      "multiple bindings named ",
      n]))))))

-- | Extract a let expression from a term
letTerm :: (Core.Term -> Compute.Flow Graph.Graph Core.Let)
letTerm term0 = (Flows.bind (Lexical.stripAndDereferenceTerm term0) (\term -> (\x -> case x of
  Core.TermLet v1 -> (Flows.pure v1)
  _ -> (Errors.unexpected "let term" (Io.showTerm term))) term))

list :: ((Core.Term -> Compute.Flow Graph.Graph t0) -> Core.Term -> Compute.Flow Graph.Graph [t0])
list f term0 = (Flows.bind (Lexical.stripAndDereferenceTerm term0) (\term -> (\x -> case x of
  Core.TermList v1 -> (Flows.mapList f v1)
  _ -> (Errors.unexpected "list" (Io.showTerm term))) term))

-- | Extract the first element of a list term
listHead :: (Core.Term -> Compute.Flow Graph.Graph Core.Term)
listHead term = (Flows.bind (list Flows.pure term) (\l -> Logic.ifElse (Lists.null l) (Flows.fail "empty list") (Flows.pure (Lists.head l))))

listType :: (Core.Type -> Compute.Flow t0 Core.Type)
listType typ =  
  let stripped = (Strip.stripType typ)
  in ((\x -> case x of
    Core.TypeList v1 -> (Flows.pure v1)
    _ -> (Errors.unexpected "list type" (Io.showType typ))) stripped)

-- | Extract a literal value from a term
literal :: (Core.Term -> Compute.Flow Graph.Graph Core.Literal)
literal term0 = (Flows.bind (Lexical.stripAndDereferenceTerm term0) (\term -> (\x -> case x of
  Core.TermLiteral v1 -> (Flows.pure v1)
  _ -> (Errors.unexpected "literal" (Io.showTerm term))) term))

map_ :: (Ord t0) => ((Core.Term -> Compute.Flow Graph.Graph t0) -> (Core.Term -> Compute.Flow Graph.Graph t1) -> Core.Term -> Compute.Flow Graph.Graph (M.Map t0 t1))
map_ fk fv term0 =  
  let pair = (\kvPair ->  
          let kterm = (fst kvPair) 
              vterm = (snd kvPair)
          in (Flows.bind (fk kterm) (\kval -> Flows.bind (fv vterm) (\vval -> Flows.pure (kval, vval)))))
  in (Flows.bind (Lexical.stripAndDereferenceTerm term0) (\term -> (\x -> case x of
    Core.TermMap v1 -> (Flows.map Maps.fromList (Flows.mapList pair (Maps.toList v1)))
    _ -> (Errors.unexpected "map" (Io.showTerm term))) term))

mapType :: (Core.Type -> Compute.Flow t0 Core.MapType)
mapType typ =  
  let stripped = (Strip.stripType typ)
  in ((\x -> case x of
    Core.TypeMap v1 -> (Flows.pure v1)
    _ -> (Errors.unexpected "map type" (Io.showType typ))) stripped)

nArgs :: (Core.Name -> Int -> [t0] -> Compute.Flow t1 ())
nArgs name n args = (Logic.ifElse (Equality.equalInt32 (Lists.length args) n) (Flows.pure ()) (Errors.unexpected (Strings.cat [
  Literals.showInt32 n,
  " arguments to primitive ",
  (Literals.showString (Core.unName name))]) (Literals.showInt32 (Lists.length args))))

optional :: ((Core.Term -> Compute.Flow Graph.Graph t0) -> Core.Term -> Compute.Flow Graph.Graph (Maybe t0))
optional f term0 = (Flows.bind (Lexical.stripAndDereferenceTerm term0) (\term -> (\x -> case x of
  Core.TermOptional v1 -> (Optionals.maybe (Flows.pure Nothing) (\t -> Flows.map Optionals.pure (f t)) v1)
  _ -> (Errors.unexpected "optional value" (Io.showTerm term))) term))

optionalType :: (Core.Type -> Compute.Flow t0 Core.Type)
optionalType typ =  
  let stripped = (Strip.stripType typ)
  in ((\x -> case x of
    Core.TypeOptional v1 -> (Flows.pure v1)
    _ -> (Errors.unexpected "optional type" (Io.showType typ))) stripped)

pair :: ((Core.Term -> Compute.Flow Graph.Graph t0) -> (Core.Term -> Compute.Flow Graph.Graph t1) -> Core.Term -> Compute.Flow Graph.Graph (t0, t1))
pair kf vf term0 = (Flows.bind (Lexical.stripAndDereferenceTerm term0) (\term -> (\x -> case x of
  Core.TermProduct v1 -> (Logic.ifElse (Equality.equalInt32 (Lists.length v1) 2) (Flows.bind (kf (Lists.head v1)) (\kVal -> Flows.bind (vf (Lists.head (Lists.tail v1))) (\vVal -> Flows.pure (kVal, vVal)))) (Errors.unexpected "pair" (Io.showTerm term)))
  _ -> (Errors.unexpected "product" (Io.showTerm term))) term))

productType :: (Core.Type -> Compute.Flow t0 [Core.Type])
productType typ =  
  let stripped = (Strip.stripType typ)
  in ((\x -> case x of
    Core.TypeProduct v1 -> (Flows.pure v1)
    _ -> (Errors.unexpected "product type" (Io.showType typ))) stripped)

-- | Extract a record's fields from a term
record :: (Core.Name -> Core.Term -> Compute.Flow Graph.Graph [Core.Field])
record expected term0 = (Flows.bind (Lexical.stripAndDereferenceTerm term0) (\term -> (\x -> case x of
  Core.TermRecord v1 -> (Logic.ifElse (Equality.equalString (Core.unName (Core.recordTypeName v1)) (Core.unName expected)) (Flows.pure (Core.recordFields v1)) (Errors.unexpected (Strings.cat [
    "record of type ",
    (Core.unName expected)]) (Core.unName (Core.recordTypeName v1))))
  _ -> (Errors.unexpected "record" (Io.showTerm term))) term))

recordType :: (Core.Name -> Core.Type -> Compute.Flow t0 [Core.FieldType])
recordType ename typ =  
  let stripped = (Strip.stripType typ)
  in ((\x -> case x of
    Core.TypeRecord v1 -> (Logic.ifElse (Equality.equalString (Core.unName (Core.rowTypeTypeName v1)) (Core.unName ename)) (Flows.pure (Core.rowTypeFields v1)) (Errors.unexpected (Strings.cat [
      "record of type ",
      (Core.unName ename)]) (Strings.cat [
      "record of type ",
      (Core.unName (Core.rowTypeTypeName v1))])))
    _ -> (Errors.unexpected "record type" (Io.showType typ))) stripped)

set :: (Ord t0) => ((Core.Term -> Compute.Flow Graph.Graph t0) -> Core.Term -> Compute.Flow Graph.Graph (S.Set t0))
set f term0 = (Flows.bind (Lexical.stripAndDereferenceTerm term0) (\term -> (\x -> case x of
  Core.TermSet v1 -> (Flows.map Sets.fromList (Flows.mapList f (Sets.toList v1)))
  _ -> (Errors.unexpected "set" (Io.showTerm term))) term))

setType :: (Core.Type -> Compute.Flow t0 Core.Type)
setType typ =  
  let stripped = (Strip.stripType typ)
  in ((\x -> case x of
    Core.TypeSet v1 -> (Flows.pure v1)
    _ -> (Errors.unexpected "set type" (Io.showType typ))) stripped)

-- | Extract a string value from a term
string :: (Core.Term -> Compute.Flow Graph.Graph String)
string t = (Flows.bind (literal t) stringLiteral)

stringLiteral :: (Core.Literal -> Compute.Flow t0 String)
stringLiteral v = ((\x -> case x of
  Core.LiteralString v1 -> (Flows.pure v1)
  _ -> (Errors.unexpected "string" (Io.showLiteral v))) v)

sumType :: (Core.Type -> Compute.Flow t0 [Core.Type])
sumType typ =  
  let stripped = (Strip.stripType typ)
  in ((\x -> case x of
    Core.TypeSum v1 -> (Flows.pure v1)
    _ -> (Errors.unexpected "sum type" (Io.showType typ))) stripped)

-- | Extract a 16-bit unsigned integer value from a term
uint16 :: (Core.Term -> Compute.Flow Graph.Graph Int)
uint16 t = (Flows.bind (literal t) (\l -> Flows.bind (integerLiteral l) (\i -> uint16Value i)))

uint16Value :: (Core.IntegerValue -> Compute.Flow t0 Int)
uint16Value v = ((\x -> case x of
  Core.IntegerValueUint16 v1 -> (Flows.pure v1)
  _ -> (Errors.unexpected "uint16" (Io.showInteger v))) v)

-- | Extract a 32-bit unsigned integer value from a term
uint32 :: (Core.Term -> Compute.Flow Graph.Graph I.Int64)
uint32 t = (Flows.bind (literal t) (\l -> Flows.bind (integerLiteral l) (\i -> uint32Value i)))

uint32Value :: (Core.IntegerValue -> Compute.Flow t0 I.Int64)
uint32Value v = ((\x -> case x of
  Core.IntegerValueUint32 v1 -> (Flows.pure v1)
  _ -> (Errors.unexpected "uint32" (Io.showInteger v))) v)

-- | Extract a 64-bit unsigned integer value from a term
uint64 :: (Core.Term -> Compute.Flow Graph.Graph Integer)
uint64 t = (Flows.bind (literal t) (\l -> Flows.bind (integerLiteral l) (\i -> uint64Value i)))

uint64Value :: (Core.IntegerValue -> Compute.Flow t0 Integer)
uint64Value v = ((\x -> case x of
  Core.IntegerValueUint64 v1 -> (Flows.pure v1)
  _ -> (Errors.unexpected "uint64" (Io.showInteger v))) v)

-- | Extract an 8-bit unsigned integer value from a term
uint8 :: (Core.Term -> Compute.Flow Graph.Graph I.Int16)
uint8 t = (Flows.bind (literal t) (\l -> Flows.bind (integerLiteral l) (\i -> uint8Value i)))

uint8Value :: (Core.IntegerValue -> Compute.Flow t0 I.Int16)
uint8Value v = ((\x -> case x of
  Core.IntegerValueUint8 v1 -> (Flows.pure v1)
  _ -> (Errors.unexpected "uint8" (Io.showInteger v))) v)

unionType :: (Core.Name -> Core.Type -> Compute.Flow t0 [Core.FieldType])
unionType ename typ =  
  let stripped = (Strip.stripType typ)
  in ((\x -> case x of
    Core.TypeUnion v1 -> (Logic.ifElse (Equality.equalString (Core.unName (Core.rowTypeTypeName v1)) (Core.unName ename)) (Flows.pure (Core.rowTypeFields v1)) (Errors.unexpected (Strings.cat [
      "union of type ",
      (Core.unName ename)]) (Strings.cat [
      "union of type ",
      (Core.unName (Core.rowTypeTypeName v1))])))
    _ -> (Errors.unexpected "union type" (Io.showType typ))) stripped)

-- | Extract a unit value (empty record) from a term
unit :: (Core.Term -> Compute.Flow Graph.Graph ())
unit term0 = (Flows.bind (record (Core.Name "hydra.core.Unit") term0) (\fields -> Logic.ifElse (Lists.null fields) (Flows.pure ()) (Errors.unexpected "unit" (Io.showTerm term0))))

-- | Extract a unit variant (a variant with an empty record value) from a union term
unitVariant :: (Core.Name -> Core.Term -> Compute.Flow Graph.Graph Core.Name)
unitVariant tname term = (Flows.bind (variant tname term) (\field -> Flows.bind (unit (Core.fieldTerm field)) (\ignored -> Flows.pure (Core.fieldName field))))

-- | Extract a field from a union term (alias for injection)
variant :: (Core.Name -> Core.Term -> Compute.Flow Graph.Graph Core.Field)
variant = injection

-- | Extract the wrapped value from a wrapped term
wrap :: (Core.Name -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
wrap expected term0 = (Flows.bind (Lexical.stripAndDereferenceTerm term0) (\term -> (\x -> case x of
  Core.TermWrap v1 -> (Logic.ifElse (Equality.equalString (Core.unName (Core.wrappedTermTypeName v1)) (Core.unName expected)) (Flows.pure (Core.wrappedTermObject v1)) (Errors.unexpected (Strings.cat [
    "wrapper of type ",
    (Core.unName expected)]) (Core.unName (Core.wrappedTermTypeName v1))))
  _ -> (Errors.unexpected (Strings.cat [
    Strings.cat [
      "wrap(",
      (Core.unName expected)],
    ")"]) (Io.showTerm term))) term))

wrappedType :: (Core.Name -> Core.Type -> Compute.Flow t0 Core.Type)
wrappedType ename typ =  
  let stripped = (Strip.stripType typ)
  in ((\x -> case x of
    Core.TypeWrap v1 -> (Logic.ifElse (Equality.equalString (Core.unName (Core.wrappedTypeTypeName v1)) (Core.unName ename)) (Flows.pure (Core.wrappedTypeObject v1)) (Errors.unexpected (Strings.cat [
      "wrapped type ",
      (Core.unName ename)]) (Strings.cat [
      "wrapped type ",
      (Core.unName (Core.wrappedTypeTypeName v1))])))
    _ -> (Errors.unexpected "wrapped type" (Io.showType typ))) stripped)
