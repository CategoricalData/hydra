-- Note: this is an automatically generated file. Do not edit.

-- | Extraction and validation for hydra.core types

module Hydra.Extract.Core where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Show.Core as Core_
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Extract an arbitrary-precision floating-point value from a term
bigfloat :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Double
bigfloat cx graph t = Eithers.bind (literal cx graph t) (\l -> Eithers.bind (floatLiteral cx l) (\f -> bigfloatValue cx f))

-- | Extract a bigfloat value from a FloatValue
bigfloatValue :: Context.Context -> Core.FloatValue -> Either (Context.InContext Error.Error) Double
bigfloatValue cx v =
    case v of
      Core.FloatValueBigfloat v0 -> Right v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "bigfloat") " but found ") (Core_.float v)))),
        Context.inContextContext = cx})

-- | Extract an arbitrary-precision integer value from a term
bigint :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Integer
bigint cx graph t = Eithers.bind (literal cx graph t) (\l -> Eithers.bind (integerLiteral cx l) (\i -> bigintValue cx i))

-- | Extract a bigint value from an IntegerValue
bigintValue :: Context.Context -> Core.IntegerValue -> Either (Context.InContext Error.Error) Integer
bigintValue cx v =
    case v of
      Core.IntegerValueBigint v0 -> Right v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "bigint") " but found ") (Core_.integer v)))),
        Context.inContextContext = cx})

-- | Extract a binary data value from a term
binary :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) B.ByteString
binary cx graph t = Eithers.bind (literal cx graph t) (\l -> binaryLiteral cx l)

-- | Extract a binary literal from a Literal value
binaryLiteral :: Context.Context -> Core.Literal -> Either (Context.InContext Error.Error) B.ByteString
binaryLiteral cx v =
    case v of
      Core.LiteralBinary v0 -> Right v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "binary") " but found ") (Core_.literal v)))),
        Context.inContextContext = cx})

-- | Extract a boolean value from a term
boolean :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Bool
boolean cx graph t = Eithers.bind (literal cx graph t) (\l -> booleanLiteral cx l)

-- | Extract a boolean literal from a Literal value
booleanLiteral :: Context.Context -> Core.Literal -> Either (Context.InContext Error.Error) Bool
booleanLiteral cx v =
    case v of
      Core.LiteralBoolean v0 -> Right v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "boolean") " but found ") (Core_.literal v)))),
        Context.inContextContext = cx})

-- | Extract a specific case handler from a case statement term
caseField :: Context.Context -> Core.Name -> String -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Core.Field
caseField cx name n graph term =
     
      let fieldName = Core.Name n
      in (Eithers.bind (cases cx name graph term) (\cs ->  
        let matching =
                Lists.filter (\f -> Equality.equal (Core.unName (Core.fieldName f)) (Core.unName fieldName)) (Core.caseStatementCases cs)
        in (Logic.ifElse (Lists.null matching) (Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError "not enough cases")),
          Context.inContextContext = cx})) (Right (Lists.head matching)))))

-- | Extract case statement from a term
cases :: Context.Context -> Core.Name -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Core.CaseStatement
cases cx name graph term0 =
    Eithers.bind (Lexical.stripAndDereferenceTerm cx graph term0) (\term -> case term of
      Core.TermFunction v0 -> case v0 of
        Core.FunctionElimination v1 -> case v1 of
          Core.EliminationUnion v2 -> Logic.ifElse (Equality.equal (Core.unName (Core.caseStatementTypeName v2)) (Core.unName name)) (Right v2) (Left (Context.InContext {
            Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " (Strings.cat2 "case statement for type " (Core.unName name))) " but found ") (Core_.term term)))),
            Context.inContextContext = cx}))
          _ -> Left (Context.InContext {
            Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "case statement") " but found ") (Core_.term term)))),
            Context.inContextContext = cx})
        _ -> Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "case statement") " but found ") (Core_.term term)))),
          Context.inContextContext = cx})
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "case statement") " but found ") (Core_.term term)))),
        Context.inContextContext = cx}))

-- | Extract a field value from a list of fields
field :: Context.Context -> Core.Name -> (Core.Term -> Either (Context.InContext Error.Error) t0) -> Graph.Graph -> [Core.Field] -> Either (Context.InContext Error.Error) t0
field cx fname mapping graph fields =
     
      let matchingFields = Lists.filter (\f -> Equality.equal (Core.unName (Core.fieldName f)) (Core.unName fname)) fields
      in (Logic.ifElse (Lists.null matchingFields) (Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " (Strings.cat2 "field " (Core.unName fname))) " but found ") "no matching field"))),
        Context.inContextContext = cx})) (Logic.ifElse (Equality.equal (Lists.length matchingFields) 1) (Eithers.bind (Lexical.stripAndDereferenceTerm cx graph (Core.fieldTerm (Lists.head matchingFields))) (\stripped -> mapping stripped)) (Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "single field") " but found ") (Strings.cat2 "multiple fields named " (Core.unName fname))))),
        Context.inContextContext = cx}))))

-- | Extract a 32-bit floating-point value from a term
float32 :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Float
float32 cx graph t = Eithers.bind (literal cx graph t) (\l -> Eithers.bind (floatLiteral cx l) (\f -> float32Value cx f))

-- | Extract a float32 value from a FloatValue
float32Value :: Context.Context -> Core.FloatValue -> Either (Context.InContext Error.Error) Float
float32Value cx v =
    case v of
      Core.FloatValueFloat32 v0 -> Right v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "float32") " but found ") (Core_.float v)))),
        Context.inContextContext = cx})

-- | Extract a 64-bit floating-point value from a term
float64 :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Double
float64 cx graph t = Eithers.bind (literal cx graph t) (\l -> Eithers.bind (floatLiteral cx l) (\f -> float64Value cx f))

-- | Extract a float64 value from a FloatValue
float64Value :: Context.Context -> Core.FloatValue -> Either (Context.InContext Error.Error) Double
float64Value cx v =
    case v of
      Core.FloatValueFloat64 v0 -> Right v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "float64") " but found ") (Core_.float v)))),
        Context.inContextContext = cx})

-- | Extract a floating-point literal from a Literal value
floatLiteral :: Context.Context -> Core.Literal -> Either (Context.InContext Error.Error) Core.FloatValue
floatLiteral cx lit =
    case lit of
      Core.LiteralFloat v0 -> Right v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "floating-point value") " but found ") (Core_.literal lit)))),
        Context.inContextContext = cx})

-- | Extract a float value from a term
floatValue :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Core.FloatValue
floatValue cx graph t = Eithers.bind (literal cx graph t) (\l -> floatLiteral cx l)

-- | Extract an either value from a term, applying functions to the left and right values
eitherTerm :: Context.Context -> (Core.Term -> Either (Context.InContext Error.Error) t0) -> (Core.Term -> Either (Context.InContext Error.Error) t1) -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) (Either t0 t1)
eitherTerm cx leftFun rightFun graph term0 =
    Eithers.bind (Lexical.stripAndDereferenceTerm cx graph term0) (\term -> case term of
      Core.TermEither v0 -> Eithers.either (\l -> Eithers.map (\x -> Left x) (leftFun l)) (\r -> Eithers.map (\x -> Right x) (rightFun r)) v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "either value") " but found ") (Core_.term term)))),
        Context.inContextContext = cx}))

-- | Extract the left and right types from an either type
eitherType :: Context.Context -> Core.Type -> Either (Context.InContext Error.Error) Core.EitherType
eitherType cx typ =
     
      let stripped = Rewriting.deannotateType typ
      in case stripped of
        Core.TypeEither v0 -> Right v0
        _ -> Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "either type") " but found ") (Core_.type_ typ)))),
          Context.inContextContext = cx})

-- | Extract a function type from a type
functionType :: Context.Context -> Core.Type -> Either (Context.InContext Error.Error) Core.FunctionType
functionType cx typ =
     
      let stripped = Rewriting.deannotateType typ
      in case stripped of
        Core.TypeFunction v0 -> Right v0
        _ -> Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "function type") " but found ") (Core_.type_ typ)))),
          Context.inContextContext = cx})

-- | Extract a field from a union term
injection :: Context.Context -> Core.Name -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Core.Field
injection cx expected graph term0 =
    Eithers.bind (Lexical.stripAndDereferenceTerm cx graph term0) (\term -> case term of
      Core.TermUnion v0 -> Logic.ifElse (Equality.equal (Core.unName (Core.injectionTypeName v0)) (Core.unName expected)) (Right (Core.injectionField v0)) (Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " (Strings.cat2 "injection of type " (Core.unName expected))) " but found ") (Core.unName (Core.injectionTypeName v0))))),
        Context.inContextContext = cx}))
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "injection") " but found ") (Core_.term term)))),
        Context.inContextContext = cx}))

-- | Extract a 16-bit signed integer value from a term
int16 :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) I.Int16
int16 cx graph t = Eithers.bind (literal cx graph t) (\l -> Eithers.bind (integerLiteral cx l) (\i -> int16Value cx i))

-- | Extract an int16 value from an IntegerValue
int16Value :: Context.Context -> Core.IntegerValue -> Either (Context.InContext Error.Error) I.Int16
int16Value cx v =
    case v of
      Core.IntegerValueInt16 v0 -> Right v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "int16") " but found ") (Core_.integer v)))),
        Context.inContextContext = cx})

-- | Extract a 32-bit signed integer value from a term
int32 :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Int
int32 cx graph t = Eithers.bind (literal cx graph t) (\l -> Eithers.bind (integerLiteral cx l) (\i -> int32Value cx i))

-- | Extract an int32 value from an IntegerValue
int32Value :: Context.Context -> Core.IntegerValue -> Either (Context.InContext Error.Error) Int
int32Value cx v =
    case v of
      Core.IntegerValueInt32 v0 -> Right v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "int32") " but found ") (Core_.integer v)))),
        Context.inContextContext = cx})

-- | Extract a 64-bit signed integer value from a term
int64 :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) I.Int64
int64 cx graph t = Eithers.bind (literal cx graph t) (\l -> Eithers.bind (integerLiteral cx l) (\i -> int64Value cx i))

-- | Extract an int64 value from an IntegerValue
int64Value :: Context.Context -> Core.IntegerValue -> Either (Context.InContext Error.Error) I.Int64
int64Value cx v =
    case v of
      Core.IntegerValueInt64 v0 -> Right v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "int64") " but found ") (Core_.integer v)))),
        Context.inContextContext = cx})

-- | Extract an 8-bit signed integer value from a term
int8 :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) I.Int8
int8 cx graph t = Eithers.bind (literal cx graph t) (\l -> Eithers.bind (integerLiteral cx l) (\i -> int8Value cx i))

-- | Extract an int8 value from an IntegerValue
int8Value :: Context.Context -> Core.IntegerValue -> Either (Context.InContext Error.Error) I.Int8
int8Value cx v =
    case v of
      Core.IntegerValueInt8 v0 -> Right v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "int8") " but found ") (Core_.integer v)))),
        Context.inContextContext = cx})

-- | Extract an integer literal from a Literal value
integerLiteral :: Context.Context -> Core.Literal -> Either (Context.InContext Error.Error) Core.IntegerValue
integerLiteral cx lit =
    case lit of
      Core.LiteralInteger v0 -> Right v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "integer value") " but found ") (Core_.literal lit)))),
        Context.inContextContext = cx})

-- | Extract an integer value from a term
integerValue :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Core.IntegerValue
integerValue cx graph t = Eithers.bind (literal cx graph t) (\l -> integerLiteral cx l)

-- | Extract the body of a lambda term
lambdaBody :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Core.Term
lambdaBody cx graph term = Eithers.map Core.lambdaBody (lambda cx graph term)

-- | Extract a lambda from a term
lambda :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Core.Lambda
lambda cx graph term0 =
    Eithers.bind (Lexical.stripAndDereferenceTerm cx graph term0) (\term -> case term of
      Core.TermFunction v0 -> case v0 of
        Core.FunctionLambda v1 -> Right v1
        _ -> Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "lambda") " but found ") (Core_.term term)))),
          Context.inContextContext = cx})
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "lambda") " but found ") (Core_.term term)))),
        Context.inContextContext = cx}))

-- | Extract a binding with the given name from a let term
letBinding :: Context.Context -> String -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Core.Term
letBinding cx n graph term =
     
      let name = Core.Name n
      in (Eithers.bind (let_ cx graph term) (\letExpr ->  
        let matchingBindings =
                Lists.filter (\b -> Equality.equal (Core.unName (Core.bindingName b)) (Core.unName name)) (Core.letBindings letExpr)
        in (Logic.ifElse (Lists.null matchingBindings) (Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "no such binding: " n))),
          Context.inContextContext = cx})) (Logic.ifElse (Equality.equal (Lists.length matchingBindings) 1) (Right (Core.bindingTerm (Lists.head matchingBindings))) (Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "multiple bindings named " n))),
          Context.inContextContext = cx}))))))

-- | Extract a let expression from a term
let_ :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Core.Let
let_ cx graph term0 =
    Eithers.bind (Lexical.stripAndDereferenceTerm cx graph term0) (\term -> case term of
      Core.TermLet v0 -> Right v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "let term") " but found ") (Core_.term term)))),
        Context.inContextContext = cx}))

-- | Extract a list of terms from a term
list :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) [Core.Term]
list cx graph term =
    Eithers.bind (Lexical.stripAndDereferenceTerm cx graph term) (\stripped -> case stripped of
      Core.TermList v0 -> Right v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "list") " but found ") (Core_.term stripped)))),
        Context.inContextContext = cx}))

-- | Extract the first element of a list term
listHead :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Core.Term
listHead cx graph term =
    Eithers.bind (list cx graph term) (\l -> Logic.ifElse (Lists.null l) (Left (Context.InContext {
      Context.inContextObject = (Error.ErrorOther (Error.OtherError "empty list")),
      Context.inContextContext = cx})) (Right (Lists.head l)))

-- | Extract a list of values from a term, mapping a function over each element
listOf :: Context.Context -> (Core.Term -> Either (Context.InContext Error.Error) t0) -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) [t0]
listOf cx f graph term = Eithers.bind (list cx graph term) (\els -> Eithers.mapList f els)

-- | Extract the element type from a list type
listType :: Context.Context -> Core.Type -> Either (Context.InContext Error.Error) Core.Type
listType cx typ =
     
      let stripped = Rewriting.deannotateType typ
      in case stripped of
        Core.TypeList v0 -> Right v0
        _ -> Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "list type") " but found ") (Core_.type_ typ)))),
          Context.inContextContext = cx})

-- | Extract a literal value from a term
literal :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Core.Literal
literal cx graph term0 =
    Eithers.bind (Lexical.stripAndDereferenceTerm cx graph term0) (\term -> case term of
      Core.TermLiteral v0 -> Right v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "literal") " but found ") (Core_.term term)))),
        Context.inContextContext = cx}))

-- | Extract a map of key-value pairs from a term, mapping functions over each key and value
map :: Ord t0 => (Context.Context -> (Core.Term -> Either (Context.InContext Error.Error) t0) -> (Core.Term -> Either (Context.InContext Error.Error) t1) -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) (M.Map t0 t1))
map cx fk fv graph term0 =
     
      let pair =
              \kvPair ->  
                let kterm = Pairs.first kvPair 
                    vterm = Pairs.second kvPair
                in (Eithers.bind (fk kterm) (\kval -> Eithers.bind (fv vterm) (\vval -> Right (kval, vval))))
      in (Eithers.bind (Lexical.stripAndDereferenceTerm cx graph term0) (\term -> case term of
        Core.TermMap v0 -> Eithers.map Maps.fromList (Eithers.mapList pair (Maps.toList v0))
        _ -> Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "map") " but found ") (Core_.term term)))),
          Context.inContextContext = cx})))

-- | Extract the key and value types from a map type
mapType :: Context.Context -> Core.Type -> Either (Context.InContext Error.Error) Core.MapType
mapType cx typ =
     
      let stripped = Rewriting.deannotateType typ
      in case stripped of
        Core.TypeMap v0 -> Right v0
        _ -> Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "map type") " but found ") (Core_.type_ typ)))),
          Context.inContextContext = cx})

-- | Ensure a function has the expected number of arguments
nArgs :: Context.Context -> Core.Name -> Int -> [t0] -> Either (Context.InContext Error.Error) ()
nArgs cx name n args =
    Logic.ifElse (Equality.equal (Lists.length args) n) (Right ()) (Left (Context.InContext {
      Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " (Strings.cat [
        Literals.showInt32 n,
        " arguments to primitive ",
        (Literals.showString (Core.unName name))])) " but found ") (Literals.showInt32 (Lists.length args))))),
      Context.inContextContext = cx}))

-- | Extract an optional value from a term, applying a function to the value if present
maybeTerm :: Context.Context -> (Core.Term -> Either (Context.InContext Error.Error) t0) -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) (Maybe t0)
maybeTerm cx f graph term0 =
    Eithers.bind (Lexical.stripAndDereferenceTerm cx graph term0) (\term -> case term of
      Core.TermMaybe v0 -> Maybes.maybe (Right Nothing) (\t -> Eithers.map Maybes.pure (f t)) v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "maybe value") " but found ") (Core_.term term)))),
        Context.inContextContext = cx}))

-- | Extract the base type from an optional type
maybeType :: Context.Context -> Core.Type -> Either (Context.InContext Error.Error) Core.Type
maybeType cx typ =
     
      let stripped = Rewriting.deannotateType typ
      in case stripped of
        Core.TypeMaybe v0 -> Right v0
        _ -> Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "maybe type") " but found ") (Core_.type_ typ)))),
          Context.inContextContext = cx})

-- | Extract a pair of values from a term, applying functions to each component
pair :: Context.Context -> (Core.Term -> Either (Context.InContext Error.Error) t0) -> (Core.Term -> Either (Context.InContext Error.Error) t1) -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) (t0, t1)
pair cx kf vf graph term0 =
    Eithers.bind (Lexical.stripAndDereferenceTerm cx graph term0) (\term -> case term of
      Core.TermPair v0 -> Eithers.bind (kf (Pairs.first v0)) (\kVal -> Eithers.bind (vf (Pairs.second v0)) (\vVal -> Right (kVal, vVal)))
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "pair") " but found ") (Core_.term term)))),
        Context.inContextContext = cx}))

-- | Extract a record's fields from a term
record :: Context.Context -> Core.Name -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) [Core.Field]
record cx expected graph term0 =
    Eithers.bind (termRecord cx graph term0) (\record -> Logic.ifElse (Equality.equal (Core.recordTypeName record) expected) (Right (Core.recordFields record)) (Left (Context.InContext {
      Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " (Strings.cat2 "record of type " (Core.unName expected))) " but found ") (Core.unName (Core.recordTypeName record))))),
      Context.inContextContext = cx})))

-- | Extract the field types from a record type
recordType :: Context.Context -> t0 -> Core.Type -> Either (Context.InContext Error.Error) [Core.FieldType]
recordType cx ename typ =
     
      let stripped = Rewriting.deannotateType typ
      in case stripped of
        Core.TypeRecord v0 -> Right v0
        _ -> Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "record type") " but found ") (Core_.type_ typ)))),
          Context.inContextContext = cx})

-- | Extract a set of terms from a term
set :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) (S.Set Core.Term)
set cx graph term =
    Eithers.bind (Lexical.stripAndDereferenceTerm cx graph term) (\stripped -> case stripped of
      Core.TermSet v0 -> Right v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "set") " but found ") (Core_.term stripped)))),
        Context.inContextContext = cx}))

-- | Extract a set of values from a term, mapping a function over each element
setOf :: Ord t0 => (Context.Context -> (Core.Term -> Either (Context.InContext Error.Error) t0) -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) (S.Set t0))
setOf cx f graph term = Eithers.bind (set cx graph term) (\els -> Eithers.mapSet f els)

-- | Extract the element type from a set type
setType :: Context.Context -> Core.Type -> Either (Context.InContext Error.Error) Core.Type
setType cx typ =
     
      let stripped = Rewriting.deannotateType typ
      in case stripped of
        Core.TypeSet v0 -> Right v0
        _ -> Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "set type") " but found ") (Core_.type_ typ)))),
          Context.inContextContext = cx})

-- | Extract a string value from a term
string :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) String
string cx graph t = Eithers.bind (literal cx graph t) (\l -> stringLiteral cx l)

-- | Extract a string literal from a Literal value
stringLiteral :: Context.Context -> Core.Literal -> Either (Context.InContext Error.Error) String
stringLiteral cx v =
    case v of
      Core.LiteralString v0 -> Right v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "string") " but found ") (Core_.literal v)))),
        Context.inContextContext = cx})

-- | Extract a record from a term
termRecord :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Core.Record
termRecord cx graph term0 =
    Eithers.bind (Lexical.stripAndDereferenceTerm cx graph term0) (\term -> case term of
      Core.TermRecord v0 -> Right v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "record") " but found ") (Core_.term term)))),
        Context.inContextContext = cx}))

-- | Extract a 16-bit unsigned integer value from a term
uint16 :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Int
uint16 cx graph t = Eithers.bind (literal cx graph t) (\l -> Eithers.bind (integerLiteral cx l) (\i -> uint16Value cx i))

-- | Extract a uint16 value from an IntegerValue
uint16Value :: Context.Context -> Core.IntegerValue -> Either (Context.InContext Error.Error) Int
uint16Value cx v =
    case v of
      Core.IntegerValueUint16 v0 -> Right v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "uint16") " but found ") (Core_.integer v)))),
        Context.inContextContext = cx})

-- | Extract a 32-bit unsigned integer value from a term
uint32 :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) I.Int64
uint32 cx graph t = Eithers.bind (literal cx graph t) (\l -> Eithers.bind (integerLiteral cx l) (\i -> uint32Value cx i))

-- | Extract a uint32 value from an IntegerValue
uint32Value :: Context.Context -> Core.IntegerValue -> Either (Context.InContext Error.Error) I.Int64
uint32Value cx v =
    case v of
      Core.IntegerValueUint32 v0 -> Right v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "uint32") " but found ") (Core_.integer v)))),
        Context.inContextContext = cx})

-- | Extract a 64-bit unsigned integer value from a term
uint64 :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Integer
uint64 cx graph t = Eithers.bind (literal cx graph t) (\l -> Eithers.bind (integerLiteral cx l) (\i -> uint64Value cx i))

-- | Extract a uint64 value from an IntegerValue
uint64Value :: Context.Context -> Core.IntegerValue -> Either (Context.InContext Error.Error) Integer
uint64Value cx v =
    case v of
      Core.IntegerValueUint64 v0 -> Right v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "uint64") " but found ") (Core_.integer v)))),
        Context.inContextContext = cx})

-- | Extract an 8-bit unsigned integer value from a term
uint8 :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) I.Int16
uint8 cx graph t = Eithers.bind (literal cx graph t) (\l -> Eithers.bind (integerLiteral cx l) (\i -> uint8Value cx i))

-- | Extract a uint8 value from an IntegerValue
uint8Value :: Context.Context -> Core.IntegerValue -> Either (Context.InContext Error.Error) I.Int16
uint8Value cx v =
    case v of
      Core.IntegerValueUint8 v0 -> Right v0
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "uint8") " but found ") (Core_.integer v)))),
        Context.inContextContext = cx})

-- | Extract the field types from a union type
unionType :: Context.Context -> t0 -> Core.Type -> Either (Context.InContext Error.Error) [Core.FieldType]
unionType cx ename typ =
     
      let stripped = Rewriting.deannotateType typ
      in case stripped of
        Core.TypeUnion v0 -> Right v0
        _ -> Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "union type") " but found ") (Core_.type_ typ)))),
          Context.inContextContext = cx})

-- | Extract a unit value from a term
unit :: Context.Context -> Core.Term -> Either (Context.InContext Error.Error) ()
unit cx term =
    case term of
      Core.TermUnit -> Right ()
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "unit") " but found ") (Core_.term term)))),
        Context.inContextContext = cx})

-- | Extract a unit variant (a variant with an empty record value) from a union term
unitVariant :: Context.Context -> Core.Name -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Core.Name
unitVariant cx tname graph term =
    Eithers.bind (injection cx tname graph term) (\field -> Eithers.bind (unit cx (Core.fieldTerm field)) (\ignored -> Right (Core.fieldName field)))

-- | Extract the wrapped value from a wrapped term
wrap :: Context.Context -> Core.Name -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Core.Term
wrap cx expected graph term0 =
    Eithers.bind (Lexical.stripAndDereferenceTerm cx graph term0) (\term -> case term of
      Core.TermWrap v0 -> Logic.ifElse (Equality.equal (Core.unName (Core.wrappedTermTypeName v0)) (Core.unName expected)) (Right (Core.wrappedTermBody v0)) (Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " (Strings.cat2 "wrapper of type " (Core.unName expected))) " but found ") (Core.unName (Core.wrappedTermTypeName v0))))),
        Context.inContextContext = cx}))
      _ -> Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " (Strings.cat2 (Strings.cat2 "wrap(" (Core.unName expected)) ")")) " but found ") (Core_.term term)))),
        Context.inContextContext = cx}))

-- | Extract the wrapped type from a wrapper type
wrappedType :: Context.Context -> t0 -> Core.Type -> Either (Context.InContext Error.Error) Core.Type
wrappedType cx ename typ =
     
      let stripped = Rewriting.deannotateType typ
      in case stripped of
        Core.TypeWrap v0 -> Right v0
        _ -> Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 (Strings.cat2 (Strings.cat2 "expected " "wrapped type") " but found ") (Core_.type_ typ)))),
          Context.inContextContext = cx})
