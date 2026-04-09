-- Note: this is an automatically generated file. Do not edit.

-- | Extraction and validation for hydra.core types

module Hydra.Extract.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
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
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as Core_
import qualified Hydra.Show.Errors as Errors_
import qualified Hydra.Strip as Strip
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.Map as M
import qualified Data.Set as S

-- | Extract an arbitrary-precision floating-point value from a term
bigfloat :: Graph.Graph -> Core.Term -> Either Errors.Error Double
bigfloat graph t = Eithers.bind (literal graph t) (\l -> Eithers.bind (floatLiteral l) (\f -> bigfloatValue f))

-- | Extract a bigfloat value from a FloatValue
bigfloatValue :: Core.FloatValue -> Either Errors.Error Double
bigfloatValue v =
    case v of
      Core.FloatValueBigfloat v0 -> Right v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "bigfloat",
        Errors.unexpectedShapeErrorActual = (Core_.float v)})))

-- | Extract an arbitrary-precision integer value from a term
bigint :: Graph.Graph -> Core.Term -> Either Errors.Error Integer
bigint graph t = Eithers.bind (literal graph t) (\l -> Eithers.bind (integerLiteral l) (\i -> bigintValue i))

-- | Extract a bigint value from an IntegerValue
bigintValue :: Core.IntegerValue -> Either Errors.Error Integer
bigintValue v =
    case v of
      Core.IntegerValueBigint v0 -> Right v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "bigint",
        Errors.unexpectedShapeErrorActual = (Core_.integer v)})))

-- | Extract a binary data value from a term
binary :: Graph.Graph -> Core.Term -> Either Errors.Error B.ByteString
binary graph t = Eithers.bind (literal graph t) (\l -> binaryLiteral l)

-- | Extract a binary literal from a Literal value
binaryLiteral :: Core.Literal -> Either Errors.Error B.ByteString
binaryLiteral v =
    case v of
      Core.LiteralBinary v0 -> Right v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "binary",
        Errors.unexpectedShapeErrorActual = (Core_.literal v)})))

-- | Extract a boolean value from a term
boolean :: Graph.Graph -> Core.Term -> Either Errors.Error Bool
boolean graph t = Eithers.bind (literal graph t) (\l -> booleanLiteral l)

-- | Extract a boolean literal from a Literal value
booleanLiteral :: Core.Literal -> Either Errors.Error Bool
booleanLiteral v =
    case v of
      Core.LiteralBoolean v0 -> Right v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "boolean",
        Errors.unexpectedShapeErrorActual = (Core_.literal v)})))

-- | Extract a specific case handler from a case statement term
caseField :: Core.Name -> String -> Graph.Graph -> Core.Term -> Either Errors.Error Core.Field
caseField name n graph term =

      let fieldName = Core.Name n
      in (Eithers.bind (cases name graph term) (\cs ->
        let matching =
                Lists.filter (\f -> Equality.equal (Core.unName (Core.fieldName f)) (Core.unName fieldName)) (Core.caseStatementCases cs)
        in (Logic.ifElse (Lists.null matching) (Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
          Errors.unexpectedShapeErrorExpected = "matching case",
          Errors.unexpectedShapeErrorActual = "no matching case"})))) (Right (Lists.head matching)))))

-- | Extract case statement from a term
cases :: Core.Name -> Graph.Graph -> Core.Term -> Either Errors.Error Core.CaseStatement
cases name graph term0 =
    Eithers.bind (Lexical.stripAndDereferenceTerm graph term0) (\term -> case term of
      Core.TermFunction v0 -> case v0 of
        Core.FunctionElimination v1 -> case v1 of
          Core.EliminationUnion v2 -> Logic.ifElse (Equality.equal (Core.unName (Core.caseStatementTypeName v2)) (Core.unName name)) (Right v2) (Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
            Errors.unexpectedShapeErrorExpected = (Strings.cat2 "case statement for type " (Core.unName name)),
            Errors.unexpectedShapeErrorActual = (Core_.term term)}))))
          _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
            Errors.unexpectedShapeErrorExpected = "case statement",
            Errors.unexpectedShapeErrorActual = (Core_.term term)})))
        _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
          Errors.unexpectedShapeErrorExpected = "case statement",
          Errors.unexpectedShapeErrorActual = (Core_.term term)})))
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "case statement",
        Errors.unexpectedShapeErrorActual = (Core_.term term)}))))

-- | Decode an Either value using the provided left and right decoders
decodeEither :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> (Graph.Graph -> Core.Term -> Either Errors.DecodingError t1) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Either t0 t1)
decodeEither leftDecoder rightDecoder g term =
    Eithers.bind (stripWithDecodingError g term) (\stripped -> case stripped of
      Core.TermEither v0 -> Eithers.either (\lv -> Eithers.map (\x -> Left x) (leftDecoder g lv)) (\rv -> Eithers.map (\x -> Right x) (rightDecoder g rv)) v0
      _ -> Left (Errors.DecodingError "expected either value"))

-- | Decode a list of elements using the provided element decoder
decodeList :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError [t0]
decodeList elemDecoder g term =
    Eithers.bind (stripWithDecodingError g term) (\stripped -> case stripped of
      Core.TermList v0 -> Eithers.mapList (elemDecoder g) v0
      _ -> Left (Errors.DecodingError "expected list"))

-- | Decode a Map using the provided key and value decoders
decodeMap :: Ord t0 => ((Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> (Graph.Graph -> Core.Term -> Either Errors.DecodingError t1) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (M.Map t0 t1))
decodeMap keyDecoder valDecoder g term =
    Eithers.bind (stripWithDecodingError g term) (\stripped -> case stripped of
      Core.TermMap v0 -> Eithers.map Maps.fromList (Eithers.mapList (\kv -> Eithers.bind (keyDecoder g (Pairs.first kv)) (\k -> Eithers.map (\v -> (k, v)) (valDecoder g (Pairs.second kv)))) (Maps.toList v0))
      _ -> Left (Errors.DecodingError "expected map"))

-- | Decode a Maybe value using the provided element decoder
decodeMaybe :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Maybe t0)
decodeMaybe elemDecoder g term =
    Eithers.bind (stripWithDecodingError g term) (\stripped -> case stripped of
      Core.TermMaybe v0 -> Eithers.mapMaybe (elemDecoder g) v0
      _ -> Left (Errors.DecodingError "expected optional value"))

-- | Decode a Pair using the provided first and second decoders
decodePair :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> (Graph.Graph -> Core.Term -> Either Errors.DecodingError t1) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (t0, t1)
decodePair firstDecoder secondDecoder g term =
    Eithers.bind (stripWithDecodingError g term) (\stripped -> case stripped of
      Core.TermPair v0 -> Eithers.bind (firstDecoder g (Pairs.first v0)) (\f -> Eithers.map (\s -> (f, s)) (secondDecoder g (Pairs.second v0)))
      _ -> Left (Errors.DecodingError "expected pair"))

-- | Decode a Set using the provided element decoder
decodeSet :: Ord t0 => ((Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (S.Set t0))
decodeSet elemDecoder g term =
    Eithers.bind (stripWithDecodingError g term) (\stripped -> case stripped of
      Core.TermSet v0 -> Eithers.map Sets.fromList (Eithers.mapList (elemDecoder g) (Sets.toList v0))
      _ -> Left (Errors.DecodingError "expected set"))

-- | Decode a unit value
decodeUnit :: Graph.Graph -> Core.Term -> Either Errors.DecodingError ()
decodeUnit g term =
    Eithers.bind (stripWithDecodingError g term) (\stripped -> case stripped of
      Core.TermUnit -> Right ()
      _ -> Left (Errors.DecodingError "expected a unit value"))

-- | Decode a wrapped value using the provided body decoder
decodeWrapped :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError t0
decodeWrapped bodyDecoder g term =
    Eithers.bind (stripWithDecodingError g term) (\stripped -> case stripped of
      Core.TermWrap v0 -> bodyDecoder g (Core.wrappedTermBody v0)
      _ -> Left (Errors.DecodingError "expected wrapped value"))

-- | Extract an either value from a term, applying functions to the left and right values
eitherTerm :: (Core.Term -> Either Errors.Error t0) -> (Core.Term -> Either Errors.Error t1) -> Graph.Graph -> Core.Term -> Either Errors.Error (Either t0 t1)
eitherTerm leftFun rightFun graph term0 =
    Eithers.bind (Lexical.stripAndDereferenceTerm graph term0) (\term -> case term of
      Core.TermEither v0 -> Eithers.either (\l -> Eithers.map (\x -> Left x) (leftFun l)) (\r -> Eithers.map (\x -> Right x) (rightFun r)) v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "either value",
        Errors.unexpectedShapeErrorActual = (Core_.term term)}))))

-- | Extract the left and right types from an either type
eitherType :: Core.Type -> Either Errors.Error Core.EitherType
eitherType typ =

      let stripped = Strip.deannotateType typ
      in case stripped of
        Core.TypeEither v0 -> Right v0
        _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
          Errors.unexpectedShapeErrorExpected = "either type",
          Errors.unexpectedShapeErrorActual = (Core_.type_ typ)})))

-- | Extract a field value from a list of fields
field :: Core.Name -> (Core.Term -> Either Errors.Error t0) -> Graph.Graph -> [Core.Field] -> Either Errors.Error t0
field fname mapping graph fields =

      let matchingFields = Lists.filter (\f -> Equality.equal (Core.unName (Core.fieldName f)) (Core.unName fname)) fields
      in (Logic.ifElse (Lists.null matchingFields) (Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = (Strings.cat2 "field " (Core.unName fname)),
        Errors.unexpectedShapeErrorActual = "no matching field"})))) (Logic.ifElse (Equality.equal (Lists.length matchingFields) 1) (Eithers.bind (Lexical.stripAndDereferenceTerm graph (Core.fieldTerm (Lists.head matchingFields))) (\stripped -> mapping stripped)) (Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "single field",
        Errors.unexpectedShapeErrorActual = (Strings.cat2 "multiple fields named " (Core.unName fname))}))))))

-- | Extract a 32-bit floating-point value from a term
float32 :: Graph.Graph -> Core.Term -> Either Errors.Error Float
float32 graph t = Eithers.bind (literal graph t) (\l -> Eithers.bind (floatLiteral l) (\f -> float32Value f))

-- | Extract a float32 value from a FloatValue
float32Value :: Core.FloatValue -> Either Errors.Error Float
float32Value v =
    case v of
      Core.FloatValueFloat32 v0 -> Right v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "float32",
        Errors.unexpectedShapeErrorActual = (Core_.float v)})))

-- | Extract a 64-bit floating-point value from a term
float64 :: Graph.Graph -> Core.Term -> Either Errors.Error Double
float64 graph t = Eithers.bind (literal graph t) (\l -> Eithers.bind (floatLiteral l) (\f -> float64Value f))

-- | Extract a float64 value from a FloatValue
float64Value :: Core.FloatValue -> Either Errors.Error Double
float64Value v =
    case v of
      Core.FloatValueFloat64 v0 -> Right v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "float64",
        Errors.unexpectedShapeErrorActual = (Core_.float v)})))

-- | Extract a floating-point literal from a Literal value
floatLiteral :: Core.Literal -> Either Errors.Error Core.FloatValue
floatLiteral lit =
    case lit of
      Core.LiteralFloat v0 -> Right v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "floating-point value",
        Errors.unexpectedShapeErrorActual = (Core_.literal lit)})))

-- | Extract a float value from a term
floatValue :: Graph.Graph -> Core.Term -> Either Errors.Error Core.FloatValue
floatValue graph t = Eithers.bind (literal graph t) (\l -> floatLiteral l)

-- | Extract a function type from a type
functionType :: Core.Type -> Either Errors.Error Core.FunctionType
functionType typ =

      let stripped = Strip.deannotateType typ
      in case stripped of
        Core.TypeFunction v0 -> Right v0
        _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
          Errors.unexpectedShapeErrorExpected = "function type",
          Errors.unexpectedShapeErrorActual = (Core_.type_ typ)})))

-- | Extract a field from a union term
injection :: Core.Name -> Graph.Graph -> Core.Term -> Either Errors.Error Core.Field
injection expected graph term0 =
    Eithers.bind (Lexical.stripAndDereferenceTerm graph term0) (\term -> case term of
      Core.TermUnion v0 -> Logic.ifElse (Equality.equal (Core.unName (Core.injectionTypeName v0)) (Core.unName expected)) (Right (Core.injectionField v0)) (Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = (Strings.cat2 "injection of type " (Core.unName expected)),
        Errors.unexpectedShapeErrorActual = (Core.unName (Core.injectionTypeName v0))}))))
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "injection",
        Errors.unexpectedShapeErrorActual = (Core_.term term)}))))

-- | Extract a 16-bit signed integer value from a term
int16 :: Graph.Graph -> Core.Term -> Either Errors.Error I.Int16
int16 graph t = Eithers.bind (literal graph t) (\l -> Eithers.bind (integerLiteral l) (\i -> int16Value i))

-- | Extract an int16 value from an IntegerValue
int16Value :: Core.IntegerValue -> Either Errors.Error I.Int16
int16Value v =
    case v of
      Core.IntegerValueInt16 v0 -> Right v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "int16",
        Errors.unexpectedShapeErrorActual = (Core_.integer v)})))

-- | Extract a 32-bit signed integer value from a term
int32 :: Graph.Graph -> Core.Term -> Either Errors.Error Int
int32 graph t = Eithers.bind (literal graph t) (\l -> Eithers.bind (integerLiteral l) (\i -> int32Value i))

-- | Extract an int32 value from an IntegerValue
int32Value :: Core.IntegerValue -> Either Errors.Error Int
int32Value v =
    case v of
      Core.IntegerValueInt32 v0 -> Right v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "int32",
        Errors.unexpectedShapeErrorActual = (Core_.integer v)})))

-- | Extract a 64-bit signed integer value from a term
int64 :: Graph.Graph -> Core.Term -> Either Errors.Error I.Int64
int64 graph t = Eithers.bind (literal graph t) (\l -> Eithers.bind (integerLiteral l) (\i -> int64Value i))

-- | Extract an int64 value from an IntegerValue
int64Value :: Core.IntegerValue -> Either Errors.Error I.Int64
int64Value v =
    case v of
      Core.IntegerValueInt64 v0 -> Right v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "int64",
        Errors.unexpectedShapeErrorActual = (Core_.integer v)})))

-- | Extract an 8-bit signed integer value from a term
int8 :: Graph.Graph -> Core.Term -> Either Errors.Error I.Int8
int8 graph t = Eithers.bind (literal graph t) (\l -> Eithers.bind (integerLiteral l) (\i -> int8Value i))

-- | Extract an int8 value from an IntegerValue
int8Value :: Core.IntegerValue -> Either Errors.Error I.Int8
int8Value v =
    case v of
      Core.IntegerValueInt8 v0 -> Right v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "int8",
        Errors.unexpectedShapeErrorActual = (Core_.integer v)})))

-- | Extract an integer literal from a Literal value
integerLiteral :: Core.Literal -> Either Errors.Error Core.IntegerValue
integerLiteral lit =
    case lit of
      Core.LiteralInteger v0 -> Right v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "integer value",
        Errors.unexpectedShapeErrorActual = (Core_.literal lit)})))

-- | Extract an integer value from a term
integerValue :: Graph.Graph -> Core.Term -> Either Errors.Error Core.IntegerValue
integerValue graph t = Eithers.bind (literal graph t) (\l -> integerLiteral l)

-- | Extract a lambda from a term
lambda :: Graph.Graph -> Core.Term -> Either Errors.Error Core.Lambda
lambda graph term0 =
    Eithers.bind (Lexical.stripAndDereferenceTerm graph term0) (\term -> case term of
      Core.TermFunction v0 -> case v0 of
        Core.FunctionLambda v1 -> Right v1
        _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
          Errors.unexpectedShapeErrorExpected = "lambda",
          Errors.unexpectedShapeErrorActual = (Core_.term term)})))
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "lambda",
        Errors.unexpectedShapeErrorActual = (Core_.term term)}))))

-- | Extract the body of a lambda term
lambdaBody :: Graph.Graph -> Core.Term -> Either Errors.Error Core.Term
lambdaBody graph term = Eithers.map Core.lambdaBody (lambda graph term)

-- | Extract a let expression from a term
let_ :: Graph.Graph -> Core.Term -> Either Errors.Error Core.Let
let_ graph term0 =
    Eithers.bind (Lexical.stripAndDereferenceTerm graph term0) (\term -> case term of
      Core.TermLet v0 -> Right v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "let term",
        Errors.unexpectedShapeErrorActual = (Core_.term term)}))))

-- | Extract a binding with the given name from a let term
letBinding :: String -> Graph.Graph -> Core.Term -> Either Errors.Error Core.Term
letBinding n graph term =

      let name = Core.Name n
      in (Eithers.bind (let_ graph term) (\letExpr ->
        let matchingBindings =
                Lists.filter (\b -> Equality.equal (Core.unName (Core.bindingName b)) (Core.unName name)) (Core.letBindings letExpr)
        in (Logic.ifElse (Lists.null matchingBindings) (Left (Errors.ErrorExtraction (Errors.ExtractionErrorNoSuchBinding (Errors.NoSuchBindingError {
          Errors.noSuchBindingErrorName = name})))) (Logic.ifElse (Equality.equal (Lists.length matchingBindings) 1) (Right (Core.bindingTerm (Lists.head matchingBindings))) (Left (Errors.ErrorExtraction (Errors.ExtractionErrorMultipleBindings (Errors.MultipleBindingsError {
          Errors.multipleBindingsErrorName = name}))))))))

-- | Extract a list of terms from a term
list :: Graph.Graph -> Core.Term -> Either Errors.Error [Core.Term]
list graph term =
    Eithers.bind (Lexical.stripAndDereferenceTerm graph term) (\stripped -> case stripped of
      Core.TermList v0 -> Right v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "list",
        Errors.unexpectedShapeErrorActual = (Core_.term stripped)}))))

-- | Extract the first element of a list term
listHead :: Graph.Graph -> Core.Term -> Either Errors.Error Core.Term
listHead graph term =
    Eithers.bind (list graph term) (\l -> Logic.ifElse (Lists.null l) (Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
      Errors.unexpectedShapeErrorExpected = "non-empty list",
      Errors.unexpectedShapeErrorActual = "empty list"})))) (Right (Lists.head l)))

-- | Extract a list of values from a term, mapping a function over each element
listOf :: (Core.Term -> Either Errors.Error t0) -> Graph.Graph -> Core.Term -> Either Errors.Error [t0]
listOf f graph term = Eithers.bind (list graph term) (\els -> Eithers.mapList f els)

-- | Extract the element type from a list type
listType :: Core.Type -> Either Errors.Error Core.Type
listType typ =

      let stripped = Strip.deannotateType typ
      in case stripped of
        Core.TypeList v0 -> Right v0
        _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
          Errors.unexpectedShapeErrorExpected = "list type",
          Errors.unexpectedShapeErrorActual = (Core_.type_ typ)})))

-- | Extract a literal value from a term
literal :: Graph.Graph -> Core.Term -> Either Errors.Error Core.Literal
literal graph term0 =
    Eithers.bind (Lexical.stripAndDereferenceTerm graph term0) (\term -> case term of
      Core.TermLiteral v0 -> Right v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "literal",
        Errors.unexpectedShapeErrorActual = (Core_.term term)}))))

-- | Extract a map of key-value pairs from a term, mapping functions over each key and value
map :: Ord t0 => ((Core.Term -> Either Errors.Error t0) -> (Core.Term -> Either Errors.Error t1) -> Graph.Graph -> Core.Term -> Either Errors.Error (M.Map t0 t1))
map fk fv graph term0 =

      let pair =
              \kvPair ->
                let kterm = Pairs.first kvPair
                    vterm = Pairs.second kvPair
                in (Eithers.bind (fk kterm) (\kval -> Eithers.bind (fv vterm) (\vval -> Right (kval, vval))))
      in (Eithers.bind (Lexical.stripAndDereferenceTerm graph term0) (\term -> case term of
        Core.TermMap v0 -> Eithers.map Maps.fromList (Eithers.mapList pair (Maps.toList v0))
        _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
          Errors.unexpectedShapeErrorExpected = "map",
          Errors.unexpectedShapeErrorActual = (Core_.term term)})))))

-- | Extract the key and value types from a map type
mapType :: Core.Type -> Either Errors.Error Core.MapType
mapType typ =

      let stripped = Strip.deannotateType typ
      in case stripped of
        Core.TypeMap v0 -> Right v0
        _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
          Errors.unexpectedShapeErrorExpected = "map type",
          Errors.unexpectedShapeErrorActual = (Core_.type_ typ)})))

-- | Extract an optional value from a term, applying a function to the value if present
maybeTerm :: (Core.Term -> Either Errors.Error t0) -> Graph.Graph -> Core.Term -> Either Errors.Error (Maybe t0)
maybeTerm f graph term0 =
    Eithers.bind (Lexical.stripAndDereferenceTerm graph term0) (\term -> case term of
      Core.TermMaybe v0 -> Maybes.maybe (Right Nothing) (\t -> Eithers.map Maybes.pure (f t)) v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "maybe value",
        Errors.unexpectedShapeErrorActual = (Core_.term term)}))))

-- | Extract the base type from an optional type
maybeType :: Core.Type -> Either Errors.Error Core.Type
maybeType typ =

      let stripped = Strip.deannotateType typ
      in case stripped of
        Core.TypeMaybe v0 -> Right v0
        _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
          Errors.unexpectedShapeErrorExpected = "maybe type",
          Errors.unexpectedShapeErrorActual = (Core_.type_ typ)})))

-- | Ensure a function has the expected number of arguments
nArgs :: Core.Name -> Int -> [t0] -> Either Errors.Error ()
nArgs name n args =
    Logic.ifElse (Equality.equal (Lists.length args) n) (Right ()) (Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
      Errors.unexpectedShapeErrorExpected = (Strings.cat [
        Literals.showInt32 n,
        " arguments to primitive ",
        (Literals.showString (Core.unName name))]),
      Errors.unexpectedShapeErrorActual = (Literals.showInt32 (Lists.length args))}))))

-- | Extract a pair of values from a term, applying functions to each component
pair :: (Core.Term -> Either Errors.Error t0) -> (Core.Term -> Either Errors.Error t1) -> Graph.Graph -> Core.Term -> Either Errors.Error (t0, t1)
pair kf vf graph term0 =
    Eithers.bind (Lexical.stripAndDereferenceTerm graph term0) (\term -> case term of
      Core.TermPair v0 -> Eithers.bind (kf (Pairs.first v0)) (\kVal -> Eithers.bind (vf (Pairs.second v0)) (\vVal -> Right (kVal, vVal)))
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "pair",
        Errors.unexpectedShapeErrorActual = (Core_.term term)}))))

-- | Extract a record's fields from a term
record :: Core.Name -> Graph.Graph -> Core.Term -> Either Errors.Error [Core.Field]
record expected graph term0 =
    Eithers.bind (termRecord graph term0) (\record -> Logic.ifElse (Equality.equal (Core.recordTypeName record) expected) (Right (Core.recordFields record)) (Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
      Errors.unexpectedShapeErrorExpected = (Strings.cat2 "record of type " (Core.unName expected)),
      Errors.unexpectedShapeErrorActual = (Core.unName (Core.recordTypeName record))})))))

-- | Extract the field types from a record type
recordType :: t0 -> Core.Type -> Either Errors.Error [Core.FieldType]
recordType ename typ =

      let stripped = Strip.deannotateType typ
      in case stripped of
        Core.TypeRecord v0 -> Right v0
        _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
          Errors.unexpectedShapeErrorExpected = "record type",
          Errors.unexpectedShapeErrorActual = (Core_.type_ typ)})))

-- | Require a field from a record's field map and decode it
requireField :: String -> (t0 -> t1 -> Either Errors.DecodingError t2) -> M.Map Core.Name t1 -> t0 -> Either Errors.DecodingError t2
requireField fieldName decoder fieldMap g =
    Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
      "missing field ",
      fieldName,
      " in record"]))) (\fieldTerm -> decoder g fieldTerm) (Maps.lookup (Core.Name fieldName) fieldMap)

-- | Extract a set of terms from a term
set :: Graph.Graph -> Core.Term -> Either Errors.Error (S.Set Core.Term)
set graph term =
    Eithers.bind (Lexical.stripAndDereferenceTerm graph term) (\stripped -> case stripped of
      Core.TermSet v0 -> Right v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "set",
        Errors.unexpectedShapeErrorActual = (Core_.term stripped)}))))

-- | Extract a set of values from a term, mapping a function over each element
setOf :: Ord t0 => ((Core.Term -> Either Errors.Error t0) -> Graph.Graph -> Core.Term -> Either Errors.Error (S.Set t0))
setOf f graph term = Eithers.bind (set graph term) (\els -> Eithers.mapSet f els)

-- | Extract the element type from a set type
setType :: Core.Type -> Either Errors.Error Core.Type
setType typ =

      let stripped = Strip.deannotateType typ
      in case stripped of
        Core.TypeSet v0 -> Right v0
        _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
          Errors.unexpectedShapeErrorExpected = "set type",
          Errors.unexpectedShapeErrorActual = (Core_.type_ typ)})))

-- | Extract a string value from a term
string :: Graph.Graph -> Core.Term -> Either Errors.Error String
string graph t = Eithers.bind (literal graph t) (\l -> stringLiteral l)

-- | Extract a string literal from a Literal value
stringLiteral :: Core.Literal -> Either Errors.Error String
stringLiteral v =
    case v of
      Core.LiteralString v0 -> Right v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "string",
        Errors.unexpectedShapeErrorActual = (Core_.literal v)})))

-- | Strip annotations and dereference variables, returning Either DecodingError Term
stripWithDecodingError :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.Term
stripWithDecodingError g term =
    Eithers.bimap (\_e -> Errors.DecodingError (Errors_.error _e)) (\x -> x) (Lexical.stripAndDereferenceTermEither g term)

-- | Extract a record from a term
termRecord :: Graph.Graph -> Core.Term -> Either Errors.Error Core.Record
termRecord graph term0 =
    Eithers.bind (Lexical.stripAndDereferenceTerm graph term0) (\term -> case term of
      Core.TermRecord v0 -> Right v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "record",
        Errors.unexpectedShapeErrorActual = (Core_.term term)}))))

-- | Convert a Record's fields to a Map from Name to Term
toFieldMap :: Core.Record -> M.Map Core.Name Core.Term
toFieldMap record = Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields record))

-- | Extract a 16-bit unsigned integer value from a term
uint16 :: Graph.Graph -> Core.Term -> Either Errors.Error Int
uint16 graph t = Eithers.bind (literal graph t) (\l -> Eithers.bind (integerLiteral l) (\i -> uint16Value i))

-- | Extract a uint16 value from an IntegerValue
uint16Value :: Core.IntegerValue -> Either Errors.Error Int
uint16Value v =
    case v of
      Core.IntegerValueUint16 v0 -> Right v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "uint16",
        Errors.unexpectedShapeErrorActual = (Core_.integer v)})))

-- | Extract a 32-bit unsigned integer value from a term
uint32 :: Graph.Graph -> Core.Term -> Either Errors.Error I.Int64
uint32 graph t = Eithers.bind (literal graph t) (\l -> Eithers.bind (integerLiteral l) (\i -> uint32Value i))

-- | Extract a uint32 value from an IntegerValue
uint32Value :: Core.IntegerValue -> Either Errors.Error I.Int64
uint32Value v =
    case v of
      Core.IntegerValueUint32 v0 -> Right v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "uint32",
        Errors.unexpectedShapeErrorActual = (Core_.integer v)})))

-- | Extract a 64-bit unsigned integer value from a term
uint64 :: Graph.Graph -> Core.Term -> Either Errors.Error Integer
uint64 graph t = Eithers.bind (literal graph t) (\l -> Eithers.bind (integerLiteral l) (\i -> uint64Value i))

-- | Extract a uint64 value from an IntegerValue
uint64Value :: Core.IntegerValue -> Either Errors.Error Integer
uint64Value v =
    case v of
      Core.IntegerValueUint64 v0 -> Right v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "uint64",
        Errors.unexpectedShapeErrorActual = (Core_.integer v)})))

-- | Extract an 8-bit unsigned integer value from a term
uint8 :: Graph.Graph -> Core.Term -> Either Errors.Error I.Int16
uint8 graph t = Eithers.bind (literal graph t) (\l -> Eithers.bind (integerLiteral l) (\i -> uint8Value i))

-- | Extract a uint8 value from an IntegerValue
uint8Value :: Core.IntegerValue -> Either Errors.Error I.Int16
uint8Value v =
    case v of
      Core.IntegerValueUint8 v0 -> Right v0
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "uint8",
        Errors.unexpectedShapeErrorActual = (Core_.integer v)})))

-- | Extract the field types from a union type
unionType :: t0 -> Core.Type -> Either Errors.Error [Core.FieldType]
unionType ename typ =

      let stripped = Strip.deannotateType typ
      in case stripped of
        Core.TypeUnion v0 -> Right v0
        _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
          Errors.unexpectedShapeErrorExpected = "union type",
          Errors.unexpectedShapeErrorActual = (Core_.type_ typ)})))

-- | Extract a unit value from a term
unit :: Core.Term -> Either Errors.Error ()
unit term =
    case term of
      Core.TermUnit -> Right ()
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = "unit",
        Errors.unexpectedShapeErrorActual = (Core_.term term)})))

-- | Extract a unit variant (a variant with an empty record value) from a union term
unitVariant :: Core.Name -> Graph.Graph -> Core.Term -> Either Errors.Error Core.Name
unitVariant tname graph term =
    Eithers.bind (injection tname graph term) (\field -> Eithers.bind (unit (Core.fieldTerm field)) (\ignored -> Right (Core.fieldName field)))

-- | Extract the wrapped value from a wrapped term
wrap :: Core.Name -> Graph.Graph -> Core.Term -> Either Errors.Error Core.Term
wrap expected graph term0 =
    Eithers.bind (Lexical.stripAndDereferenceTerm graph term0) (\term -> case term of
      Core.TermWrap v0 -> Logic.ifElse (Equality.equal (Core.unName (Core.wrappedTermTypeName v0)) (Core.unName expected)) (Right (Core.wrappedTermBody v0)) (Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = (Strings.cat2 "wrapper of type " (Core.unName expected)),
        Errors.unexpectedShapeErrorActual = (Core.unName (Core.wrappedTermTypeName v0))}))))
      _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
        Errors.unexpectedShapeErrorExpected = (Strings.cat2 (Strings.cat2 "wrap(" (Core.unName expected)) ")"),
        Errors.unexpectedShapeErrorActual = (Core_.term term)}))))

-- | Extract the wrapped type from a wrapper type
wrappedType :: t0 -> Core.Type -> Either Errors.Error Core.Type
wrappedType ename typ =

      let stripped = Strip.deannotateType typ
      in case stripped of
        Core.TypeWrap v0 -> Right v0
        _ -> Left (Errors.ErrorExtraction (Errors.ExtractionErrorUnexpectedShape (Errors.UnexpectedShapeError {
          Errors.unexpectedShapeErrorExpected = "wrapped type",
          Errors.unexpectedShapeErrorActual = (Core_.type_ typ)})))
