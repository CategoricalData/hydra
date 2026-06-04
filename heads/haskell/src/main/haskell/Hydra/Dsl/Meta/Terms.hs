{-# LANGUAGE FlexibleContexts #-}

-- | A domain-specific language for constructing term-encoded Hydra terms in Haskell;
--   these functions enable you to build terms (programs) which build terms.
module Hydra.Dsl.Meta.Terms (
  module Hydra.Dsl.Meta.Base,
  module Hydra.Dsl.Meta.Terms,
) where

import Hydra.Kernel
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Meta.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import Hydra.Dsl.Meta.Base
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Meta.Lib.Maps as Maps

import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Scientific as Sci
import qualified Data.Set as S
import qualified Data.Maybe as Y
import Data.Int
import Prelude hiding (map, product, sum)


-- Operators

-- | Function composition operator for term-encoded terms: f <.> g creates a function that applies g then f
-- Example: var "stringLength" <.> var "toString"
(<.>) :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
f <.> g = compose f g

-- | Function application operator for term-encoded terms
-- Example: fun @@ arg
(@@) :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
f @@ x = apply f x

-- | Attach a map of annotations to a term-encoded term, wrapping the map as a
-- TermMap annotation per Hydra's convention (#386). Each Name key is lifted
-- to a TermVariable in the resulting TermMap.
-- Distinct from 'Phantoms.annot' (which attaches a single annotation by key).
-- Example: annots (Phantoms.map M.empty) (int32 42)
annots :: TypedTerm (M.Map Name Term) -> TypedTerm Term -> TypedTerm Term
annots annMap term = Core.termAnnotated $ Core.annotatedTerm term (annotationMapToTerm annMap)

-- | Create a term-encoded annotated term (term with type annotations).
-- The map argument is wrapped as a TermMap annotation per Hydra's convention.
-- Example: annotated (int32 42) (Phantoms.map M.empty)
annotated :: TypedTerm Term -> TypedTerm (M.Map Name Term) -> TypedTerm Term
annotated term annMap = Core.termAnnotated $ Core.annotatedTerm term (annotationMapToTerm annMap)

-- | Lift a term-encoded Map<Name, Term> to a term-encoded TermMap with each
-- Name key wrapped as a TermVariable. The canonical encoding for Hydra's
-- annotation map convention after #386.
annotationMapToTerm :: TypedTerm (M.Map Name Term) -> TypedTerm Term
annotationMapToTerm m = Core.termMap
  (Maps.mapKeys (Phantoms.lambda "n" (Core.termVariable (Phantoms.var "n"))) m)

annotatedTerm :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
annotatedTerm body ann = inject (Core.nameLift _Term) "annotated" $ record (Core.nameLift _AnnotatedTerm) [
  _AnnotatedTerm_body>>: body,
  _AnnotatedTerm_annotation>>: ann]

-- | Apply a term-encoded function to a term-encoded argument
-- Example: apply (var "add") (int32 1)
apply :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
apply func arg = Core.termApplication $ Core.application func arg

-- | Create a term-encoded unlimited precision integer
-- Example: bigint 42
bigint :: Integer -> TypedTerm Term
bigint = bigintLift . TypedTerm . Terms.bigint

-- | Lift a TypedTerm Integer to a term-encoded bigint literal
-- Example: bigintLift (varPhantom "x" :: TypedTerm Integer)
bigintLift :: TypedTerm Integer -> TypedTerm Term
bigintLift = Core.termLiteral . Core.literalInteger . Core.integerValueBigint

-- | Create a term-encoded binary (byte string) literal
-- Example: binary (B.pack [0x48, 0x65, 0x6C, 0x6C, 0x6F])
binary :: B.ByteString -> TypedTerm Term
binary = binaryLift . TypedTerm . Terms.binary

-- | Lift a TypedTerm ByteString to a term-encoded binary literal
binaryLift :: TypedTerm B.ByteString -> TypedTerm Term
binaryLift = Core.termLiteral . Core.literalBinary

-- | Create a term-encoded boolean literal
-- Example: boolean True
boolean :: Bool -> TypedTerm Term
boolean = booleanLift . TypedTerm . Terms.boolean

-- | Lift a TypedTerm Bool to a term-encoded boolean literal
-- Example: booleanLift $ Phantoms.true
booleanLift :: TypedTerm Bool -> TypedTerm Term
booleanLift = Core.termLiteral . Core.literalBoolean

-- | Create a meta-level term encoding a boolean literal
-- Example: booleanTerm True creates a term that *represents* the boolean True
booleanTerm :: Bool -> TypedTerm Term
booleanTerm b = inject (Core.nameLift _Term) "literal" $ inject (Core.nameLift _Literal) "boolean" $ boolean b

-- | Create a term-encoded character literal via int32
-- Example: char 'A'
char :: Char -> TypedTerm Term
char = int32 . ord
  where
    ord = fromEnum

-- | Convert a Comparison enum value to a term-encoded term
-- Example: comparison ComparisonEqualTo
comparison :: Comparison -> TypedTerm Term
comparison t = case t of
  ComparisonEqualTo -> Phantoms.injectUnit _Comparison _Comparison_equalTo
  ComparisonLessThan -> Phantoms.injectUnit _Comparison _Comparison_lessThan
  ComparisonGreaterThan -> Phantoms.injectUnit _Comparison _Comparison_greaterThan


-- | Term-encoded function composition (apply g then f)
-- Example: compose f g creates a function that applies g then f
compose :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
compose f g = lambda "arg_" $ f @@ (g @@ var "arg_")

-- | Create a term-encoded constant function that always returns the same term-encoded value
-- Example: constant (int32 42)
constant :: TypedTerm Term -> TypedTerm Term
constant = lambda ignoredVariable

-- | Create a term-encoded arbitrary-precision exact decimal literal
-- Example: decimal 42
decimal :: Sci.Scientific -> TypedTerm Term
decimal = decimalLift . TypedTerm . Terms.decimal

-- | Lift a TypedTerm Scientific to a term-encoded decimal literal
decimalLift :: TypedTerm Sci.Scientific -> TypedTerm Term
decimalLift = Core.termLiteral . Core.literalDecimal

-- | Term-encoded boolean false literal
false :: TypedTerm Term
false = boolean False

-- | Create a term-encoded field with the given name and term-encoded value
-- Example: field "age" (int32 30)
field :: String -> TypedTerm Term -> TypedTerm Field
field s = Core.field (name s)

-- | Create a term-encoded floating-point literal with specified precision
-- Example: float (FloatValueFloat32 3.14)
float :: TypedTerm FloatValue -> TypedTerm Term
float = Core.termLiteral . Core.literalFloat

-- | Create a term-encoded 32-bit floating point literal
-- Example: float32 3.14
float32 :: Float -> TypedTerm Term
float32 = float32Lift . TypedTerm . Terms.float32

-- | Lift a TypedTerm Float to a term-encoded float32 literal
-- Example: float32Lift (varPhantom "x" :: TypedTerm Float)
float32Lift :: TypedTerm Float -> TypedTerm Term
float32Lift = Core.termLiteral . Core.literalFloat . Core.floatValueFloat32

-- | Create a term-encoded 64-bit floating point literal
-- Example: float64 3.14159
float64 :: Double -> TypedTerm Term
float64 = float64Lift . TypedTerm . Terms.float64

-- | Lift a TypedTerm Float to a term-encoded float64 literal
-- Example: float64Lift (varPhantom "x" :: TypedTerm Float)
float64Lift :: TypedTerm Double -> TypedTerm Term
float64Lift = Core.termLiteral . Core.literalFloat . Core.floatValueFloat64

-- | Term-encoded identity function
-- Example: identity
identity :: TypedTerm Term
identity = lambda "x_" $ var "x_"

-- | Create a term-encoded union injection
-- Example: inject (name "Result") "success" (int32 42)
inject :: AsTerm t Name => t -> String -> TypedTerm Term -> TypedTerm Term
inject tname fname = Core.termInject . Core.injection (asTerm tname) . Core.field (name fname)

injectPhantom :: Name -> Name -> TypedTerm Term -> TypedTerm Term
injectPhantom tname fname term = Core.termInject $ Core.injection (Core.nameLift tname) $ Core.field (Core.nameLift fname) term

injectUnit :: AsTerm t Name => t -> String -> TypedTerm Term
injectUnit tname fname = inject tname fname unit

injectUnitPhantom :: Name -> Name -> TypedTerm Term
injectUnitPhantom tname fname = injectPhantom tname fname Core.termUnit

-- | Create a term-encoded 16-bit signed integer literal
-- Example: int16 32767
int16 :: Int16 -> TypedTerm Term
int16 = int16Lift . TypedTerm . Terms.int16

-- | Lift a TypedTerm Int16 to a term-encoded int16 literal
-- Example: int16Lift (varPhantom "x" :: TypedTerm Int16)
int16Lift :: TypedTerm Int16 -> TypedTerm Term
int16Lift = Core.termLiteral . Core.literalInteger . Core.integerValueInt16

-- | Create a meta-level term encoding a 16-bit signed integer literal
-- Example: int16Term 100 creates a term that *represents* the int16 literal 100
int16Term :: Int16 -> TypedTerm Term
int16Term n = inject (Core.nameLift _Term) "literal" $
  inject (Core.nameLift _Literal) "integer" $
  inject (Core.nameLift _IntegerValue) "int16" $ int16 n

-- | Create a term-encoded 32-bit signed integer literal
-- Example: int32 42
int32 :: Int -> TypedTerm Term
int32 = int32Lift . TypedTerm . Terms.int32

-- | Lift a TypedTerm Int to a term-encoded int32 literal
-- Example: int32Lift (varPhantom "x" :: TypedTerm Int)
int32Lift :: TypedTerm Int -> TypedTerm Term
int32Lift = Core.termLiteral . Core.literalInteger . Core.integerValueInt32

-- | Create a meta-level term encoding a 32-bit signed integer literal
-- Example: int32Term 42 creates a term that *represents* the int32 literal 42
int32Term :: Int -> TypedTerm Term
int32Term n = inject (Core.nameLift _Term) "literal" $
  inject (Core.nameLift _Literal) "integer" $
  inject (Core.nameLift _IntegerValue) "int32" $ int32 n

-- | Create a term-encoded 64-bit signed integer literal
-- Example: int64 9223372036854775807
int64 :: Int64 -> TypedTerm Term
int64 = int64Lift . TypedTerm . Terms.int64

-- | Lift a TypedTerm Int64 to a term-encoded int64 literal
-- Example: int64Lift (varPhantom "x" :: TypedTerm Int64)
int64Lift :: TypedTerm Int64 -> TypedTerm Term
int64Lift = Core.termLiteral . Core.literalInteger . Core.integerValueInt64

-- | Create a meta-level term encoding a 64-bit signed integer literal
-- Example: int64Term 137 creates a term that *represents* the int64 literal 137
int64Term :: Int64 -> TypedTerm Term
int64Term n = inject (Core.nameLift _Term) "literal" $
  inject (Core.nameLift _Literal) "integer" $
  inject (Core.nameLift _IntegerValue) "int64" $ int64 n

-- | Create a term-encoded 8-bit signed integer literal
-- Example: int8 127
int8 :: Int8 -> TypedTerm Term
int8 = int8Lift . TypedTerm . Terms.int8

-- | Lift a TypedTerm Int8 to a term-encoded int8 literal
-- Example: int8Lift (varPhantom "x" :: TypedTerm Int8)
int8Lift :: TypedTerm Int8 -> TypedTerm Term
int8Lift = Core.termLiteral . Core.literalInteger . Core.integerValueInt8

-- | Create a term-encoded integer literal with specified bit width
-- Example: integer (IntegerValueInt32 42)
integer :: TypedTerm IntegerValue -> TypedTerm Term
integer = Core.termLiteral . Core.literalInteger

-- | Create a term-encoded 'Just' optional value
-- Example: just (string "found")
just :: TypedTerm Term -> TypedTerm (Maybe Term)
just = Phantoms.just

-- | Create a term-encoded lambda function with one parameter
-- Example: lambda "x" (var "add" @@ var "x" @@ int32 1)
lambda :: String -> TypedTerm Term -> TypedTerm Term
lambda var body = Core.termLambda $ Core.lambda (name var) Phantoms.nothing body

-- | Create a term-encoded lambda with a type annotation
-- Example: lambdaTyped "x" T.int32 (var "x")
lambdaTyped :: String -> TypedTerm Type -> TypedTerm Term -> TypedTerm Term
lambdaTyped param dom body = Core.termLambda $ Core.lambda (name param) (Phantoms.just dom) body

-- | Create a term-encoded multi-parameter lambda function (curried form)
-- Example: lambdas ["x", "y"] (var "add" @@ var "x" @@ var "y")
lambdas :: [String] -> TypedTerm Term -> TypedTerm Term
lambdas params body = case params of
  [] -> body
  (h:rest) -> Core.termLambda $ Core.lambda (name h) Phantoms.nothing $ lambdas rest body

-- | Create a term-encoded left either value
-- Example: left (string "error")
left :: TypedTerm Term -> TypedTerm Term
left t = Core.termEither $ Phantoms.left t

-- | Create a term-encoded let expression with multiple bindings
-- Example: lets ["x">: int32 1, "y">: int32 2] (var "add" @@ var "x" @@ var "y")
lets :: [(TypedTerm Name, TypedTerm Term)] -> TypedTerm Term -> TypedTerm Term
lets pairs body = Core.termLet $ Core.let_ (Phantoms.list $ toBinding pairs) body
  where
    toBinding = fmap (\(n, t) -> Core.binding n t Phantoms.nothing)

-- | Create a term-encoded let expression with type annotations on bindings
-- Example: letsTyped [("x", int32 1, T.mono T.int32)] (var "x")
letsTyped :: [(String, TypedTerm Term, TypedTerm TypeScheme)] -> TypedTerm Term -> TypedTerm Term
letsTyped bindings body = Core.termLet $ Core.let_ (Phantoms.list $ toBinding bindings) body
  where
    toBinding = fmap (\(n, t, ts) -> Core.binding (name n) t (Phantoms.just ts))

-- | Create a term-encoded list
-- Example: list [int32 1, int32 2, int32 3]
list :: [TypedTerm Term] -> TypedTerm Term
list = Core.termList . Phantoms.list

-- | Create a term-encoded literal from a Literal value
-- Example: literal (LiteralInteger (IntegerValueInt32 42))
literal :: TypedTerm Literal -> TypedTerm Term
literal = Core.termLiteral

-- | Create a term-encoded map/dictionary
-- Example: map (fromList [(string "key", int32 42)])
map :: TypedTerm (M.Map Term Term) -> TypedTerm Term
map = Core.termMap

mapTerm1 :: TypedTerm (M.Map Term Term) -> TypedTerm Term
mapTerm1 m = inject (Core.nameLift _Term) "map" $ Core.termMap m

-- | Create a term-encoded pattern match on a union
-- Example: match (name "Result") nothing ["success">: int32 42, "error">: string "fail"]
match :: AsTerm t Name => t -> TypedTerm (Maybe Term) -> [(TypedTerm Name, TypedTerm Term)] -> TypedTerm Term
match tname def pairs = Core.termCases
    $ Core.caseStatement (asTerm tname) def $ Phantoms.list $ toCaseAlternative pairs
  where
    toCaseAlternative = fmap (\(n, t) -> Core.caseAlternative n t)

--meta :: TypedTerm a -> TypedTerm Term
--meta (TypedTerm term) = TypedTerm $ EncodeCore.term term

metaref :: TypedTermDefinition a -> TypedTerm Term
metaref (TypedTermDefinition name _) = Core.termVariable $ Core.nameLift name

-- | Create a meta-level term encoding a Name
-- Example: nameTerm "foo" creates a term that *represents* the name "foo"
nameTerm :: String -> TypedTerm Term
nameTerm s = wrap (Core.nameLift _Name) $ string s

-- | Create a term-encoded 'Nothing' optional value
nothing :: TypedTerm (Maybe Term)
nothing = Phantoms.nothing

-- | Create a term-encoded optional value from a Maybe
-- Example: optional (just (int32 42))
optional :: TypedTerm (Maybe Term) -> TypedTerm Term
optional = Core.termMaybe

-- | Create a term-encoded pair
-- Example: pair (string "name") (int32 42)
pair :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
pair t1 t2 = Core.termPair $ Phantoms.pair t1 t2

-- | Create a term-encoded reference to a primitive function.
-- Uses termVariable; the name resolves via graphPrimitives fallthrough.
-- Example: primitive (Name "hydra.lib.strings.length")
primitive :: Name -> TypedTerm Term
primitive = Core.termVariable . TypedTerm . EncodeCore.name

-- | Create a term-encoded field projection function
-- Example: project (name "Person") (name "firstName")
project :: (AsTerm t Name, AsTerm f Name) => t -> f -> TypedTerm Term
project tname fname = Core.termProject
  $ Core.projection (asTerm tname) (asTerm fname)

-- | Create a term-encoded record with named fields
-- Example: record (name "Person") ["name">: string "John", "age">: int32 30]
record :: AsTerm t Name => t -> [(TypedTerm Name, TypedTerm Term)] -> TypedTerm Term
record name pairs = Core.termRecord $ Core.record (asTerm name) $ Phantoms.list (toField <$> pairs)
  where
    toField (n, t) = Core.field n t

recordLift :: Name -> [(TypedTerm Name, TypedTerm Term)] -> TypedTerm Term
recordLift name pairs = record (TypedTerm $ EncodeCore.name name) pairs

-- | Create a term-encoded right either value
-- Example: right (int32 42)
right :: TypedTerm Term -> TypedTerm Term
right t = Core.termEither $ Phantoms.right t

-- | Create a term-encoded set
-- Example: set [string "a", string "b", string "c"]
set :: [TypedTerm Term] -> TypedTerm Term
set els = Core.termSet $ TypedTerm $ TermSet $ S.fromList (unTypedTerm <$> els)

-- | Create a term-encoded string literal
-- Example: string "hello world"
string :: String -> TypedTerm Term
string = stringLift . TypedTerm . Terms.string

-- | Lift a TypedTerm String to a term-encoded string literal
-- Example: stringLift $ Phantoms.string "hello world"
stringLift :: TypedTerm String -> TypedTerm Term
stringLift = Core.termLiteral . Core.literalString

stringTerm :: String -> TypedTerm Term
stringTerm s = inject (Core.nameLift _Term) "literal" $ inject (Core.nameLift _Literal) "string" $ string s

-- | Create a term-encoded 3-tuple using nested pairs
-- Example: triple (int32 1) (string "test") (boolean True)
triple :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term -> TypedTerm Term
triple t1 t2 t3 = pair t1 (pair t2 t3)

-- | Term-encoded boolean true literal
true :: TypedTerm Term
true = boolean True

-- | Create a term-encoded tuple with multiple components using nested pairs
-- Example: tuple [string "name", int32 42, boolean True]
tuple :: [TypedTerm Term] -> TypedTerm Term
tuple [] = unit
tuple [a] = a
tuple [a, b] = pair a b
tuple (a:rest) = pair a (tuple rest)

-- | Create a term-encoded 2-tuple (same as pair)
-- Example: tuple2 (string "name") (int32 42)
tuple2 :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
tuple2 = pair

-- | Create a term-encoded 4-tuple using nested pairs
-- Example: tuple4 (int32 1) (string "test") (boolean True) (float32 3.14)
tuple4 :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term -> TypedTerm Term -> TypedTerm Term
tuple4 t1 t2 t3 t4 = pair t1 (pair t2 (pair t3 t4))

-- | Create a term-encoded 5-tuple using nested pairs
-- Example: tuple5 (int32 1) (string "a") (boolean True) (float32 3.14) (int32 42)
tuple5 :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term -> TypedTerm Term -> TypedTerm Term -> TypedTerm Term
tuple5 t1 t2 t3 t4 t5 = pair t1 (pair t2 (pair t3 (pair t4 t5)))

-- | Create a term-encoded type application
-- Example: tyapp (list []) T.int32
tyapp :: TypedTerm Term -> TypedTerm Type -> TypedTerm Term
tyapp term typ = Core.termTypeApplication $ Core.typeApplicationTerm term typ

-- | Apply multiple type arguments to a term
-- Example: tyapps (pair (int32 1) (string "a")) [T.int32, T.string]
tyapps :: TypedTerm Term -> [TypedTerm Type] -> TypedTerm Term
tyapps = L.foldl tyapp

-- | Create a term-encoded type lambda (System F)
-- Example: tylam "t0" (lambda "x" $ var "x")
tylam :: String -> TypedTerm Term -> TypedTerm Term
tylam var body = Core.termTypeLambda $ Core.typeLambda (name var) body

-- | Create multiple term-encoded type lambdas
-- Example: tylams ["t0", "t1"] (lambda "x" $ var "x")
tylams :: [String] -> TypedTerm Term -> TypedTerm Term
tylams vars body = L.foldl (\b v -> Core.termTypeLambda $ Core.typeLambda (name v) b) body $ L.reverse vars

-- | Apply type arguments to a polymorphic term (same as tyapps)
-- Example: typeApplication (list []) [T.int32]
typeApplication :: TypedTerm Term -> [TypedTerm Type] -> TypedTerm Term
typeApplication = tyapps

-- | Create term-encoded type lambda(s) (same as tylams)
-- Example: typeLambda ["t0"] (lambda "x" $ var "x")
typeLambda :: [String] -> TypedTerm Term -> TypedTerm Term
typeLambda = tylams

-- | Create a term-encoded 16-bit unsigned integer literal
-- Example: uint16 65535
uint16 :: Int -> TypedTerm Term
uint16 = uint16Lift . TypedTerm . Terms.uint16

-- | Lift a TypedTerm Int to a term-encoded uint16 literal
-- Example: uint16Lift (varPhantom "x" :: TypedTerm Int)
uint16Lift :: TypedTerm Int -> TypedTerm Term
uint16Lift = Core.termLiteral . Core.literalInteger . Core.integerValueUint16

-- | Create a term-encoded 32-bit unsigned integer literal
-- Example: uint32 4294967295
uint32 :: Int64 -> TypedTerm Term
uint32 = uint32Lift . TypedTerm . Terms.uint32

-- | Lift a TypedTerm Int64 to a term-encoded uint32 literal
uint32Lift :: TypedTerm Int64 -> TypedTerm Term
uint32Lift = Core.termLiteral . Core.literalInteger . Core.integerValueUint32

-- | Create a term-encoded 64-bit unsigned integer literal
-- Example: uint64 18446744073709551615
uint64 :: Integer -> TypedTerm Term
uint64 = uint64Lift . TypedTerm . Terms.uint64

-- | Lift a TypedTerm Integer to a term-encoded uint64 literal
-- Example: uint64Lift (varPhantom "x" :: TypedTerm Integer)
uint64Lift :: TypedTerm Integer -> TypedTerm Term
uint64Lift = Core.termLiteral . Core.literalInteger . Core.integerValueUint64

-- | Create a term-encoded 8-bit unsigned integer literal
-- Example: uint8 255
uint8 :: Int16 -> TypedTerm Term
uint8 = uint8Lift . TypedTerm . Terms.uint8

uint8Lift :: TypedTerm Int16 -> TypedTerm Term
uint8Lift = Core.termLiteral . Core.literalInteger . Core.integerValueUint8

-- | Create a term-encoded unit value
-- Example: unit
unit :: TypedTerm Term
unit = Core.termUnit

-- | Create a term-encoded unwrap function for a wrapped type
-- Example: unwrap (name "Email")
unwrap :: AsTerm t Name => t -> TypedTerm Term
unwrap = Core.termUnwrap . asTerm

-- | Create a term-encoded variable reference from a string
-- Example: var "x"
var :: String -> TypedTerm Term
var = Core.termVariable . name

-- | Create a term-encoded variable reference from a Name
-- Example: varName (Name "x")
varName :: Name -> TypedTerm Term
varName (Name n) = Core.termVariable $ TypedTerm $ Terms.string n

-- | Create a phantom-typed variable reference from a Name
-- Example: varNamePhantom (Name "x") :: TypedTerm Int
varNamePhantom :: Name -> TypedTerm a
varNamePhantom = TypedTerm . TermVariable

-- | Maps a string to a phantom-typed variable term
-- Example: varPhantom "x" :: TypedTerm Int
varPhantom :: String -> TypedTerm a
varPhantom = TypedTerm . TermVariable . Name

-- | Create a term-encoded wrapped term (newtype)
-- Example: wrap (name "Email") (string "user@example.com")
wrap :: AsTerm t Name => t -> TypedTerm Term -> TypedTerm Term
wrap n = Core.termWrap . Core.wrappedTerm (asTerm n)
