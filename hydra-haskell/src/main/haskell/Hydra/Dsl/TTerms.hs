-- | A domain-specific language for constructing term-encoded Hydra terms in Haskell;
--   these functions enable you to build terms (programs) which build terms.
module Hydra.Dsl.TTerms (
  module Hydra.Dsl.TBase,
  module Hydra.Dsl.TTerms,
) where

import Hydra.Kernel
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import Hydra.Dsl.TBase
import qualified Hydra.Dsl.Phantoms as Phantoms

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import Data.Int
import Prelude hiding (map, product, sum)


-- * Operators

-- | Function application operator for term-encoded terms
-- Example: fun @@ arg
(@@) :: TTerm Term -> TTerm Term -> TTerm Term
f @@ x = apply f x

-- * Fundamentals

-- | Apply a term-encoded function to a term-encoded argument
-- Example: apply (var "add") (int32 1)
apply :: TTerm Term -> TTerm Term -> TTerm Term
apply func arg = Core.termApplication $ Core.application func arg

-- | Create a term-encoded field with the given name and term-encoded value
-- Example: field "age" (int32 30)
field :: String -> TTerm Term -> TTerm Field
field s = Core.field (name s)

-- | Create a term-encoded let expression with multiple bindings
-- Example: lets ["x">: int32 1, "y">: int32 2] (var "add" @@ var "x" @@ var "y")
lets :: [(TTerm Name, TTerm Term)] -> TTerm Term -> TTerm Term
lets pairs body = Core.termLet $ Core.let_ (Phantoms.list $ toBinding pairs) body
  where
    toBinding = fmap (\(n, t) -> Core.letBinding n t Phantoms.nothing)

-- | Create a term-encoded variable reference from a string
-- Example: var "x"
var :: String -> TTerm Term
var = Core.termVariable . name

-- | Create a term-encoded variable reference from a Name
-- Example: varName (Name "x")
varName :: Name -> TTerm Term
varName (Name n) = Core.termVariable $ TTerm $ Terms.string n

-- | Maps a string to a phantom-typed variable term
-- Example: varPhantom "x" :: TTerm Int
varPhantom :: String -> TTerm a
varPhantom = TTerm . TermVariable . Name

-- | Create a phantom-typed variable reference from a Name
-- Example: varNamePhantom (Name "x") :: TTerm Int
varNamePhantom :: Name -> TTerm a
varNamePhantom = TTerm . TermVariable

-- * Functions

-- | Create a term-encoded constant function that always returns the same term-encoded value
-- Example: constant (int32 42)
constant :: TTerm Term -> TTerm Term
constant = lambda ignoredVariable

-- | Create a term-encoded lambda function with one parameter
-- Example: lambda "x" (var "add" @@ var "x" @@ int32 1)
lambda :: String -> TTerm Term -> TTerm Term
lambda var body = Core.termFunction $ Core.functionLambda $ Core.lambda (name var) Phantoms.nothing body

-- | Create a term-encoded multi-parameter lambda function (curried form)
-- Example: lambdas ["x", "y"] (var "add" @@ var "x" @@ var "y")
lambdas :: [String] -> TTerm Term -> TTerm Term
lambdas params body = case params of
  [] -> body
  (h:rest) -> Core.termFunction $ Core.functionLambda $ Core.lambda (name h) Phantoms.nothing $ lambdas rest body

-- | Create a term-encoded primitive function reference
-- Example: primitive (Name "hydra.lib.strings.length")
primitive :: Name -> TTerm Term
primitive = primitiveLift . TTerm . EncodeCore.name

primitiveLift :: TTerm Name -> TTerm Term
primitiveLift = Core.termFunction . Core.functionPrimitive

-- | Create a term-encoded field projection function
-- Example: project (name "Person") (name "firstName")
project :: TTerm Name -> TTerm Name -> TTerm Term
project tname fname = Core.termFunction $ Core.functionElimination $ Core.eliminationRecord
  $ Core.projection tname fname

-- | Create a term-encoded unwrap function for a wrapped type
-- Example: unwrap (name "Email")
unwrap :: TTerm Name -> TTerm Term
unwrap = Core.termFunction . Core.functionElimination . Core.eliminationWrap

-- * Literal values

-- | Create a term-encoded unlimited precision integer
-- Example: bigint 42
bigint :: Integer -> TTerm Term
bigint = bigintLift . TTerm . Terms.bigint

-- | Lift a TTerm Integer to a term-encoded bigint literal
-- Example: bigintLift (varPhantom "x" :: TTerm Integer)
bigintLift :: TTerm Integer -> TTerm Term
bigintLift = Core.termLiteral . Core.literalInteger . Core.integerValueBigint

-- | Create a term-encoded boolean literal
-- Example: boolean True
boolean :: Bool -> TTerm Term
boolean = booleanLift . TTerm . Terms.boolean

-- | Lift a TTerm Bool to a term-encoded boolean literal
-- Example: booleanLift $ Phantoms.true
booleanLift :: TTerm Bool -> TTerm Term
booleanLift = Core.termLiteral . Core.literalBoolean

-- | Term-encoded boolean false literal
false :: TTerm Term
false = boolean False

-- | Create a term-encoded unlimited precision floating point literal
-- Example: bigfloat 3.14159
bigfloat :: Double -> TTerm Term
bigfloat = bigfloatLift . TTerm . Terms.bigfloat

-- | Lift a TTerm Double to a term-encoded bigfloat literal
bigfloatLift :: TTerm Double -> TTerm Term
bigfloatLift = Core.termLiteral . Core.literalFloat . Core.floatValueBigfloat

-- | Create a term-encoded 32-bit floating point literal
-- Example: float32 3.14
float32 :: Float -> TTerm Term
float32 = float32Lift . TTerm . Terms.float32

-- | Lift a TTerm Float to a term-encoded float32 literal
-- Example: float32Lift (varPhantom "x" :: TTerm Float)
float32Lift :: TTerm Float -> TTerm Term
float32Lift = Core.termLiteral . Core.literalFloat . Core.floatValueFloat32

-- | Create a term-encoded 64-bit floating point literal
-- Example: float64 3.14159
float64 :: Double -> TTerm Term
float64 = float64Lift . TTerm . Terms.float64

-- | Lift a TTerm Float to a term-encoded float64 literal
-- Example: float64Lift (varPhantom "x" :: TTerm Float)
float64Lift :: TTerm Double -> TTerm Term
float64Lift = Core.termLiteral . Core.literalFloat . Core.floatValueFloat64

-- | Create a term-encoded 8-bit signed integer literal
-- Example: int8 127
int8 :: Int8 -> TTerm Term
int8 = int8Lift . TTerm . Terms.int8

-- | Lift a TTerm Int8 to a term-encoded int8 literal
-- Example: int8Lift (varPhantom "x" :: TTerm Int8)
int8Lift :: TTerm Int8 -> TTerm Term
int8Lift = Core.termLiteral . Core.literalInteger . Core.integerValueInt8

-- | Create a term-encoded 16-bit signed integer literal
-- Example: int16 32767
int16 :: Int16 -> TTerm Term
int16 = int16Lift . TTerm . Terms.int16

-- | Lift a TTerm Int16 to a term-encoded int16 literal
-- Example: int16Lift (varPhantom "x" :: TTerm Int16)
int16Lift :: TTerm Int16 -> TTerm Term
int16Lift = Core.termLiteral . Core.literalInteger . Core.integerValueInt16

-- | Create a term-encoded 32-bit signed integer literal
-- Example: int32 42
int32 :: Int -> TTerm Term
int32 = int32Lift . TTerm . Terms.int32

-- | Lift a TTerm Int to a term-encoded int32 literal
-- Example: int32Lift (varPhantom "x" :: TTerm Int)
int32Lift :: TTerm Int -> TTerm Term
int32Lift = Core.termLiteral . Core.literalInteger . Core.integerValueInt32

-- | Create a term-encoded 64-bit signed integer literal
-- Example: int64 9223372036854775807
int64 :: Int64 -> TTerm Term
int64 = int64Lift . TTerm . Terms.int64

-- | Lift a TTerm Int64 to a term-encoded int64 literal
-- Example: int64Lift (varPhantom "x" :: TTerm Int64)
int64Lift :: TTerm Int64 -> TTerm Term
int64Lift = Core.termLiteral . Core.literalInteger . Core.integerValueInt64

-- | Create a term-encoded string literal
-- Example: string "hello world"
string :: String -> TTerm Term
string = stringLift . TTerm . Terms.string

-- | Lift a TTerm String to a term-encoded string literal
-- Example: stringLift $ Phantoms.string "hello world"
stringLift :: TTerm String -> TTerm Term
stringLift = Core.termLiteral . Core.literalString

-- | Term-encoded boolean true literal
true :: TTerm Term
true = boolean True

-- | Create a term-encoded 8-bit unsigned integer literal
-- Example: uint8 255
uint8 :: Int16 -> TTerm Term
uint8 = uint8Lift . TTerm . Terms.uint8

uint8Lift :: TTerm Int16 -> TTerm Term
uint8Lift = Core.termLiteral . Core.literalInteger . Core.integerValueUint8

-- | Create a term-encoded 16-bit unsigned integer literal
-- Example: uint16 65535
uint16 :: Int -> TTerm Term
uint16 = uint16Lift . TTerm . Terms.uint16

-- | Lift a TTerm Int to a term-encoded uint16 literal
-- Example: uint16Lift (varPhantom "x" :: TTerm Int)
uint16Lift :: TTerm Int -> TTerm Term
uint16Lift = Core.termLiteral . Core.literalInteger . Core.integerValueUint16

-- | Create a term-encoded 32-bit unsigned integer literal
-- Example: uint32 4294967295
uint32 :: Int64 -> TTerm Term
uint32 = uint32Lift . TTerm . Terms.uint32

-- | Lift a TTerm Int64 to a term-encoded uint32 literal
uint32Lift :: TTerm Int64 -> TTerm Term
uint32Lift = Core.termLiteral . Core.literalInteger . Core.integerValueUint32

-- | Create a term-encoded 64-bit unsigned integer literal
-- Example: uint64 18446744073709551615
uint64 :: Integer -> TTerm Term
uint64 = uint64Lift . TTerm . Terms.uint64

-- | Lift a TTerm Integer to a term-encoded uint64 literal
-- Example: uint64Lift (varPhantom "x" :: TTerm Integer)
uint64Lift :: TTerm Integer -> TTerm Term
uint64Lift = Core.termLiteral . Core.literalInteger . Core.integerValueUint64

-- * Collections

-- | Create a term-encoded 'Just' optional value
-- Example: just (string "found")
just :: TTerm Term -> TTerm (Maybe Term)
just = Phantoms.just

-- | Create a term-encoded list
-- Example: list [int32 1, int32 2, int32 3]
list :: [TTerm Term] -> TTerm Term
list = Core.termList . Phantoms.list

-- | Create a term-encoded map/dictionary
-- Example: map (fromList [(string "key", int32 42)])
map :: TTerm (M.Map Term Term) -> TTerm Term
map = Core.termMap

-- | Create a term-encoded 'Nothing' optional value
nothing :: TTerm (Maybe Term)
nothing = Phantoms.nothing

-- | Create a term-encoded optional value from a Maybe
-- Example: optional (just (int32 42))
optional :: TTerm (Maybe Term) -> TTerm Term
optional = Core.termOptional

-- | Create a term-encoded set
-- Example: set [string "a", string "b", string "c"]
set :: [TTerm Term] -> TTerm Term
set els = Core.termSet $ TTerm $ TermSet $ S.fromList (unTTerm <$> els)

-- * Products and tuples

-- | Create a term-encoded pair (2-tuple)
-- Example: pair (string "name") (int32 42)
pair :: TTerm Term -> TTerm Term -> TTerm Term
pair t1 t2 = tuple [t1, t2]

---- | Create a term-encoded product (tuple) with multiple components
---- Example: product [string "name", int32 42, boolean True]
--product :: [TTerm Term] -> TTerm Term
--product terms = Core.termProduct $ TTerm $ TermList (unTTerm <$> terms)

-- | Create a term-encoded tuple with multiple components
-- Example: tuple [string "name", int32 42, boolean True]
tuple :: [TTerm Term] -> TTerm Term
tuple = Core.termProduct . Phantoms.list

-- | Create a term-encoded tuple projection function
-- Example: untuple 3 1 extracts the second element of a 3-tuple
untuple :: Int -> Int -> TTerm Term
untuple arity idx = Core.termFunction $ Core.functionElimination $ Core.eliminationProduct
  $ Core.tupleProjection (Phantoms.int32 arity) (Phantoms.int32 idx) Phantoms.nothing

-- * Records and unions

-- | Create a term-encoded union injection
-- Example: inject (name "Result") "success" (int32 42)
inject :: TTerm Name -> String -> TTerm Term -> TTerm Term
inject tname fname = Core.termUnion . Core.injection tname . Core.field (name fname)

-- | Create a term-encoded pattern match on a union
-- Example: match (name "Result") nothing ["success">: int32 42, "error">: string "fail"]
match :: TTerm Name -> TTerm (Maybe Term) -> [(TTerm Name, TTerm Term)] -> TTerm Term
match tname def pairs = Core.termFunction $ Core.functionElimination $ Core.eliminationUnion
    $ Core.caseStatement tname def $ Phantoms.list $ toField pairs
  where
    toField = fmap (\(n, t) -> Core.field n t)

-- | Create a term-encoded record with named fields
-- Example: record (name "Person") ["name">: string "John", "age">: int32 30]
record :: TTerm Name -> [(TTerm Name, TTerm Term)] -> TTerm Term
record name pairs = Core.termRecord $ Core.record name $ Phantoms.list (toField <$> pairs)
  where
    toField (n, t) = Core.field n t

-- | Create a term-encoded sum type instance
-- Example: sum 0 3 (int32 1) represents the first element of a 3-element sum
sum :: Int -> Int -> TTerm Term -> TTerm Term
sum i s = Core.termSum . Core.sum (Phantoms.int32 i) (Phantoms.int32 s)

unitVariantPhantom :: Name -> Name -> TTerm Term
unitVariantPhantom tname fname = variantPhantom tname fname $ record (Core.nameLift _Unit) []

variantPhantom :: Name -> Name -> TTerm Term -> TTerm Term
variantPhantom tname fname term = Core.termUnion $ Core.injection (Core.nameLift tname) $ Core.field (Core.nameLift fname) term

-- | Create a term-encoded wrapped term (newtype)
-- Example: wrap (name "Email") (string "user@example.com")
wrap :: TTerm Name -> TTerm Term -> TTerm Term
wrap name = Core.termWrap . Core.wrappedTerm name
