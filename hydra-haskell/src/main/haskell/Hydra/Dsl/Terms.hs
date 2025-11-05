-- | A domain-specific language for constructing Hydra terms in Haskell.
module Hydra.Dsl.Terms where

import Hydra.Constants
import Hydra.Core
import Hydra.Graph
import Hydra.Mantle
import Hydra.Dsl.Common
import qualified Hydra.Dsl.Literals as Literals

import Prelude hiding (map, product, sum)
import Data.Int
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


-- Operators

-- | Function composition operator: f <.> g creates a function that applies g then f
-- Example: var "stringLength" <.> var "toString"
(<.>) :: Term -> Term -> Term
f <.> g = compose f g

-- | Term application operator: function @@ argument
-- Example: var "add" @@ int32 1
(@@) :: Term -> Term -> Term
fun @@ arg = apply fun arg

-- | Field definition operator: name>: value
-- Example: "name">: string "John"
infixr 0 >:
(>:) :: String -> Term -> Field
name>: term = field name term


-- | Attach an annotation to a term
-- Example: annot (M.fromList [(Name "comment", string "A User ID")]) (var "userId")
annot :: M.Map Name Term -> Term -> Term
annot ann term = TermAnnotated $ AnnotatedTerm term ann

-- | Attach an annotation to a term
-- Example: annotated (var "userId") (M.fromList [(Name "comment", string "A User ID")])
annotated :: Term -> M.Map Name Term -> Term
annotated term ann = TermAnnotated $ AnnotatedTerm term ann

-- | Apply a function term to an argument
-- Example: apply (var "capitalize") (string "arthur")
apply :: Term -> Term -> Term
apply fun arg = TermApplication $ Application fun arg

applyAll :: Term -> [Term] -> Term
applyAll fun args = foldl apply fun args

-- | Create a binary data literal. The encoding scheme is currently application-dependent.
-- Example: binary ""\x48\x65\x00\xff\x20\x7a\x1b\x80"
binary :: String -> Term
binary = literal . Literals.binary

-- | Create a bigfloat literal. Note: in practice, precision is limited to 64 bits (same as Double) in Haskell.
-- Example: bigfloat 3.14159265359
bigfloat :: Double -> Term
bigfloat = literal . Literals.bigfloat

-- | Create a bigint literal
-- Example: bigint 9223372036854775808
bigint :: Integer -> Term
bigint = literal . Literals.bigint

-- | Create a boolean literal
-- Example: boolean True
boolean :: Bool -> Term
boolean = literal . Literals.boolean

char :: Char -> Term
char = int32 . C.ord

comparison :: Comparison -> Term
comparison t = case t of
  ComparisonEqualTo -> unitVariant _Comparison _Comparison_equalTo
  ComparisonLessThan -> unitVariant _Comparison _Comparison_lessThan
  ComparisonGreaterThan -> unitVariant _Comparison _Comparison_greaterThan

-- | Compose two functions (apply g then f) to create a new function
-- Example: compose (var "stringLength") (var "toString")
-- This creates a function equivalent to \x -> stringLength(toString(x))
-- Function composition applies right-to-left: (f ∘ g)(x) = f(g(x))
compose :: Term -> Term -> Term
compose f g = lambda "arg_" $ apply f (apply g $ var "arg_")

-- | Create a constant function that always returns the same value
-- Example: constant true
constant :: Term -> Term
constant = lambda ignoredVariable

-- | Boolean false literal
false :: Term
false = boolean False

-- | Create a field with the given name and value
-- Example: field "age" (int32 30)
field :: String -> Term -> Field
field n = Field (Name n)

-- | First element projection function for pairs
first :: Term
first = untuple 2 0

-- | Create a floating-point literal with specified precision
-- Example: float (FloatValueFloat32 3.14)
float :: FloatValue -> Term
float = literal . Literals.float

-- | Create a float32 literal
-- Example: float32 3.14
float32 :: Float -> Term
float32 = literal . Literals.float32

-- | Create a float64 literal
-- Example: float64 3.14159265359
float64 :: Double -> Term
float64 = literal . Literals.float64

-- | Identity function
identity :: Term
identity = lambda "x_" $ var "x_"

-- | Create a union value by injecting a value into a specific variant
-- Example: inject (Name "Result") ("success">: int32 42)
-- This creates a "Result" union with the "success" variant containing value 42
-- Use this to construct values of union types at runtime
inject :: Name -> Field -> Term
inject tname = TermUnion . Injection tname

-- | Create an int8 literal
-- Example: int8 127
int8 :: Int8 -> Term
int8 = literal . Literals.int8

-- | Create an int16 literal
-- Example: int16 32767
int16 :: Int16 -> Term
int16 = literal . Literals.int16

-- | Create an int32 literal
-- Example: int32 42
int32 :: Int -> Term
int32 = literal . Literals.int32

-- | Create an int64 literal
-- Example: int64 9223372036854775807
int64 :: Int64 -> Term
int64 = literal . Literals.int64

-- | Create an integer literal with specified bit width
-- Example: integer (IntegerValueInt32 42)
integer :: IntegerValue -> Term
integer = literal . Literals.integer

-- | Create a 'Just' optional value
-- Example: just (string "found")
just :: Term -> Term
just = optional . Just

-- | Create a lambda function with one parameter
-- Example: lambda "x" (var "x" @@ int32 1)
lambda :: String -> Term -> Term
lambda param body = TermFunction $ FunctionLambda $ Lambda (Name param) Nothing body

-- | Create a multi-parameter lambda function (curried)
-- Example: lambdas ["x", "y"] (var "add" @@ var "x" @@ var "y")
-- This creates the function \x.\y.add x y
lambdas :: [String] -> Term -> Term
lambdas params body = case params of
  [] -> body
  (h:r) -> lambda h $ lambdas r body

-- | Create a lambda function with a given domain
-- Example: lambdaTyped "x" Types.int32 (list [var "x"])
lambdaTyped :: String -> Type -> Term -> Term
lambdaTyped param dom body = TermFunction $ FunctionLambda $ Lambda (Name param) (Just dom) body

-- | Create a let term with any number of bindings
-- Example: lets ["x">: int32 1, "y">: int32 2] (pair (var "x") (var "y"))
lets :: [Field] -> Term -> Term
lets bindings env = TermLet $ Let (toBinding <$> bindings) env
  where
    toBinding (Field name value) = Binding name value Nothing

letsTyped :: [(String, Term, TypeScheme)] -> Term -> Term
letsTyped bindings env = TermLet $ Let (toBinding <$> bindings) env
  where
    toBinding (name, value, ts) = Binding (Name name) value (Just ts)

-- | Create a list of terms
-- Example: list [int32 1, int32 2, int32 3]
list :: [Term] -> Term
list = TermList

-- | Create a term from a literal value
-- Example: literal (LiteralString "hello")
literal :: Literal -> Term
literal = TermLiteral

-- | Create a map/dictionary term
-- Example: map (M.fromList [(string "January", int32 31), (string "February", int32 28)])
map :: M.Map Term Term -> Term
map = TermMap

-- | Create a pattern match on a union type
-- Example: match (Name "Result") (Just (string "unknown"))
--               ["success">: lambda "s" (var "processSuccess" @@ var "s"),
--                "error">: lambda "e" (var "handleError" @@ var "e")]
-- This allows handling different cases of a union type with specific logic for each variant.
-- The optional second parameter provides a default case for any unmatched variants.
match :: Name -> Maybe Term -> [Field] -> Term
match tname def fields = TermFunction $ FunctionElimination $ EliminationUnion $ CaseStatement tname def fields

-- | Create a pattern match using variant name pairs
-- Example: matchWithVariants (Name "Result") Nothing [(Name "success", Name "handleSuccess"), (Name "error", Name "handleError")]
matchWithVariants :: Name -> Maybe Term -> [(Name, Name)] -> Term
matchWithVariants tname def pairs = match tname def (toField <$> pairs)
  where
    toField (from, to) = Field from $ constant $ unitVariant tname to

-- | Create a 'Nothing' optional value
nothing :: Term
nothing = optional Nothing

-- | Create an optional (nullable) term
-- Example: optional (Just (string "found"))
optional :: Maybe Term -> Term
optional = TermMaybe

-- | Create a pair (2-tuple)
-- Example: pair (string "name") (int32 42)
pair :: Term -> Term -> Term
pair a b = TermProduct [a, b]

-- | Create a primitive function
-- Example: primitive (Name "hydra.lib.strings.length")
primitive :: Name -> Term
primitive = TermFunction . FunctionPrimitive

-- | Create a field projection function
-- Example: project (Name "Person") (Name "firstName")
project :: Name -> Name -> Term
project tname fname = TermFunction $ FunctionElimination $ EliminationRecord $ Projection tname fname

-- | Create a record with named fields of the specified type
-- Example: record (Name "Person") [
--            "name">: string "John",
--            "age">: int32 30,
--            "email">: string "john@example.com"]
-- Records are products of named fields with values that can be accessed by field name
record :: Name -> [Field] -> Term
record tname fields = TermRecord $ Record tname fields

-- | Second element projection function for pairs
second :: Term
second = untuple 2 1

-- | Create a set of terms
-- Example: set (S.fromList [string "a", string "b", string "c"])
set :: S.Set Term -> Term
set = TermSet

-- | Create a string literal
-- Example: string "hello world"
string :: String -> Term
string = TermLiteral . LiteralString

-- | Create a sum term
-- Example: sum 0 3 (int32 1) represents the first element of a 3-element sum
sum :: Int -> Int -> Term -> Term
sum idx arity term = TermSum $ Sum idx arity term

triple :: Term -> Term -> Term -> Term
triple a b c = tuple [a, b, c]

-- | Boolean true literal
true :: Term
true = boolean True

-- | Create a product (tuple) with multiple components
-- Example: product [string "name", int32 42, true]
tuple :: [Term] -> Term
tuple = TermProduct

tuple4 :: Term -> Term -> Term -> Term -> Term
tuple4 a b c d = tuple [a, b, c, d]

tuple5 :: Term -> Term -> Term -> Term -> Term -> Term
tuple5 a b c d e = tuple [a, b, c, d, e]

tyapp :: Term -> Type -> Term
tyapp term typ = TermTypeApplication $ TypeApplicationTerm term typ

tyapps = typeApplication

tylam :: String -> Term -> Term
tylam var body = TermTypeLambda $ TypeLambda (Name var) body

tylams :: [String] -> Term -> Term
tylams vars body = L.foldl (\b v -> TermTypeLambda $ TypeLambda (Name v) b) body $ L.reverse vars

-- | Create a type abstraction (universal quantification)
-- Example: typeLambda [Name "a", Name "b"] (lambdaTyped "f" (Types.function (Types.var "a") (Types.var "b"))
--                                               (lambdaTyped "x" (Types.var "a") (var "f" @@ var "x")))
-- This creates a polymorphic term with type variables.
-- The example creates a higher-order function with type 'forall a b. (a -> b) -> a -> b',
-- which is the polymorphic apply function that works for any types a and b.
typeLambda :: [Name] -> Term -> Term
typeLambda vars body = L.foldl (\b v -> TermTypeLambda $ TypeLambda v b) body vars

-- | Apply type arguments to a polymorphic term
-- Example: typeApplication (var "map") [Types.int32, Types.string]
-- This instantiates a polymorphic function with concrete types.
-- For instance, if 'map' has type 'forall a b. (a -> b) -> list a -> list b',
-- the example would instantiate it to '(int32 -> string) -> list int32 -> list string'.
typeApplication :: Term -> [Type] -> Term
typeApplication term types = L.foldl (\t ty -> TermTypeApplication $ TypeApplicationTerm t ty) term types

-- | Create a uint8 literal
-- Example: uint8 255
uint8 :: Int16 -> Term
uint8 = literal . Literals.uint8

-- | Create a uint16 literal
-- Example: uint16 65535
uint16 :: Int -> Term
uint16 = literal . Literals.uint16

-- | Create a uint32 literal
-- Example: uint32 4294967295
uint32 :: Int64 -> Term
uint32 = literal . Literals.uint32

-- | Create a uint64 literal
-- Example: uint64 18446744073709551615
uint64 :: Integer -> Term
uint64 = literal . Literals.uint64

-- | Unit value (empty record)
unit :: Term
unit = TermUnit

-- | Create a unit variant of a union
-- Example: unitVariant (Name "Result") (Name "success")
unitVariant :: Name -> Name -> Term
unitVariant tname fname = variant tname fname unit

-- | Create a untyped tuple projection function
-- Example: untuple 3 1 Nothing extracts the second element of a 3-tuple
untuple :: Int -> Int -> Term
untuple arity idx = TermFunction $ FunctionElimination $ EliminationProduct $ TupleProjection arity idx Nothing

-- | Create an unwrap function for a wrapped type
-- Example: unwrap (Name "Email")
unwrap :: Name -> Term
unwrap = TermFunction . FunctionElimination . EliminationWrap

-- | Create a variable reference
-- Example: var "x"
var :: String -> Term
var = TermVariable . Name

-- | Create a union variant
-- Example: variant (Name "Result") (Name "success") (string "ok")
variant :: Name -> Name -> Term -> Term
variant tname fname term = TermUnion $ Injection tname $ Field fname term

-- | Create a constant function that produces a unit variant
-- Example: withVariant (Name "Result") (Name "success")
withVariant :: Name -> Name -> Term
withVariant tname = constant . unitVariant tname

-- | Create a wrapped term
-- Example: wrap (Name "Email") (string "user@example.com")
wrap :: Name -> Term -> Term
wrap name term = TermWrap $ WrappedTerm name term
