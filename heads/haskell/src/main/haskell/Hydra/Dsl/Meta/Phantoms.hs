{-# LANGUAGE FlexibleContexts #-}

-- | Term-level DSL which makes use of phantom types. Use this DSL for defining programs as opposed to data type definitions.
-- The phantom types provide static type checking in Haskell prior to Hydra's runtime type checking.
module Hydra.Dsl.Meta.Phantoms (
  module Hydra.Dsl.Meta.Phantoms,
  module Hydra.Dsl.Meta.Literals,
  module Hydra.Dsl.AsTerm,
) where

import Hydra.Kernel
import Hydra.Dsl.AsTerm
import Hydra.Dsl.Meta.Common
import Hydra.Dsl.Meta.Literals
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Annotations as Ann
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Show.Core as ShowCore
import Hydra.Encoding (encodeBindingName)
import Hydra.Decoding (decodeBindingName)

import Prelude hiding ((++))
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


-- Operators

infixr 0 ~>
(~>) :: String -> TypedTerm x -> TypedTerm (a -> b)
name ~> body = lambda name body

infixl 1 <~
(<~) :: String -> TypedTerm a -> TypedTerm b -> TypedTerm b
name <~ value = let1 name value

infixl 1 <<~
(<<~) :: AsTerm t (Either e a) => String -> t -> TypedTerm (Either e b) -> TypedTerm (Either e b)
name <<~ def = eitherBind name (asTerm def)

-- | Function composition operator: f <.> g creates a function that applies g then f
-- Example: toString <.> increment
-- Accepts TypedTerm or TypedBinding for both operands (via AsTerm)
(<.>) :: (AsTerm f (b -> c), AsTerm g (a -> b)) => f -> g -> TypedTerm (a -> c)
f <.> g = compose (asTerm f) (asTerm g)

-- | Function application operator: function @@ argument
-- Example: add @@ int32 1
-- Accepts TypedTerm or TypedBinding for both operands (via AsTerm)
(@@) :: (AsTerm f (a -> b), AsTerm g a) => f -> g -> TypedTerm b
fun @@ arg = apply (asTerm fun) (asTerm arg)

(++) :: (AsTerm f String, AsTerm g String) => f -> g -> TypedTerm String
f ++ g = primitive2 _strings_cat2 (asTerm f) (asTerm g)

-- | Field definition operator for records: name>: value
-- Example: "name">: string "John"
-- Accepts TypedTerm or TypedBinding (via AsTerm)
infixr 0 >:
(>:) :: AsTerm t a => String -> t -> Field
name>: term = Field (Name name) (unTypedTerm $ asTerm term)

-- | Field definition operator with pre-constructed name: fname>>: value
-- Example: _Person_name>>: string "John"
-- Accepts TypedTerm or TypedBinding (via AsTerm)
infixr 0 >>:
(>>:) :: AsTerm t a => Name -> t -> Field
fname >>: d = field fname (asTerm d)

-- | Add an annotation to a term
-- Example: annot (Name "deprecated") (Just (boolean True)) myFunction
annot :: Name -> Maybe Term -> TypedTerm a -> TypedTerm a
annot key mvalue (TypedTerm term) = TypedTerm $ Ann.annotateTerm key mvalue term

-- | Apply a function to an argument
-- Example: apply (var "add") (int32 1)
apply :: TypedTerm (a -> b) -> TypedTerm a -> TypedTerm b
apply (TypedTerm lhs) (TypedTerm rhs) = TypedTerm $ Terms.apply lhs rhs



-- | Apply a named case match to an argument
-- Example: cases resultTypeName myResult Nothing [onSuccess, onError]
-- See also: 'match'
cases :: Name -> TypedTerm a -> Maybe (TypedTerm b) -> [Field] -> TypedTerm b
cases name arg dflt fields = TypedTerm $ Terms.apply (Terms.match name (unTypedTerm <$> dflt) fields) (unTypedTerm arg)

-- | Compose two functions (g then f)
-- Example: compose (var "stringLength") (var "toString")
-- Accepts TypedTerm or TypedBinding for both operands (via AsTerm)
compose :: (AsTerm f (b -> c), AsTerm g (a -> b)) => f -> g -> TypedTerm (a -> c)
compose f g = TypedTerm $ Terms.compose (unTypedTerm $ asTerm f) (unTypedTerm $ asTerm g)

-- | Create a constant function that always returns the same value
-- Example: constant true
-- Accepts TypedTerm or TypedBinding (via AsTerm)
constant :: AsTerm t a => t -> TypedTerm (b -> a)
constant t = TypedTerm $ Terms.constant (unTypedTerm $ asTerm t)

-- | Get a reference to a decoder function in hydra.decode.core for a given type name
-- The decoder takes a graph context and a term, returning either a decoding error or the decoded value
decoderFor :: Name -> TypedTerm (Graph -> Term -> Prelude.Either DecodingError a)
decoderFor typeName = var $ unName $ decodeBindingName typeName

-- | Create a definition in a module
-- Example: definitionInModule myModule "addInts" (lambda "x" (lambda "y" (add @@ var "x" @@ var "y")))
definitionInModule :: Module -> String -> TypedTerm a -> TypedTermDefinition a
definitionInModule mod = definitionInModuleName $ moduleName mod

-- | Create a definition in a namespace
-- Example: definitionInModuleName (ModuleName "com.example") "addInts" myFunction
definitionInModuleName :: ModuleName -> String -> TypedTerm a -> TypedTermDefinition a
definitionInModuleName ns lname = TypedTermDefinition $ unqualifyName $ QualifiedName (Just ns) lname

-- | Add documentation to a term
-- Example: doc "Adds two integers" addFunction
-- Accepts TypedTerm or TypedBinding (via AsTerm)
doc :: AsTerm t a => String -> t -> TypedTerm a
doc s t = TypedTerm $ setTermDescription (Just s) (unTypedTerm $ asTerm t)

-- | Add documentation with line wrapping at the specified width
-- Example: docWrapped 80 "This is a long documentation string that will be wrapped..." myFunction
docWrapped :: Int -> String -> TypedTerm a -> TypedTerm a
docWrapped len = doc . wrapLine len

-- | Bind over Either: extracts the Right value into a variable, short-circuiting on Left
eitherBind :: AsTerm t (Either e a) => String -> t -> TypedTerm (Either e b) -> TypedTerm (Either e b)
eitherBind v def body = primitive2 _eithers_bind (asTerm def) $ lambda v $ body

-- | Convert a typed element to an untyped element (legacy name, prefer 'toBinding')
-- Example: el (definitionInModule myModule "addInts" myFunction)
el :: TypedBinding a -> Binding
el = toBinding

encodedBinary :: TypedTerm String -> TypedTerm Term
encodedBinary = encodedLiteral . inject _Literal _Literal_binary

encodedBoolean :: TypedTerm Bool -> TypedTerm Term
encodedBoolean = encodedLiteral . inject _Literal _Literal_boolean

encodedCase :: AsTerm t (a -> Term) => Name -> Name -> t -> Field
encodedCase tname fname enc = field fname $ "v" ~> encodedVariant tname fname (enc @@ var "v")

encodedEither :: TypedTerm (Prelude.Either a b) -> TypedTerm Term
encodedEither = inject _Term _Term_either

encodedField :: Name -> TypedTerm Term -> TypedTerm Term
encodedField fname term = encodedFieldRaw (encodedName fname) term

encodedFieldRaw :: TypedTerm Name -> TypedTerm Term -> TypedTerm Term
encodedFieldRaw (TypedTerm fname) (TypedTerm term) = TypedTerm $ Terms.record _Field [
  Field _Field_name fname,
  Field _Field_term term]

encodedFloatValue :: TypedTerm FloatValue -> TypedTerm Term
encodedFloatValue = encodedLiteral . inject _Literal _Literal_float

encodedInjection :: Name -> Name -> TypedTerm Term -> TypedTerm Term
encodedInjection tname fname term = TypedTerm $ Terms.record _Injection [
  field _Injection_typeName $ encodedName tname,
  field _Injection_field $ encodedField fname term]

encodedInt32 :: TypedTerm Int -> TypedTerm Term
encodedInt32 v = encodedIntegerValue $ inject _IntegerValue _IntegerValue_int32 v

encodedIntegerValue :: TypedTerm IntegerValue -> TypedTerm Term
encodedIntegerValue = encodedLiteral . inject _Literal _Literal_integer

encodedList :: TypedTerm [a] -> TypedTerm Term
encodedList = inject _Term _Term_list

encodedLiteral :: TypedTerm Literal -> TypedTerm Term
encodedLiteral = inject _Term _Term_literal

encodedMap :: TypedTerm (M.Map k v) -> TypedTerm Term
encodedMap = inject _Term _Term_map

encodedName :: Name -> TypedTerm Name
encodedName = wrap _Name . string . unName

encodedOptional :: TypedTerm (Maybe a) -> TypedTerm Term
encodedOptional = inject _Term _Term_maybe

encodedPair :: TypedTerm (a, b) -> TypedTerm Term
encodedPair = inject _Term _Term_pair

encodedRecord :: Name -> [Field] -> TypedTerm Term
encodedRecord tname fields = TypedTerm $ Terms.inject _Term _Term_record $ Terms.record _Record [
    field _Record_typeName $ encodedName tname,
    field _Record_fields $ list (encField <$> fields)]
  where
    encField (Field fname term) = encodedField fname $ TypedTerm term

encodedSet :: TypedTerm (S.Set a) -> TypedTerm Term
encodedSet = inject _Term _Term_set

encodedString :: TypedTerm String -> TypedTerm Term
encodedString = encodedLiteral . inject _Literal _Literal_string

encodedUnion :: TypedTerm Term -> TypedTerm Term
encodedUnion = inject _Term _Term_inject

encodedUnit :: TypedTerm Term
encodedUnit = injectUnit _Term _Term_unit

encodedVariant :: Name -> Name -> TypedTerm Term -> TypedTerm Term
encodedVariant tname fname term = encodedUnion $ encodedInjection tname fname term

encodedWrappedTerm :: Name -> TypedTerm Term -> TypedTerm Term
encodedWrappedTerm name = encodedWrappedTermRaw (encodedName name)

encodedWrappedTermRaw :: TypedTerm Name -> TypedTerm Term -> TypedTerm Term
encodedWrappedTermRaw (TypedTerm name) (TypedTerm term) = TypedTerm $ Terms.inject _Term _Term_wrap $ Terms.record _WrappedTerm [
  Field _WrappedTerm_typeName name,
  Field _WrappedTerm_body term]

-- | Get a reference to an encoder function in hydra.encode.core for a given type name
encoderFor :: Name -> TypedTerm (a -> Term)
encoderFor typeName = var $ unName $ encodeBindingName typeName

-- | Create a field with the given name and value
-- Example: field (Name "age") (int32 30)
field :: Name -> TypedTerm a -> Field
field fname (TypedTerm val) = Field fname val

-- | Mark a type as first-class
-- Example: firstClassType (record ...)
firstClassType :: TypedTerm Type -> TypedTerm Type
firstClassType typ = annot keyFirstClassType (Just $ Terms.boolean True) typ

-- | Create a fold function to process lists
-- Example: fold (lambda "acc" (lambda "x" (add @@ var "acc" @@ var "x")))
fold :: AsTerm t (b -> a -> b) => t -> TypedTerm (b -> [a] -> b)
fold f = (primitive _lists_foldl) @@ asTerm f

-- | Identity function that returns its argument unchanged
-- Example: identity
identity :: TypedTerm (a -> a)
identity = TypedTerm Terms.identity

-- | Create a union injection
-- Example: inject (Name "Result") (Name "success") (string "ok")
inject :: (AsName t, AsName f) => t -> f -> TypedTerm a -> TypedTerm b
inject n fn (TypedTerm term) = TypedTerm $ Terms.inject (asName n) (asName fn) term

-- | Create a function that injects its argument into a union variant
-- Example: injectLambda (Name "Result") (Name "success")
injectLambda :: Name -> Name -> TypedTerm (a -> b)
injectLambda name fname = lambda "injected_" $ inject name fname $ var "injected_"

-- | Create a unit variant of a union
-- Example: injectUnit (Name "Result") (Name "success")
injectUnit :: (AsName t, AsName f) => t -> f -> TypedTerm a
injectUnit n fn = TypedTerm $ Terms.inject (asName n) (asName fn) Terms.unit

-- | Create a 'Just' optional value
-- Example: just (string "found")
-- Accepts TypedTerm or TypedBinding (via AsTerm)
just :: AsTerm t a => t -> TypedTerm (Maybe a)
just t = TypedTerm $ Terms.just (unTypedTerm $ asTerm t)

-- | Function that wraps a value in 'Just'
-- Example: just_ @@ myValue
just_ :: TypedTerm (a -> Maybe a)
just_ = TypedTerm $ Terms.lambda "just_" $ Terms.just $ Terms.var "just_"

-- | Create a lambda function with one parameter
-- Example: lambda "x" (var "add" @@ var "x" @@ int32 1)
lambda :: String -> TypedTerm x -> TypedTerm (a -> b)
lambda v (TypedTerm body) = TypedTerm $ Terms.lambda v body

-- | Create a multi-parameter lambda function
-- Example: lambdas ["x", "y"] (add @@ var "x" @@ var "y")
lambdas :: [String] -> TypedTerm x -> TypedTerm (a -> b)
lambdas params (TypedTerm body) = TypedTerm $ Terms.lambdas params body

-- | Create a 'Left' either value
-- Example: left (string "error")
left :: TypedTerm a -> TypedTerm (Either a b)
left (TypedTerm term) = TypedTerm $ Terms.left term

-- | Function that wraps a value in 'Left'
-- Example: left_ @@ myValue
left_ :: TypedTerm (a -> Either a b)
left_ = TypedTerm $ Terms.lambda "left_" $ Terms.left $ Terms.var "left_"

let1 :: String -> TypedTerm a -> TypedTerm b -> TypedTerm b
let1 name (TypedTerm value) (TypedTerm env) = TypedTerm $ TermLet $ Let [Binding (Name name) value Nothing] env

-- | Create a let expression with multiple bindings
-- Example: lets ["x">: int32 1, "y">: int32 2] (var "add" @@ var "x" @@ var "y")
lets :: [Field] -> TypedTerm a -> TypedTerm a
lets fields (TypedTerm env) = TypedTerm $ TermLet $ Let (toBinding <$> fields) env
  where
     toBinding (Field name value) = Binding name value Nothing

-- | Create a list of terms
-- Example: list [int32 1, int32 2, int32 3]
-- Accepts TypedTerm or TypedBinding elements (via AsTerm)
list :: AsTerm t a => [t] -> TypedTerm [a]
list els = TypedTerm $ Terms.list (unTypedTerm . asTerm <$> els)

-- | Create a map/dictionary term
-- Example: map (M.fromList [(string "a", int32 1), (string "b", int32 2)])
map :: M.Map (TypedTerm a) (TypedTerm b) -> TypedTerm (M.Map a b)
map = TypedTerm . Terms.map . M.fromList . fmap fromTypedTerm . M.toList
  where
    fromTypedTerm (TypedTerm k, TypedTerm v) = (k, v)

-- | Create a pattern match on a union term
-- Example: match (Name "Result") (Just $ string "what?") ["success">: string "yay", "error">: string "boo"]
match :: AsName n => n -> Maybe (TypedTerm b) -> [Field] -> TypedTerm (a -> b)
match n dflt fields = TypedTerm $ Terms.match (asName n) (unTypedTerm <$> dflt) fields

-- | Lift a Haskell Name value to a phantom-typed TypedTerm Name
nameLift :: Name -> TypedTerm Name
nameLift (Name n) = wrap _Name $ string n

-- | Create a 'Nothing' optional value
-- Example: nothing
nothing :: TypedTerm (Maybe a)
nothing = TypedTerm Terms.nothing

-- | Create an optional value from a Maybe
-- Example: opt (Just myValue)
opt :: Maybe (TypedTerm a) -> TypedTerm (Maybe a)
opt mc = TypedTerm $ Terms.optional (unTypedTerm <$> mc)

optCases :: AsTerm f (a -> b) => TypedTerm (Maybe a) -> TypedTerm b -> f -> TypedTerm b
optCases arg ifNothing ifJust = primitive3 (Name "hydra.lib.maybes.cases") arg ifNothing (asTerm ifJust)

-- | Create a pair
-- Example: pair (string "age") (int32 32)
-- Accepts TypedTerm or TypedBinding (via AsTerm)
pair :: (AsTerm t1 a, AsTerm t2 b) => t1 -> t2 -> TypedTerm (a, b)
pair l r = TypedTerm $ Terms.pair (unTypedTerm $ asTerm l) (unTypedTerm $ asTerm r)

-- | Primitive function by name
-- Example: primitive (Name "hydra.lib.strings.length")
primitive :: Name -> TypedTerm a
primitive = TypedTerm . Terms.primitive

-- | Apply a primitive function to one argument
-- Example: primitive1 _math_abs (int32 (-5))
primitive1 :: Name -> TypedTerm a -> TypedTerm b
primitive1 primName (TypedTerm a) = TypedTerm $ Terms.primitive primName Terms.@@ a

-- | Apply a primitive function to two arguments
-- Example: primitive2 _math_add (int32 2) (int32 3)
primitive2 :: Name -> TypedTerm a -> TypedTerm b -> TypedTerm c
primitive2 primName (TypedTerm a) (TypedTerm b) = TypedTerm $ Terms.primitive primName Terms.@@ a Terms.@@ b

-- | Apply a primitive function to three arguments
-- Example: primitive3 _string_replace (string "hello") (string "e") (string "a")
primitive3 :: Name -> TypedTerm a -> TypedTerm b -> TypedTerm c -> TypedTerm d
primitive3 primName (TypedTerm a) (TypedTerm b) (TypedTerm c) = TypedTerm $ Terms.primitive primName Terms.@@ a Terms.@@ b Terms.@@ c

-- | Extract a field from a record
-- Example: project (Name "Person") (Name "name")
project :: (AsName t, AsName f) => t -> f -> TypedTerm (a -> b)
project n fn = TypedTerm $ Terms.project (asName n) (asName fn)

-- | Create a record with named fields
-- Example: record (Name "Person") [field (Name "name") (string "John"), field (Name "age") (int32 30)]
record :: AsName n => n -> [Field] -> TypedTerm a
record n fields = TypedTerm $ Terms.record (asName n) fields

-- | Reify a Haskell-level meta-function over phantom-typed terms into a first-class
-- term-level function. See also 'reify2' for the binary form.
-- Example: @reify Literals.showInt32@ has type @TypedTerm (Int -> String)@
reify :: (TypedTerm a -> TypedTerm b) -> TypedTerm (a -> b)
reify f = case (unTypedTerm $ f $ var "x") of
  TermApplication (Application lhs _) -> TypedTerm lhs
  TermEither (Prelude.Left _) -> lambda "x" $ TypedTerm $ TermEither $ Prelude.Left $ Terms.var "x"
  TermEither (Prelude.Right _) -> lambda "x" $ TypedTerm $ TermEither $ Prelude.Right $ Terms.var "x"
  TermMaybe (Just _) -> primitive _maybes_pure
  TermInject (Injection tname (Field fname _)) -> lambda "x" $ inject tname fname $ var "x"
  TermWrap (WrappedTerm tname _) -> lambda "x" $ wrap tname $ var "x"

-- | Reify a binary Haskell-level meta-function over phantom-typed terms into a
-- first-class term-level function. See also 'reify' for the unary form.
-- Example: @reify2 (\\x y -> Maths.add x (Maths.mul x y))@
reify2 :: (TypedTerm a -> TypedTerm b -> TypedTerm c) -> TypedTerm (a -> b -> c)
reify2 f = case (unTypedTerm $ f (var "x") (var "y")) of
  TermApplication (Application (TermApplication (Application lhs _)) _) -> TypedTerm lhs
  t -> TypedTerm $ Terms.string $ "unexpected term as binary function: " <> ShowCore.term t



-- | Create a 'Right' either value
-- Example: right (int32 42)
right :: TypedTerm b -> TypedTerm (Either a b)
right (TypedTerm term) = TypedTerm $ Terms.right term

-- | Function that wraps a value in 'Right'
-- Example: right_ @@ myValue
right_ :: TypedTerm (b -> Either a b)
right_ = TypedTerm $ Terms.lambda "right_" $ Terms.right $ Terms.var "right_"

-- | Create a set of terms
-- Example: set [string "a", string "b", string "c"]
set :: [TypedTerm a] -> TypedTerm (S.Set a)
set = TypedTerm . Terms.set . S.fromList . fmap unTypedTerm

-- | Convert a typed binding to an untyped Binding for use in module element lists
-- Example: toBinding functionArity
toBinding :: TypedBinding a -> Binding
toBinding (TypedBinding name (TypedTerm term)) = Binding name term Nothing

-- | Convert a phantom-typed term definition to a Definition for use in module definition lists
-- Example: toDefinition functionArity
toDefinition :: TypedTermDefinition a -> Definition
toDefinition (TypedTermDefinition name (TypedTerm term)) = DefinitionTerm $ TermDefinition name Nothing Nothing term

-- | Convert a phantom-typed term definition to a primitive Definition, using the term body as the
-- declarative default implementation. The TermSignature describes the primitive's logical type.
-- isPure / isTotal default to True; use withImpurity / withPartiality to flag exceptions.
-- The 'comments' argument (second-to-last) is a list of long-form prose notes elaborating the one-line
-- description, one element per logical observation (e.g. semantics, edge cases, totality, host correspondence);
-- the default-implementation argument stays last because its value is typically large.
-- Example: toPrimitive "logical AND" andSig [] and_
toPrimitive :: String -> TermSignature -> [String] -> TypedTermDefinition a -> Definition
toPrimitive description sig comments (TypedTermDefinition name (TypedTerm term)) =
  DefinitionPrimitive $ PrimitiveDefinition name (primitiveMetadata description comments) sig True True (Just term)

-- | Convert a Name to a primitive Definition with no default implementation. Used for primitives
-- whose meaning is host-native and not expressible as a Hydra term (e.g. currentUnixTimeSeconds).
-- The trailing 'comments' argument is a list of long-form prose notes, one element per logical observation.
-- Example: toPrimitiveNoDefault "Current UNIX time, in seconds" sig (Name "hydra.lib.math.currentUnixTimeSeconds") []
toPrimitiveNoDefault :: String -> TermSignature -> Name -> [String] -> Definition
toPrimitiveNoDefault description sig name comments =
  DefinitionPrimitive $ PrimitiveDefinition name (primitiveMetadata description comments) sig True True Nothing

-- | Build the entity metadata for a primitive from its (always-present) one-line description and
-- long-form comments. Folds the former PrimitiveDefinition.description/comments fields into
-- EntityMetadata without data loss: description -> metadata.description, comments -> metadata.comments.
-- Each comments element is one logical observation, kept distinct rather than concatenated.
primitiveMetadata :: String -> [String] -> Maybe EntityMetadata
primitiveMetadata description comments =
  Just (EntityMetadata (Just description) comments [] Nothing)



-- | Convert a typed binding to a term Definition for use in module definition lists
-- Example: toTermDefinition functionArity
toTermDefinition :: TypedBinding a -> Definition
toTermDefinition (TypedBinding name (TypedTerm term)) = DefinitionTerm $ TermDefinition name Nothing Nothing term


triple :: TypedTerm a -> TypedTerm b -> TypedTerm c -> TypedTerm (a, b, c)
triple (TypedTerm a) (TypedTerm b) (TypedTerm c) = TypedTerm $ Terms.triple a b c

tuple4 :: TypedTerm a -> TypedTerm b -> TypedTerm c -> TypedTerm d -> TypedTerm (a, b, c, d)
tuple4 (TypedTerm a) (TypedTerm b) (TypedTerm c) (TypedTerm d) = TypedTerm $ Terms.tuple4 a b c d

tuple5 :: TypedTerm a -> TypedTerm b -> TypedTerm c -> TypedTerm d -> TypedTerm e -> TypedTerm (a, b, c, d, e)
tuple5 (TypedTerm a) (TypedTerm b) (TypedTerm c) (TypedTerm d) (TypedTerm e) = TypedTerm $ Terms.tuple5 a b c d e

-- | Unit value (empty record)
unit :: TypedTerm a
unit = TypedTerm Terms.unit

-- | Unsafe phantom type cast. Used during bootstrap when changing Module field types.
unsafeCast :: TypedTerm a -> TypedTerm b
unsafeCast (TypedTerm t) = TypedTerm t

-- | Create an unwrap function for a wrapped type
-- Example: unwrap (Name "Email")
unwrap :: AsName n => n -> TypedTerm (a -> b)
unwrap = TypedTerm . Terms.unwrap . asName

-- | Create a variable reference
-- Example: var "x"
var :: String -> TypedTerm a
var v = TypedTerm $ Terms.var v

-- | Create a wrapped term (instance of a newtype)
-- Example: wrap (Name "Email") (string "user@example.com")
-- Note: the phantom types provide no guarantee of type safety in this case
wrap :: AsName n => n -> TypedTerm a -> TypedTerm b
wrap n (TypedTerm term) = TypedTerm $ Terms.wrap (asName n) term
