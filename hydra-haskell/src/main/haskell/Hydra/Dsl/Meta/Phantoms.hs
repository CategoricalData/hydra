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

import Prelude hiding ((++))
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


-- Operators

infixr 0 ~>
(~>) :: String -> TTerm x -> TTerm (a -> b)
name ~> body = lambda name body

infixl 1 <~
(<~) :: String -> TTerm a -> TTerm b -> TTerm b
name <~ value = let1 name value

infixl 1 <<~
(<<~) :: AsTerm t (Flow s a) => String -> t -> TTerm (Flow s b) -> TTerm (Flow s b)
name <<~ def = bind name (asTerm def)

-- | Function composition operator: f <.> g creates a function that applies g then f
-- Example: toString <.> increment
-- Accepts TTerm or TBinding for both operands (via AsTerm)
(<.>) :: (AsTerm f (b -> c), AsTerm g (a -> b)) => f -> g -> TTerm (a -> c)
f <.> g = compose (asTerm f) (asTerm g)

-- | Function application operator: function @@ argument
-- Example: add @@ int32 1
-- Accepts TTerm or TBinding for both operands (via AsTerm)
(@@) :: (AsTerm f (a -> b), AsTerm g a) => f -> g -> TTerm b
fun @@ arg = apply (asTerm fun) (asTerm arg)

(++) :: (AsTerm f String, AsTerm g String) => f -> g -> TTerm String
f ++ g = primitive2 _strings_cat2 (asTerm f) (asTerm g)

-- | Field definition operator for records: name>: value
-- Example: "name">: string "John"
-- Accepts TTerm or TBinding (via AsTerm)
infixr 0 >:
(>:) :: AsTerm t a => String -> t -> Field
name>: term = Field (Name name) (unTTerm $ asTerm term)

-- | Field definition operator with pre-constructed name: fname>>: value
-- Example: _Person_name>>: string "John"
-- Accepts TTerm or TBinding (via AsTerm)
infixr 0 >>:
(>>:) :: AsTerm t a => Name -> t -> Field
fname >>: d = field fname (asTerm d)

-- | Add an annotation to a term
-- Example: annot (Name "deprecated") (Just (boolean True)) myFunction
annot :: Name -> Maybe Term -> TTerm a -> TTerm a
annot key mvalue (TTerm term) = TTerm $ Ann.annotateTerm key mvalue term

-- | Apply a function to an argument
-- Example: apply (var "add") (int32 1)
apply :: TTerm (a -> b) -> TTerm a -> TTerm b
apply (TTerm lhs) (TTerm rhs) = TTerm $ Terms.apply lhs rhs

bind :: AsTerm t (Flow s a) => String -> t -> TTerm (Flow s b) -> TTerm (Flow s b)
bind v def body = primitive2 _flows_bind (asTerm def) $ lambda v $ body

binaryFunction :: (TTerm a -> TTerm b -> TTerm c) -> TTerm (a -> b -> c)
binaryFunction f = case (unTTerm $ f (var "x") (var "y")) of
  TermApplication (Application (TermApplication (Application lhs _)) _) -> TTerm lhs
  t -> TTerm $ Terms.string $ "unexpected term as binary function: " <> ShowCore.term t

binds :: [Field] -> TTerm (Flow s a) -> TTerm (Flow s a)
binds fields rhs = L.foldr withField rhs fields
  where
    withField (Field (Name fname) fterm) b = bind fname (TTerm fterm) b

-- | Apply a named case match to an argument
-- Example: cases resultTypeName myResult Nothing [onSuccess, onError]
-- See also: 'match'
cases :: Name -> TTerm a -> Maybe (TTerm b) -> [Field] -> TTerm b
cases name arg dflt fields = TTerm $ Terms.apply (Terms.match name (unTTerm <$> dflt) fields) (unTTerm arg)

-- | Compose two functions (g then f)
-- Example: compose (var "stringLength") (var "toString")
-- Accepts TTerm or TBinding for both operands (via AsTerm)
compose :: (AsTerm f (b -> c), AsTerm g (a -> b)) => f -> g -> TTerm (a -> c)
compose f g = TTerm $ Terms.compose (unTTerm $ asTerm f) (unTTerm $ asTerm g)

-- | Create a constant function that always returns the same value
-- Example: constant true
-- Accepts TTerm or TBinding (via AsTerm)
constant :: AsTerm t a => t -> TTerm (b -> a)
constant t = TTerm $ Terms.constant (unTTerm $ asTerm t)

-- | Create a definition in a module
-- Example: definitionInModule myModule "addInts" (lambda "x" (lambda "y" (add @@ var "x" @@ var "y")))
definitionInModule :: Module -> String -> TTerm a -> TBinding a
definitionInModule mod = definitionInNamespace $ moduleNamespace mod

-- | Create a definition in a namespace
-- Example: definitionInNamespace (Namespace "com.example") "addInts" myFunction
definitionInNamespace :: Namespace -> String -> TTerm a -> TBinding a
definitionInNamespace ns lname = TBinding $ unqualifyName $ QualifiedName (Just ns) lname

-- | Add documentation to a term
-- Example: doc "Adds two integers" addFunction
-- Accepts TTerm or TBinding (via AsTerm)
doc :: AsTerm t a => String -> t -> TTerm a
doc s t = TTerm $ setTermDescription (Just s) (unTTerm $ asTerm t)

-- | Add documentation with line wrapping at the specified width
-- Example: docWrapped 80 "This is a long documentation string that will be wrapped..." myFunction
docWrapped :: Int -> String -> TTerm a -> TTerm a
docWrapped len = doc . wrapLine len

encodedBinary :: TTerm String -> TTerm Term
encodedBinary = encodedLiteral . inject _Literal _Literal_binary

encodedBoolean :: TTerm Bool -> TTerm Term
encodedBoolean = encodedLiteral . inject _Literal _Literal_boolean

encodedCase :: AsTerm t (a -> Term) => Name -> Name -> t -> Field
encodedCase tname fname enc = field fname $ "v" ~> encodedVariant tname fname (enc @@ var "v")

encodedEither :: TTerm (Prelude.Either a b) -> TTerm Term
encodedEither = inject _Term _Term_either

encodedField :: Name -> TTerm Term -> TTerm Term
encodedField fname term = encodedFieldRaw (encodedName fname) term

encodedFieldRaw :: TTerm Name -> TTerm Term -> TTerm Term
encodedFieldRaw (TTerm fname) (TTerm term) = TTerm $ Terms.record _Field [
  Field _Field_name fname,
  Field _Field_term term]

encodedFloatValue :: TTerm FloatValue -> TTerm Term
encodedFloatValue = encodedLiteral . inject _Literal _Literal_float

encodedInjection :: Name -> Name -> TTerm Term -> TTerm Term
encodedInjection tname fname term = TTerm $ Terms.record _Injection [
  field _Injection_typeName $ encodedName tname,
  field _Injection_field $ encodedField fname term]

encodedInt32 :: TTerm Int -> TTerm Term
encodedInt32 v = encodedIntegerValue $ inject _IntegerValue _IntegerValue_int32 v

encodedIntegerValue :: TTerm IntegerValue -> TTerm Term
encodedIntegerValue = encodedLiteral . inject _Literal _Literal_integer

encodedList :: TTerm [a] -> TTerm Term
encodedList = inject _Term _Term_list

encodedLiteral :: TTerm Literal -> TTerm Term
encodedLiteral = inject _Term _Term_literal

encodedMap :: TTerm (M.Map k v) -> TTerm Term
encodedMap = inject _Term _Term_map

encodedName :: Name -> TTerm Name
encodedName = wrap _Name . string . unName

encodedUnit :: TTerm Term
encodedUnit = injectUnit _Term _Term_unit

encodedWrappedTerm :: Name -> TTerm Term -> TTerm Term
encodedWrappedTerm name = encodedWrappedTermRaw (encodedName name)

encodedWrappedTermRaw :: TTerm Name -> TTerm Term -> TTerm Term
encodedWrappedTermRaw (TTerm name) (TTerm term) = TTerm $ Terms.inject _Term _Term_wrap $ Terms.record _WrappedTerm [
  Field _WrappedTerm_typeName name,
  Field _WrappedTerm_body term]

encodedOptional :: TTerm (Maybe a) -> TTerm Term
encodedOptional = inject _Term _Term_maybe

encodedPair :: TTerm (a, b) -> TTerm Term
encodedPair = inject _Term _Term_pair

encodedRecord :: Name -> [Field] -> TTerm Term
encodedRecord tname fields = TTerm $ Terms.inject _Term _Term_record $ Terms.record _Record [
    field _Record_typeName $ encodedName tname,
    field _Record_fields $ list (encField <$> fields)]
  where
    encField (Field fname term) = encodedField fname $ TTerm term

encodedSet :: TTerm (S.Set a) -> TTerm Term
encodedSet = inject _Term _Term_set

encodedString :: TTerm String -> TTerm Term
encodedString = encodedLiteral . inject _Literal _Literal_string

encodedUnion :: TTerm Term -> TTerm Term
encodedUnion = inject _Term _Term_union

encodedVariant :: Name -> Name -> TTerm Term -> TTerm Term
encodedVariant tname fname term = encodedUnion $ encodedInjection tname fname term

-- | Get a reference to an encoder function in hydra.encode.core for a given type name
encoderFor :: Name -> TTerm (a -> Term)
encoderFor typeName = var $ unName $ encodeBindingName typeName

-- | Convert a typed element to an untyped element (legacy name, prefer 'toBinding')
-- Example: el (definitionInModule myModule "addInts" myFunction)
el :: TBinding a -> Binding
el = toBinding

exec :: TTerm (Flow s a) -> TTerm (Flow s b) -> TTerm (Flow s b)
exec f b = primitive2 _flows_bind f (lambda ignoredVariable b)

-- | Create a field with the given name and value
-- Example: field (Name "age") (int32 30)
field :: Name -> TTerm a -> Field
field fname (TTerm val) = Field fname val

-- | Mark a type as first-class
-- Example: firstClassType (record ...)
firstClassType :: TTerm Type -> TTerm Type
firstClassType typ = annot key_firstClassType (Just $ Terms.boolean True) typ

-- | Create a fold function to process lists
-- Example: fold (lambda "acc" (lambda "x" (add @@ var "acc" @@ var "x")))
fold :: AsTerm t (b -> a -> b) => t -> TTerm (b -> [a] -> b)
fold f = (primitive _lists_foldl) @@ asTerm f

-- | Identity function that returns its argument unchanged
-- Example: identity
identity :: TTerm (a -> a)
identity = TTerm Terms.identity

-- | Create a union injection
-- Example: inject (Name "Result") (Name "success") (string "ok")
inject :: Name -> Name -> TTerm a -> TTerm b
inject name fname (TTerm term) = TTerm $ Terms.inject name fname term

-- | Create a function that injects its argument into a union variant
-- Example: injectLambda (Name "Result") (Name "success")
injectLambda :: Name -> Name -> TTerm (a -> b)
injectLambda name fname = lambda "injected_" $ inject name fname $ var "injected_"

-- | Create a 'Just' optional value
-- Example: just (string "found")
-- Accepts TTerm or TBinding (via AsTerm)
just :: AsTerm t a => t -> TTerm (Maybe a)
just t = TTerm $ Terms.just (unTTerm $ asTerm t)

-- | Function that wraps a value in 'Just'
-- Example: just_ @@ myValue
just_ :: TTerm (a -> Maybe a)
just_ = TTerm $ Terms.lambda "just_" $ Terms.just $ Terms.var "just_"

-- | Create a 'Left' either value
-- Example: left (string "error")
left :: TTerm a -> TTerm (Either a b)
left (TTerm term) = TTerm $ Terms.left term

-- | Function that wraps a value in 'Left'
-- Example: left_ @@ myValue
left_ :: TTerm (a -> Either a b)
left_ = TTerm $ Terms.lambda "left_" $ Terms.left $ Terms.var "left_"

-- | Create a 'Right' either value
-- Example: right (int32 42)
right :: TTerm b -> TTerm (Either a b)
right (TTerm term) = TTerm $ Terms.right term

-- | Function that wraps a value in 'Right'
-- Example: right_ @@ myValue
right_ :: TTerm (b -> Either a b)
right_ = TTerm $ Terms.lambda "right_" $ Terms.right $ Terms.var "right_"

-- | Create a lambda function with one parameter
-- Example: lambda "x" (var "add" @@ var "x" @@ int32 1)
lambda :: String -> TTerm x -> TTerm (a -> b)
lambda v (TTerm body) = TTerm $ Terms.lambda v body

-- | Create a multi-parameter lambda function
-- Example: lambdas ["x", "y"] (add @@ var "x" @@ var "y")
lambdas :: [String] -> TTerm x -> TTerm (a -> b)
lambdas params (TTerm body) = TTerm $ Terms.lambdas params body

let1 :: String -> TTerm a -> TTerm b -> TTerm b
let1 name (TTerm value) (TTerm env) = TTerm $ TermLet $ Let [Binding (Name name) value Nothing] env

-- | Create a let expression with multiple bindings
-- Example: lets ["x">: int32 1, "y">: int32 2] (var "add" @@ var "x" @@ var "y")
lets :: [Field] -> TTerm a -> TTerm a
lets fields (TTerm env) = TTerm $ TermLet $ Let (toBinding <$> fields) env
  where
     toBinding (Field name value) = Binding name value Nothing

-- | Create a list of terms
-- Example: list [int32 1, int32 2, int32 3]
-- Accepts TTerm or TBinding elements (via AsTerm)
list :: AsTerm t a => [t] -> TTerm [a]
list els = TTerm $ Terms.list (unTTerm . asTerm <$> els)

-- | Create a map/dictionary term
-- Example: map (M.fromList [(string "a", int32 1), (string "b", int32 2)])
map :: M.Map (TTerm a) (TTerm b) -> TTerm (M.Map a b)
map = TTerm . Terms.map . M.fromList . fmap fromTTerm . M.toList
  where
    fromTTerm (TTerm k, TTerm v) = (k, v)

-- | Create a pattern match on a union term
-- Example: match (Name "Result") (Just $ string "what?") ["success">: string "yay", "error">: string "boo"]
match :: Name -> Maybe (TTerm b) -> [Field] -> TTerm (a -> b)
match name dflt fields = TTerm $ Terms.match name (unTTerm <$> dflt) fields

-- | Create a 'Nothing' optional value
-- Example: nothing
nothing :: TTerm (Maybe a)
nothing = TTerm Terms.nothing

-- | Create an optional value from a Maybe
-- Example: opt (Just myValue)
opt :: Maybe (TTerm a) -> TTerm (Maybe a)
opt mc = TTerm $ Terms.optional (unTTerm <$> mc)

optCases :: AsTerm f (a -> b) => TTerm (Maybe a) -> TTerm b -> f -> TTerm b
optCases arg ifNothing ifJust = primitive3 (Name "hydra.lib.maybes.maybe") ifNothing (asTerm ifJust) arg

-- | Create a pair
-- Example: pair (string "age") (int32 32)
-- Accepts TTerm or TBinding (via AsTerm)
pair :: (AsTerm t1 a, AsTerm t2 b) => t1 -> t2 -> TTerm (a, b)
pair l r = TTerm $ Terms.pair (unTTerm $ asTerm l) (unTTerm $ asTerm r)

-- | Primitive function by name
-- Example: primitive (Name "hydra.lib.strings.length")
primitive :: Name -> TTerm a
primitive = TTerm . Terms.primitive

-- | Apply a primitive function to one argument
-- Example: primitive1 _math_abs (int32 (-5))
primitive1 :: Name -> TTerm a -> TTerm b
primitive1 primName (TTerm a) = TTerm $ Terms.primitive primName Terms.@@ a

-- | Apply a primitive function to two arguments
-- Example: primitive2 _math_add (int32 2) (int32 3)
primitive2 :: Name -> TTerm a -> TTerm b -> TTerm c
primitive2 primName (TTerm a) (TTerm b) = TTerm $ Terms.primitive primName Terms.@@ a Terms.@@ b

-- | Apply a primitive function to three arguments
-- Example: primitive3 _string_replace (string "hello") (string "e") (string "a")
primitive3 :: Name -> TTerm a -> TTerm b -> TTerm c -> TTerm d
primitive3 primName (TTerm a) (TTerm b) (TTerm c) = TTerm $ Terms.primitive primName Terms.@@ a Terms.@@ b Terms.@@ c

produce :: AsTerm t a => t -> TTerm (Flow s a)
produce x = primitive1 _flows_pure (asTerm x)

-- | Extract a field from a record
-- Example: project (Name "Person") (Name "name")
project :: Name -> Name -> TTerm (a -> b)
project name fname = TTerm $ Terms.project name fname

-- | Create a record with named fields
-- Example: record (Name "Person") [field (Name "name") (string "John"), field (Name "age") (int32 30)]
record :: Name -> [Field] -> TTerm a
record name fields = TTerm $ Terms.record name fields

-- | Create a set of terms
-- Example: set [string "a", string "b", string "c"]
set :: [TTerm a] -> TTerm (S.Set a)
set = TTerm . Terms.set . S.fromList . fmap unTTerm

-- | Convert a typed binding to an untyped Binding for use in module element lists
-- Example: toBinding functionArity
toBinding :: TBinding a -> Binding
toBinding (TBinding name (TTerm term)) = Binding name term Nothing

trace :: TTerm String -> TTerm (Flow s a) -> TTerm (Flow s a)
trace msg flow = var "hydra.monads.withTrace" @@ msg @@ flow

triple :: TTerm a -> TTerm b -> TTerm c -> TTerm (a, b, c)
triple (TTerm a) (TTerm b) (TTerm c) = TTerm $ Terms.triple a b c

tuple4 :: TTerm a -> TTerm b -> TTerm c -> TTerm d -> TTerm (a, b, c, d)
tuple4 (TTerm a) (TTerm b) (TTerm c) (TTerm d) = TTerm $ Terms.tuple4 a b c d

tuple5 :: TTerm a -> TTerm b -> TTerm c -> TTerm d -> TTerm e -> TTerm (a, b, c, d, e)
tuple5 (TTerm a) (TTerm b) (TTerm c) (TTerm d) (TTerm e) = TTerm $ Terms.tuple5 a b c d e

unaryFunction :: (TTerm a -> TTerm b) -> TTerm (a -> b)
unaryFunction f = case (unTTerm $ f $ var "x") of
  TermApplication (Application lhs _) -> TTerm lhs
  TermEither (Prelude.Left _) -> lambda "x" $ TTerm $ TermEither $ Prelude.Left $ Terms.var "x"
  TermEither (Prelude.Right _) -> lambda "x" $ TTerm $ TermEither $ Prelude.Right $ Terms.var "x"
  TermMaybe (Just _) -> primitive _maybes_pure
  TermUnion (Injection tname (Field fname _)) -> lambda "x" $ inject tname fname $ var "x"
  TermWrap (WrappedTerm tname _) -> lambda "x" $ wrap tname $ var "x"

-- | Unit value (empty record)
unit :: TTerm a
unit = TTerm Terms.unit

-- | Create a unit variant of a union
-- Example: injectUnit (Name "Result") (Name "success")
injectUnit :: Name -> Name -> TTerm a
injectUnit name fname = TTerm $ Terms.inject name fname Terms.unit

-- | Create an unwrap function for a wrapped type
-- Example: unwrap (Name "Email")
unwrap :: Name -> TTerm (a -> b)
unwrap = TTerm . Terms.unwrap

-- | Create a variable reference
-- Example: var "x"
var :: String -> TTerm a
var v = TTerm $ Terms.var v

-- | Associate the Eq type class with the inferred type of a term
-- Example: withEq "t0" myTerm
withEq :: String -> TTerm a -> TTerm a
withEq v = withTypeClasses $ M.fromList [(Name v, S.singleton TypeClassEquality)]

-- | Associate the Ord type class with the inferred type of a term
-- Example: withOrd "t0" myTerm
withOrd :: String -> TTerm a -> TTerm a
withOrd v = withOrds [v]

withOrds :: [String] -> TTerm a -> TTerm a
withOrds vs = withTypeClasses $ M.fromList $ fmap toPair vs
  where
    toPair v = (Name v, S.singleton TypeClassOrdering)

-- | Associate type classes with the inferred type of a term
-- Example: withTypeClasses (M.fromList [(Name "t0", S.singleton TypeClassOrdering)]) myTerm
withTypeClasses :: M.Map Name (S.Set TypeClass) -> TTerm a -> TTerm a
withTypeClasses classes (TTerm term) = TTerm $ setTypeClasses classes term

-- | Create a wrapped term (instance of a newtype)
-- Example: wrap (Name "Email") (string "user@example.com")
-- Note: the phantom types provide no guarantee of type safety in this case
wrap :: Name -> TTerm a -> TTerm b
wrap name (TTerm term) = TTerm $ Terms.wrap name term
