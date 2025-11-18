-- | Term-level DSL which makes use of phantom types. Use this DSL for defining programs as opposed to data type definitions.
-- The phantom types provide static type checking in Haskell prior to Hydra's runtime type checking.
module Hydra.Dsl.Meta.Phantoms (
  module Hydra.Dsl.Meta.Phantoms,
  module Hydra.Dsl.Meta.Literals,
) where

import Hydra.Kernel
import Hydra.Dsl.Meta.Common
import Hydra.Dsl.Meta.Literals
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Annotations as Ann
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Show.Core as ShowCore

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
(<<~) :: String -> TTerm (Flow s a) -> TTerm (Flow s b) -> TTerm (Flow s b)
name <<~ def = bind name def

-- | Function composition operator: f <.> g creates a function that applies g then f
-- Example: toString <.> increment
(<.>) :: TTerm (b -> c) -> TTerm (a -> b) -> TTerm (a -> c)
f <.> g = compose f g

-- | Function application operator: function @@ argument
-- Example: add @@ int32 1
(@@) :: TTerm (a -> b) -> TTerm a -> TTerm b
fun @@ arg = apply fun arg

-- | Field definition operator for records: name>: value
-- Example: "name">: string "John"
infixr 0 >:
(>:) :: String -> TTerm a -> Field
name>: term = Field (Name name) (unTTerm term)

-- | Field definition operator with pre-constructed name: fname>>: value
-- Example: _Person_name>>: string "John"
infixr 0 >>:
(>>:) :: Name -> TTerm a -> Field
fname >>: d = field fname d

-- | Add an annotation to a term
-- Example: annot (Name "deprecated") (Just (boolean True)) myFunction
annot :: Name -> Maybe Term -> TTerm a -> TTerm a
annot key mvalue (TTerm term) = TTerm $ Ann.annotateTerm key mvalue term

-- | Apply a function to an argument
-- Example: apply (var "add") (int32 1)
apply :: TTerm (a -> b) -> TTerm a -> TTerm b
apply (TTerm lhs) (TTerm rhs) = TTerm $ Terms.apply lhs rhs

bind :: String -> TTerm (Flow s a) -> TTerm (Flow s b) -> TTerm (Flow s b)
bind v def body = primitive2 _flows_bind def $ lambda v $ body

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
compose :: TTerm (b -> c) -> TTerm (a -> b) -> TTerm (a -> c)
compose (TTerm f) (TTerm g) = TTerm $ Terms.compose f g

-- | Create a constant function that always returns the same value
-- Example: constant true
constant :: TTerm a -> TTerm (b -> a)
constant (TTerm term) = TTerm $ Terms.constant term

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
doc :: String -> TTerm a -> TTerm a
doc s (TTerm term) = TTerm $ setTermDescription (Just s) term

-- | Add documentation with line wrapping at the specified width
-- Example: docWrapped 80 "This is a long documentation string that will be wrapped..." myFunction
docWrapped :: Int -> String -> TTerm a -> TTerm a
docWrapped len = doc . wrapLine len

-- | Convert a typed element to an untyped element
-- Example: el (definitionInModule myModule "addInts" myFunction)
el :: TBinding a -> Binding
el (TBinding name (TTerm term)) = Binding name term Nothing

exec :: TTerm (Flow s a) -> TTerm (Flow s b) -> TTerm (Flow s b)
exec f b = primitive2 _flows_bind f (lambda ignoredVariable b)

-- | Create a field with the given name and value
-- Example: field (Name "age") (int32 30)
field :: Name -> TTerm a -> Field
field fname (TTerm val) = Field fname val

-- | First element projection function for pairs
-- Example: first $ pair (string "foo") (string "bar")
first :: TTerm (a, b) -> TTerm a
first pair = TTerm (Terms.untuple 2 0) @@ pair

-- | Mark a type as first-class
-- Example: firstClassType (record ...)
firstClassType :: TTerm Type -> TTerm Type
firstClassType typ = annot key_firstClassType (Just $ Terms.boolean True) typ

-- | Create a fold function to process lists
-- Example: fold (lambda "acc" (lambda "x" (add @@ var "acc" @@ var "x")))
fold :: TTerm (b -> a -> b) -> TTerm (b -> [a] -> b)
fold f = (primitive _lists_foldl) @@ f

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
just :: TTerm a -> TTerm (Maybe a)
just (TTerm term) = TTerm $ Terms.just term

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
list :: [TTerm a] -> TTerm [a]
list els = TTerm $ Terms.list (unTTerm <$> els)

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

optCases :: TTerm (Maybe a) -> TTerm b -> TTerm (a -> b) -> TTerm b
optCases arg ifNothing ifJust = primitive3 (Name "hydra.lib.maybes.maybe") ifNothing ifJust arg

-- | Create a 2-tuple
-- Example: tuple2 (string "age") (int32 32)
tuple2 :: (TTerm a) -> (TTerm b) -> TTerm (a, b)
tuple2 (TTerm l) (TTerm r) = TTerm $ Terms.tuple2 l r

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

produce :: TTerm a -> TTerm (Flow s a)
produce = primitive1 _flows_pure

-- | Extract a field from a record
-- Example: project (Name "Person") (Name "name")
project :: Name -> Name -> TTerm (a -> b)
project name fname = TTerm $ Terms.project name fname

-- | Create a record with named fields
-- Example: record (Name "Person") [field (Name "name") (string "John"), field (Name "age") (int32 30)]
record :: Name -> [Field] -> TTerm a
record name fields = TTerm $ Terms.record name fields

-- | Reference a defined element
-- Example: ref (definitionInModule myModule "addInts")
ref :: TBinding a -> TTerm a
ref (TBinding name _) = TTerm (TermVariable name)

-- | Second element projection function for pairs
-- Example: second $ pair (string "foo") (string "bar")
second :: TTerm (a, b) -> TTerm b
second pair = TTerm (Terms.untuple 2 1) @@ pair

-- | Create a set of terms
-- Example: set [string "a", string "b", string "c"]
set :: [TTerm a] -> TTerm (S.Set a)
set = TTerm . Terms.set . S.fromList . fmap unTTerm

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

-- | Create a tuple projection function
-- Example: untuple 3 1 extracts the second element of a 3-tuple
untuple :: Int -> Int -> TTerm (a -> b)
untuple arity idx = TTerm $ Terms.untuple arity idx

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
