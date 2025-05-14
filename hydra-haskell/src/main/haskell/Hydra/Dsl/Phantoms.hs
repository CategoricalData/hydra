-- | Term-level DSL which makes use of phantom types. Use this DSL for defining programs as opposed to data type definitions.
-- The phantom types provide static type checking in Haskell prior to Hydra's runtime type checking.
module Hydra.Dsl.Phantoms (
  module Hydra.Dsl.Phantoms,
  module Hydra.Dsl.PhantomLiterals,
  hydraCoreGraph,
) where

import Hydra.Dsl.Common
import Hydra.Dsl.PhantomLiterals
import Hydra.Sources.Tier0.Core
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Annotations as Ann
import qualified Hydra.Dsl.Terms as Terms

import Prelude hiding ((++))
import qualified Data.Map as M
import qualified Data.Set as S


-- * Operators

-- | Function composition operator: f <.> g creates a function that applies g then f
-- Example: toString <.> increment
(<.>) :: TTerm (b -> c) -> TTerm (a -> b) -> TTerm (a -> c)
f <.> g = compose f g

-- | Function application operator: function @@ argument
-- Example: add @@ int32 1
(@@) :: TTerm (a -> b) -> TTerm a -> TTerm b
fun @@ arg = apply fun arg

-- | Field definition operator for records: name>: value
-- Example: "name" >: string "John"
infixr 0 >:
(>:) :: String -> TTerm a -> Field
name>: term = Field (Name name) (unTTerm term)

-- | Field definition operator with pre-constructed name: fname>>: value
-- Example: _Person_name>>: string "John"
infixr 0 >>:
(>>:) :: Name -> TTerm a -> Field
fname >>: d = Field fname (unTTerm d)

-- | Pair constructor used in pattern matching
-- Example: "Red" @-> rgb255
infixr 0 @->
(@->) :: a -> b -> (a, b)
x @-> y = (x, y)

-- | Case mapping operator for pattern matching
-- Example: TCase _Result_success --> string "success"
infixr 0 -->
(-->) :: TCase a -> TTerm (a -> b) -> Field
c --> t = caseField c t

-- * Fundamentals

-- | Apply a function to an argument
-- Example: apply (var "add") (int32 1)
apply :: TTerm (a -> b) -> TTerm a -> TTerm b
apply (TTerm lhs) (TTerm rhs) = TTerm $ Terms.apply lhs rhs

-- | Create a let expression with multiple bindings
-- Example: lets ["x">: int32 1, "y">: int32 2] (var "add" @@ var "x" @@ var "y")
lets :: [Field] -> TTerm a -> TTerm a
lets fields (TTerm env) = TTerm $ TermLet $ Let (toBinding <$> fields) env
  where
     toBinding (Field name value) = LetBinding name value Nothing

-- | Create a variable reference
-- Example: var "x"
var :: String -> TTerm a
var v = TTerm $ Terms.var v

-- * Functions

-- | Compose two functions (g then f)
-- Example: compose (var "stringLength") (var "toString")
compose :: TTerm (b -> c) -> TTerm (a -> b) -> TTerm (a -> c)
compose (TTerm f) (TTerm g) = TTerm $ Terms.compose f g

-- | Create a constant function that always returns the same value
-- Example: constant true
constant :: TTerm a -> TTerm (b -> a)
constant (TTerm term) = TTerm $ Terms.constant term

-- | Identity function that returns its argument unchanged
-- Example: identity
identity :: TTerm (a -> a)
identity = TTerm Terms.identity

-- | Create a lambda function with one parameter
-- Example: lambda "x" (var "add" @@ var "x" @@ int32 1)
lambda :: String -> TTerm x -> TTerm (a -> b)
lambda v (TTerm body) = TTerm $ Terms.lambda v body

-- | Create a multi-parameter lambda function
-- Example: lambdas ["x", "y"] (add @@ var "x" @@ var "y")
lambdas :: [String] -> TTerm x -> TTerm (a -> b)
lambdas params (TTerm body) = TTerm $ Terms.lambdas params body

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

-- * Collections

-- | Create a fold function to process lists
-- Example: fold (lambda "acc" (lambda "x" (add @@ var "acc" @@ var "x")))
fold :: TTerm (b -> a -> b) -> TTerm (b -> [a] -> b)
fold f = (primitive _lists_foldl) @@ f

-- | Create a 'Just' optional value
-- Example: just (string "found")
just :: TTerm a -> TTerm (Maybe a)
just (TTerm term) = TTerm $ Terms.just term

-- | Function that wraps a value in 'Just'
-- Example: just_ @@ myValue
just_ :: TTerm (a -> Maybe a)
just_ = TTerm $ Terms.lambda "just_" $ Terms.just $ Terms.var "just_"

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

-- | Create a 'Nothing' optional value
-- Example: nothing
nothing :: TTerm (Maybe a)
nothing = TTerm Terms.nothing

-- | Create an optional value from a Maybe
-- Example: opt (Just myValue)
opt :: Maybe (TTerm a) -> TTerm (Maybe a)
opt mc = TTerm $ Terms.optional (unTTerm <$> mc)

-- | Create a set of terms
-- Example: set [string "a", string "b", string "c"]
set :: [TTerm a] -> TTerm (S.Set a)
set = TTerm . Terms.set . S.fromList . fmap unTTerm

-- * Products and tuples

-- | First element projection function for pairs
-- Example: first @@ myPair
first :: TTerm ((a, b) -> a)
first = TTerm $ Terms.untuple 2 0 Nothing

-- | Create a pair (2-tuple)
-- Example: pair (string "age") (int32 32)
pair :: (TTerm a) -> (TTerm b) -> TTerm (a, b)
pair (TTerm l) (TTerm r) = TTerm $ Terms.pair l r

-- | Second element projection function for pairs
-- Example: second @@ myPair
second :: TTerm ((a, b) -> b)
second = TTerm $ Terms.untuple 2 1 Nothing

-- | Create a tuple projection function
-- Example: untuple 3 1 extracts the second element of a 3-tuple
untuple :: Int -> Int -> TTerm (a -> b)
untuple arity idx = TTerm $ Terms.untuple arity idx Nothing

-- * Records, unions and newtypes

-- | Create a field with the given name and value
-- Example: field (Name "age") (int32 30)
field :: Name -> TTerm a -> Field
field fname (TTerm val) = Field fname val

-- | Create a typed field
-- Example: fld (Name "age") (int32 30)
fld :: Name -> TTerm a -> TField a
fld fname (TTerm val) = TField $ Field fname val

-- | Create a union injection
-- Example: inject (Name "Result") (Name "success") (string "ok")
inject :: Name -> Name -> TTerm a -> TTerm b
inject name fname (TTerm term) = TTerm $ Terms.inject name (Field fname term)

-- | Create a function that injects its argument into a union variant
-- Example: injectLambda (Name "Result") (Name "success")
injectLambda :: Name -> Name -> TTerm (a -> b)
injectLambda name fname = lambda "injected_" $ inject name fname $ var "injected_"

-- | Extract a field from a record
-- Example: project (Name "Person") (Name "name")
project :: Name -> Name -> TTerm (a -> b)
project name fname = TTerm $ Terms.project name fname

-- | Create a record with named fields
-- Example: record (Name "Person") [field (Name "name") (string "John"), field (Name "age") (int32 30)]
record :: Name -> [Field] -> TTerm a
record name fields = TTerm $ Terms.record name fields

-- | Unit value (empty record)
unit :: TTerm a
unit = TTerm Terms.unit

-- | Create a unit variant of a union
-- Example: unitVariant (Name "Result") (Name "success")
unitVariant :: Name -> Name -> TTerm a
unitVariant name fname = TTerm $ Terms.inject name $ Field fname Terms.unit

-- | Create an unwrap function for a wrapped type
-- Example: unwrap (Name "Email")
unwrap :: Name -> TTerm (a -> b)
unwrap = TTerm . Terms.unwrap

-- | Create a union variant
-- Example: variant (Name "Result") (Name "success") (string "ok")
variant :: Name -> Name -> TTerm a -> TTerm b
variant name fname (TTerm term) = TTerm $ Terms.inject name $ Field fname term

-- | Create a wrapped term (instance of a newtype)
-- Example: wrap (Name "Email") (string "user@example.com")
-- Note: the phantom types provide no guarantee of type safety in this case
wrap :: Name -> TTerm a -> TTerm b
wrap name (TTerm term) = TTerm $ Terms.wrap name term

-- * Pattern matching

-- | Create a case field for pattern matching
-- Example: caseField (TCase "success") handleSuccess
caseField :: TCase a -> TTerm (a -> b) -> Field
caseField (TCase fname) (TTerm f) = Field fname f

-- | Apply a named case match to an argument
-- Example: cases resultTypeName myResult Nothing [onSuccess, onError]
-- See also: 'match'
cases :: Name -> TTerm a -> Maybe (TTerm b) -> [Field] -> TTerm b
cases name arg dflt fields = TTerm $ Terms.apply (Terms.match name (unTTerm <$> dflt) fields) (unTTerm arg)

-- | Create a pattern match on a union
-- Example: match (Name "Result") (Just $ string "what?") ["success">: string "yay", "error">: string "boo"]
match :: Name -> Maybe (TTerm b) -> [Field] -> TTerm (a -> b)
match name dflt fields = TTerm $ Terms.match name (unTTerm <$> dflt) fields

-- | Create a pattern match on a union with explicit handlers
-- Example: matchData (Name "Result") Nothing [(Name "success", handleSuccess), (Name "error", handleError)]
matchData :: Name -> Maybe (TTerm b) -> [(Name, TTerm (x -> b))] -> TTerm (a -> b)
matchData name dflt pairs = TTerm $ Terms.match name (unTTerm <$> dflt) (toField <$> pairs)
  where
    toField (fname, TTerm term) = Field fname term

-- | Create a pattern match that maps enum variants to enum variants
-- Example: matchToEnum (Name "Color") (Name "Result") Nothing [(Name "Red", Name "Success"), (Name "Green", Name "Success"), (Name "Blue", Name "Error")]
matchToEnum :: Name -> Name -> Maybe (TTerm b) -> [(Name, Name)] -> TTerm (a -> b)
matchToEnum domName codName dflt pairs = matchData domName dflt (toCase <$> pairs)
  where
    toCase (fromName, toName) = (fromName, constant $ unitVariant codName toName)

-- | Create a pattern match that maps enum variants to union fields
-- Example: matchToUnion (Name "Color") (Name "Result") Nothing [(Name "Red", field (Name "success") (string "ok")), (Name "Blue", field (Name "error") (string "bad"))]
matchToUnion :: Name -> Name -> Maybe (TTerm b) -> [(Name, Field)] -> TTerm (a -> b)
matchToUnion domName codName dflt pairs = matchData domName dflt (toCase <$> pairs)
  where
    toCase (fromName, fld) = (fromName, constant $ TTerm $ Terms.inject codName fld)

-- * Definitions and modules

-- | Create a definition in a module
-- Example: definitionInModule myModule "addInts" (lambda "x" (lambda "y" (add @@ var "x" @@ var "y")))
definitionInModule :: Module -> String -> TTerm a -> TElement a
definitionInModule mod = definitionInNamespace $ moduleNamespace mod

-- | Create a definition in a namespace
-- Example: definitionInNamespace (Namespace "com.example") "addInts" myFunction
definitionInNamespace :: Namespace -> String -> TTerm a -> TElement a
definitionInNamespace ns lname = TElement $ unqualifyName $ QualifiedName (Just ns) lname

-- | Convert a typed element to an untyped element
-- Example: el (definitionInModule myModule "addInts" myFunction)
el :: TElement a -> Element
el (TElement name (TTerm term)) = Element name term Nothing

-- | Reference a defined element
-- Example: ref (definitionInModule myModule "addInts")
ref :: TElement a -> TTerm a
ref (TElement name _) = TTerm (TermVariable name)

-- * Metadata and annotations

-- | Add an annotation to a term
-- Example: annot (Name "deprecated") (Just (boolean True)) myFunction
annot :: Name -> Maybe Term -> TTerm a -> TTerm a
annot key mvalue (TTerm term) = TTerm $ Ann.annotateTerm key mvalue term

-- | Add documentation to a term
-- Example: doc "Adds two integers" addFunction
doc :: String -> TTerm a -> TTerm a
doc s (TTerm term) = TTerm $ setTermDescription (Just s) term

-- | Add documentation with line wrapping at the specified width
-- Example: docWrapped 80 "This is a long documentation string that will be wrapped..." myFunction
docWrapped :: Int -> String -> TTerm a -> TTerm a
docWrapped len = doc . wrapLine len

-- | Mark a type as first-class
-- Example: firstClassType (record ...)
firstClassType :: TTerm Type -> TTerm Type
firstClassType typ = annot key_firstClassType (Just $ Terms.boolean True) typ

-- | Associate type classes with a term
-- Example: withTypeClasses (M.fromList [(Name "Ord", S.singleton ShowTC)]) myTerm
withTypeClasses :: M.Map Name (S.Set TypeClass) -> TTerm a -> TTerm a
withTypeClasses classes (TTerm term) = TTerm $ setTypeClasses classes term
