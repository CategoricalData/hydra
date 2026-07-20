-- | Tests for default primitive implementations.
-- For each primitive that carries a defaultImplementation in its PrimitiveDefinition,
-- these tests verify that the default implementation produces the same result
-- as the native Haskell implementation by evaluating the same input term through
-- both the native graph and the default-impl graph and comparing results.
--
-- The default-impl graph (from Hydra.Test.DefaultImplGraph) replaces native
-- implementations with reducer-based fallbacks for primitives that have a
-- defaultImplementation; all other primitives retain their native implementations
-- so cross-primitive calls resolve correctly.

module Hydra.DefaultImplSpec where

import Hydra.Kernel
import Hydra.Reduction (reduceTerm)
import Hydra.Print.Core (term)
import Hydra.Test.DefaultImplGraph (defaultImplGraph, defaultImplPrimCount)
import Hydra.Test.TestGraph (testGraph, testTypes, testTerms)

import qualified Hydra.Lib.Eithers  as DefEithers
import qualified Hydra.Lib.Equality as DefEquality
import qualified Hydra.Lib.Lists    as DefLists
import qualified Hydra.Lib.Logic    as DefLogic
import qualified Hydra.Lib.Maps     as DefMaps
import qualified Hydra.Lib.Math     as DefMath
import qualified Hydra.Lib.Optionals as DefOptionals
import qualified Hydra.Lib.Pairs    as DefPairs
import qualified Hydra.Lib.Sets     as DefSets
import qualified Hydra.Packaging    as Packaging

import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Test.Hspec as H


-- | The test graph with default implementations substituted.
diGraph :: Graph
diGraph = defaultImplGraph testTypes testTerms

-- | Evaluate a term against the default-impl graph, returning a string.
evalDI :: Term -> String
evalDI t = case reduceTerm emptyInferenceContext diGraph True t of
  Left err    -> "<<error: " ++ show err ++ ">>"
  Right result -> term result

-- | Evaluate the same term against the native test graph.
evalNative :: Term -> String
evalNative t = case reduceTerm emptyInferenceContext testGraph True t of
  Left err    -> "<<error: " ++ show err ++ ">>"
  Right result -> term result

-- | Assert default-impl result equals native result.
checkDefaultMatchesNative :: Term -> H.Expectation
checkDefaultMatchesNative t = evalDI t `H.shouldBe` evalNative t

-- | Apply a primitive definition by name to a list of argument terms.
applyPrim :: Packaging.PrimitiveDefinition -> [Term] -> Term
applyPrim pd = L.foldl (\f a -> TermApplication (Application f a))
  (TermVariable (Packaging.primitiveDefinitionName pd))

-- | Shorthand: the Name of a primitive definition.
primName :: Packaging.PrimitiveDefinition -> Name
primName = Packaging.primitiveDefinitionName

-- | Apply a named primitive to one argument.
applyN :: Packaging.PrimitiveDefinition -> Term -> Term
applyN pd arg = TermApplication (Application (TermVariable (primName pd)) arg)

-- | Partially apply a named primitive to one argument.
applyN2 :: Packaging.PrimitiveDefinition -> Term -> Term -> Term
applyN2 pd a b = TermApplication (Application (applyN pd a) b)

-- Term constructors
bool_ :: Bool -> Term
bool_ b = TermLiteral (LiteralBoolean b)

int32_ :: Int -> Term
int32_ n = TermLiteral (LiteralInteger (IntegerValueInt32 (fromIntegral n)))

list_ :: [Term] -> Term
list_ = TermList

set_ :: [Term] -> Term
set_ ts = TermSet (S.fromList ts)

map_ :: [(Term, Term)] -> Term
map_ pairs = TermMap (M.fromList pairs)

just_ :: Term -> Term
just_ t = TermOptional (Just t)

nothing_ :: Term
nothing_ = TermOptional Nothing

left_ :: Term -> Term
left_ t = TermEither (Left t)

right_ :: Term -> Term
right_ t = TermEither (Right t)

pair_ :: Term -> Term -> Term
pair_ a b = TermPair (a, b)

-- | A 1-argument lambda: \x -> body
lam1 :: String -> (Term -> Term) -> Term
lam1 x body = TermLambda (Lambda (Name x) Nothing (body (TermVariable (Name x))))

-- | A 2-argument lambda: \x y -> body
lam2 :: String -> String -> (Term -> Term -> Term) -> Term
lam2 x y body = lam1 x (\xv -> lam1 y (\yv -> body xv yv))

-- | Apply prim with a 1-arg lambda as first argument, then extra args.
withFn1 :: Packaging.PrimitiveDefinition -> (Term -> Term) -> [Term] -> Term
withFn1 pd fn rest = applyPrim pd (lam1 "x" fn : rest)

-- | Apply prim with a 2-arg lambda as first argument, then extra args.
withFn2 :: Packaging.PrimitiveDefinition -> (Term -> Term -> Term) -> [Term] -> Term
withFn2 pd fn rest = applyPrim pd (lam2 "x" "y" fn : rest)


spec :: H.Spec
spec = do
  H.describe ("Default primitive implementation tests (" ++ show defaultImplPrimCount ++ " primitives with defaults)") $ do

    H.it "at least 30 primitives have default implementations" $
      defaultImplPrimCount `H.shouldSatisfy` (>= 30)

    -- -------------------------------------------------------------------------
    H.describe "hydra.lib.logic" $ do
      H.describe "and" $ do
        H.it "true && true"   $ checkDefaultMatchesNative (applyPrim DefLogic.and [bool_ True,  bool_ True])
        H.it "true && false"  $ checkDefaultMatchesNative (applyPrim DefLogic.and [bool_ True,  bool_ False])
        H.it "false && true"  $ checkDefaultMatchesNative (applyPrim DefLogic.and [bool_ False, bool_ True])
        H.it "false && false" $ checkDefaultMatchesNative (applyPrim DefLogic.and [bool_ False, bool_ False])
      H.describe "not" $ do
        H.it "not true"  $ checkDefaultMatchesNative (applyPrim DefLogic.not [bool_ True])
        H.it "not false" $ checkDefaultMatchesNative (applyPrim DefLogic.not [bool_ False])
      H.describe "or" $ do
        H.it "true || true"   $ checkDefaultMatchesNative (applyPrim DefLogic.or [bool_ True,  bool_ True])
        H.it "true || false"  $ checkDefaultMatchesNative (applyPrim DefLogic.or [bool_ True,  bool_ False])
        H.it "false || false" $ checkDefaultMatchesNative (applyPrim DefLogic.or [bool_ False, bool_ False])

    -- -------------------------------------------------------------------------
    H.describe "hydra.lib.math" $ do
      H.describe "even" $ do
        H.it "even 0"  $ checkDefaultMatchesNative (applyPrim DefMath.even [int32_ 0])
        H.it "even 2"  $ checkDefaultMatchesNative (applyPrim DefMath.even [int32_ 2])
        H.it "even 3"  $ checkDefaultMatchesNative (applyPrim DefMath.even [int32_ 3])
        H.it "even -4" $ checkDefaultMatchesNative (applyPrim DefMath.even [int32_ (-4)])
        H.it "even 7"  $ checkDefaultMatchesNative (applyPrim DefMath.even [int32_ 7])
      H.describe "odd" $ do
        H.it "odd 0"  $ checkDefaultMatchesNative (applyPrim DefMath.odd [int32_ 0])
        H.it "odd 1"  $ checkDefaultMatchesNative (applyPrim DefMath.odd [int32_ 1])
        H.it "odd 3"  $ checkDefaultMatchesNative (applyPrim DefMath.odd [int32_ 3])
        H.it "odd 4"  $ checkDefaultMatchesNative (applyPrim DefMath.odd [int32_ 4])
        H.it "odd -5" $ checkDefaultMatchesNative (applyPrim DefMath.odd [int32_ (-5)])

    -- -------------------------------------------------------------------------
    H.describe "hydra.lib.equality" $ do
      H.describe "identity" $ do
        H.it "identity 42"    $ checkDefaultMatchesNative (applyPrim DefEquality.identity [int32_ 42])
        H.it "identity true"  $ checkDefaultMatchesNative (applyPrim DefEquality.identity [bool_ True])
        H.it "identity false" $ checkDefaultMatchesNative (applyPrim DefEquality.identity [bool_ False])
      H.describe "max" $ do
        H.it "max 3 5"   $ checkDefaultMatchesNative (applyPrim DefEquality.max [int32_ 3, int32_ 5])
        H.it "max 7 2"   $ checkDefaultMatchesNative (applyPrim DefEquality.max [int32_ 7, int32_ 2])
        H.it "max 4 4"   $ checkDefaultMatchesNative (applyPrim DefEquality.max [int32_ 4, int32_ 4])
      H.describe "min" $ do
        H.it "min 3 5"   $ checkDefaultMatchesNative (applyPrim DefEquality.min [int32_ 3, int32_ 5])
        H.it "min 7 2"   $ checkDefaultMatchesNative (applyPrim DefEquality.min [int32_ 7, int32_ 2])
        H.it "min 4 4"   $ checkDefaultMatchesNative (applyPrim DefEquality.min [int32_ 4, int32_ 4])

    -- -------------------------------------------------------------------------
    H.describe "hydra.lib.optionals" $ do
      H.describe "isGiven" $ do
        H.it "isGiven (Just 1)" $ checkDefaultMatchesNative (applyPrim DefOptionals.isGiven [just_ (int32_ 1)])
        H.it "isGiven Nothing"  $ checkDefaultMatchesNative (applyPrim DefOptionals.isGiven [nothing_])
      H.describe "isNone" $ do
        H.it "isNone Nothing"   $ checkDefaultMatchesNative (applyPrim DefOptionals.isNone [nothing_])
        H.it "isNone (Just 1)"  $ checkDefaultMatchesNative (applyPrim DefOptionals.isNone [just_ (int32_ 1)])
      H.describe "fromOptional" $ do
        H.it "fromOptional 0 (Just 42)" $ checkDefaultMatchesNative (applyPrim DefOptionals.fromOptional [int32_ 0, just_ (int32_ 42)])
        H.it "fromOptional 0 Nothing"   $ checkDefaultMatchesNative (applyPrim DefOptionals.fromOptional [int32_ 0, nothing_])
      H.describe "pure" $ do
        H.it "pure 42"   $ checkDefaultMatchesNative (applyPrim DefOptionals.pure [int32_ 42])
        H.it "pure true" $ checkDefaultMatchesNative (applyPrim DefOptionals.pure [bool_ True])
      H.describe "toList" $ do
        H.it "toList (Just 3)" $ checkDefaultMatchesNative (applyPrim DefOptionals.toList [just_ (int32_ 3)])
        H.it "toList Nothing"  $ checkDefaultMatchesNative (applyPrim DefOptionals.toList [nothing_])
      H.describe "map" $ do
        H.it "map negate (Just 5)" $ checkDefaultMatchesNative
          (withFn1 DefOptionals.map (\x -> applyN DefMath.negate x) [just_ (int32_ 5)])
        H.it "map negate Nothing"  $ checkDefaultMatchesNative
          (withFn1 DefOptionals.map (\x -> applyN DefMath.negate x) [nothing_])
      H.describe "bind" $ do
        H.it "bind (\\x -> Just (x+1)) (Just 5)" $ checkDefaultMatchesNative
          (withFn1 DefOptionals.bind
            (\x -> just_ (applyN2 DefMath.add x (int32_ 1)))
            [just_ (int32_ 5)])
        H.it "bind (\\x -> Just x) Nothing" $ checkDefaultMatchesNative
          (withFn1 DefOptionals.bind (\x -> just_ x) [nothing_])
      H.describe "apply" $ do
        H.it "apply (Just negate) (Just 5)" $ checkDefaultMatchesNative
          (applyPrim DefOptionals.apply
            [ just_ (lam1 "x" (\x -> applyN DefMath.negate x))
            , just_ (int32_ 5)])
        H.it "apply Nothing (Just 5)" $ checkDefaultMatchesNative
          (applyPrim DefOptionals.apply [nothing_, just_ (int32_ 5)])
        H.it "apply (Just negate) Nothing" $ checkDefaultMatchesNative
          (applyPrim DefOptionals.apply
            [ just_ (lam1 "x" (\x -> applyN DefMath.negate x))
            , nothing_])
      H.describe "cat" $ do
        H.it "cat [Just 1, Nothing, Just 3]" $ checkDefaultMatchesNative
          (applyPrim DefOptionals.cat [list_ [just_ (int32_ 1), nothing_, just_ (int32_ 3)]])
        H.it "cat []" $ checkDefaultMatchesNative
          (applyPrim DefOptionals.cat [list_ []])
        H.it "cat [Nothing, Nothing]" $ checkDefaultMatchesNative
          (applyPrim DefOptionals.cat [list_ [nothing_, nothing_]])
      H.describe "compose" $ do
        -- compose f g x = bind (f x) g
        H.it "compose (Just . negate) (Just . even) 4" $ checkDefaultMatchesNative
          (applyPrim DefOptionals.compose
            [ lam1 "x" (\x -> just_ (applyN DefMath.negate x))
            , lam1 "x" (\x -> just_ (applyN DefMath.even x))
            , int32_ 4])
        H.it "compose (const Nothing) (Just . even) 4" $ checkDefaultMatchesNative
          (applyPrim DefOptionals.compose
            [ lam1 "_" (const nothing_)
            , lam1 "x" (\x -> just_ (applyN DefMath.even x))
            , int32_ 4])
      H.describe "mapOptional" $ do
        H.it "mapOptional Just [1,2,3]" $ checkDefaultMatchesNative
          (withFn1 DefOptionals.mapOptional
            (\x -> just_ x)
            [list_ [int32_ 1, int32_ 2, int32_ 3]])
        H.it "mapOptional (const Nothing) [1,2,3]" $ checkDefaultMatchesNative
          (withFn1 DefOptionals.mapOptional
            (const nothing_)
            [list_ [int32_ 1, int32_ 2, int32_ 3]])
        H.it "mapOptional Just []" $ checkDefaultMatchesNative
          (withFn1 DefOptionals.mapOptional
            (\x -> just_ x)
            [list_ []])

    -- -------------------------------------------------------------------------
    H.describe "hydra.lib.eithers" $ do
      H.describe "isLeft" $ do
        H.it "isLeft (Left 1)"  $ checkDefaultMatchesNative (applyPrim DefEithers.isLeft [left_  (int32_ 1)])
        H.it "isLeft (Right 2)" $ checkDefaultMatchesNative (applyPrim DefEithers.isLeft [right_ (int32_ 2)])
      H.describe "isRight" $ do
        H.it "isRight (Right 2)" $ checkDefaultMatchesNative (applyPrim DefEithers.isRight [right_ (int32_ 2)])
        H.it "isRight (Left 1)"  $ checkDefaultMatchesNative (applyPrim DefEithers.isRight [left_  (int32_ 1)])
      H.describe "lefts" $ do
        H.it "lefts [L 1, R 2, L 3]" $ checkDefaultMatchesNative
          (applyPrim DefEithers.lefts [list_ [left_ (int32_ 1), right_ (int32_ 2), left_ (int32_ 3)]])
        H.it "lefts []" $ checkDefaultMatchesNative
          (applyPrim DefEithers.lefts [list_ []])
      H.describe "rights" $ do
        H.it "rights [L 1, R 2, R 3]" $ checkDefaultMatchesNative
          (applyPrim DefEithers.rights [list_ [left_ (int32_ 1), right_ (int32_ 2), right_ (int32_ 3)]])
        H.it "rights []" $ checkDefaultMatchesNative
          (applyPrim DefEithers.rights [list_ []])
      H.describe "map" $ do
        H.it "map negate (Right 5)" $ checkDefaultMatchesNative
          (withFn1 DefEithers.map (\x -> applyN DefMath.negate x) [right_ (int32_ 5)])
        H.it "map negate (Left 5)"  $ checkDefaultMatchesNative
          (withFn1 DefEithers.map (\x -> applyN DefMath.negate x) [left_  (int32_ 5)])
      H.describe "bimap" $ do
        H.it "bimap negate even (Left 4)" $ checkDefaultMatchesNative
          (applyPrim DefEithers.bimap
            [ lam1 "x" (\x -> applyN DefMath.negate x)
            , lam1 "x" (\x -> applyN DefMath.even x)
            , left_ (int32_ 4)])
        H.it "bimap negate even (Right 3)" $ checkDefaultMatchesNative
          (applyPrim DefEithers.bimap
            [ lam1 "x" (\x -> applyN DefMath.negate x)
            , lam1 "x" (\x -> applyN DefMath.even x)
            , right_ (int32_ 3)])
      H.describe "bind" $ do
        H.it "bind (Right 5) (\\x -> Right (x+1))" $ checkDefaultMatchesNative
          (applyPrim DefEithers.bind
            [ right_ (int32_ 5)
            , lam1 "x" (\x -> right_ (applyN2 DefMath.add x (int32_ 1)))])
        H.it "bind (Left 5) (\\x -> Right (x+1))" $ checkDefaultMatchesNative
          (applyPrim DefEithers.bind
            [ left_ (int32_ 5)
            , lam1 "x" (\x -> right_ (applyN2 DefMath.add x (int32_ 1)))])
      H.describe "fromLeft" $ do
        H.it "fromLeft 0 (Left 7)"  $ checkDefaultMatchesNative (applyPrim DefEithers.fromLeft [int32_ 0, left_  (int32_ 7)])
        H.it "fromLeft 0 (Right 3)" $ checkDefaultMatchesNative (applyPrim DefEithers.fromLeft [int32_ 0, right_ (int32_ 3)])
      H.describe "fromRight" $ do
        H.it "fromRight 0 (Right 7)" $ checkDefaultMatchesNative (applyPrim DefEithers.fromRight [int32_ 0, right_ (int32_ 7)])
        H.it "fromRight 0 (Left 3)"  $ checkDefaultMatchesNative (applyPrim DefEithers.fromRight [int32_ 0, left_  (int32_ 3)])
      H.describe "foldl" $ do
        -- foldl :: (acc -> el -> Either err acc) -> acc -> [el] -> Either err acc
        -- Use (\acc el -> Right (acc + el)) as accumulator
        H.it "foldl (+) 0 [1,2,3]" $ checkDefaultMatchesNative
          (applyPrim DefEithers.foldl
            [ lam2 "acc" "el" (\acc el -> right_ (applyN2 DefMath.add acc el))
            , int32_ 0
            , list_ [int32_ 1, int32_ 2, int32_ 3]])
        H.it "foldl (+) 0 []" $ checkDefaultMatchesNative
          (applyPrim DefEithers.foldl
            [ lam2 "acc" "el" (\acc el -> right_ (applyN2 DefMath.add acc el))
            , int32_ 0
            , list_ []])
      H.describe "mapList" $ do
        -- mapList :: (a -> Either e b) -> [a] -> Either e [b]
        H.it "mapList Right [1,2,3]" $ checkDefaultMatchesNative
          (withFn1 DefEithers.mapList (\x -> right_ x) [list_ [int32_ 1, int32_ 2, int32_ 3]])
        H.it "mapList Right []" $ checkDefaultMatchesNative
          (withFn1 DefEithers.mapList (\x -> right_ x) [list_ []])
      H.describe "mapOptional" $ do
        -- mapOptional :: (a -> Either e b) -> Maybe a -> Either e (Maybe b)
        H.it "mapOptional Right (Just 5)" $ checkDefaultMatchesNative
          (withFn1 DefEithers.mapOptional (\x -> right_ x) [just_ (int32_ 5)])
        H.it "mapOptional Right Nothing" $ checkDefaultMatchesNative
          (withFn1 DefEithers.mapOptional (\x -> right_ x) [nothing_])
      H.describe "mapSet" $ do
        -- mapSet :: (a -> Either e b) -> Set a -> Either e (Set b)
        H.it "mapSet Right {1,2,3}" $ checkDefaultMatchesNative
          (withFn1 DefEithers.mapSet (\x -> right_ x)
            [set_ [int32_ 1, int32_ 2, int32_ 3]])
        H.it "mapSet Right {}" $ checkDefaultMatchesNative
          (withFn1 DefEithers.mapSet (\x -> right_ x) [set_ []])
      H.describe "partitionEithers" $ do
        H.it "partitionEithers [L 1, R 2, L 3, R 4]" $ checkDefaultMatchesNative
          (applyPrim DefEithers.partitionEithers
            [list_ [left_ (int32_ 1), right_ (int32_ 2), left_ (int32_ 3), right_ (int32_ 4)]])
        H.it "partitionEithers []" $ checkDefaultMatchesNative
          (applyPrim DefEithers.partitionEithers [list_ []])

    -- -------------------------------------------------------------------------
    H.describe "hydra.lib.lists" $ do
      H.describe "filter" $ do
        H.it "filter even [1..5]" $ checkDefaultMatchesNative
          (withFn1 DefLists.filter (\x -> applyN DefMath.even x)
            [list_ [int32_ 1, int32_ 2, int32_ 3, int32_ 4, int32_ 5]])
        H.it "filter even []" $ checkDefaultMatchesNative
          (withFn1 DefLists.filter (\x -> applyN DefMath.even x) [list_ []])
      H.describe "find" $ do
        H.it "find even [1,2,3,4]" $ checkDefaultMatchesNative
          (withFn1 DefLists.find (\x -> applyN DefMath.even x)
            [list_ [int32_ 1, int32_ 2, int32_ 3, int32_ 4]])
        H.it "find even []" $ checkDefaultMatchesNative
          (withFn1 DefLists.find (\x -> applyN DefMath.even x) [list_ []])
      H.describe "bind" $ do
        H.it "bind [1,2,3] (\\x -> [x,x])" $ checkDefaultMatchesNative
          (withFn1 DefLists.bind (\x -> list_ [x, x])
            [list_ [int32_ 1, int32_ 2, int32_ 3]])
        H.it "bind [] (\\x -> [x])" $ checkDefaultMatchesNative
          (withFn1 DefLists.bind (\x -> list_ [x]) [list_ []])
      H.describe "dropWhile" $ do
        H.it "dropWhile even [2,4,1,2,3]" $ checkDefaultMatchesNative
          (withFn1 DefLists.dropWhile (\x -> applyN DefMath.even x)
            [list_ [int32_ 2, int32_ 4, int32_ 1, int32_ 2, int32_ 3]])
        H.it "dropWhile even []" $ checkDefaultMatchesNative
          (withFn1 DefLists.dropWhile (\x -> applyN DefMath.even x) [list_ []])
        H.it "dropWhile even [1,2,3]" $ checkDefaultMatchesNative
          (withFn1 DefLists.dropWhile (\x -> applyN DefMath.even x)
            [list_ [int32_ 1, int32_ 2, int32_ 3]])
      H.describe "partition" $ do
        H.it "partition even [1,2,3,4,5]" $ checkDefaultMatchesNative
          (withFn1 DefLists.partition (\x -> applyN DefMath.even x)
            [list_ [int32_ 1, int32_ 2, int32_ 3, int32_ 4, int32_ 5]])
        H.it "partition even []" $ checkDefaultMatchesNative
          (withFn1 DefLists.partition (\x -> applyN DefMath.even x) [list_ []])
      H.describe "span" $ do
        H.it "span even [2,4,1,2,3]" $ checkDefaultMatchesNative
          (withFn1 DefLists.span (\x -> applyN DefMath.even x)
            [list_ [int32_ 2, int32_ 4, int32_ 1, int32_ 2, int32_ 3]])
        H.it "span even []" $ checkDefaultMatchesNative
          (withFn1 DefLists.span (\x -> applyN DefMath.even x) [list_ []])
        H.it "span even [1,2,3]" $ checkDefaultMatchesNative
          (withFn1 DefLists.span (\x -> applyN DefMath.even x)
            [list_ [int32_ 1, int32_ 2, int32_ 3]])

    -- -------------------------------------------------------------------------
    H.describe "hydra.lib.pairs" $ do
      H.describe "bimap" $ do
        H.it "bimap negate not (1, true)" $ checkDefaultMatchesNative
          (applyPrim DefPairs.bimap
            [ lam1 "x" (\x -> applyN DefMath.negate x)
            , lam1 "y" (\y -> applyN DefLogic.not y)
            , pair_ (int32_ 1) (bool_ True)])
        H.it "bimap negate not (0, false)" $ checkDefaultMatchesNative
          (applyPrim DefPairs.bimap
            [ lam1 "x" (\x -> applyN DefMath.negate x)
            , lam1 "y" (\y -> applyN DefLogic.not y)
            , pair_ (int32_ 0) (bool_ False)])

    -- -------------------------------------------------------------------------
    H.describe "hydra.lib.maps" $ do
      H.describe "filter" $ do
        H.it "filter even {1->2, 2->3, 3->4}" $ checkDefaultMatchesNative
          (withFn1 DefMaps.filter (\v -> applyN DefMath.even v)
            [map_ [(int32_ 1, int32_ 2), (int32_ 2, int32_ 3), (int32_ 3, int32_ 4)]])
        H.it "filter even {}" $ checkDefaultMatchesNative
          (withFn1 DefMaps.filter (\v -> applyN DefMath.even v) [map_ []])
      H.describe "filterWithKey" $ do
        H.it "filterWithKey (\\k v -> even k) {1->10, 2->20, 3->30}" $ checkDefaultMatchesNative
          (withFn2 DefMaps.filterWithKey (\k _v -> applyN DefMath.even k)
            [map_ [(int32_ 1, int32_ 10), (int32_ 2, int32_ 20), (int32_ 3, int32_ 30)]])
        H.it "filterWithKey (\\k v -> even v) {1->2, 2->3}" $ checkDefaultMatchesNative
          (withFn2 DefMaps.filterWithKey (\_k v -> applyN DefMath.even v)
            [map_ [(int32_ 1, int32_ 2), (int32_ 2, int32_ 3)]])
      H.describe "findWithDefault" $ do
        H.it "findWithDefault 0 1 {1->10, 2->20}" $ checkDefaultMatchesNative
          (applyPrim DefMaps.findWithDefault
            [int32_ 0, int32_ 1, map_ [(int32_ 1, int32_ 10), (int32_ 2, int32_ 20)]])
        H.it "findWithDefault 0 99 {1->10, 2->20}" $ checkDefaultMatchesNative
          (applyPrim DefMaps.findWithDefault
            [int32_ 0, int32_ 99, map_ [(int32_ 1, int32_ 10), (int32_ 2, int32_ 20)]])
      H.describe "map" $ do
        H.it "map negate {1->3, 2->5}" $ checkDefaultMatchesNative
          (withFn1 DefMaps.map (\v -> applyN DefMath.negate v)
            [map_ [(int32_ 1, int32_ 3), (int32_ 2, int32_ 5)]])
        H.it "map negate {}" $ checkDefaultMatchesNative
          (withFn1 DefMaps.map (\v -> applyN DefMath.negate v) [map_ []])
      H.describe "mapKeys" $ do
        H.it "mapKeys negate {1->10, 2->20}" $ checkDefaultMatchesNative
          (withFn1 DefMaps.mapKeys (\k -> applyN DefMath.negate k)
            [map_ [(int32_ 1, int32_ 10), (int32_ 2, int32_ 20)]])
        H.it "mapKeys negate {}" $ checkDefaultMatchesNative
          (withFn1 DefMaps.mapKeys (\k -> applyN DefMath.negate k) [map_ []])
      H.describe "bimap" $ do
        H.it "bimap negate even {1->3, 2->4}" $ checkDefaultMatchesNative
          (applyPrim DefMaps.bimap
            [ lam1 "k" (\k -> applyN DefMath.negate k)
            , lam1 "v" (\v -> applyN DefMath.even v)
            , map_ [(int32_ 1, int32_ 3), (int32_ 2, int32_ 4)]])
      H.describe "alter" $ do
        -- alter (const (Just 99)) 2 {1->10, 2->20}  -- update existing key
        H.it "alter (const (Just 99)) 2 {1->10, 2->20}" $ checkDefaultMatchesNative
          (applyPrim DefMaps.alter
            [ lam1 "_" (const (just_ (int32_ 99)))
            , int32_ 2
            , map_ [(int32_ 1, int32_ 10), (int32_ 2, int32_ 20)]])
        -- alter (const Nothing) 1 {1->10, 2->20}  -- delete key
        H.it "alter (const Nothing) 1 {1->10, 2->20}" $ checkDefaultMatchesNative
          (applyPrim DefMaps.alter
            [ lam1 "_" (const nothing_)
            , int32_ 1
            , map_ [(int32_ 1, int32_ 10), (int32_ 2, int32_ 20)]])
        -- alter (const (Just 5)) 3 {1->10, 2->20}  -- insert new key
        H.it "alter (const (Just 5)) 3 {1->10, 2->20}" $ checkDefaultMatchesNative
          (applyPrim DefMaps.alter
            [ lam1 "_" (const (just_ (int32_ 5)))
            , int32_ 3
            , map_ [(int32_ 1, int32_ 10), (int32_ 2, int32_ 20)]])

    -- -------------------------------------------------------------------------
    H.describe "hydra.lib.sets" $ do
      H.describe "difference" $ do
        H.it "difference {1,2,3} {2,3,4}" $ checkDefaultMatchesNative
          (applyPrim DefSets.difference [set_ [int32_ 1, int32_ 2, int32_ 3], set_ [int32_ 2, int32_ 3, int32_ 4]])
        H.it "difference {1,2} {}" $ checkDefaultMatchesNative
          (applyPrim DefSets.difference [set_ [int32_ 1, int32_ 2], set_ []])
        H.it "difference {} {1,2}" $ checkDefaultMatchesNative
          (applyPrim DefSets.difference [set_ [], set_ [int32_ 1, int32_ 2]])
      H.describe "intersection" $ do
        H.it "intersection {1,2,3} {2,3,4}" $ checkDefaultMatchesNative
          (applyPrim DefSets.intersection [set_ [int32_ 1, int32_ 2, int32_ 3], set_ [int32_ 2, int32_ 3, int32_ 4]])
        H.it "intersection {1,2} {}" $ checkDefaultMatchesNative
          (applyPrim DefSets.intersection [set_ [int32_ 1, int32_ 2], set_ []])
      H.describe "map" $ do
        H.it "map negate {1,2,3}" $ checkDefaultMatchesNative
          (withFn1 DefSets.map (\x -> applyN DefMath.negate x) [set_ [int32_ 1, int32_ 2, int32_ 3]])
        H.it "map negate {}" $ checkDefaultMatchesNative
          (withFn1 DefSets.map (\x -> applyN DefMath.negate x) [set_ []])
      H.describe "union" $ do
        H.it "union {1,2} {2,3}" $ checkDefaultMatchesNative
          (applyPrim DefSets.union [set_ [int32_ 1, int32_ 2], set_ [int32_ 2, int32_ 3]])
        H.it "union {} {1,2}" $ checkDefaultMatchesNative
          (applyPrim DefSets.union [set_ [], set_ [int32_ 1, int32_ 2]])
      H.describe "unions" $ do
        H.it "unions [{1,2},{2,3},{3,4}]" $ checkDefaultMatchesNative
          (applyPrim DefSets.unions
            [list_ [set_ [int32_ 1, int32_ 2], set_ [int32_ 2, int32_ 3], set_ [int32_ 3, int32_ 4]]])
        H.it "unions []" $ checkDefaultMatchesNative
          (applyPrim DefSets.unions [list_ []])
