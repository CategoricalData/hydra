{-# LANGUAGE FlexibleContexts #-}

-- | Test cases for let-binding hoisting transformations

module Hydra.Sources.Test.Hoisting.Let where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

import Hydra.Testing
import Hydra.Sources.Libraries

import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Hoisting as HoistingModule


ns :: Namespace
ns = Namespace "hydra.test.hoisting.let"

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleTermDependencies = [ShowCore.ns, HoistingModule.ns],
            moduleTypeDependencies = kernelTypesNamespaces,
            moduleDescription = Just "Test cases for let-binding hoisting transformations"}
  where
    definitions = [Phantoms.toDefinition allTests]

allTests :: TTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for let-binding hoisting transformations" $
    supergroup "hoistLet" [
      hoistLetBindingsGroup,
      hoistPolymorphicLetBindingsGroup,
      hoistPolymorphicTypeParametersGroup]

-- Helper to build names
nm :: String -> TTerm Name
nm s = Core.name $ Phantoms.string s

-- Local alias for polymorphic application (Phantoms.@@ applies TBindings; Terms.@@ only works on TTerm Term)
(#) :: (AsTerm f (a -> b), AsTerm g a) => f -> g -> TTerm b
(#) = (Phantoms.@@)
infixl 1 #

-- | Show a Let as a string using ShowCore.let_
showLet :: TTerm Let -> TTerm String
showLet l = ShowCore.let_ # l

-- | Local universal version of hoistLetBindingsCase (hoistAll=True)
hoistLetBindingsCase :: String -> TTerm Let -> TTerm Let -> TTerm TestCaseWithMetadata
hoistLetBindingsCase cname input output = universalCase cname
  (showLet (HoistingModule.hoistAllLetBindings # input))
  (showLet output)

-- | Local universal version of hoistPolymorphicLetBindingsCase
hoistPolymorphicLetBindingsCase :: String -> TTerm Let -> TTerm Let -> TTerm TestCaseWithMetadata
hoistPolymorphicLetBindingsCase cname input output = universalCase cname
  (showLet (HoistingModule.hoistPolymorphicLetBindings # Phantoms.lambda "b" (Phantoms.boolean True) # input))
  (showLet output)

-- Helper for single-binding let
letExpr :: String -> TTerm Term -> TTerm Term -> TTerm Term
letExpr varName value body = lets [(nm varName, value)] body

-- Helper for multi-binding let
multiLet :: [(String, TTerm Term)] -> TTerm Term -> TTerm Term
multiLet bindings body = lets ((\(n, v) -> (nm n, v)) <$> bindings) body

-- | Convenience function for creating hoist let bindings test cases (hoistAll=True)
hoistAllCase :: String -> TTerm Let -> TTerm Let -> TTerm TestCaseWithMetadata
hoistAllCase cname input output = hoistLetBindingsCase cname input output

-- | Helper for creating a Let term with typed bindings
mkLet :: [(TTerm Name, TTerm Term, TTerm (Maybe TypeScheme))] -> TTerm Term -> TTerm Let
mkLet bindings body = Core.let_ (Phantoms.list $ mkBinding <$> bindings) body
  where
    mkBinding :: (TTerm Name, TTerm Term, TTerm (Maybe TypeScheme)) -> TTerm Binding
    mkBinding (n, t, ts) = Core.binding n t ts

-- | Helper for creating a Let term with untyped bindings
mkLetUntyped :: [(TTerm Name, TTerm Term)] -> TTerm Term -> TTerm Let
mkLetUntyped bindings body = Core.let_ (Phantoms.list $ mkBinding <$> bindings) body
  where
    mkBinding :: (TTerm Name, TTerm Term) -> TTerm Binding
    mkBinding (n, t) = Core.binding n t Phantoms.nothing

-- | Helper for creating a monomorphic type scheme
monoType :: TTerm Type -> TTerm (Maybe TypeScheme)
monoType typ = Phantoms.just $ Core.typeScheme (Phantoms.list ([] :: [TTerm Name])) typ Phantoms.nothing

-- | Helper for creating a polymorphic type scheme
polyType :: [String] -> TTerm Type -> TTerm (Maybe TypeScheme)
polyType vars typ = Phantoms.just $ Core.typeScheme (Phantoms.list $ nm <$> vars) typ Phantoms.nothing

-- | Convenience function for creating hoist polymorphic let bindings test cases
hoistPolyCase :: String -> TTerm Let -> TTerm Let -> TTerm TestCaseWithMetadata
hoistPolyCase cname input output = hoistPolymorphicLetBindingsCase cname input output

-- | Test cases for hoistPolymorphicLetBindings
-- This function hoists polymorphic let bindings (those with non-empty type scheme variables)
-- to the top level of the given let term.
hoistPolymorphicLetBindingsGroup :: TTerm TestGroup
hoistPolymorphicLetBindingsGroup = subgroup "hoistPolymorphicLetBindings" [
    -- ============================================================
    -- Test: No polymorphic bindings - unchanged
    -- ============================================================

    hoistPolyCase "no polymorphic bindings: simple let unchanged"
      -- Input: let x : Int32 = 42 in x
      (mkLet [(nm "x", int32 42, monoType T.int32)] (var "x"))
      -- Output: unchanged
      (mkLet [(nm "x", int32 42, monoType T.int32)] (var "x")),

    hoistPolyCase "no polymorphic bindings: multiple monomorphic bindings"
      -- Input: let x : Int32 = 1; y : String = "hi" in pair x y
      (mkLet [
        (nm "x", int32 1, monoType T.int32),
        (nm "y", string "hi", monoType T.string)]
        (apply (apply (var "pair") (var "x")) (var "y")))
      -- Output: unchanged
      (mkLet [
        (nm "x", int32 1, monoType T.int32),
        (nm "y", string "hi", monoType T.string)]
        (apply (apply (var "pair") (var "x")) (var "y"))),

    -- ============================================================
    -- Test: Single polymorphic binding at top level - stays in place
    -- ============================================================

    hoistPolyCase "single polymorphic binding: already at top level"
      -- Input: let id : forall a. a -> a = \x -> x in id 42
      (mkLet [(nm "id", lambda "x" (var "x"), polyType ["a"] (T.function (T.var "a") (T.var "a")))]
        (apply (var "id") (int32 42)))
      -- Output: unchanged (already at top)
      (mkLet [(nm "id", lambda "x" (var "x"), polyType ["a"] (T.function (T.var "a") (T.var "a")))]
        (apply (var "id") (int32 42))),

    -- ============================================================
    -- Test: Polymorphic binding inside lambda body
    -- ============================================================

    hoistPolyCase "polymorphic binding inside lambda: no capture"
      -- Input: let f = \a -> (let id : forall b. b -> b = \x -> x in id a) in f 42
      -- Here 'id' does not reference 'a', so it can be hoisted directly
      (mkLet [(nm "f",
        lambda "a" (Core.termLet $ mkLet [(nm "id", lambda "x" (var "x"), polyType ["b"] (T.function (T.var "b") (T.var "b")))]
          (apply (var "id") (var "a"))),
        monoType (T.function T.int32 T.int32))]
        (apply (var "f") (int32 42)))
      -- Output: f comes first (original binding), then f_id is hoisted (no wrapping needed)
      (mkLet [
        (nm "f",
          lambda "a" (apply (var "f_id") (var "a")),
          monoType (T.function T.int32 T.int32)),
        (nm "f_id", tylam "b" (lambda "x" (var "x")), polyType ["b"] (T.function (T.var "b") (T.var "b")))]
        (apply (var "f") (int32 42))),

    -- ============================================================
    -- Test: Polymorphic binding captures lambda variable - requires wrapping
    -- ============================================================

    hoistPolyCase "polymorphic binding captures lambda variable: wrapped in lambda"
      -- Input: let f = \(a:String) -> (let g : forall b. b -> Pair<String, b> = \x -> pair a x in g 42) in f "hello"
      -- Here 'g' references 'a' (the enclosing lambda's variable), so when hoisted,
      -- it must be wrapped in a lambda that takes 'a', and the reference replaced with an application
      (mkLet [(nm "f",
        lambdaTyped "a" T.string (Core.termLet $ mkLet [
          (nm "g", lambda "x" (apply (apply (var "pair") (var "a")) (var "x")),
            polyType ["b"] (T.function (T.var "b") (T.pair T.string (T.var "b"))))]
          (apply (var "g") (int32 42))),
        monoType (T.function T.string (T.pair T.string T.int32)))]
        (apply (var "f") (string "hello")))
      -- Output: f comes first (original binding), then f_g is hoisted wrapped in \(a:String) -> ..., and reference becomes (f_g a)
      (mkLet [
        (nm "f",
          lambdaTyped "a" T.string (apply (apply (var "f_g") (var "a")) (int32 42)),
          monoType (T.function T.string (T.pair T.string T.int32))),
        (nm "f_g", tylam "b" (lambdaTyped "a" T.string (lambda "x" (apply (apply (var "pair") (var "a")) (var "x")))),
          polyType ["b"] (T.function T.string (T.function (T.var "b") (T.pair T.string (T.var "b")))))]
        (apply (var "f") (string "hello"))),

    hoistPolyCase "polymorphic binding captures multiple lambda variables"
      -- Input: let f = \(a:Int32) -> \(b:Int32) -> (let g : forall c. c -> c = \x -> triple a b x in g 42) in f 1 2
      -- Here 'g' references both 'a' and 'b', so it needs to be wrapped with both
      (mkLet [(nm "f",
        lambdaTyped "a" T.int32 (lambdaTyped "b" T.int32 (Core.termLet $ mkLet [
          (nm "g", lambda "x" (apply (apply (apply (var "triple") (var "a")) (var "b")) (var "x")),
            polyType ["c"] (T.function (T.var "c") (T.var "c")))]
          (apply (var "g") (int32 42)))),
        monoType (T.function T.int32 (T.function T.int32 T.int32)))]
        (apply (apply (var "f") (int32 1)) (int32 2)))
      -- Output: f comes first (original binding), then f_g is wrapped in \(a:Int32) -> \(b:Int32) -> ..., references become ((f_g a) b)
      (mkLet [
        (nm "f",
          lambdaTyped "a" T.int32 (lambdaTyped "b" T.int32 (apply (apply (apply (var "f_g") (var "a")) (var "b")) (int32 42))),
          monoType (T.function T.int32 (T.function T.int32 T.int32))),
        (nm "f_g", tylam "c" (lambdaTyped "a" T.int32 (lambdaTyped "b" T.int32 (lambda "x" (apply (apply (apply (var "triple") (var "a")) (var "b")) (var "x"))))),
          polyType ["c"] (T.function T.int32 (T.function T.int32 (T.function (T.var "c") (T.var "c")))))]
        (apply (apply (var "f") (int32 1)) (int32 2))),

    hoistPolyCase "polymorphic binding captures some but not all lambda variables"
      -- Input: let f = \(a:Int32) -> \(b:Int32) -> (let g : forall c. c -> Pair<Int32, c> = \x -> pair a x in g b) in f 1 2
      -- Here 'g' only references 'a', not 'b'
      (mkLet [(nm "f",
        lambdaTyped "a" T.int32 (lambdaTyped "b" T.int32 (Core.termLet $ mkLet [
          (nm "g", lambda "x" (apply (apply (var "pair") (var "a")) (var "x")),
            polyType ["c"] (T.function (T.var "c") (T.pair T.int32 (T.var "c"))))]
          (apply (var "g") (var "b")))),
        monoType (T.function T.int32 (T.function T.int32 (T.pair T.int32 T.int32))))]
        (apply (apply (var "f") (int32 1)) (int32 2)))
      -- Output: f comes first (original binding), then f_g is wrapped only in \(a:Int32) -> ..., reference becomes (f_g a)
      (mkLet [
        (nm "f",
          lambdaTyped "a" T.int32 (lambdaTyped "b" T.int32 (apply (apply (var "f_g") (var "a")) (var "b"))),
          monoType (T.function T.int32 (T.function T.int32 (T.pair T.int32 T.int32)))),
        (nm "f_g", tylam "c" (lambdaTyped "a" T.int32 (lambda "x" (apply (apply (var "pair") (var "a")) (var "x")))),
          polyType ["c"] (T.function T.int32 (T.function (T.var "c") (T.pair T.int32 (T.var "c")))))]
        (apply (apply (var "f") (int32 1)) (int32 2))),

    hoistPolyCase "polymorphic binding captures both lambda-bound and let-bound variables"
      -- Input: let f = \(a:Int32) -> (let x : Int32 = 1; g : forall b. b -> b = \y -> add (add a x) y in g 42) in f 10
      -- Here 'g' references 'a' (lambda-bound from enclosing scope) and 'x' (let-bound sibling).
      -- When g is hoisted to the top level, it goes OUTSIDE the inner let block, so it must capture
      -- both 'a' (from the enclosing lambda) AND 'x' (which is no longer a sibling after hoisting).
      (mkLet [(nm "f",
        lambdaTyped "a" T.int32 (Core.termLet $ mkLet [
          (nm "x", int32 1, monoType T.int32),
          (nm "g", lambda "y" (apply (apply (primitive _math_add)
                    (apply (apply (primitive _math_add) (var "a")) (var "x"))) (var "y")),
            polyType ["b"] (T.function (T.var "b") (T.var "b")))]
          (apply (var "g") (int32 42))),
        monoType (T.function T.int32 T.int32))]
        (apply (var "f") (int32 10)))
      -- Output: f comes first (original binding), then f_g captures both 'a' and 'x', reference becomes ((f_g a) x)
      (mkLet [
        (nm "f",
          lambdaTyped "a" T.int32 (Core.termLet $ mkLet [
            (nm "x", int32 1, monoType T.int32)]
            (apply (apply (apply (var "f_g") (var "a")) (var "x")) (int32 42))),
          monoType (T.function T.int32 T.int32)),
        (nm "f_g", tylam "b" (lambdaTyped "a" T.int32 (lambdaTyped "x" T.int32
          (lambda "y" (apply (apply (primitive _math_add)
            (apply (apply (primitive _math_add) (var "a")) (var "x"))) (var "y"))))),
          polyType ["b"] (T.function T.int32 (T.function T.int32 (T.function (T.var "b") (T.var "b")))))]
        (apply (var "f") (int32 10))),

    hoistPolyCase "sibling polymorphic bindings inside lambda: one calls the other"
      -- Input: let wrapper = \outer : Int32 -> let g : forall a. a -> a = \y -> add outer y; h : forall b. b -> b = \z -> g z in h 42 in wrapper 10
      -- This mimics the structure in unifyTypeConstraints where bind and tryBinding are
      -- both inside withConstraint lambda and one calls the other.
      -- g captures 'outer', h calls g which means h must transitively capture 'outer'
      (mkLet [(nm "wrapper",
        lambdaTyped "outer" T.int32 (Core.termLet $ mkLet [
          (nm "g", lambda "y" (apply (apply (primitive _math_add) (var "outer")) (var "y")),
            polyType ["a"] (T.function (T.var "a") (T.var "a"))),
          (nm "h", lambda "z" (apply (var "g") (var "z")),
            polyType ["b"] (T.function (T.var "b") (T.var "b")))]
          (apply (var "h") (int32 42))),
        monoType (T.function T.int32 T.int32))]
        (apply (var "wrapper") (int32 10)))
      -- Output: wrapper first (original), then wrapper_g and wrapper_h hoisted (both capture outer)
      -- wrapper_g is wrapped: \outer -> \y -> add outer y
      -- wrapper_h is wrapped: \outer -> \z -> (wrapper_g outer) z
      -- The reference to wrapper_g inside wrapper_h becomes (wrapper_g outer)
      (mkLet [
        (nm "wrapper",
          lambdaTyped "outer" T.int32 (apply (apply (var "wrapper_h") (var "outer")) (int32 42)),
          monoType (T.function T.int32 T.int32)),
        (nm "wrapper_g", tylam "a" (lambdaTyped "outer" T.int32 (lambda "y" (apply (apply (primitive _math_add) (var "outer")) (var "y")))),
          polyType ["a"] (T.function T.int32 (T.function (T.var "a") (T.var "a")))),
        (nm "wrapper_h", tylam "b" (lambdaTyped "outer" T.int32 (lambda "z" (apply (apply (var "wrapper_g") (var "outer")) (var "z")))),
          polyType ["b"] (T.function T.int32 (T.function (T.var "b") (T.var "b"))))]
        (apply (var "wrapper") (int32 10))),

    hoistPolyCase "sibling polymorphic bindings inside lambda: h passes its own args to g"
      -- Input: let wrapper = \outer -> let g : forall a. a -> a = \v -> \t -> add outer (add v t);
      --                                    h : forall b. b -> b = \v -> \t -> g v t
      --                                in h 1 2
      --        in wrapper 10
      -- Here h calls g with its own args v and t. After hoisting:
      -- g becomes \outer -> \v -> \t -> add outer (add v t)
      -- h becomes \outer -> \v -> \t -> ((g outer) v) t
      -- The reference to g in h should NOT duplicate outer, v, or t
      (mkLet [(nm "wrapper",
        lambdaTyped "outer" T.int32 (Core.termLet $ mkLet [
          (nm "g", lambda "v" (lambda "t" (apply (apply (primitive _math_add) (var "outer"))
            (apply (apply (primitive _math_add) (var "v")) (var "t")))),
            polyType ["a"] (T.function (T.var "a") (T.function (T.var "a") (T.var "a")))),
          (nm "h", lambda "v" (lambda "t" (apply (apply (var "g") (var "v")) (var "t"))),
            polyType ["b"] (T.function (T.var "b") (T.function (T.var "b") (T.var "b"))))]
          (apply (apply (var "h") (int32 1)) (int32 2))),
        monoType (T.function T.int32 T.int32))]
        (apply (var "wrapper") (int32 10)))
      -- Output: wrapper first (original), then wrapper_g and wrapper_h hoisted (both capture outer)
      -- wrapper_g: \outer -> \v -> \t -> add outer (add v t)
      -- wrapper_h: \outer -> \v -> \t -> ((wrapper_g outer) v) t
      (mkLet [
        (nm "wrapper",
          lambdaTyped "outer" T.int32 (apply (apply (apply (var "wrapper_h") (var "outer")) (int32 1)) (int32 2)),
          monoType (T.function T.int32 T.int32)),
        (nm "wrapper_g", tylam "a" (lambdaTyped "outer" T.int32
          (lambda "v" (lambda "t" (apply (apply (primitive _math_add) (var "outer"))
            (apply (apply (primitive _math_add) (var "v")) (var "t")))))),
          polyType ["a"] (T.function T.int32 (T.function (T.var "a") (T.function (T.var "a") (T.var "a"))))),
        (nm "wrapper_h", tylam "b" (lambdaTyped "outer" T.int32
          (lambda "v" (lambda "t" (apply (apply (apply (var "wrapper_g") (var "outer")) (var "v")) (var "t"))))),
          polyType ["b"] (T.function T.int32 (T.function (T.var "b") (T.function (T.var "b") (T.var "b")))))]
        (apply (var "wrapper") (int32 10))),

    -- ============================================================
    -- Test: Untyped bindings are treated as monomorphic
    -- ============================================================

    hoistPolyCase "untyped binding: not hoisted"
      -- Input: let x = 1 in (let y = 2 in x + y)
      -- Neither binding has a type, so neither is polymorphic
      (mkLetUntyped [(nm "x", int32 1)]
        (Core.termLet $ mkLetUntyped [(nm "y", int32 2)]
          (apply (apply (primitive _math_add) (var "x")) (var "y"))))
      -- Output: unchanged (untyped bindings are not polymorphic)
      (mkLetUntyped [(nm "x", int32 1)]
        (Core.termLet $ mkLetUntyped [(nm "y", int32 2)]
          (apply (apply (primitive _math_add) (var "x")) (var "y")))),

    -- ============================================================
    -- Test: Name collision - nested binding has same name as top-level (after unshadowing)
    -- Note: This test uses distinct names as unshadowVariables would produce.
    -- The hoisting code assumes no shadowing. If there were shadowing,
    -- two bindings named 'id' would collide at top level.
    -- ============================================================

    hoistPolyCase "no name collision: distinct names after unshadowing"
      -- Input: let id = \x -> x; f = \a -> (let id2 : forall b. b -> b = \y -> y in id2 (id a)) in f 42
      -- Here 'id' is monomorphic at top level, 'id2' is polymorphic nested
      -- They have distinct names (as unshadowVariables would ensure)
      (mkLet [
        (nm "id", lambda "x" (var "x"), monoType (T.function T.int32 T.int32)),
        (nm "f",
          lambda "a" (Core.termLet $ mkLet [
            (nm "id2", lambda "y" (var "y"), polyType ["b"] (T.function (T.var "b") (T.var "b")))]
            (apply (var "id2") (apply (var "id") (var "a")))),
          monoType (T.function T.int32 T.int32))]
        (apply (var "f") (int32 42)))
      -- Output: id and f first (original), then f_id2 hoisted
      (mkLet [
        (nm "id", lambda "x" (var "x"), monoType (T.function T.int32 T.int32)),
        (nm "f",
          lambda "a" (apply (var "f_id2") (apply (var "id") (var "a"))),
          monoType (T.function T.int32 T.int32)),
        (nm "f_id2", tylam "b" (lambda "y" (var "y")), polyType ["b"] (T.function (T.var "b") (T.var "b")))]
        (apply (var "f") (int32 42))),

    -- ============================================================
    -- Test: Nested polymorphic binding calls enclosing polymorphic binding
    -- When a polymorphic binding h at a nested level calls a polymorphic
    -- binding g from an enclosing (but not top) level, g should NOT be
    -- passed as a term argument to h. Instead, h's body should reference
    -- the hoisted g (wrapper_g) directly, since polymorphic let-bound
    -- variables are excluded from capture.
    -- ============================================================

    hoistPolyCase "nested polymorphic binding calls enclosing polymorphic binding"
      -- Input: let wrapper = \outer:Int32 ->
      --          let g : forall a. a -> a = \y -> y
      --          in \inner:Int32 ->
      --            let h : forall b. b -> b = \z -> g z
      --            in h 42
      --        in wrapper 10 20
      -- Here g is polymorphic at an outer let level, and h is polymorphic
      -- at an inner let level. h calls g. When both are hoisted:
      -- - g becomes wrapper_g (no captures needed)
      -- - h becomes wrapper_h, and should reference wrapper_g directly
      --   (NOT receive g as a term parameter)
      (mkLet [(nm "wrapper",
        lambdaTyped "outer" T.int32 (Core.termLet $ mkLet [
          -- g : forall a. a -> a (at outer let level)
          (nm "g",
            lambda "y" (var "y"),
            polyType ["a"] (T.function (T.var "a") (T.var "a")))]
          -- inner lambda
          (lambdaTyped "inner" T.int32 (Core.termLet $ mkLet [
            -- h : forall b. b -> b (at inner let level, calls g)
            (nm "h",
              lambda "z" (apply (var "g") (var "z")),
              polyType ["b"] (T.function (T.var "b") (T.var "b")))]
            (apply (var "h") (int32 42))))),
        monoType (T.function T.int32 (T.function T.int32 T.int32)))]
        (apply (apply (var "wrapper") (int32 10)) (int32 20)))
      -- Output: wrapper simplified, wrapper_g and wrapper_h hoisted.
      -- CRITICAL: wrapper_h references wrapper_g directly, NOT through a parameter.
      -- Neither g nor any enclosing lambda variables are captured by h,
      -- because g is polymorphic (excluded from capture) and h doesn't use outer/inner.
      (mkLet [
        (nm "wrapper",
          lambdaTyped "outer" T.int32
            (lambdaTyped "inner" T.int32
              (apply (var "wrapper_h") (int32 42))),
          monoType (T.function T.int32 (T.function T.int32 T.int32))),
        -- wrapper_g: hoisted, no captures
        (nm "wrapper_g",
          tylam "a" (lambda "y" (var "y")),
          polyType ["a"] (T.function (T.var "a") (T.var "a"))),
        -- wrapper_h: hoisted, references wrapper_g directly (no g parameter)
        (nm "wrapper_h",
          tylam "b" (lambda "z" (apply (var "wrapper_g") (var "z"))),
          polyType ["b"] (T.function (T.var "b") (T.var "b")))]
        (apply (apply (var "wrapper") (int32 10)) (int32 20))),

    -- ============================================================
    -- Test: Polymorphic binding captures monomorphic sibling in same let
    -- This test case demonstrates the bug in joinTypes where:
    -- - sleft/sright are monomorphic bindings (NOT hoisted)
    -- - cannotUnify is polymorphic (HOISTED) and references sleft/sright
    -- When cannotUnify is hoisted, it must capture sleft/sright as parameters
    -- because they are defined in the same let and won't be hoisted with it.
    -- ============================================================

    hoistPolyCase "polymorphic binding captures monomorphic sibling in same let"
      -- This mirrors the joinTypes structure:
      -- let wrapper = \left -> \right ->
      --   let sleft : Int32 = f left;           -- monomorphic, NOT hoisted
      --       sright : Int32 = f right;         -- monomorphic, NOT hoisted
      --       cannotUnify : forall a. a -> a = \x -> add sleft (add sright x)  -- polymorphic, refs sleft/sright
      --   in cannotUnify 42
      -- in wrapper 1 2
      --
      -- When cannotUnify is hoisted, it must capture sleft and sright since they are
      -- defined in the same let and won't be hoisted with it.
      (mkLet [(nm "wrapper",
        lambdaTyped "left" T.int32
          (lambdaTyped "right" T.int32
            (Core.termLet $ mkLet [
              (nm "sleft", apply (var "f") (var "left"), monoType T.int32),
              (nm "sright", apply (var "f") (var "right"), monoType T.int32),
              (nm "cannotUnify",
                lambda "x" (apply (apply (primitive _math_add) (var "sleft"))
                  (apply (apply (primitive _math_add) (var "sright")) (var "x"))),
                polyType ["a"] (T.function (T.var "a") (T.var "a")))]
              (apply (var "cannotUnify") (int32 42)))),
        monoType (T.function T.int32 (T.function T.int32 T.int32)))]
        (apply (apply (var "wrapper") (int32 1)) (int32 2)))
      -- Expected output:
      -- wrapper is simplified, wrapper_cannotUnify is hoisted with sleft and sright captured
      -- wrapper_cannotUnify: forall a. \sleft:Int32 -> \sright:Int32 -> \x -> add sleft (add sright x)
      -- Reference becomes: ((wrapper_cannotUnify sleft) sright) 42
      (mkLet [
        (nm "wrapper",
          lambdaTyped "left" T.int32
            (lambdaTyped "right" T.int32
              (Core.termLet $ mkLet [
                (nm "sleft", apply (var "f") (var "left"), monoType T.int32),
                (nm "sright", apply (var "f") (var "right"), monoType T.int32)]
                (apply (apply (apply (var "wrapper_cannotUnify") (var "sleft")) (var "sright")) (int32 42)))),
          monoType (T.function T.int32 (T.function T.int32 T.int32))),
        (nm "wrapper_cannotUnify",
          tylam "a"
            (lambdaTyped "sleft" T.int32
              (lambdaTyped "sright" T.int32
                (lambda "x" (apply (apply (primitive _math_add) (var "sleft"))
                  (apply (apply (primitive _math_add) (var "sright")) (var "x")))))),
          polyType ["a"] (T.function T.int32 (T.function T.int32 (T.function (T.var "a") (T.var "a")))))]
        (apply (apply (var "wrapper") (int32 1)) (int32 2))),

    -- This test uses NESTED lets (as the DSL generates with <~) rather than a single flat let.
    -- The bug: when poly bindings in different nested lets reference each other,
    -- transitive captured variables are not propagated correctly.
    --
    -- Structure (nested lets, like DSL generates):
    --   wrapper = \left ->
    --     let sleft = left                              -- mono (outermost nested let)
    --     in let cannotUnify = fail sleft              -- poly, refs sleft (middle nested let)
    --        in let joinList = cannotUnify 1           -- poly, refs cannotUnify (innermost nested let)
    --           in joinList
    --
    -- When joinList is hoisted, it captures cannotUnify (from outer let).
    -- When cannotUnify is hoisted, it captures sleft and its replacement becomes: wrapper_cannotUnify sleft
    -- The replacement for cannotUnify gets substituted into joinList's body.
    -- Bug: joinList ends up with free variable sleft because it wasn't captured transitively.
    hoistPolyCase "nested lets: poly binding references poly sibling from outer let"
      (mkLet [(nm "wrapper",
        lambdaTyped "left" T.int32
          -- Outer nested let: sleft (mono)
          (Core.termLet $ mkLet [
            (nm "sleft", var "left", monoType T.int32)]
            -- Middle nested let: cannotUnify (poly, refs sleft)
            (Core.termLet $ mkLet [
              (nm "cannotUnify",
                lambda "x" (apply (apply (primitive _math_add) (var "sleft")) (var "x")),
                polyType ["a"] (T.function (T.var "a") (T.var "a")))]
              -- Inner nested let: joinList (poly, refs cannotUnify)
              (Core.termLet $ mkLet [
                (nm "joinList",
                  lambda "y" (apply (var "cannotUnify") (var "y")),
                  polyType ["b"] (T.function (T.var "b") (T.var "b")))]
                (apply (var "joinList") (int32 42))))),
        monoType (T.function T.int32 T.int32))]
        (apply (var "wrapper") (int32 1)))
      -- Expected: both cannotUnify and joinList are hoisted.
      -- wrapper_cannotUnify captures sleft.
      -- wrapper_joinList must ALSO capture sleft (transitively through cannotUnify).
      -- The reference to cannotUnify in joinList becomes wrapper_cannotUnify sleft,
      -- so joinList needs sleft in scope.
      (mkLet [
        (nm "wrapper",
          lambdaTyped "left" T.int32
            (Core.termLet $ mkLet [
              (nm "sleft", var "left", monoType T.int32)]
              (apply (apply (var "wrapper_joinList") (var "sleft")) (int32 42))),
          monoType (T.function T.int32 T.int32)),
        (nm "wrapper_cannotUnify",
          tylam "a"
            (lambdaTyped "sleft" T.int32
              (lambda "x" (apply (apply (primitive _math_add) (var "sleft")) (var "x")))),
          polyType ["a"] (T.function T.int32 (T.function (T.var "a") (T.var "a")))),
        (nm "wrapper_joinList",
          tylam "b"
            (lambdaTyped "sleft" T.int32
              (lambda "y" (apply (apply (var "wrapper_cannotUnify") (var "sleft")) (var "y")))),
          polyType ["b"] (T.function T.int32 (T.function (T.var "b") (T.var "b"))))]
        (apply (var "wrapper") (int32 1))),

    -- ============================================================
    -- Regression test: polymorphic binding with pair term must preserve
    -- type application wrappers after hoisting. This reproduces the
    -- "pair type requires 2 type arguments, got 0" error in Java code
    -- generation. The binding `init` (like in hoistLetBindingsWithPredicate)
    -- contains a pair of empty list and a set. After inference, the pair
    -- has TypeApplication wrappers. These wrappers must survive hoisting.
    -- ============================================================

    hoistPolyCase "polymorphic binding with pair: type applications preserved"
      -- Input: let f = \b:Name ->
      --          let init : forall t0. Pair<List<t0>, Set<Name>>
      --                   = Λt0. @(Set<Name>) @(List<t0>) pair((@t0 []), singleton(b))
      --          in init
      --        in f (name "x")
      -- The init binding is polymorphic (has type var t0 from empty list).
      -- After hoisting, the pair must KEEP its TypeApplication wrappers.
      (mkLet [(nm "f",
        lambdaTyped "b" (T.wrap (Core.name (Phantoms.string "hydra.core.Name")) T.string) (Core.termLet $ mkLet [
          (nm "init",
            -- Term with type lambda and type applications (as inference would produce):
            -- Λt0. TypeApp(TypeApp(Pair(TypeApp([], t0), singleton(b)), List<t0>), Set<Name>)
            tylam "t0" (tyapp (tyapp
              (pair
                (tyapp (list ([] :: [TTerm Term])) (T.var "t0"))
                (apply (var "singleton") (var "b")))
              (T.list (T.var "t0")))
              (T.set (T.wrap (Core.name (Phantoms.string "hydra.core.Name")) T.string))),
            polyType ["t0"] (T.pair (T.list (T.var "t0")) (T.set (T.wrap (Core.name (Phantoms.string "hydra.core.Name")) T.string))))]
          (var "init")),
        monoType (T.function (T.wrap (Core.name (Phantoms.string "hydra.core.Name")) T.string)
          (T.pair (T.list (T.var "t0")) (T.set (T.wrap (Core.name (Phantoms.string "hydra.core.Name")) T.string)))))]
        (apply (var "f") (var "name_x")))
      -- Expected output: f first (original), then f_init hoisted.
      -- The hoisted binding must retain TypeApplication wrappers on the pair.
      -- Λt0 is stripped and re-added by hoisting, but inner type apps on pair are preserved.
      (mkLet [
        (nm "f",
          lambdaTyped "b" (T.wrap (Core.name (Phantoms.string "hydra.core.Name")) T.string)
            (apply (var "f_init") (var "b")),
          monoType (T.function (T.wrap (Core.name (Phantoms.string "hydra.core.Name")) T.string)
            (T.pair (T.list (T.var "t0")) (T.set (T.wrap (Core.name (Phantoms.string "hydra.core.Name")) T.string))))),
        (nm "f_init",
          -- After hoisting: captures b, re-adds type lambda for t0
          -- The inner type applications on the pair MUST be preserved
          tylam "t0" (lambdaTyped "b" (T.wrap (Core.name (Phantoms.string "hydra.core.Name")) T.string)
            (tyapp (tyapp
              (pair
                (tyapp (list ([] :: [TTerm Term])) (T.var "t0"))
                (apply (var "singleton") (var "b")))
              (T.list (T.var "t0")))
              (T.set (T.wrap (Core.name (Phantoms.string "hydra.core.Name")) T.string)))),
          polyType ["t0"] (T.function (T.wrap (Core.name (Phantoms.string "hydra.core.Name")) T.string)
            (T.pair (T.list (T.var "t0")) (T.set (T.wrap (Core.name (Phantoms.string "hydra.core.Name")) T.string)))))]
        (apply (var "f") (var "name_x"))),

    -- ============================================================
    -- Regression test: monomorphic binding referencing outer type variables
    -- must get type applications at the call site after hoisting.
    -- This reproduces the Java "T70848" bug: without type apps on the
    -- hoisted reference, the Java coder falls back to tryTypeOf which
    -- generates fresh inference variables instead of using the correct
    -- type parameters from the enclosing scope.
    -- ============================================================

    hoistPolyCase "monomorphic binding captures type vars: replacement includes type applications"
      -- Input: let f = TypeLambda a (TypeLambda b (
      --          \(x:a) -> let q : TypeScheme([], a -> b) = \(y:a) -> g y
      --                    in q x))
      --        in f
      -- Here 'q' is monomorphic but references outer type vars 'a' and 'b'.
      -- When hoisted, q becomes polymorphic in [a, b].
      -- The replacement must include TypeApp(b, TypeApp(a, Var(f_q))) to instantiate
      -- the hoisted binding with the correct type variables.
      (mkLet [(nm "f",
        tylam "a" (tylam "b" (
          lambdaTyped "x" (T.var "a") (Core.termLet $ mkLet [
            (nm "q",
              lambdaTyped "y" (T.var "a") (apply (var "g") (var "y")),
              monoType (T.function (T.var "a") (T.var "b")))]
            (apply (var "q") (var "x"))))),
        polyType ["a", "b"] (T.function (T.var "a") (T.var "b")))]
        (var "f"))
      -- Expected output: f first, then f_q hoisted with type lambdas for [a, b].
      -- q does not reference x, so no lambda capture is needed.
      -- The replacement for q SHOULD be TypeApp(b, TypeApp(a, Var(f_q)))
      -- so that downstream code knows the type variable instantiation.
      -- Using Core.termTypeApplication directly to avoid meta-encoding issues with tyapp.
      (mkLet [
        (nm "f",
          tylam "a" (tylam "b" (
            lambdaTyped "x" (T.var "a")
              (apply
                (Core.termTypeApplication $ Core.typeApplicationTerm
                  (Core.termTypeApplication $ Core.typeApplicationTerm (var "f_q") (T.var "a"))
                  (T.var "b"))
                (var "x")))),
          polyType ["a", "b"] (T.function (T.var "a") (T.var "b"))),
        (nm "f_q",
          tylam "a" (tylam "b" (
            lambdaTyped "y" (T.var "a") (apply (var "g") (var "y")))),
          polyType ["a", "b"] (T.function (T.var "a") (T.var "b")))]
        (var "f"))]

-- | Test cases for hoistLetBindings with hoistAll=True
-- This function hoists ALL let bindings (not just polymorphic ones) to the top level.
-- This is used for Java which cannot have let expressions in arbitrary positions.
-- Key behavior: type lambdas are boundaries - we don't hoist bindings OUT of type lambdas
-- because doing so could move code that references type variables outside their scope.
hoistLetBindingsGroup :: TTerm TestGroup
hoistLetBindingsGroup = subgroup "hoistLetBindings" [
    -- ============================================================
    -- Test: Basic nested let hoisting (no type lambdas)
    -- ============================================================

    hoistAllCase "nested let inside lambda: binding hoisted with lambda capture"
      -- Input: let f = \a -> (let g = a + 1 in g * 2) in f 10
      (mkLetUntyped [(nm "f",
        lambda "a" (Core.termLet $ mkLetUntyped [(nm "g", apply (apply (primitive _math_add) (var "a")) (int32 1))]
          (apply (apply (primitive _math_mul) (var "g")) (int32 2))))]
        (apply (var "f") (int32 10)))
      -- Output: f comes first (original binding), then f_g is hoisted with lambda to capture 'a', reference becomes (f_g a)
      (mkLetUntyped [
        (nm "f",
          lambda "a" (apply (apply (primitive _math_mul) (apply (var "f_g") (var "a"))) (int32 2))),
        (nm "f_g", lambda "a" (apply (apply (primitive _math_add) (var "a")) (int32 1)))]
        (apply (var "f") (int32 10))),

    -- ============================================================
    -- Test: Type applications are processed normally (they don't introduce type variables)
    -- But the inner lambda still has nested let that gets hoisted
    -- ============================================================

    hoistAllCase "type application: nested let outside lambda CAN be hoisted"
      -- Input: let f = (let y = 1 in \x -> x + y) @Int32 in f 10
      -- The let is OUTSIDE the lambda, so y can be hoisted without capture
      (mkLetUntyped [(nm "f",
        tyapp (Core.termLet $ mkLetUntyped [(nm "y", int32 1)]
          (lambda "x" (apply (apply (primitive _math_add) (var "x")) (var "y")))) T.int32)]
        (apply (var "f") (int32 10)))
      -- Output: f comes first (original binding), then f_y is hoisted
      (mkLetUntyped [
        (nm "f",
          tyapp (lambda "x" (apply (apply (primitive _math_add) (var "x")) (var "f_y"))) T.int32),
        (nm "f_y", int32 1)]
        (apply (var "f") (int32 10)))]

-- | Test cases for type parameter extraction when hoisting polymorphic let bindings
-- This group specifically tests scenarios where Java code generation fails because
-- type parameters (t0, t1, t2, etc.) are used in generated code but not declared
-- in method signatures. The root cause is that javaTypeParametersForType doesn't
-- properly collect all free type variables from nested generic types.
--
-- These tests illustrate the EXPECTED behavior after the issue is fixed.
-- Currently, the Java coder fails to generate compilable code for these cases.
hoistPolymorphicTypeParametersGroup :: TTerm TestGroup
hoistPolymorphicTypeParametersGroup = subgroup "hoistPolymorphicTypeParameters" [
    -- ============================================================
    -- Test: Nested polymorphic bindings with multiple type variables
    -- This is the core issue: when a polymorphic binding like `choose`
    -- uses type variables (t0, t1, t2) in nested function types,
    -- those variables must be declared in the hoisted method signature.
    -- ============================================================

    hoistPolyCase "nested function types: all type variables must be declared"
      -- Input: let f = (let choose : forall t0 t1 t2. (t0 -> t1) -> (t2 -> t1) -> t0 -> t1 = ... in choose) in f
      -- This simulates the `mutateTrace` scenario where `choose` is a polymorphic
      -- local binding with multiple type parameters in nested function types.
      -- (Simplified to avoid using "Either" as a type variable name)
      (mkLet [(nm "f",
        Core.termLet $ mkLet [(nm "choose",
          lambda "forLeft" (lambda "forRight" (lambda "e"
            (apply (var "forLeft") (var "e")))),
          -- Type: forall t0 t1 t2. (t0 -> t1) -> (t2 -> t1) -> t0 -> t1
          polyType ["t0", "t1", "t2"]
            (T.function
              (T.function (T.var "t0") (T.var "t1"))
              (T.function
                (T.function (T.var "t2") (T.var "t1"))
                (T.function (T.var "t0") (T.var "t1")))))]
          (var "choose"),
        monoType (T.function
          (T.function T.string T.int32)
          (T.function
            (T.function T.boolean T.int32)
            (T.function T.string T.int32))))]
        (var "f"))
      -- Output: f first (original), then f_choose hoisted with ALL type parameters preserved
      -- When generating Java, the method signature must declare t0, t1, t2
      (mkLet [
        (nm "f",
          var "f_choose",
          monoType (T.function
            (T.function T.string T.int32)
            (T.function
              (T.function T.boolean T.int32)
              (T.function T.string T.int32)))),
        (nm "f_choose",
          tylam "t0" (tylam "t1" (tylam "t2" (lambda "forLeft" (lambda "forRight" (lambda "e"
            (apply (var "forLeft") (var "e"))))))),
          polyType ["t0", "t1", "t2"]
            (T.function
              (T.function (T.var "t0") (T.var "t1"))
              (T.function
                (T.function (T.var "t2") (T.var "t1"))
                (T.function (T.var "t0") (T.var "t1")))))]
        (var "f")),

    -- ============================================================
    -- Test: Type variable in return position only
    -- Type variables appearing only in the return type of a nested
    -- function must still be declared.
    -- ============================================================

    hoistPolyCase "type variable in return position only"
      -- Input: let f = (let returnT : forall t. () -> t = ... in returnT) in f
      (mkLet [(nm "f",
        Core.termLet $ mkLet [(nm "returnT",
          lambda "unit" (var "undefined"),
          polyType ["t"] (T.function T.unit (T.var "t")))]
          (var "returnT"),
        monoType (T.function T.unit T.int32))]
        (var "f"))
      -- Output: f first (original), then f_returnT hoisted with t declared
      (mkLet [
        (nm "f",
          var "f_returnT",
          monoType (T.function T.unit T.int32)),
        (nm "f_returnT",
          tylam "t" (lambda "unit" (var "undefined")),
          polyType ["t"] (T.function T.unit (T.var "t")))]
        (var "f")),

    -- ============================================================
    -- Test: Type variables in deeply nested generic types
    -- The Java coder uses freeVariablesInType but may not recurse
    -- into all nested type structures.
    -- ============================================================

    hoistPolyCase "type variables in deeply nested generics"
      -- Input: let f = (let nested : forall t0 t1 t2. ((t0, t1), t2) -> t0 = ... in nested) in f
      -- Type variables t0, t1, t2 are in nested pair types
      -- (Simplified from original which used List<Map<...>> - those are treated as type vars in tests)
      (mkLet [(nm "f",
        Core.termLet $ mkLet [(nm "nested",
          lambda "x" (var "undefined"),
          -- Type: forall t0 t1 t2. ((t0, t1), t2) -> t0
          polyType ["t0", "t1", "t2"]
            (T.function
              (T.pair (T.pair (T.var "t0") (T.var "t1")) (T.var "t2"))
              (T.var "t0")))]
          (var "nested"),
        monoType (T.function
          (T.pair (T.pair T.string T.int32) T.boolean)
          T.string))]
        (var "f"))
      -- Output: f first (original), then f_nested hoisted with all type parameters t0, t1, t2 declared
      (mkLet [
        (nm "f",
          var "f_nested",
          monoType (T.function
            (T.pair (T.pair T.string T.int32) T.boolean)
            T.string)),
        (nm "f_nested",
          tylam "t0" (tylam "t1" (tylam "t2" (lambda "x" (var "undefined")))),
          polyType ["t0", "t1", "t2"]
            (T.function
              (T.pair (T.pair (T.var "t0") (T.var "t1")) (T.var "t2"))
              (T.var "t0")))]
        (var "f")),

    -- ============================================================
    -- Test: Multiple polymorphic bindings that share type variable names
    -- Each binding should independently track its own type variables.
    -- ============================================================

    hoistPolyCase "multiple bindings with overlapping type variable names"
      -- Input: let outer = (let id1 : forall t. t -> t = \x -> x; id2 : forall t. t -> t = \y -> y in pair id1 id2) in outer
      -- Both id1 and id2 use "t" but they are independent
      (mkLet [(nm "outer",
        Core.termLet $ mkLet [
          (nm "id1", lambda "x" (var "x"), polyType ["t"] (T.function (T.var "t") (T.var "t"))),
          (nm "id2", lambda "y" (var "y"), polyType ["t"] (T.function (T.var "t") (T.var "t")))]
          (apply (apply (var "pair") (var "id1")) (var "id2")),
        monoType (T.pair
          (T.function T.int32 T.int32)
          (T.function T.string T.string)))]
        (var "outer"))
      -- Output: outer first (original), then outer_id1 and outer_id2 hoisted (each with their own t parameter)
      (mkLet [
        (nm "outer",
          apply (apply (var "pair") (var "outer_id1")) (var "outer_id2"),
          monoType (T.pair
            (T.function T.int32 T.int32)
            (T.function T.string T.string))),
        (nm "outer_id1", tylam "t" (lambda "x" (var "x")), polyType ["t"] (T.function (T.var "t") (T.var "t"))),
        (nm "outer_id2", tylam "t" (lambda "y" (var "y")), polyType ["t"] (T.function (T.var "t") (T.var "t")))]
        (var "outer")),

    -- ============================================================
    -- Test: Polymorphic binding with captured term variable AND type parameters
    -- This is the combination case: the binding both captures a lambda-bound
    -- term variable AND has type parameters that need to be declared.
    -- ============================================================

    hoistPolyCase "captured variable with type parameters"
      -- Input: let f = \(a:String) -> (let g : forall t. t -> Pair<String, t> = \x -> pair a x in g 42) in f "hello"
      -- Here 'g' captures 'a' AND has type parameter t
      -- When hoisted: g must be wrapped in lambda for 'a', AND method must declare t
      (mkLet [(nm "f",
        lambdaTyped "a" T.string (Core.termLet $ mkLet [
          (nm "g",
            lambda "x" (apply (apply (var "pair") (var "a")) (var "x")),
            polyType ["t"] (T.function (T.var "t") (T.pair T.string (T.var "t"))))]
          (apply (var "g") (int32 42))),
        monoType (T.function T.string (T.pair T.string T.int32)))]
        (apply (var "f") (string "hello")))
      -- Output: f first (original), then f_g hoisted with lambda wrapper for 'a', and t must be declared
      -- Java signature: <t> Function<String, Function<t, Pair<String, t>>> f_g = a -> x -> pair(a, x);
      (mkLet [
        (nm "f",
          lambdaTyped "a" T.string (apply (apply (var "f_g") (var "a")) (int32 42)),
          monoType (T.function T.string (T.pair T.string T.int32))),
        (nm "f_g",
          tylam "t" (lambdaTyped "a" T.string (lambda "x" (apply (apply (var "pair") (var "a")) (var "x")))),
          polyType ["t"] (T.function T.string (T.function (T.var "t") (T.pair T.string (T.var "t")))))]
        (apply (var "f") (string "hello"))),

    -- ============================================================
    -- Test: Short type variable names (the isLambdaBoundVariable heuristic)
    -- The Java coder's isLambdaBoundVariable uses name length <= 4 to identify
    -- type variables. These tests ensure that this heuristic correctly
    -- identifies variables that should become Java type parameters.
    -- ============================================================

    hoistPolyCase "short type variable names are treated as type parameters"
      -- Input: let f = (let g : forall s t v. s -> t -> v = ... in g) in f
      -- s, t, v have length <= 4, so they should be recognized as type parameters
      (mkLet [(nm "f",
        Core.termLet $ mkLet [(nm "g",
          lambda "s" (lambda "t" (var "undefined")),
          polyType ["s", "t", "v"]
            (T.function (T.var "s")
              (T.function (T.var "t") (T.var "v"))))]
          (var "g"),
        monoType (T.function T.int32 (T.function T.string T.boolean)))]
        (var "f"))
      -- Output: f first (original), then f_g hoisted with s, t, v declared as type parameters
      (mkLet [
        (nm "f",
          var "f_g",
          monoType (T.function T.int32 (T.function T.string T.boolean))),
        (nm "f_g",
          tylam "s" (tylam "t" (tylam "v" (lambda "s" (lambda "t" (var "undefined"))))),
          polyType ["s", "t", "v"]
            (T.function (T.var "s")
              (T.function (T.var "t") (T.var "v"))))]
        (var "f")),

    hoistPolyCase "numbered type variables like t0 t1 t2"
      -- Input: let f = (let g : forall t0 t1 t2. t0 -> t1 -> t2 = ... in g) in f
      -- t0, t1, t2 have length <= 4, common pattern in generated code
      (mkLet [(nm "f",
        Core.termLet $ mkLet [(nm "g",
          lambda "x" (lambda "y" (var "undefined")),
          polyType ["t0", "t1", "t2"]
            (T.function (T.var "t0")
              (T.function (T.var "t1") (T.var "t2"))))]
          (var "g"),
        monoType (T.function T.int32 (T.function T.string T.boolean)))]
        (var "f"))
      -- Output: f first (original), then f_g hoisted with t0, t1, t2 declared
      (mkLet [
        (nm "f",
          var "f_g",
          monoType (T.function T.int32 (T.function T.string T.boolean))),
        (nm "f_g",
          tylam "t0" (tylam "t1" (tylam "t2" (lambda "x" (lambda "y" (var "undefined"))))),
          polyType ["t0", "t1", "t2"]
            (T.function (T.var "t0")
              (T.function (T.var "t1") (T.var "t2"))))]
        (var "f")),

    -- ============================================================
    -- Test: Complex "choose" pattern from mutateTrace
    -- This directly models the failing code in Monads.java
    -- NOTE: This test uses T.var "Either" which causes "Either" to be
    -- treated as a free type variable. In real code, Either would be a defined
    -- type constructor and wouldn't be captured as a free variable. This test
    -- has been simplified to avoid this issue by using only type variables.
    -- ============================================================

    hoistPolyCase "choose pattern from mutateTrace"
      -- Input simulates: let mutateTrace = ... in
      --   choose <~ (forLeft ~> forRight ~> e ~> either forLeft forRight e) $
      --   ... rest of mutateTrace ...
      -- The `choose` binding has type:
      --   forall t0 t1 t2. (t0 -> t1) -> (t2 -> t1) -> t0 -> t1
      -- (Simplified to avoid using "Either" as a type variable name)
      (mkLet [(nm "mutateTrace",
        lambda "mutate" (lambda "restore" (lambda "f"
          (Core.termLet $ mkLet [(nm "choose",
            -- choose = \forLeft -> \forRight -> \e -> forLeft e
            lambda "forLeft" (lambda "forRight" (lambda "e"
              (apply (var "forLeft") (var "e")))),
            -- Type: forall t0 t1 t2. (t0 -> t1) -> (t2 -> t1) -> t0 -> t1
            polyType ["t0", "t1", "t2"]
              (T.function
                (T.function (T.var "t0") (T.var "t1"))
                (T.function
                  (T.function (T.var "t2") (T.var "t1"))
                  (T.function (T.var "t0") (T.var "t1")))))]
            -- Body uses choose
            (apply (apply (apply (var "choose") (var "forLeft")) (var "forRight")) (var "e"))))),
        -- Full type of mutateTrace (using concrete types)
        monoType (T.function T.int32
          (T.function T.int32
            (T.function T.int32 T.int32))))]
        (var "mutateTrace"))
      -- Output: mutateTrace first (original), then mutateTrace_choose hoisted, MUST have t0, t1, t2 declared
      (mkLet [
        (nm "mutateTrace",
          lambda "mutate" (lambda "restore" (lambda "f"
            (apply (apply (apply (var "mutateTrace_choose") (var "forLeft")) (var "forRight")) (var "e")))),
          monoType (T.function T.int32
            (T.function T.int32
              (T.function T.int32 T.int32)))),
        (nm "mutateTrace_choose",
          tylam "t0" (tylam "t1" (tylam "t2" (lambda "forLeft" (lambda "forRight" (lambda "e"
            (apply (var "forLeft") (var "e"))))))),
          polyType ["t0", "t1", "t2"]
            (T.function
              (T.function (T.var "t0") (T.var "t1"))
              (T.function
                (T.function (T.var "t2") (T.var "t1"))
                (T.function (T.var "t0") (T.var "t1")))))]
        (var "mutateTrace"))]
