{-# LANGUAGE FlexibleInstances #-} -- for IsString with nontrivial parameters

-- | A DSL for constructing primitive function definitions
module Hydra.Dsl.Prims where

import Hydra.Kernel
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import Hydra.Rewriting (removeTermAnnotations)
import Data.String(IsString(..))

instance IsString (TermCoder a (Term a)) where fromString = variable

boolean :: Show a => TermCoder a Bool
boolean = TermCoder Types.boolean $ Coder encode decode
  where
    encode = Terms.expectBoolean
    decode = pure . Terms.boolean

flow :: TermCoder a s -> TermCoder a x -> TermCoder a (Flow s x)
flow states values = TermCoder (Types.wrap _Flow Types.@@ (termCoderType states) Types.@@ (termCoderType values)) $
    Coder encode decode
  where
    encode _ = fail $ "cannot currently encode flows from terms"
    decode _ = fail $ "cannot decode flows to terms"

function :: (Ord a, Show a) => TermCoder a x -> TermCoder a y -> TermCoder a (x -> y)
function dom cod = TermCoder (Types.function (termCoderType dom) (termCoderType cod)) $ Coder encode decode
  where
    encode term = fail $ "cannot encode terms to functions"
    decode _ = fail $ "cannot decode functions to terms"

int32 :: Show a => TermCoder a Int
int32 = TermCoder Types.int32 $ Coder encode decode
  where
    encode = Terms.expectInt32
    decode = pure . Terms.int32

list :: Show a => TermCoder a x -> TermCoder a [x]
list els = TermCoder (Types.list $ termCoderType els) $ Coder encode decode
  where
    encode = Terms.expectList (coderEncode $ termCoderCoder els)
    decode l = Terms.list <$> mapM (coderDecode $ termCoderCoder els) l

map :: (Ord k, Ord a, Show a) => TermCoder a k -> TermCoder a v -> TermCoder a (M.Map k v)
map keys values = TermCoder (Types.map (termCoderType keys) (termCoderType values)) $ Coder encode decode
  where
    encode = Terms.expectMap (coderEncode $ termCoderCoder keys) (coderEncode $ termCoderCoder values)
    decode m = Terms.map . M.fromList <$> mapM decodePair (M.toList m)
      where
        decodePair (k, v) = do
          ke <- (coderDecode $ termCoderCoder keys) k
          ve <- (coderDecode $ termCoderCoder values) v
          return (ke, ve)

optional :: Show a => TermCoder a x -> TermCoder a (Y.Maybe x)
optional mel = TermCoder (Types.optional $ termCoderType mel) $ Coder encode decode
  where
    encode = Terms.expectOptional (coderEncode $ termCoderCoder mel)
    decode mv = Terms.optional <$> case mv of
      Nothing -> pure Nothing
      Just v -> Just <$> (coderDecode $ termCoderCoder mel) v

prim0 :: Name -> TermCoder a x -> x -> Primitive a
prim0 name output value = Primitive name (termCoderType output) impl
  where
    impl _ = coderDecode (termCoderCoder output) value

prim1 :: Name -> TermCoder a x -> TermCoder a y -> (x -> y) -> Primitive a
prim1 name input1 output compute = Primitive name ft impl
  where
    ft = TypeFunction $ FunctionType (termCoderType input1) $ termCoderType output
    impl args = do
      Terms.expectNArgs 1 args
      arg1 <- coderEncode (termCoderCoder input1) (args !! 0)
      coderDecode (termCoderCoder output) $ compute arg1

prim2 :: Name -> TermCoder a x -> TermCoder a y -> TermCoder a z -> (x -> y -> z) -> Primitive a
prim2 name input1 input2 output compute = Primitive name ft impl
  where
    ft = TypeFunction $ FunctionType (termCoderType input1) (Types.function (termCoderType input2) (termCoderType output))
    impl args = do
      Terms.expectNArgs 2 args
      arg1 <- coderEncode (termCoderCoder input1) (args !! 0)
      arg2 <- coderEncode (termCoderCoder input2) (args !! 1)
      coderDecode (termCoderCoder output) $ compute arg1 arg2

prim2Raw :: Name -> TermCoder a x -> TermCoder a y -> TermCoder a z -> (Term a -> Term a -> Flow (Graph a) (Term a)) -> Primitive a
prim2Raw name input1 input2 output compute = Primitive name ft impl
  where
    ft = TypeFunction $ FunctionType (termCoderType input1) (Types.function (termCoderType input2) (termCoderType output))
    impl args = do
      Terms.expectNArgs 2 args
      compute (args !! 0) (args !! 1)

set :: (Ord x, Ord a, Show a) => TermCoder a x -> TermCoder a (S.Set x)
set els = TermCoder (Types.set $ termCoderType els) $ Coder encode decode
  where
    encode = Terms.expectSet (coderEncode $ termCoderCoder els)
    decode s = Terms.set . S.fromList <$> mapM (coderDecode $ termCoderCoder els) (S.toList s)

string :: Show a => TermCoder a String
string = TermCoder Types.string $ Coder encode decode
  where
    encode = Terms.expectString
    decode = pure . Terms.string

variable :: String -> TermCoder a (Term a)
variable v = TermCoder (Types.variable v) $ Coder encode decode
  where
    encode = pure
    decode = pure
