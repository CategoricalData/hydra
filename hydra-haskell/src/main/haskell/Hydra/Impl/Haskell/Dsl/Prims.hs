module Hydra.Impl.Haskell.Dsl.Prims where

import Hydra.Core
import Hydra.Evaluation
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Monads

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
--import Data.String(IsString(..))


--instance IsString (TermCoder m (Term m)) where fromString = variable

binaryPrimitive :: Name -> TermCoder m a -> TermCoder m b -> TermCoder m c -> (a -> b -> c) -> PrimitiveFunction m
binaryPrimitive name input1 input2 output compute = PrimitiveFunction name ft impl
  where
    ft = FunctionType (termCoderType input1) (Types.function (termCoderType input2) (termCoderType output))
    impl args = do
      Terms.expectNArgs 2 args
      arg1 <- coderEncode (termCoderCoder input1) (args !! 0)
      arg2 <- coderEncode (termCoderCoder input2) (args !! 1)
      coderDecode (termCoderCoder output) $ compute arg1 arg2

boolean :: Show m => TermCoder m Bool
boolean = TermCoder Types.boolean $ Coder encode decode
  where
    encode = Terms.expectBoolean
    decode = pure . Terms.boolean

function :: TermCoder m a -> TermCoder m b -> TermCoder m (a -> b)
function dom cod = TermCoder (Types.function (termCoderType dom) (termCoderType cod)) $ Coder encode decode
  where
    encode _ = fail $ "cannot currently encode functions from terms" 
    decode _ = fail $ "cannot decode functions to terms"
      
int32 :: Show m => TermCoder m Int
int32 = TermCoder Types.int32 $ Coder encode decode
  where
    encode = Terms.expectInt32
    decode = pure . Terms.int32

list :: Show m => TermCoder m a -> TermCoder m [a]
list els = TermCoder (Types.list $ termCoderType els) $ Coder encode decode
  where
    encode = Terms.expectList (coderEncode $ termCoderCoder els)
    decode l = Terms.list <$> mapM (coderDecode $ termCoderCoder els) l

map :: (Ord k, Ord m, Show m) => TermCoder m k -> TermCoder m v -> TermCoder m (M.Map k v)
map keys values = TermCoder (Types.map (termCoderType keys) (termCoderType values)) $ Coder encode decode
  where
    encode = Terms.expectMap (coderEncode $ termCoderCoder keys) (coderEncode $ termCoderCoder values)
    decode m = Terms.map . M.fromList <$> mapM decodePair (M.toList m)
      where
        decodePair (k, v) = do
          ke <- (coderDecode $ termCoderCoder keys) k
          ve <- (coderDecode $ termCoderCoder values) v
          return (ke, ve)

optional :: Show m => TermCoder m a -> TermCoder m (Y.Maybe a)
optional mel = TermCoder (Types.optional $ termCoderType mel) $ Coder encode decode
  where
    encode = Terms.expectOptional (coderEncode $ termCoderCoder mel)
    decode mv = Terms.optional <$> case mv of
      Nothing -> pure Nothing
      Just v -> Just <$> (coderDecode $ termCoderCoder mel) v

set :: (Ord a, Ord m, Show m) => TermCoder m a -> TermCoder m (S.Set a)
set els = TermCoder (Types.set $ termCoderType els) $ Coder encode decode
  where
    encode = Terms.expectSet (coderEncode $ termCoderCoder els)
    decode s = Terms.set . S.fromList <$> mapM (coderDecode $ termCoderCoder els) (S.toList s)

string :: Show m => TermCoder m String
string = TermCoder Types.string $ Coder encode decode
  where
    encode = Terms.expectString
    decode = pure . Terms.string

unaryPrimitive :: Name -> TermCoder m a -> TermCoder m b -> (a -> b) -> PrimitiveFunction m
unaryPrimitive name input1 output compute = PrimitiveFunction name ft impl
  where
    ft = FunctionType (termCoderType input1) $ termCoderType output
    impl args = do
      Terms.expectNArgs 1 args
      arg1 <- coderEncode (termCoderCoder input1) (args !! 0)
      coderDecode (termCoderCoder output) $ compute arg1

variable :: String -> TermCoder m (Term m)
variable v = TermCoder (Types.variable v) $ Coder encode decode
  where
    encode = pure
    decode = pure
