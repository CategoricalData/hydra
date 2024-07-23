{-# LANGUAGE FlexibleInstances #-} -- for IsString with nontrivial parameters

-- | A DSL for constructing primitive function definitions
module Hydra.Dsl.Prims where

import Hydra.Compute
import Hydra.Core
import Hydra.Graph
import Hydra.CoreDecoding
import Hydra.CoreEncoding
import qualified Hydra.Dsl.Expect as Expect
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import Data.Int
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import Hydra.Rewriting (removeTermAnnotations)
import Data.String(IsString(..))

instance IsString (TermCoder (Term)) where fromString = variable

bigfloat :: TermCoder Double
bigfloat = TermCoder Types.bigfloat $ Coder encode decode
  where
    encode = Expect.bigfloat
    decode = pure . Terms.bigfloat

bigint :: TermCoder Integer
bigint = TermCoder Types.bigint $ Coder encode decode
  where
    encode = Expect.bigint
    decode = pure . Terms.bigint

binary :: TermCoder String
binary = TermCoder Types.binary $ Coder encode decode
  where
    encode = Expect.binary
    decode = pure . Terms.binary

boolean :: TermCoder Bool
boolean = TermCoder Types.boolean $ Coder encode decode
  where
    encode = Expect.boolean
    decode = pure . Terms.boolean

float32 :: TermCoder Float
float32 = TermCoder Types.float32 $ Coder encode decode
  where
    encode = Expect.float32
    decode = pure . Terms.float32

float64 :: TermCoder Double
float64 = TermCoder Types.float64 $ Coder encode decode
  where
    encode = Expect.float64
    decode = pure . Terms.float64

flow :: TermCoder s -> TermCoder x -> TermCoder (Flow s x)
flow states values = TermCoder (TypeVariable _Flow Types.@@ (termCoderType states) Types.@@ (termCoderType values)) $
    Coder encode decode
  where
    encode _ = fail $ "cannot currently encode flows from terms"
    decode _ = fail $ "cannot decode flows to terms"

function :: TermCoder x -> TermCoder y -> TermCoder (x -> y)
function dom cod = TermCoder (Types.function (termCoderType dom) (termCoderType cod)) $ Coder encode decode
  where
    encode term = fail $ "cannot encode terms to functions"
    decode _ = fail $ "cannot decode functions to terms"

int8 :: TermCoder Int8
int8 = TermCoder Types.int8 $ Coder encode decode
  where
    encode = Expect.int8
    decode = pure . Terms.int8

int16 :: TermCoder Int16
int16 = TermCoder Types.int16 $ Coder encode decode
  where
    encode = Expect.int16
    decode = pure . Terms.int16

int32 :: TermCoder Int
int32 = TermCoder Types.int32 $ Coder encode decode
  where
    encode = Expect.int32
    decode = pure . Terms.int32

int64 :: TermCoder Int64
int64 = TermCoder Types.int64 $ Coder encode decode
  where
    encode = Expect.int64
    decode = pure . Terms.int64

list :: TermCoder x -> TermCoder [x]
list els = TermCoder (Types.list $ termCoderType els) $ Coder encode decode
  where
    encode = Expect.list (coderEncode $ termCoderCoder els)
    decode l = Terms.list <$> mapM (coderDecode $ termCoderCoder els) l

map :: Ord k => TermCoder k -> TermCoder v -> TermCoder (M.Map k v)
map keys values = TermCoder (Types.map (termCoderType keys) (termCoderType values)) $ Coder encode decode
  where
    encode = Expect.map (coderEncode $ termCoderCoder keys) (coderEncode $ termCoderCoder values)
    decode m = Terms.map . M.fromList <$> mapM decodePair (M.toList m)
      where
        decodePair (k, v) = do
          ke <- (coderDecode $ termCoderCoder keys) k
          ve <- (coderDecode $ termCoderCoder values) v
          return (ke, ve)

optional :: TermCoder x -> TermCoder (Y.Maybe x)
optional mel = TermCoder (Types.optional $ termCoderType mel) $ Coder encode decode
  where
    encode = Expect.optional (coderEncode $ termCoderCoder mel)
    decode mv = Terms.optional <$> case mv of
      Nothing -> pure Nothing
      Just v -> Just <$> (coderDecode $ termCoderCoder mel) v

pair :: TermCoder k -> TermCoder v -> TermCoder (k, v)
pair kCoder vCoder = TermCoder (Types.product [termCoderType kCoder, termCoderType vCoder]) $ Coder encode decode
  where
    encode = Expect.pair (coderEncode $ termCoderCoder kCoder) (coderEncode $ termCoderCoder vCoder)
    decode (k, v) = do
      kTerm <- coderDecode (termCoderCoder kCoder) k
      vTerm <- coderDecode (termCoderCoder vCoder) v
      return $ Terms.product [kTerm, vTerm]

prim0 :: Name -> TermCoder x -> x -> Primitive
prim0 name output value = Primitive name (termCoderType output) impl
  where
    impl _ = coderDecode (termCoderCoder output) value

prim1 :: Name -> TermCoder x -> TermCoder y -> (x -> y) -> Primitive
prim1 name input1 output compute = Primitive name ft impl
  where
    ft = TypeFunction $ FunctionType (termCoderType input1) $ termCoderType output
    impl args = do
      Expect.nArgs 1 args
      arg1 <- coderEncode (termCoderCoder input1) (args !! 0)
      coderDecode (termCoderCoder output) $ compute arg1

prim2 :: Name -> TermCoder x -> TermCoder y -> TermCoder z -> (x -> y -> z) -> Primitive
prim2 name input1 input2 output compute = Primitive name ft impl
  where
    ft = TypeFunction $ FunctionType (termCoderType input1) (Types.function (termCoderType input2) (termCoderType output))
    impl args = do
      Expect.nArgs 2 args
      arg1 <- coderEncode (termCoderCoder input1) (args !! 0)
      arg2 <- coderEncode (termCoderCoder input2) (args !! 1)
      coderDecode (termCoderCoder output) $ compute arg1 arg2

prim2Interp :: Name -> TermCoder x -> TermCoder y -> TermCoder z -> (Term -> Term -> Flow (Graph) (Term)) -> Primitive
prim2Interp name input1 input2 output compute = Primitive name ft impl
  where
    ft = TypeFunction $ FunctionType (termCoderType input1) (Types.function (termCoderType input2) (termCoderType output))
    impl args = do
      Expect.nArgs 2 args
      compute (args !! 0) (args !! 1)

prim3 :: Name -> TermCoder w -> TermCoder x -> TermCoder y -> TermCoder z -> (w -> x -> y -> z) -> Primitive
prim3 name input1 input2 input3 output compute = Primitive name ft impl
  where
    ft = TypeFunction $ FunctionType
      (termCoderType input1)
      (Types.function (termCoderType input2)
      (Types.function (termCoderType input3) (termCoderType output)))
    impl args = do
      Expect.nArgs 2 args
      arg1 <- coderEncode (termCoderCoder input1) (args !! 0)
      arg2 <- coderEncode (termCoderCoder input2) (args !! 1)
      arg3 <- coderEncode (termCoderCoder input3) (args !! 2)
      coderDecode (termCoderCoder output) $ compute arg1 arg2 arg3

set :: Ord x => TermCoder x -> TermCoder (S.Set x)
set els = TermCoder (Types.set $ termCoderType els) $ Coder encode decode
  where
    encode = Expect.set (coderEncode $ termCoderCoder els)
    decode s = Terms.set . S.fromList <$> mapM (coderDecode $ termCoderCoder els) (S.toList s)

string :: TermCoder String
string = TermCoder Types.string $ Coder encode decode
  where
    encode = Expect.string
    decode = pure . Terms.string

term :: TermCoder (Term)
term = TermCoder (Types.apply (TypeVariable _Term) (Types.var "a")) $ Coder encode decode
  where
    encode = pure
    decode = pure

type_ :: TermCoder (Type)
type_ = TermCoder (Types.apply (TypeVariable _Type) (Types.var "a")) $ Coder encode decode
  where
    encode = coreDecodeType
    decode = pure . coreEncodeType

uint8 :: TermCoder Int16
uint8 = TermCoder Types.uint8 $ Coder encode decode
  where
    encode = Expect.uint8
    decode = pure . Terms.uint8

uint16 :: TermCoder Int
uint16 = TermCoder Types.uint16 $ Coder encode decode
  where
    encode = Expect.uint16
    decode = pure . Terms.uint16

uint32 :: TermCoder Int64
uint32 = TermCoder Types.uint32 $ Coder encode decode
  where
    encode = Expect.uint32
    decode = pure . Terms.uint32

uint64 :: TermCoder Integer
uint64 = TermCoder Types.uint64 $ Coder encode decode
  where
    encode = Expect.uint64
    decode = pure . Terms.uint64

variable :: String -> TermCoder (Term)
variable v = TermCoder (Types.var v) $ Coder encode decode
  where
    encode = pure
    decode = pure
