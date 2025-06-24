{-# LANGUAGE FlexibleInstances #-} -- for IsString with nontrivial parameters

-- | A DSL for constructing primitive function definitions
module Hydra.Dsl.Prims where

import Hydra.Compute
import Hydra.Core
import Hydra.Graph
import Hydra.Decode.Core
import Hydra.CoreEncoding
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Lib.Io

import Data.Int
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import Hydra.Rewriting (removeTermAnnotations)
import Data.String(IsString(..))

instance IsString (TermCoder Term) where fromString = variable

bigfloat :: TermCoder Double
bigfloat = TermCoder Types.bigfloat $ Coder encode decode
  where
    encode = ExtractCore.bigfloat
    decode = pure . Terms.bigfloat

bigint :: TermCoder Integer
bigint = TermCoder Types.bigint $ Coder encode decode
  where
    encode = ExtractCore.bigint
    decode = pure . Terms.bigint

binary :: TermCoder String
binary = TermCoder Types.binary $ Coder encode decode
  where
    encode = ExtractCore.binary
    decode = pure . Terms.binary

boolean :: TermCoder Bool
boolean = TermCoder Types.boolean $ Coder encode decode
  where
    encode = ExtractCore.boolean
    decode = pure . Terms.boolean

comparison :: TermCoder Comparison
comparison = TermCoder (TypeVariable _Comparison) $ Coder encode decode
  where
    encode = ExtractCore.comparison
    decode = pure . Terms.comparison

floatType :: TermCoder FloatType
floatType = TermCoder (TypeVariable _FloatType) $ Coder encode decode
  where
    encode = coreDecodeFloatType
    decode = pure . coreEncodeFloatType

floatValue :: TermCoder FloatValue
floatValue = TermCoder (TypeVariable _FloatValue) $ Coder encode decode
  where
    encode = ExtractCore.floatValue
    decode = pure . Terms.float

float32 :: TermCoder Float
float32 = TermCoder Types.float32 $ Coder encode decode
  where
    encode = ExtractCore.float32
    decode = pure . Terms.float32

float64 :: TermCoder Double
float64 = TermCoder Types.float64 $ Coder encode decode
  where
    encode = ExtractCore.float64
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
    encode term = fail $ "cannot encode term to a function: " ++ showTerm term
    decode _ = fail $ "cannot decode functions to terms"

integerType :: TermCoder IntegerType
integerType = TermCoder (TypeVariable _IntegerType) $ Coder encode decode
  where
    encode = coreDecodeIntegerType
    decode = pure . coreEncodeIntegerType

integerValue :: TermCoder IntegerValue
integerValue = TermCoder (TypeVariable _IntegerValue) $ Coder encode decode
  where
    encode = ExtractCore.integerValue
    decode = pure . Terms.integer

int8 :: TermCoder Int8
int8 = TermCoder Types.int8 $ Coder encode decode
  where
    encode = ExtractCore.int8
    decode = pure . Terms.int8

int16 :: TermCoder Int16
int16 = TermCoder Types.int16 $ Coder encode decode
  where
    encode = ExtractCore.int16
    decode = pure . Terms.int16

int32 :: TermCoder Int
int32 = TermCoder Types.int32 $ Coder encode decode
  where
    encode = ExtractCore.int32
    decode = pure . Terms.int32

int64 :: TermCoder Int64
int64 = TermCoder Types.int64 $ Coder encode decode
  where
    encode = ExtractCore.int64
    decode = pure . Terms.int64

list :: TermCoder x -> TermCoder [x]
list els = TermCoder (Types.list $ termCoderType els) $ Coder encode decode
  where
    encode = ExtractCore.list (coderEncode $ termCoderCoder els)
    decode l = Terms.list <$> mapM (coderDecode $ termCoderCoder els) l

literal :: TermCoder Literal
literal = TermCoder (TypeVariable _Literal) $ Coder encode decode
  where
    encode = ExtractCore.literal
    decode = pure . Terms.literal

literalType :: TermCoder LiteralType
literalType = TermCoder (TypeVariable _LiteralType) $ Coder encode decode
  where
    encode = coreDecodeLiteralType
    decode = pure . coreEncodeLiteralType

map :: Ord k => TermCoder k -> TermCoder v -> TermCoder (M.Map k v)
map keys values = TermCoder (Types.map (termCoderType keys) (termCoderType values)) $ Coder encode decode
  where
    encode = ExtractCore.map_ (coderEncode $ termCoderCoder keys) (coderEncode $ termCoderCoder values)
    decode m = Terms.map . M.fromList <$> mapM decodePair (M.toList m)
      where
        decodePair (k, v) = do
          ke <- (coderDecode $ termCoderCoder keys) k
          ve <- (coderDecode $ termCoderCoder values) v
          return (ke, ve)

noInterpretedForm :: Name -> Flow Graph Term
noInterpretedForm name = fail $ "primitive " ++ unName name ++ " does not have an interpreted form; it can only be used in compiled code"

optional :: TermCoder x -> TermCoder (Y.Maybe x)
optional mel = TermCoder (Types.optional $ termCoderType mel) $ Coder encode decode
  where
    encode = ExtractCore.optional (coderEncode $ termCoderCoder mel)
    decode mv = Terms.optional <$> case mv of
      Nothing -> pure Nothing
      Just v -> Just <$> (coderDecode $ termCoderCoder mel) v

pair :: TermCoder k -> TermCoder v -> TermCoder (k, v)
pair kCoder vCoder = TermCoder (Types.product [termCoderType kCoder, termCoderType vCoder]) $ Coder encode decode
  where
    encode = ExtractCore.pair (coderEncode $ termCoderCoder kCoder) (coderEncode $ termCoderCoder vCoder)
    decode (k, v) = do
      kTerm <- coderDecode (termCoderCoder kCoder) k
      vTerm <- coderDecode (termCoderCoder vCoder) v
      return $ Terms.tuple [kTerm, vTerm]

prim0 :: Name -> x -> [String]  -> TermCoder x -> Primitive
prim0 name value vars output = Primitive name typ impl
  where
    typ = Types.poly vars $ termCoderType output
    impl _ = coderDecode (termCoderCoder output) value

prim1 :: Name -> (x -> y) -> [String] -> TermCoder x -> TermCoder y -> Primitive
prim1 name compute vars input1 output = Primitive name typ impl
  where
    typ = Types.poly vars $ Types.functionMany [
      termCoderType input1,
      termCoderType output]
    impl args = do
      ExtractCore.nArgs name 1 args
      arg1 <- coderEncode (termCoderCoder input1) (args !! 0)
      coderDecode (termCoderCoder output) $ compute arg1

prim2 :: Name -> (x -> y -> z) -> [String] -> TermCoder x -> TermCoder y -> TermCoder z -> Primitive
prim2 name compute vars input1 input2 output = Primitive name typ impl
  where
    typ = Types.poly vars $ Types.functionMany [
      termCoderType input1,
      termCoderType input2,
      termCoderType output]
    impl args = do
      ExtractCore.nArgs name 2 args
      arg1 <- coderEncode (termCoderCoder input1) (args !! 0)
      arg2 <- coderEncode (termCoderCoder input2) (args !! 1)
      coderDecode (termCoderCoder output) $ compute arg1 arg2

prim2Interp :: Name -> Maybe (Term -> Term -> Flow Graph Term) -> [String] -> TermCoder x -> TermCoder y -> TermCoder z -> Primitive
prim2Interp name mcompute vars input1 input2 output = Primitive name typ impl
  where
    compute = Y.fromMaybe (\a b -> noInterpretedForm name) mcompute
    typ = Types.poly vars $ Types.functionMany [
      termCoderType input1,
      termCoderType input2,
      termCoderType output]
    impl args = do
      ExtractCore.nArgs name 2 args
      compute (args !! 0) (args !! 1)

prim3 :: Name -> (w -> x -> y -> z) -> [String] -> TermCoder w -> TermCoder x -> TermCoder y -> TermCoder z -> Primitive
prim3 name compute vars input1 input2 input3 output = Primitive name typ impl
  where
    typ = Types.poly vars $ Types.functionMany [
      termCoderType input1,
      termCoderType input2,
      termCoderType input3,
      termCoderType output]
    impl args = do
      ExtractCore.nArgs name 3 args
      arg1 <- coderEncode (termCoderCoder input1) (args !! 0)
      arg2 <- coderEncode (termCoderCoder input2) (args !! 1)
      arg3 <- coderEncode (termCoderCoder input3) (args !! 2)
      coderDecode (termCoderCoder output) $ compute arg1 arg2 arg3

prim3Interp :: Name -> Maybe (Term -> Term -> Term -> Flow Graph Term) -> [String] -> TermCoder w -> TermCoder x -> TermCoder y -> TermCoder z -> Primitive
prim3Interp name mcompute vars input1 input2 input3 output = Primitive name typ impl
  where
    compute = Y.fromMaybe (\a b c -> noInterpretedForm name) mcompute
    typ = Types.poly vars $ Types.functionMany [
      termCoderType input1,
      termCoderType input2,
      termCoderType input3,
      termCoderType output]
    impl args = do
      ExtractCore.nArgs name 3 args
      compute (args !! 0) (args !! 1) (args !! 2)

set :: Ord x => TermCoder x -> TermCoder (S.Set x)
set els = TermCoder (Types.set $ termCoderType els) $ Coder encode decode
  where
    encode = ExtractCore.set (coderEncode $ termCoderCoder els)
    decode s = Terms.set . S.fromList <$> mapM (coderDecode $ termCoderCoder els) (S.toList s)

string :: TermCoder String
string = TermCoder Types.string $ Coder encode decode
  where
    encode = ExtractCore.string
    decode = pure . Terms.string

term :: TermCoder Term
term = TermCoder (TypeVariable _Term) $ Coder encode decode
  where
    encode = pure
    decode = pure

type_ :: TermCoder Type
type_ = TermCoder (TypeVariable _Type) $ Coder encode decode
  where
    encode = coreDecodeType
    decode = pure . coreEncodeType

uint8 :: TermCoder Int16
uint8 = TermCoder Types.uint8 $ Coder encode decode
  where
    encode = ExtractCore.uint8
    decode = pure . Terms.uint8

uint16 :: TermCoder Int
uint16 = TermCoder Types.uint16 $ Coder encode decode
  where
    encode = ExtractCore.uint16
    decode = pure . Terms.uint16

uint32 :: TermCoder Int64
uint32 = TermCoder Types.uint32 $ Coder encode decode
  where
    encode = ExtractCore.uint32
    decode = pure . Terms.uint32

uint64 :: TermCoder Integer
uint64 = TermCoder Types.uint64 $ Coder encode decode
  where
    encode = ExtractCore.uint64
    decode = pure . Terms.uint64

variable :: String -> TermCoder Term
variable v = TermCoder (Types.var v) $ Coder encode decode
  where
    encode = pure
    decode = pure
