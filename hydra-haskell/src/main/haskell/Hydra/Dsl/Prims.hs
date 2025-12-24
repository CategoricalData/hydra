{-# LANGUAGE FlexibleInstances #-} -- for IsString with nontrivial parameters

-- | A DSL for constructing primitive function definitions
module Hydra.Dsl.Prims where

import Hydra.Compute
import Hydra.Core
import Hydra.Graph
import Hydra.Util
import qualified Hydra.Monads as Monads
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Util as Util
import qualified Hydra.Extract.Util as ExtractUtil
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Show.Core as ShowCore

import Data.Int
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import Hydra.Rewriting (removeTermAnnotations)
import Data.String(IsString(..))

-- | A type variable specification with optional class constraints
data TypeVar = TypeVar {
  typeVarName :: String,
  typeVarClasses :: [Name]
}

-- | Create an unconstrained type variable
v :: String -> TypeVar
v name = TypeVar name []

-- | Create a type variable with Ord constraint
vOrd :: String -> TypeVar
vOrd name = TypeVar name [Name "hydra.typeclass.Ord"]

-- | Create a type variable with Eq constraint
vEq :: String -> TypeVar
vEq name = TypeVar name [Name "hydra.typeclass.Eq"]

-- | Convert a list of TypeVars to the format needed by polyConstrained
-- Filters out variables with no constraints
typeVarsToConstraints :: [TypeVar] -> [(String, [Name])]
typeVarsToConstraints = filter (not . L.null . snd) . fmap (\tv -> (typeVarName tv, typeVarClasses tv))

-- | Get just the variable names from a list of TypeVars
typeVarNames :: [TypeVar] -> [String]
typeVarNames = fmap typeVarName

-- | Build a TypeScheme from type variables and a type
-- Uses polyConstrained if there are any constraints, otherwise poly
buildTypeScheme :: [TypeVar] -> Type -> TypeScheme
buildTypeScheme vars typ =
  let constraints = typeVarsToConstraints vars
  in if L.null constraints
     then Types.poly (typeVarNames vars) typ
     else Types.polyConstrained (fmap (\tv -> (typeVarName tv, typeVarClasses tv)) vars) typ

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
    encode = ExtractUtil.comparison
    decode = pure . Terms.comparison

either_ :: TermCoder x -> TermCoder y -> TermCoder (Prelude.Either x y)
either_ xCoder yCoder = TermCoder (Types.either_ (termCoderType xCoder) (termCoderType yCoder)) $ Coder encode decode
  where
    encode term = case term of
      TermEither (Prelude.Left l) -> Prelude.Left <$> coderEncode (termCoderCoder xCoder) l
      TermEither (Prelude.Right r) -> Prelude.Right <$> coderEncode (termCoderCoder yCoder) r
      _ -> fail $ "expected either term, got: " ++ show term
    decode ev = case ev of
      Prelude.Left x -> do
        xTerm <- coderDecode (termCoderCoder xCoder) x
        return $ Terms.left xTerm
      Prelude.Right y -> do
        yTerm <- coderDecode (termCoderCoder yCoder) y
        return $ Terms.right yTerm

floatType :: TermCoder FloatType
floatType = TermCoder (TypeVariable _FloatType) $ Coder encode decode
  where
    encode term = do
      g <- Monads.getState
      Monads.eitherToFlow Util.unDecodingError $ DecodeCore.floatType g term
    decode = pure . EncodeCore.floatType

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
    encode term = fail $ "cannot encode term to a function: " ++ ShowCore.term term
    decode _ = fail $ "cannot decode functions to terms"

integerType :: TermCoder IntegerType
integerType = TermCoder (TypeVariable _IntegerType) $ Coder encode decode
  where
    encode term = do
      g <- Monads.getState
      Monads.eitherToFlow Util.unDecodingError $ DecodeCore.integerType g term
    decode = pure . EncodeCore.integerType

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
    encode = ExtractCore.listOf (coderEncode $ termCoderCoder els)
    decode l = Terms.list <$> mapM (coderDecode $ termCoderCoder els) l

literal :: TermCoder Literal
literal = TermCoder (TypeVariable _Literal) $ Coder encode decode
  where
    encode = ExtractCore.literal
    decode = pure . Terms.literal

literalType :: TermCoder LiteralType
literalType = TermCoder (TypeVariable _LiteralType) $ Coder encode decode
  where
    encode term = do
      g <- Monads.getState
      Monads.eitherToFlow Util.unDecodingError $ DecodeCore.literalType g term
    decode = pure . EncodeCore.literalType

map :: Ord k => TermCoder k -> TermCoder v -> TermCoder (M.Map k v)
map keys values = TermCoder (Types.map (termCoderType keys) (termCoderType values)) $ Coder encode decode
  where
    encode = ExtractCore.map (coderEncode $ termCoderCoder keys) (coderEncode $ termCoderCoder values)
    decode m = Terms.map . M.fromList <$> mapM decodePair (M.toList m)
      where
        decodePair (k, v) = do
          ke <- (coderDecode $ termCoderCoder keys) k
          ve <- (coderDecode $ termCoderCoder values) v
          return (ke, ve)

optional :: TermCoder x -> TermCoder (Y.Maybe x)
optional mel = TermCoder (Types.optional $ termCoderType mel) $ Coder encode decode
  where
    encode = ExtractCore.maybeTerm (coderEncode $ termCoderCoder mel)
    decode mv = Terms.optional <$> case mv of
      Nothing -> pure Nothing
      Just v -> Just <$> (coderDecode $ termCoderCoder mel) v

pair :: TermCoder x -> TermCoder y -> TermCoder (x, y)
pair xCoder yCoder = TermCoder (Types.pair (termCoderType xCoder) (termCoderType yCoder)) $ Coder encode decode
  where
    encode = ExtractCore.pair (coderEncode $ termCoderCoder xCoder) (coderEncode $ termCoderCoder yCoder)
    decode (x, y) = do
      xTerm <- coderDecode (termCoderCoder xCoder) x
      yTerm <- coderDecode (termCoderCoder yCoder) y
      return $ Terms.pair xTerm yTerm

prim0 :: Name -> x -> [TypeVar] -> TermCoder x -> Primitive
prim0 name value vars output = Primitive name typ impl
  where
    typ = buildTypeScheme vars $ termCoderType output
    impl _ = coderDecode (termCoderCoder output) value

prim1 :: Name -> (x -> y) -> [TypeVar] -> TermCoder x -> TermCoder y -> Primitive
prim1 name compute vars input1 output = Primitive name typ impl
  where
    typ = buildTypeScheme vars $ Types.functionMany [
      termCoderType input1,
      termCoderType output]
    impl args = do
      ExtractCore.nArgs name 1 args
      arg1 <- coderEncode (termCoderCoder input1) (args !! 0)
      coderDecode (termCoderCoder output) $ compute arg1

prim2 :: Name -> (x -> y -> z) -> [TypeVar] -> TermCoder x -> TermCoder y -> TermCoder z -> Primitive
prim2 name compute vars input1 input2 output = Primitive name typ impl
  where
    typ = buildTypeScheme vars $ Types.functionMany [
      termCoderType input1,
      termCoderType input2,
      termCoderType output]
    impl args = do
      ExtractCore.nArgs name 2 args
      arg1 <- coderEncode (termCoderCoder input1) (args !! 0)
      arg2 <- coderEncode (termCoderCoder input2) (args !! 1)
      coderDecode (termCoderCoder output) $ compute arg1 arg2

prim3 :: Name -> (w -> x -> y -> z) -> [TypeVar] -> TermCoder w -> TermCoder x -> TermCoder y -> TermCoder z -> Primitive
prim3 name compute vars input1 input2 input3 output = Primitive name typ impl
  where
    typ = buildTypeScheme vars $ Types.functionMany [
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

prim1Eval :: Name -> (Term -> Flow Graph Term) -> [TypeVar] -> TermCoder x -> TermCoder y -> Primitive
prim1Eval name compute vars input1 output = Primitive name typ impl
  where
    typ = buildTypeScheme vars $ Types.functionMany [
      termCoderType input1,
      termCoderType output]
    impl args = do
      ExtractCore.nArgs name 1 args
      compute (args !! 0)

prim2Eval :: Name -> (Term -> Term -> Flow Graph Term) -> [TypeVar] -> TermCoder x -> TermCoder y -> TermCoder z -> Primitive
prim2Eval name compute vars input1 input2 output = Primitive name typ impl
  where
    typ = buildTypeScheme vars $ Types.functionMany [
      termCoderType input1,
      termCoderType input2,
      termCoderType output]
    impl args = do
      ExtractCore.nArgs name 2 args
      compute (args !! 0) (args !! 1)

prim3Eval :: Name -> (Term -> Term -> Term -> Flow Graph Term) -> [TypeVar] -> TermCoder w -> TermCoder x -> TermCoder y -> TermCoder z -> Primitive
prim3Eval name compute vars input1 input2 input3 output = Primitive name typ impl
  where
    typ = buildTypeScheme vars $ Types.functionMany [
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
    encode = ExtractCore.setOf (coderEncode $ termCoderCoder els)
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
    encode term = do
      g <- Monads.getState
      Monads.eitherToFlow Util.unDecodingError $ DecodeCore.type_ g term
    decode = pure . EncodeCore.type_

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
