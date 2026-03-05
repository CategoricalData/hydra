{-# LANGUAGE FlexibleInstances #-} -- for IsString with nontrivial parameters

-- | A DSL for constructing primitive function definitions
module Hydra.Dsl.Prims where

import Hydra.Compute
import Hydra.Core
import Hydra.Classes
import Hydra.Graph
import Hydra.Util
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Context as Context
import qualified Hydra.Error as Error
import qualified Hydra.Extract.Util as ExtractUtil
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Show.Core as ShowCore

import Data.Int
import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import Hydra.Rewriting (removeTermAnnotations)
import Data.String(IsString(..))
import Data.Either (Either)

-- | A helper to create an OtherError InContext from a string message and context
otherErr :: Context.Context -> String -> Context.InContext Error.OtherError
otherErr cx msg = Context.InContext (Error.OtherError msg) cx

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
vOrd name = TypeVar name [_TypeClass_ordering]

-- | Create a type variable with Eq constraint
vEq :: String -> TypeVar
vEq name = TypeVar name [_TypeClass_equality]

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
bigfloat = TermCoder Types.bigfloat encode decode
  where
    encode cx g = ExtractCore.bigfloat cx g
    decode _cx = Right . Terms.bigfloat

bigint :: TermCoder Integer
bigint = TermCoder Types.bigint encode decode
  where
    encode cx g = ExtractCore.bigint cx g
    decode _cx = Right . Terms.bigint

binary :: TermCoder B.ByteString
binary = TermCoder Types.binary encode decode
  where
    encode cx g = ExtractCore.binary cx g
    decode _cx = Right . Terms.binary

boolean :: TermCoder Bool
boolean = TermCoder Types.boolean encode decode
  where
    encode cx g = ExtractCore.boolean cx g
    decode _cx = Right . Terms.boolean

comparison :: TermCoder Comparison
comparison = TermCoder (TypeVariable _Comparison) encode decode
  where
    encode cx g = ExtractUtil.comparison cx g
    decode _cx = Right . Terms.comparison

either_ :: TermCoder x -> TermCoder y -> TermCoder (Prelude.Either x y)
either_ xCoder yCoder = TermCoder (Types.either_ (termCoderType xCoder) (termCoderType yCoder)) encode decode
  where
    encode cx g term = case term of
      TermEither (Prelude.Left l) -> Prelude.Left <$> termCoderEncode xCoder cx g l
      TermEither (Prelude.Right r) -> Prelude.Right <$> termCoderEncode yCoder cx g r
      _ -> Left $ otherErr cx $ "expected either term, got: " ++ show term
    decode cx ev = case ev of
      Prelude.Left x -> do
        xTerm <- termCoderDecode xCoder cx x
        return $ Terms.left xTerm
      Prelude.Right y -> do
        yTerm <- termCoderDecode yCoder cx y
        return $ Terms.right yTerm

floatType :: TermCoder FloatType
floatType = TermCoder (TypeVariable _FloatType) encode decode
  where
    encode _cx g term = case DecodeCore.floatType g term of
      Left err -> Left $ Context.InContext (Error.OtherError (Error.unDecodingError err)) _cx
      Right v -> Right v
    decode _cx = Right . EncodeCore.floatType

floatValue :: TermCoder FloatValue
floatValue = TermCoder (TypeVariable _FloatValue) encode decode
  where
    encode cx g = ExtractCore.floatValue cx g
    decode _cx = Right . Terms.float

float32 :: TermCoder Float
float32 = TermCoder Types.float32 encode decode
  where
    encode cx g = ExtractCore.float32 cx g
    decode _cx = Right . Terms.float32

float64 :: TermCoder Double
float64 = TermCoder Types.float64 encode decode
  where
    encode cx g = ExtractCore.float64 cx g
    decode _cx = Right . Terms.float64

function :: TermCoder x -> TermCoder y -> TermCoder (x -> y)
function dom cod = TermCoder (Types.function (termCoderType dom) (termCoderType cod)) encode decode
  where
    encode cx _g term = Left $ otherErr cx $ "cannot encode term to a function: " ++ ShowCore.term term
    decode cx _val = Left $ otherErr cx "cannot decode functions to terms"

-- | A TermCoder for function types, using a reducer to bridge term-level functions to native functions.
--   The reducer is called to evaluate function application at the term level.
--   Failures in reduction or encoding/decoding will result in a runtime error.
functionWithReduce :: (Context.Context -> Graph -> Term -> Either (Context.InContext Error.OtherError) Term) -> TermCoder x -> TermCoder y -> TermCoder (x -> y)
functionWithReduce reduce dom cod = TermCoder (Types.function (termCoderType dom) (termCoderType cod)) encode decode
  where
    encode cx g funTerm = Right $ \x ->
      let argTerm = case termCoderDecode dom cx x of
            Left _ -> error "functionWithReduce: failed to encode argument"
            Right t -> t
          resultTerm = case reduce cx g (TermApplication (Application funTerm argTerm)) of
            Left _ -> error "functionWithReduce: failed to reduce application"
            Right t -> t
      in case termCoderEncode cod cx g resultTerm of
           Left _ -> error "functionWithReduce: failed to decode result"
           Right v -> v
    decode cx _val = Left $ otherErr cx "cannot decode functions to terms"

integerType :: TermCoder IntegerType
integerType = TermCoder (TypeVariable _IntegerType) encode decode
  where
    encode _cx g term = case DecodeCore.integerType g term of
      Left err -> Left $ Context.InContext (Error.OtherError (Error.unDecodingError err)) _cx
      Right v -> Right v
    decode _cx = Right . EncodeCore.integerType

integerValue :: TermCoder IntegerValue
integerValue = TermCoder (TypeVariable _IntegerValue) encode decode
  where
    encode cx g = ExtractCore.integerValue cx g
    decode _cx = Right . Terms.integer

int8 :: TermCoder Int8
int8 = TermCoder Types.int8 encode decode
  where
    encode cx g = ExtractCore.int8 cx g
    decode _cx = Right . Terms.int8

int16 :: TermCoder Int16
int16 = TermCoder Types.int16 encode decode
  where
    encode cx g = ExtractCore.int16 cx g
    decode _cx = Right . Terms.int16

int32 :: TermCoder Int
int32 = TermCoder Types.int32 encode decode
  where
    encode cx g = ExtractCore.int32 cx g
    decode _cx = Right . Terms.int32

int64 :: TermCoder Int64
int64 = TermCoder Types.int64 encode decode
  where
    encode cx g = ExtractCore.int64 cx g
    decode _cx = Right . Terms.int64

list :: TermCoder x -> TermCoder [x]
list els = TermCoder (Types.list $ termCoderType els) encode decode
  where
    encode cx g = ExtractCore.listOf cx (termCoderEncode els cx g) g
    decode cx l = Terms.list <$> mapM (termCoderDecode els cx) l

literal :: TermCoder Literal
literal = TermCoder (TypeVariable _Literal) encode decode
  where
    encode cx g = ExtractCore.literal cx g
    decode _cx = Right . Terms.literal

literalType :: TermCoder LiteralType
literalType = TermCoder (TypeVariable _LiteralType) encode decode
  where
    encode _cx g term = case DecodeCore.literalType g term of
      Left err -> Left $ Context.InContext (Error.OtherError (Error.unDecodingError err)) _cx
      Right v -> Right v
    decode _cx = Right . EncodeCore.literalType

map :: Ord k => TermCoder k -> TermCoder v -> TermCoder (M.Map k v)
map keys values = TermCoder (Types.map (termCoderType keys) (termCoderType values)) encode decode
  where
    encode cx g = ExtractCore.map cx (termCoderEncode keys cx g) (termCoderEncode values cx g) g
    decode cx m = Terms.map . M.fromList <$> mapM decodePair (M.toList m)
      where
        decodePair (k, v) = do
          ke <- termCoderDecode keys cx k
          ve <- termCoderDecode values cx v
          return (ke, ve)

optional :: TermCoder x -> TermCoder (Y.Maybe x)
optional mel = TermCoder (Types.optional $ termCoderType mel) encode decode
  where
    encode cx g = ExtractCore.maybeTerm cx (termCoderEncode mel cx g) g
    decode cx mv = Terms.optional <$> case mv of
      Nothing -> pure Nothing
      Just v -> Just <$> termCoderDecode mel cx v

pair :: TermCoder x -> TermCoder y -> TermCoder (x, y)
pair xCoder yCoder = TermCoder (Types.pair (termCoderType xCoder) (termCoderType yCoder)) encode decode
  where
    encode cx g = ExtractCore.pair cx (termCoderEncode xCoder cx g) (termCoderEncode yCoder cx g) g
    decode cx (x, y) = do
      xTerm <- termCoderDecode xCoder cx x
      yTerm <- termCoderDecode yCoder cx y
      return $ Terms.pair xTerm yTerm

prim0 :: Name -> x -> [TypeVar] -> TermCoder x -> Primitive
prim0 name value vars output = Primitive name typ impl
  where
    typ = buildTypeScheme vars $ termCoderType output
    impl cx _g _args = case termCoderDecode output cx value of
      Left icoe -> Left $ Context.InContext (Error.ErrorOther (Context.inContextObject icoe)) (Context.inContextContext icoe)
      Right v -> Right v

prim1 :: Name -> (x -> y) -> [TypeVar] -> TermCoder x -> TermCoder y -> Primitive
prim1 name compute vars input1 output = Primitive name typ impl
  where
    typ = buildTypeScheme vars $ Types.functionMany [
      termCoderType input1,
      termCoderType output]
    impl cx g args = wrapOther cx $ do
      ExtractCore.nArgs cx name 1 args
      arg1 <- termCoderEncode input1 cx g (args !! 0)
      termCoderDecode output cx $ compute arg1

prim2 :: Name -> (x -> y -> z) -> [TypeVar] -> TermCoder x -> TermCoder y -> TermCoder z -> Primitive
prim2 name compute vars input1 input2 output = Primitive name typ impl
  where
    typ = buildTypeScheme vars $ Types.functionMany [
      termCoderType input1,
      termCoderType input2,
      termCoderType output]
    impl cx g args = wrapOther cx $ do
      ExtractCore.nArgs cx name 2 args
      arg1 <- termCoderEncode input1 cx g (args !! 0)
      arg2 <- termCoderEncode input2 cx g (args !! 1)
      termCoderDecode output cx $ compute arg1 arg2

prim3 :: Name -> (w -> x -> y -> z) -> [TypeVar] -> TermCoder w -> TermCoder x -> TermCoder y -> TermCoder z -> Primitive
prim3 name compute vars input1 input2 input3 output = Primitive name typ impl
  where
    typ = buildTypeScheme vars $ Types.functionMany [
      termCoderType input1,
      termCoderType input2,
      termCoderType input3,
      termCoderType output]
    impl cx g args = wrapOther cx $ do
      ExtractCore.nArgs cx name 3 args
      arg1 <- termCoderEncode input1 cx g (args !! 0)
      arg2 <- termCoderEncode input2 cx g (args !! 1)
      arg3 <- termCoderEncode input3 cx g (args !! 2)
      termCoderDecode output cx $ compute arg1 arg2 arg3

-- | Wrap an Either (InContext OtherError) into Either (InContext Error)
wrapOther :: Context.Context -> Either (Context.InContext Error.OtherError) a -> Either (Context.InContext Error.Error) a
wrapOther _cx (Right v) = Right v
wrapOther _cx (Left (Context.InContext oe cx')) = Left $ Context.InContext (Error.ErrorOther oe) cx'

set :: Ord x => TermCoder x -> TermCoder (S.Set x)
set els = TermCoder (Types.set $ termCoderType els) encode decode
  where
    encode cx g = ExtractCore.setOf cx (termCoderEncode els cx g) g
    decode cx s = Terms.set . S.fromList <$> mapM (termCoderDecode els cx) (S.toList s)

string :: TermCoder String
string = TermCoder Types.string encode decode
  where
    encode cx g = ExtractCore.string cx g
    decode _cx = Right . Terms.string

term :: TermCoder Term
term = TermCoder (TypeVariable _Term) encode decode
  where
    encode _cx _g = Right
    decode _cx = Right

type_ :: TermCoder Type
type_ = TermCoder (TypeVariable _Type) encode decode
  where
    encode _cx g t = case DecodeCore.type_ g t of
      Left err -> Left $ Context.InContext (Error.OtherError (Error.unDecodingError err)) _cx
      Right v -> Right v
    decode _cx = Right . EncodeCore.type_

uint8 :: TermCoder Int16
uint8 = TermCoder Types.uint8 encode decode
  where
    encode cx g = ExtractCore.uint8 cx g
    decode _cx = Right . Terms.uint8

uint16 :: TermCoder Int
uint16 = TermCoder Types.uint16 encode decode
  where
    encode cx g = ExtractCore.uint16 cx g
    decode _cx = Right . Terms.uint16

uint32 :: TermCoder Int64
uint32 = TermCoder Types.uint32 encode decode
  where
    encode cx g = ExtractCore.uint32 cx g
    decode _cx = Right . Terms.uint32

uint64 :: TermCoder Integer
uint64 = TermCoder Types.uint64 encode decode
  where
    encode cx g = ExtractCore.uint64 cx g
    decode _cx = Right . Terms.uint64

variable :: String -> TermCoder Term
variable v = TermCoder (Types.var v) encode decode
  where
    encode _cx _g = Right
    decode _cx = Right
