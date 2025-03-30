module Hydra.Ext.Haskell.Language where

import Hydra.Kernel

import qualified Data.Set as S


haskellLanguage :: Language
haskellLanguage = Language (LanguageName "hydra.ext.haskell") $ LanguageConstraints {
  languageConstraintsEliminationVariants = S.fromList eliminationVariants,
  languageConstraintsLiteralVariants = S.fromList [
    LiteralVariantBoolean,
    LiteralVariantFloat,
    LiteralVariantInteger,
    LiteralVariantString],
  languageConstraintsFloatTypes = S.fromList [
    -- Bigfloat is excluded for now
    FloatTypeFloat32, -- Float
    FloatTypeFloat64], -- Double
  languageConstraintsFunctionVariants = S.fromList functionVariants,
  languageConstraintsIntegerTypes = S.fromList [
    IntegerTypeBigint, -- Integer
    IntegerTypeInt8, -- Int8
    IntegerTypeInt16, -- Int16
    IntegerTypeInt32, -- Int
    IntegerTypeInt64], -- Int64
  languageConstraintsTermVariants = S.fromList [
    TermVariantApplication,
    TermVariantFunction,
    TermVariantLet,
    TermVariantList,
    TermVariantLiteral,
    TermVariantMap,
    TermVariantOptional,
    TermVariantProduct,
    TermVariantRecord,
    TermVariantSet,
    TermVariantUnion,
    TermVariantVariable,
    TermVariantWrap],
  languageConstraintsTypeVariants = S.fromList [
    TypeVariantAnnotated,
    TypeVariantApplication,
    TypeVariantFunction,
    TypeVariantForall,
    TypeVariantList,
    TypeVariantLiteral,
    TypeVariantMap,
    TypeVariantOptional,
    TypeVariantProduct,
    TypeVariantRecord,
    TypeVariantSet,
    TypeVariantUnion,
    TypeVariantVariable,
    TypeVariantWrap],
  languageConstraintsTypes = const True }

{-
  Created on 2025-02-28 using GHCi 9.6.6

  You can reproduce these lists of symbols by issuing the command `:browse Prelude` in GHCi, pasting the results into
  /tmp/browse_Prelude.txt, and then running the Bash command provided with each list.

  See also https://www.haskell.org/onlinereport/standard-prelude.html
-}
reservedWords :: S.Set String
reservedWords = S.fromList $ classSymbols ++ dataSymbols ++ functionSymbols ++ newtypeSymbols ++ typeSymbols
  where
    {-
      cat /tmp/browse_Prelude.txt | grep "^class" | sed 's/^class *//' | sed 's/.*=>//' | sed 's/^ *//' | sed 's/ .*//' | grep "^[A-Z]" | sort | sed 's/^/"/' | sed 's/$/",/' | tr '\n' ' '
    -}
    classSymbols = [
      "Applicative", "Bounded", "Enum", "Eq", "Floating", "Foldable", "Fractional", "Functor", "Integral", "Monad",
      "MonadFail", "Monoid", "Num", "Ord", "Read", "Real", "RealFloat", "RealFrac", "Semigroup", "Show", "Traversable"]

    {-
      cat /tmp/browse_Prelude.txt | grep "^data" | sed 's/^data *//' | tr '=' '\n' | tr '|' '\n' | sed 's/ *//' | sed 's/ .*//' | sort | grep "^[A-Z]" | sed 's/^/"/' | sed 's/$/",/' | tr '\n' ' '
    -}
    dataSymbols = [
      "Bool", "Char", "Double", "EQ", "Either", "False", "Float", "GT", "Int", "Integer", "Just", "LT", "Left", "Maybe",
      "Nothing", "Ordering", "Right", "True", "Word"]

    {-
      cat /tmp/browse_Prelude.txt | grep "::" | grep -v "^type" | sed 's/^ *//' | sed 's/ .*//' | grep -v "'" | sed 's/^.*\.//' | sort | grep "^[a-z]" | sed 's/^/"/' | sed 's/$/",/' | tr '\n' ' '
    -}
    functionSymbols = [
      "abs", "acos", "acosh", "all", "and", "any", "appendFile", "asTypeOf", "asin", "asinh", "atan", "atan2", "atanh",
      "break", "ceiling", "compare", "concat", "concatMap", "const", "cos", "cosh", "curry", "cycle", "decodeFloat",
      "div", "divMod", "drop", "dropWhile", "either", "elem", "encodeFloat", "enumFrom", "enumFromThen",
      "enumFromThenTo", "enumFromTo", "error", "errorWithoutStackTrace", "even", "exp", "expm1", "exponent", "fail",
      "filter", "flip", "floatDigits", "floatRadix", "floatRange", "floor", "fmap", "fold", "foldMap", "foldl",
      "foldl1", "foldr", "foldr1", "fromEnum", "fromInteger", "fromIntegral", "fromRational", "fst", "gcd", "getChar",
      "getContents", "getLine", "head", "id", "init", "interact", "ioError", "isDenormalized", "isIEEE", "isInfinite",
      "isNaN", "isNegativeZero", "iterate", "last", "lcm", "length", "lex", "liftA2", "lines", "log", "log1mexp",
      "log1p", "log1pexp", "logBase", "lookup", "map", "mapM", "mapM_", "mappend", "max", "maxBound", "maximum",
      "maybe", "mconcat", "mempty", "min", "minBound", "minimum", "mod", "negate", "not", "notElem", "null", "odd",
      "or", "otherwise", "pi", "pred", "print", "product", "properFraction", "pure", "putChar", "putStr", "putStrLn",
      "quot", "quotRem", "read", "readFile", "readIO", "readList", "readListPrec", "readLn", "readParen", "readPrec",
      "reads", "readsPrec", "realToFrac", "recip", "rem", "repeat", "replicate", "return", "reverse", "round",
      "scaleFloat", "scanl", "scanl1", "scanr", "scanr1", "sconcat", "seq", "sequence", "sequenceA", "sequence_",
      "show", "showChar", "showList", "showParen", "showString", "shows", "showsPrec", "significand", "signum", "sin",
      "sinh", "snd", "span", "splitAt", "sqrt", "stimes", "subtract", "succ", "sum", "tail", "take", "takeWhile", "tan",
      "tanh", "toEnum", "toInteger", "toList", "toRational", "traverse", "truncate", "uncurry", "undefined", "unlines",
      "until", "unwords", "unzip", "unzip3", "userError", "words", "writeFile", "zip", "zip3", "zipWith", "zipWith3"]

    {-
      cat /tmp/browse_Prelude.txt | grep "^newtype" | sed 's/^newtype *//' | sed 's/ .*//' | sort | sed 's/^/"/' | sed 's/$/",/' | tr '\n' ' '
    -}
    newtypeSymbols = [
      "IO"]

    {-
      cat /tmp/browse_Prelude.txt | grep "^type" | sed 's/^type *//' | grep "^[A-Z]" | sed 's/ .*//' | sort | sed 's/^/"/' | sed 's/$/",/' | tr '\n' ' '
    -}
    typeSymbols = [
      "Applicative", "Bool", "Bounded", "Char", "Double", "Either", "Enum", "Eq", "FilePath", "FilePath", "Float",
      "Floating", "Foldable", "Fractional", "Functor", "IO", "IOError", "IOError", "Int", "Integer", "Integral",
      "Maybe", "Monad", "MonadFail", "Monoid", "Num", "Ord", "Ordering", "Rational", "Rational", "Read", "ReadS",
      "ReadS", "Real", "RealFloat", "RealFrac", "Semigroup", "Show", "ShowS", "ShowS", "String", "String",
      "Traversable", "Word"]
