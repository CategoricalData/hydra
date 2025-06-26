module Hydra.Ext.Haskell.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Variants as Variants

import qualified Data.Set as S


haskellLanguage :: Coders.Language
haskellLanguage = Coders.Language (Coders.LanguageName "hydra.ext.haskell") $ Coders.LanguageConstraints {
  Coders.languageConstraintsEliminationVariants = S.fromList Variants.eliminationVariants,
  Coders.languageConstraintsLiteralVariants = S.fromList [
    Mantle.LiteralVariantBoolean,
    Mantle.LiteralVariantFloat,
    Mantle.LiteralVariantInteger,
    Mantle.LiteralVariantString],
  Coders.languageConstraintsFloatTypes = S.fromList [
    -- Bigfloat is excluded for now
    Core.FloatTypeFloat32, -- Float
    Core.FloatTypeFloat64], -- Double
  Coders.languageConstraintsFunctionVariants = S.fromList Variants.functionVariants,
  Coders.languageConstraintsIntegerTypes = S.fromList [
    Core.IntegerTypeBigint, -- Integer
    Core.IntegerTypeInt8, -- Int8
    Core.IntegerTypeInt16, -- Int16
    Core.IntegerTypeInt32, -- Int
    Core.IntegerTypeInt64], -- Int64
  Coders.languageConstraintsTermVariants = S.fromList [
    Mantle.TermVariantApplication,
    Mantle.TermVariantFunction,
    Mantle.TermVariantLet,
    Mantle.TermVariantList,
    Mantle.TermVariantLiteral,
    Mantle.TermVariantMap,
    Mantle.TermVariantOptional,
    Mantle.TermVariantProduct,
    Mantle.TermVariantRecord,
    Mantle.TermVariantSet,
    Mantle.TermVariantUnion,
    Mantle.TermVariantVariable,
    Mantle.TermVariantWrap],
  Coders.languageConstraintsTypeVariants = S.fromList [
    Mantle.TypeVariantAnnotated,
    Mantle.TypeVariantApplication,
    Mantle.TypeVariantFunction,
    Mantle.TypeVariantForall,
    Mantle.TypeVariantList,
    Mantle.TypeVariantLiteral,
    Mantle.TypeVariantMap,
    Mantle.TypeVariantOptional,
    Mantle.TypeVariantProduct,
    Mantle.TypeVariantRecord,
    Mantle.TypeVariantSet,
    Mantle.TypeVariantUnion,
    Mantle.TypeVariantVariable,
    Mantle.TypeVariantWrap],
  Coders.languageConstraintsTypes = const True }

{-
  Created on 2025-02-28 using GHCi 9.6.6

  You can reproduce these lists of symbols by issuing the command `:browse Prelude` in GHCi, pasting the results into
  /tmp/browse_Prelude.txt, and then running the Bash command provided with each list.

  See also https://www.haskell.org/onlinereport/standard-prelude.html
-}
reservedWords :: S.Set String
reservedWords = S.fromList $ keywordSymbols ++ reservedSymbols
--reservedWords = S.fromList $ keywordSymbols ++ classSymbols ++ dataSymbols ++ functionSymbols ++ newtypeSymbols ++ typeSymbols
  where
    -- Haskell's strictly reserved keywords; they cannot be used as identifiers.
    keywordSymbols = [
      "case", "class", "data", "default", "deriving", "do", "else", "forall", "foreign", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where"]

    -- Hydra uses these symbols in generated code, so we reserve them to avoid conflicts.
    reservedSymbols = [
      "Bool", "Double", "False", "Float", "Int", "Integer", "Just", "Maybe", "Nothing", "Ord", "Show", "String", "True"]

    -- TODO: remove the nonessential symbols if they will not be used
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
      "show", "showChar", "list", "showParen", "showString", "shows", "showsPrec", "significand", "signum", "sin",
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