-- Note: this is an automatically generated file. Do not edit.

-- | String formatting types and functions.

module Hydra.Formatting where

import qualified Hydra.Lib.Chars as Chars
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Capitalize the first letter of a string
capitalize :: (String -> String)
capitalize = (mapFirstLetter Strings.toUpper)

-- | Convert a string from one case convention to another
convertCase :: (Util.CaseConvention -> Util.CaseConvention -> String -> String)
convertCase from to original =  
  let parts =  
          let byCaps =  
                  let splitOnUppercase = (\acc -> \c -> Lists.concat2 (Logic.ifElse (Chars.isUpper c) [
                          []] []) (Lists.cons (Lists.cons c (Lists.head acc)) (Lists.tail acc)))
                  in (Lists.map Strings.fromList (Lists.foldl splitOnUppercase [
                    []] (Lists.reverse (Strings.toList (decapitalize original))))) 
              byUnderscores = (Strings.splitOn "_" original)
          in ((\x -> case x of
            Util.CaseConventionCamel -> byCaps
            Util.CaseConventionPascal -> byCaps
            Util.CaseConventionLowerSnake -> byUnderscores
            Util.CaseConventionUpperSnake -> byUnderscores) from)
  in ((\x -> case x of
    Util.CaseConventionCamel -> (decapitalize (Strings.cat (Lists.map (\arg_ -> capitalize (Strings.toLower arg_)) parts)))
    Util.CaseConventionPascal -> (Strings.cat (Lists.map (\arg_ -> capitalize (Strings.toLower arg_)) parts))
    Util.CaseConventionLowerSnake -> (Strings.intercalate "_" (Lists.map Strings.toLower parts))
    Util.CaseConventionUpperSnake -> (Strings.intercalate "_" (Lists.map Strings.toUpper parts))) to)

-- | Convert a string from camel case to lower snake case
convertCaseCamelToLowerSnake :: (String -> String)
convertCaseCamelToLowerSnake = (convertCase Util.CaseConventionCamel Util.CaseConventionLowerSnake)

-- | Convert a string from camel case to upper snake case
convertCaseCamelToUpperSnake :: (String -> String)
convertCaseCamelToUpperSnake = (convertCase Util.CaseConventionCamel Util.CaseConventionUpperSnake)

-- | Convert a string from pascal case to upper snake case
convertCasePascalToUpperSnake :: (String -> String)
convertCasePascalToUpperSnake = (convertCase Util.CaseConventionPascal Util.CaseConventionUpperSnake)

-- | Decapitalize the first letter of a string
decapitalize :: (String -> String)
decapitalize = (mapFirstLetter Strings.toLower)

-- | Escape reserved words by appending an underscore
escapeWithUnderscore :: (S.Set String -> String -> String)
escapeWithUnderscore reserved s = (Logic.ifElse (Sets.member s reserved) (Strings.cat [
  s,
  "_"]) s)

-- | Indent each line of a string with four spaces
indentLines :: (String -> String)
indentLines s =  
  let indent = (\l -> Strings.cat [
          "    ",
          l])
  in (Strings.unlines (Lists.map indent (Strings.lines s)))

-- | Format a string as a Java-style block comment
javaStyleComment :: (String -> String)
javaStyleComment s = (Strings.cat [
  Strings.cat [
    Strings.cat [
      "/**\n",
      " * "],
    s],
  "\n */"])

-- | A helper which maps the first letter of a string to another string
mapFirstLetter :: ((String -> String) -> String -> String)
mapFirstLetter mapping s =  
  let list = (Strings.toList s)
  in  
    let firstLetter = (mapping (Strings.fromList (Lists.pure (Lists.head list))))
    in (Logic.ifElse (Strings.null s) s (Strings.cat2 firstLetter (Strings.fromList (Lists.tail list))))

-- | Replace sequences of non-alphanumeric characters with single underscores
nonAlnumToUnderscores :: (String -> String)
nonAlnumToUnderscores input =  
  let isAlnum = (\c -> Logic.or (Logic.and (Equality.gte c 65) (Equality.lte c 90)) (Logic.or (Logic.and (Equality.gte c 97) (Equality.lte c 122)) (Logic.and (Equality.gte c 48) (Equality.lte c 57))))
  in  
    let replace = (\p -> \c ->  
            let s = (Pairs.first p)
            in  
              let b = (Pairs.second p)
              in (Logic.ifElse (isAlnum c) (Lists.cons c s, False) (Logic.ifElse b (s, True) (Lists.cons 95 s, True))))
    in  
      let result = (Lists.foldl replace ([], False) (Strings.toList input))
      in (Strings.fromList (Lists.reverse (Pairs.first result)))

-- | Sanitize a string by replacing non-alphanumeric characters and escaping reserved words
sanitizeWithUnderscores :: (S.Set String -> String -> String)
sanitizeWithUnderscores reserved s = (escapeWithUnderscore reserved (nonAlnumToUnderscores s))

showList :: ((t0 -> String) -> [t0] -> String)
showList f els = (Strings.cat [
  "[",
  Strings.intercalate ", " (Lists.map f els),
  "]"])

-- | Remove leading and trailing whitespace from a string
stripLeadingAndTrailingWhitespace :: (String -> String)
stripLeadingAndTrailingWhitespace s = (Strings.fromList (Lists.dropWhile Chars.isSpace (Lists.reverse (Lists.dropWhile Chars.isSpace (Lists.reverse (Strings.toList s))))))

-- | Replace special characters with their alphanumeric aliases
withCharacterAliases :: (String -> String)
withCharacterAliases original =  
  let aliases = (Maps.fromList [
          (32, "sp"),
          (33, "excl"),
          (34, "quot"),
          (35, "num"),
          (36, "dollar"),
          (37, "percnt"),
          (38, "amp"),
          (39, "apos"),
          (40, "lpar"),
          (41, "rpar"),
          (42, "ast"),
          (43, "plus"),
          (44, "comma"),
          (45, "minus"),
          (46, "period"),
          (47, "sol"),
          (58, "colon"),
          (59, "semi"),
          (60, "lt"),
          (61, "equals"),
          (62, "gt"),
          (63, "quest"),
          (64, "commat"),
          (91, "lsqb"),
          (92, "bsol"),
          (93, "rsqb"),
          (94, "circ"),
          (95, "lowbar"),
          (96, "grave"),
          (123, "lcub"),
          (124, "verbar"),
          (125, "rcub"),
          (126, "tilde")]) 
      alias = (\c -> Maybes.fromMaybe (Lists.pure c) (Maybes.map Strings.toList (Maps.lookup c aliases)))
  in (Strings.fromList (Lists.filter Chars.isAlphaNum (Lists.concat (Lists.map alias (Strings.toList original)))))

-- | A simple soft line wrap which is suitable for code comments
wrapLine :: (Int -> String -> String)
wrapLine maxlen input =  
  let helper = (\prev -> \rem ->  
          let trunc = (Lists.take maxlen rem) 
              spanResult = (Lists.span (\c -> Logic.and (Logic.not (Equality.equal c 32)) (Logic.not (Equality.equal c 9))) (Lists.reverse trunc))
              prefix = (Lists.reverse (Pairs.second spanResult))
              suffix = (Lists.reverse (Pairs.first spanResult))
          in (Logic.ifElse (Equality.lte (Lists.length rem) maxlen) (Lists.reverse (Lists.cons rem prev)) (Logic.ifElse (Lists.null prefix) (helper (Lists.cons trunc prev) (Lists.drop maxlen rem)) (helper (Lists.cons (Lists.init prefix) prev) (Lists.concat2 suffix (Lists.drop maxlen rem))))))
  in (Strings.fromList (Lists.intercalate [
    10] (helper [] (Strings.toList input))))
