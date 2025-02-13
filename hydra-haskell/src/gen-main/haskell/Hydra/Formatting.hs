-- | String formatting types and functions.

module Hydra.Formatting where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Chars as Chars
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Mantle as Mantle
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | Capitalize the first letter of a string
capitalize :: (String -> String)
capitalize = (mapFirstLetter Strings.toUpper)

-- | Convert a string from one case convention to another
convertCase :: (Mantle.CaseConvention -> Mantle.CaseConvention -> String -> String)
convertCase from to original =  
  let parts =  
          let byCaps =  
                  let splitOnUppercase = (\acc -> \c -> Lists.concat2 (Logic.ifElse [
                          []] [] (Chars.isUpper c)) (Lists.cons (Lists.cons c (Lists.head acc)) (Lists.tail acc)))
                  in (Lists.map Strings.fromList (Lists.foldl splitOnUppercase [
                    []] (Lists.reverse (Strings.toList (decapitalize original))))) 
              byUnderscores = (Strings.splitOn "_" original)
          in ((\x -> case x of
            Mantle.CaseConventionCamel -> byCaps
            Mantle.CaseConventionPascal -> byCaps
            Mantle.CaseConventionLowerSnake -> byUnderscores
            Mantle.CaseConventionUpperSnake -> byUnderscores) from)
  in ((\x -> case x of
    Mantle.CaseConventionCamel -> (decapitalize (Strings.cat (Lists.map (\x -> capitalize (Strings.toLower x)) parts)))
    Mantle.CaseConventionPascal -> (Strings.cat (Lists.map (\x -> capitalize (Strings.toLower x)) parts))
    Mantle.CaseConventionLowerSnake -> (Strings.intercalate "_" (Lists.map Strings.toLower parts))
    Mantle.CaseConventionUpperSnake -> (Strings.intercalate "_" (Lists.map Strings.toUpper parts))) to)

-- | Convert a string from camel case to lower snake case
convertCaseCamelToLowerSnake :: (String -> String)
convertCaseCamelToLowerSnake = (convertCase Mantle.CaseConventionCamel Mantle.CaseConventionLowerSnake)

-- | Convert a string from camel case to upper snake case
convertCaseCamelToUpperSnake :: (String -> String)
convertCaseCamelToUpperSnake = (convertCase Mantle.CaseConventionCamel Mantle.CaseConventionUpperSnake)

-- | Convert a string from pascal case to upper snake case
convertCasePascalToUpperSnake :: (String -> String)
convertCasePascalToUpperSnake = (convertCase Mantle.CaseConventionPascal Mantle.CaseConventionUpperSnake)

-- | Decapitalize the first letter of a string
decapitalize :: (String -> String)
decapitalize = (mapFirstLetter Strings.toLower)

-- | A helper which maps the first letter of a string to another string
mapFirstLetter :: ((String -> String) -> String -> String)
mapFirstLetter mapping s =  
  let firstLetter = (mapping (Strings.fromList (Lists.pure (Lists.head list)))) 
      list = (Strings.toList s)
  in (Logic.ifElse s (Strings.cat2 firstLetter (Strings.fromList (Lists.tail list))) (Strings.isEmpty s))