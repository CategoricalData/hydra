module Hydra.Util.Formatting where

import qualified Hydra.Lib.Strings as Strings

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Set as S


data Case = CaseCamel | CasePascal | CaseLowerSnake | CaseUpperSnake

capitalize :: String -> String
capitalize s = case s of
  [] -> []
  (h:r) -> C.toUpper h : r

convertCase :: Case -> Case -> String -> String
convertCase from to original = case to of
    CaseCamel -> decapitalize $ L.concat (capitalize . fmap C.toLower <$> parts)
    CasePascal -> L.concat (capitalize . fmap C.toLower <$> parts)
    CaseLowerSnake -> L.intercalate "_" (fmap C.toLower <$> parts)
    CaseUpperSnake -> L.intercalate "_" (fmap C.toUpper <$> parts)
  where
    parts = case from of
      CaseCamel -> byCaps
      CasePascal -> byCaps
      CaseLowerSnake -> byUnderscores
      CaseUpperSnake -> byUnderscores
    byUnderscores = Strings.splitOn "_" original
    byCaps = L.foldl helper [""] $ L.reverse $ decapitalize original
      where
        helper (h:r) c = ["" | C.isUpper c] ++ ((c:h):r)

decapitalize :: String -> String
decapitalize s = case s of
  [] -> []
  (h:r) -> C.toLower h : r

dotsToUnderscores :: String -> String
dotsToUnderscores s = if preserve
  then s
  else fmap (\c -> if c == '.' then '_' else c) s
  where
    -- TODO: hack
    preserve = L.isInfixOf " " s

escapeWithUnderscore :: S.Set String -> String -> String
escapeWithUnderscore reserved s = if S.member s reserved then s ++ "_" else s

javaStyleComment :: String -> String
javaStyleComment s = "/**\n" ++ " * " ++ s ++ "\n */"

sanitizeWithUnderscores :: S.Set String -> String -> String
sanitizeWithUnderscores reserved = escapeWithUnderscore reserved . dotsToUnderscores
