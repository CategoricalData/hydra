module Hydra.Util.Formatting where

import qualified Hydra.Lib.Strings as Strings

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


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

toLower :: String -> String
toLower = fmap C.toLower

toUpper :: String -> String
toUpper = fmap C.toLower

withCharacterAliases :: String -> String
withCharacterAliases original = L.filter C.isAlphaNum $ L.concat $ alias <$> original
  where    
    alias c = Y.maybe [c] capitalize $ M.lookup (C.ord c) aliases

    -- Taken from: https://cs.stanford.edu/people/miles/iso8859.html
    aliases = M.fromList [
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
      (126, "tilde")]
