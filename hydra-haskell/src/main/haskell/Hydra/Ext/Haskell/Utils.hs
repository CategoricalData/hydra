module Hydra.Ext.Haskell.Utils where

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Extras
import Hydra.Util.Coders
import Hydra.Util.Formatting
import qualified Hydra.Ext.Haskell.Ast as H
import qualified Hydra.Lib.Strings as Strings

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


elementReference :: M.Map GraphName H.ModuleName -> Name -> H.Name
elementReference aliases name = case alias of
    Nothing -> simpleName local
    Just (H.ModuleName a) -> rawName $ a ++ "." ++ sanitize local
  where
    (ns, local) = toQname name
    alias = M.lookup ns aliases

hsapp :: H.Expression -> H.Expression -> H.Expression
hsapp l r = H.ExpressionApplication $ H.Expression_Application l r

hslambda :: String -> H.Expression -> H.Expression
hslambda v rhs = H.ExpressionLambda (H.Expression_Lambda [H.PatternName $ rawName v] rhs)

hslit :: H.Literal -> H.Expression
hslit = H.ExpressionLiteral

hsPrimitiveReference :: Name -> H.Name
hsPrimitiveReference name = H.NameNormal $ H.QualifiedName [prefix] $ H.NamePart local
  where
    (GraphName ns, local) = toQname name
    prefix = H.NamePart $ capitalize $ L.last $ Strings.splitOn "/" ns

hsvar :: String -> H.Expression
hsvar s = H.ExpressionVariable $ rawName s

importAliasesForGraph :: Graph m -> M.Map GraphName H.ModuleName
importAliasesForGraph g = fst $ L.foldl addPair (M.empty, S.empty)
    (toPair <$> S.toList (dataGraphDependencies True True True g))
  where
    toPair name@(GraphName n) = (name, H.ModuleName $ capitalize $ L.last $ Strings.splitOn "/" n)
    addPair (m, s) (name, alias@(H.ModuleName aliasStr)) = if S.member alias s
      then addPair (m, s) (name, H.ModuleName $ aliasStr ++ "_")
      else (M.insert name alias m, S.insert alias s)

newtypeAccessorName :: Name -> String
newtypeAccessorName name = "un" ++ localNameOf name

rawName :: String -> H.Name
rawName n = H.NameNormal $ H.QualifiedName [] $ H.NamePart n

recordFieldReference :: M.Map GraphName H.ModuleName -> Name -> FieldName -> H.Name
recordFieldReference aliases sname (FieldName fname) = elementReference aliases $ fromQname (fst $ toQname sname) nm
  where
    nm = decapitalize (typeNameForRecord sname) ++ capitalize fname

sanitize :: String -> String
sanitize s = if preserve
  then s
  else fmap (\c -> if c == '.' then '_' else c) s
  where
    -- TODO: hack
    preserve = L.isInfixOf " " s

simpleName :: String -> H.Name
simpleName = rawName . sanitize

toApplicationType :: [H.Type] -> H.Type
toApplicationType = app . L.reverse
  where
    app l = case l of
      [e] -> e
      (h:r) -> H.TypeApplication $ H.Type_Application (app r) h

typeNameForRecord :: Name -> String
typeNameForRecord (Name sname) = L.last (Strings.splitOn "." sname)

unionFieldReference :: M.Map GraphName H.ModuleName -> Name -> FieldName -> H.Name
unionFieldReference aliases sname (FieldName fname) = elementReference aliases $ fromQname (fst $ toQname sname) nm
  where
    nm = capitalize (typeNameForRecord sname) ++ capitalize fname

unpackUniversalType :: Type m -> ([TypeVariable], Type m)
unpackUniversalType t = case typeTerm t of
  TypeTermUniversal (UniversalType v tbody) -> (v:vars, t')
    where
      (vars, t') = unpackUniversalType tbody
  _ -> ([], t)
