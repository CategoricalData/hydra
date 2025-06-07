module Hydra.Ext.Haskell.Utils where

import Hydra.Kernel
import Hydra.Staging.Adapters
import Hydra.Ext.Haskell.Language
import qualified Hydra.Ext.Haskell.Ast as H
import qualified Hydra.Lib.Strings as Strings

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


type HaskellNamespaces = Namespaces H.ModuleName

applicationPattern name args = H.PatternApplication $ H.ApplicationPattern name args

elementReference :: HaskellNamespaces -> Name -> H.Name
elementReference (Namespaces (gname, H.ModuleName gmod) namespaces) name = case (qualifiedNameNamespace qname) of
    Nothing -> simpleName local
    Just ns -> case M.lookup ns namespaces of
      Nothing -> simpleName local
      Just (H.ModuleName a) -> if ns == gname
         then simpleName escLocal
         else rawName $ a ++ "." ++ sanitizeWithUnderscores S.empty local
  where
    qname = qualifyName name
    local = qualifiedNameLocal qname
    escLocal = sanitizeHaskellName local
    simple = simpleName $ qualifiedNameLocal qname

hsapp :: H.Expression -> H.Expression -> H.Expression
hsapp l r = H.ExpressionApplication $ H.ApplicationExpression l r

hslambda :: String -> H.Expression -> H.Expression
hslambda v rhs = H.ExpressionLambda (H.LambdaExpression [H.PatternName $ rawName v] rhs)

hslit :: H.Literal -> H.Expression
hslit = H.ExpressionLiteral

hsvar :: String -> H.Expression
hsvar s = H.ExpressionVariable $ rawName s

namespacesForModule :: Module -> Flow Graph HaskellNamespaces
namespacesForModule mod = do
    nss <- moduleDependencyNamespaces True True True True mod
    return $ Namespaces focusPair $ fst $ L.foldl addPair (M.empty, S.empty) (toPair <$> S.toList nss)
  where
    ns = moduleNamespace mod
    focusPair = toPair ns
    toModuleName (Namespace n) = H.ModuleName $ capitalize $ L.last $ Strings.splitOn "." n
    toPair name = (name, toModuleName name)
    addPair (m, s) (name, alias@(H.ModuleName aliasStr)) = if S.member alias s
      then addPair (m, s) (name, H.ModuleName $ aliasStr ++ "_")
      else (M.insert name alias m, S.insert alias s)

newtypeAccessorName :: Name -> String
newtypeAccessorName name = "un" ++ localNameOf name

rawName :: String -> H.Name
rawName n = H.NameNormal $ H.QualifiedName [] $ H.NamePart n

recordFieldReference :: HaskellNamespaces -> Name -> Name -> H.Name
recordFieldReference namespaces sname (Name fname) = elementReference namespaces $
    unqualifyName $ QualifiedName (qualifiedNameNamespace $ qualifyName sname) nm
  where
    nm = decapitalize (typeNameForRecord sname) ++ capitalize fname

sanitizeHaskellName :: String -> String
sanitizeHaskellName = sanitizeWithUnderscores reservedWords

simpleName :: String -> H.Name
simpleName = rawName . sanitizeHaskellName

simpleValueBinding :: H.Name -> H.Expression -> Maybe H.LocalBindings -> H.ValueBinding
simpleValueBinding hname rhs bindings = H.ValueBindingSimple $ H.SimpleValueBinding pat (H.RightHandSide rhs) bindings
  where
    pat = H.PatternApplication $ H.ApplicationPattern hname []

toTypeApplication :: [H.Type] -> H.Type
toTypeApplication = app . L.reverse
  where
    app l = case l of
      [e] -> e
      (h:r) -> H.TypeApplication $ H.ApplicationType (app r) h

typeNameForRecord :: Name -> String
typeNameForRecord (Name sname) = L.last (Strings.splitOn "." sname)

unionFieldReference :: HaskellNamespaces -> Name -> Name -> H.Name
unionFieldReference namespaces sname (Name fname) = elementReference namespaces $
    unqualifyName $ QualifiedName ns nm
  where
    ns = qualifiedNameNamespace $ qualifyName sname
    nm = capitalize (typeNameForRecord sname) ++ capitalize fname

unpackForallType :: Graph -> Type -> ([Name], Type)
unpackForallType cx t = case stripType t of
  TypeForall (ForallType v tbody) -> (v:vars, t')
    where
      (vars, t') = unpackForallType cx tbody
  _ -> ([], t)
