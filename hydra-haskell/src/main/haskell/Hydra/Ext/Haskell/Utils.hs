module Hydra.Ext.Haskell.Utils where

import Hydra.Core
import Hydra.Module
import Hydra.Compute
import Hydra.Monads
import Hydra.Adapters.Coders
import Hydra.Util.Formatting
import Hydra.Ext.Haskell.Language
import qualified Hydra.Ext.Haskell.Ast as H
import qualified Hydra.Lib.Strings as Strings
import Hydra.Rewriting

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


data Namespaces = Namespaces {
  namespacesFocus :: (Namespace, H.ModuleName),
  namespacesMapping :: M.Map Namespace H.ModuleName}

elementReference :: Namespaces -> Name -> H.Name
elementReference (Namespaces (gname, H.ModuleName gmod) namespaces) name = case alias of
    Nothing -> simpleName local
    Just (H.ModuleName a) -> if ns == gname
      then simpleName escLocal
      else rawName $ a ++ "." ++ escLocal
  where
    (ns, local) = toQname name
    alias = M.lookup ns namespaces
    escLocal = sanitizeHaskellName local

hsapp :: H.Expression -> H.Expression -> H.Expression
hsapp l r = H.ExpressionApplication $ H.Expression_Application l r

hslambda :: String -> H.Expression -> H.Expression
hslambda v rhs = H.ExpressionLambda (H.Expression_Lambda [H.PatternName $ rawName v] rhs)

hslit :: H.Literal -> H.Expression
hslit = H.ExpressionLiteral

hsPrimitiveReference :: Name -> H.Name
hsPrimitiveReference name = H.NameNormal $ H.QualifiedName [prefix] $ H.NamePart local
  where
    (Namespace ns, local) = toQname name
    prefix = H.NamePart $ capitalize $ L.last $ Strings.splitOn "/" ns

hsvar :: String -> H.Expression
hsvar s = H.ExpressionVariable $ rawName s

namespacesForModule :: Module m -> Namespaces
namespacesForModule mod = Namespaces focusPair mapping
  where
    ns = moduleNamespace mod
    focusPair = toPair ns
    mapping = fst $ L.foldl addPair (M.empty, S.empty) (toPair <$> S.toList (moduleDependencyNamespaces True True True mod))
    toModuleName (Namespace n) = H.ModuleName $ capitalize $ L.last $ Strings.splitOn "/" n
    toPair name = (name, toModuleName name)
    addPair (m, s) (name, alias@(H.ModuleName aliasStr)) = if S.member alias s
      then addPair (m, s) (name, H.ModuleName $ aliasStr ++ "_")
      else (M.insert name alias m, S.insert alias s)

newtypeAccessorName :: Name -> String
newtypeAccessorName name = "un" ++ localNameOf name

rawName :: String -> H.Name
rawName n = H.NameNormal $ H.QualifiedName [] $ H.NamePart n

recordFieldReference :: Namespaces -> Name -> FieldName -> H.Name
recordFieldReference namespaces sname (FieldName fname) = elementReference namespaces $ fromQname (fst $ toQname sname) nm
  where
    nm = decapitalize (typeNameForRecord sname) ++ capitalize fname

sanitizeHaskellName :: String -> String
sanitizeHaskellName = sanitizeWithUnderscores reservedWords

simpleName :: String -> H.Name
simpleName = rawName . sanitizeHaskellName

toTypeApplication :: [H.Type] -> H.Type
toTypeApplication = app . L.reverse
  where
    app l = case l of
      [e] -> e
      (h:r) -> H.TypeApplication $ H.Type_Application (app r) h

typeNameForRecord :: Name -> String
typeNameForRecord (Name sname) = L.last (Strings.splitOn "." sname)

unionFieldReference :: Namespaces -> Name -> FieldName -> H.Name
unionFieldReference namespaces sname (FieldName fname) = elementReference namespaces $ fromQname (fst $ toQname sname) nm
  where
    nm = capitalize (typeNameForRecord sname) ++ capitalize fname

unpackLambdaType :: Context m -> Type m -> ([VariableType], Type m)
unpackLambdaType cx t = case stripType t of
  TypeLambda (LambdaType v tbody) -> (v:vars, t')
    where
      (vars, t') = unpackLambdaType cx tbody
  _ -> ([], t)
