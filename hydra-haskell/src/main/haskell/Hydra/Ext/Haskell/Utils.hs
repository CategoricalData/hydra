module Hydra.Ext.Haskell.Utils where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Ext.Haskell.Ast as H
import qualified Hydra.Ext.Haskell.Language as HaskellLanguage
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Qnames as Qnames
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Strip as Strip

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


type HaskellNamespaces = Module.Namespaces H.ModuleName

applicationPattern :: H.Name -> [H.Pattern] -> H.Pattern
applicationPattern name args = H.PatternApplication $ H.ApplicationPattern name args

elementReference :: HaskellNamespaces -> Core.Name -> H.Name
elementReference (Module.Namespaces (gname, H.ModuleName gmod) namespaces) name = case (Module.qualifiedNameNamespace qname) of
    Nothing -> simpleName local
    Just ns -> case M.lookup ns namespaces of
      Nothing -> simpleName local
      Just (H.ModuleName a) -> if ns == gname
         then simpleName escLocal
         else rawName $ a ++ "." ++ sanitizeHaskellName local
  where
    qname = Qnames.qualifyName name
    local = Module.qualifiedNameLocal qname
    escLocal = sanitizeHaskellName local
    simple = simpleName $ Module.qualifiedNameLocal qname

hsapp :: H.Expression -> H.Expression -> H.Expression
hsapp l r = H.ExpressionApplication $ H.ApplicationExpression l r

hslambda :: H.Name -> H.Expression -> H.Expression
hslambda name rhs = H.ExpressionLambda (H.LambdaExpression [H.PatternName name] rhs)

hslit :: H.Literal -> H.Expression
hslit = H.ExpressionLiteral

hsvar :: String -> H.Expression
hsvar s = H.ExpressionVariable $ rawName s

namespacesForModule :: Module.Module -> Compute.Flow Graph.Graph HaskellNamespaces
namespacesForModule mod = do
    nss <- Schemas.moduleDependencyNamespaces True True True True mod
    return $ Module.Namespaces focusPair $ fst $ L.foldl addPair (M.empty, S.empty) (toPair <$> S.toList nss)
  where
    ns = Module.moduleNamespace mod
    focusPair = toPair ns
    toModuleName (Module.Namespace n) = H.ModuleName $ Formatting.capitalize $ L.last $ Strings.splitOn "." n
    toPair name = (name, toModuleName name)
    addPair (m, s) (name, alias@(H.ModuleName aliasStr)) = if S.member alias s
      then addPair (m, s) (name, H.ModuleName $ aliasStr ++ "_")
      else (M.insert name alias m, S.insert alias s)

newtypeAccessorName :: Core.Name -> String
newtypeAccessorName name = "un" ++ Qnames.localNameOf name

rawName :: String -> H.Name
rawName n = H.NameNormal $ H.QualifiedName [] $ H.NamePart n

recordFieldReference :: HaskellNamespaces -> Core.Name -> Core.Name -> H.Name
recordFieldReference namespaces sname (Core.Name fname) = elementReference namespaces $
    Qnames.unqualifyName $ Module.QualifiedName (Module.qualifiedNameNamespace $ Qnames.qualifyName sname) nm
  where
    nm = Formatting.decapitalize (typeNameForRecord sname) ++ Formatting.capitalize fname

sanitizeHaskellName :: String -> String
sanitizeHaskellName = Formatting.sanitizeWithUnderscores HaskellLanguage.reservedWords

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

typeNameForRecord :: Core.Name -> String
typeNameForRecord (Core.Name sname) = L.last (Strings.splitOn "." sname)

unionFieldReference :: HaskellNamespaces -> Core.Name -> Core.Name -> H.Name
unionFieldReference namespaces sname (Core.Name fname) = elementReference namespaces $
    Qnames.unqualifyName $ Module.QualifiedName ns nm
  where
    ns = Module.qualifiedNameNamespace $ Qnames.qualifyName sname
    nm = Formatting.capitalize (typeNameForRecord sname) ++ Formatting.capitalize fname

unpackForallType :: Graph.Graph -> Core.Type -> ([Core.Name], Core.Type)
unpackForallType cx t = case Strip.stripType t of
  Core.TypeForall (Core.ForallType v tbody) -> (v:vars, t')
    where
      (vars, t') = unpackForallType cx tbody
  _ -> ([], t)
