module Hydra.Ext.Scala.Utils where

import Hydra.Kernel
import qualified Hydra.Ext.Scala.Meta as Scala
import qualified Hydra.Lib.Strings as Strings
import Hydra.Ext.Scala.Language

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Maybe as Y


nameOfType :: Context m -> Type m -> Y.Maybe Name
nameOfType cx t = case stripType t of
  TypeWrapped name -> Just name
  TypeLambda (LambdaType _ body) -> nameOfType cx body
  _ -> Nothing

qualifyUnionFieldName :: String -> Y.Maybe Name -> FieldName -> String
qualifyUnionFieldName dlft sname (FieldName fname) = (Y.maybe dlft (\n -> scalaTypeName True n ++ ".") sname) ++ fname

scalaTypeName :: Bool -> Name -> String
scalaTypeName qualify name@(Name n) = if qualify || S.member local reservedWords
    then L.intercalate "." $ Strings.splitOn "/" n
    else local
  where
    (_, local) = toQnameLazy name

sapply :: Scala.Data -> [Scala.Data] -> Scala.Data
sapply fun args = Scala.DataApply $ Scala.Data_Apply fun args

sassign :: Scala.Data -> Scala.Data -> Scala.Data
sassign lhs rhs = Scala.DataAssign $ Scala.Data_Assign lhs rhs

slambda :: String -> Scala.Data -> Y.Maybe Scala.Type -> Scala.Data
slambda v body sdom = Scala.DataFunctionData $ Scala.Data_FunctionDataFunction
    $ Scala.Data_Function [Scala.Data_Param mods name sdom def] body
  where
    mods = []
    name = Scala.NameValue v
    def = Nothing

sname :: String -> Scala.Data
sname = Scala.DataRef . Scala.Data_RefName . Scala.Data_Name . Scala.PredefString

sprim :: Name -> Scala.Data
sprim name = sname $ prefix ++ "." ++ local
  where
    (Namespace ns, local) = toQnameLazy name
    prefix = L.last $ Strings.splitOn "/" ns

stapply :: Scala.Type -> [Scala.Type] -> Scala.Type
stapply t args = Scala.TypeApply $ Scala.Type_Apply t args

stapply1 :: Scala.Type -> Scala.Type -> Scala.Type
stapply1 t1 t2 = stapply t1 [t2]

stapply2 :: Scala.Type -> Scala.Type -> Scala.Type -> Scala.Type
stapply2 t1 t2 t3 = stapply t1 [t2, t3]

stparam :: VariableType -> Scala.Type_Param
stparam (VariableType v) = Scala.Type_Param [] (Scala.NameValue v) [] [] [] []

stref :: String -> Scala.Type
stref = Scala.TypeRef . Scala.Type_RefName . Scala.Type_Name

svar :: Variable -> Scala.Pat
svar (Variable v) = (Scala.PatVar . Scala.Pat_Var . Scala.Data_Name . Scala.PredefString) v
