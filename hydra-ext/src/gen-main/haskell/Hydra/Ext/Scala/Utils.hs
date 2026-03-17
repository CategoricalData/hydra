-- Note: this is an automatically generated file. Do not edit.

-- | Utility functions for constructing Scala AST nodes

module Hydra.Ext.Scala.Utils where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Scala.Language as Language
import qualified Hydra.Ext.Scala.Meta as Meta
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import qualified Hydra.Rewriting as Rewriting
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Extract the name from a type, if it is a named type
nameOfType :: t0 -> Core.Type -> Maybe Core.Name
nameOfType cx t =
    case (Rewriting.deannotateType t) of
      Core.TypeVariable v0 -> Just v0
      Core.TypeForall v0 -> nameOfType cx (Core.forallTypeBody v0)
      _ -> Nothing

-- | Qualify a union field name, optionally prefixing with the Scala type name
qualifyUnionFieldName :: String -> Maybe Core.Name -> Core.Name -> String
qualifyUnionFieldName dlft sname fname =
    Strings.cat2 (Maybes.maybe dlft (\n -> Strings.cat2 (scalaTypeName True n) ".") sname) (Core.unName fname)

-- | Convert a Hydra name to a Scala type name
scalaTypeName :: Bool -> Core.Name -> String
scalaTypeName qualify name =
    Logic.ifElse (Logic.or qualify (Sets.member (Names.localNameOf name) scalaReservedWords)) (Core.unName name) (Names.localNameOf name)

-- | Apply a Scala data expression to a list of arguments
sapply :: Meta.Data -> [Meta.Data] -> Meta.Data
sapply fun args =
    Meta.DataApply (Meta.Data_Apply {
      Meta.data_ApplyFun = fun,
      Meta.data_ApplyArgs = args})

-- | Create a Scala assignment expression
sassign :: Meta.Data -> Meta.Data -> Meta.Data
sassign lhs rhs =
    Meta.DataAssign (Meta.Data_Assign {
      Meta.data_AssignLhs = lhs,
      Meta.data_AssignRhs = rhs})

-- | Create a Scala lambda (function) expression
slambda :: String -> Meta.Data -> Maybe Meta.Type -> Meta.Data
slambda v body sdom =
    Meta.DataFunctionData (Meta.Data_FunctionDataFunction (Meta.Data_Function {
      Meta.data_FunctionParams = [
        Meta.Data_Param {
          Meta.data_ParamMods = [],
          Meta.data_ParamName = (Meta.NameValue v),
          Meta.data_ParamDecltpe = sdom,
          Meta.data_ParamDefault = Nothing}],
      Meta.data_FunctionBody = body}))

-- | Create a Scala name reference
sname :: String -> Meta.Data
sname s = Meta.DataRef (Meta.Data_RefName (Meta.Data_Name {
  Meta.data_NameValue = (Meta.PredefString s)}))

-- | Create a Scala primitive reference from a Hydra name
sprim :: Core.Name -> Meta.Data
sprim name =

      let qname = Names.qualifyName name
          prefix = Lists.last (Strings.splitOn "." (Module.unNamespace (Maybes.fromJust (Module.qualifiedNameNamespace qname))))
          local = Module.qualifiedNameLocal qname
      in (sname (Strings.cat2 (Strings.cat2 prefix ".") local))

-- | Apply a Scala type to a list of type arguments
stapply :: Meta.Type -> [Meta.Type] -> Meta.Type
stapply t args =
    Meta.TypeApply (Meta.Type_Apply {
      Meta.type_ApplyTpe = t,
      Meta.type_ApplyArgs = args})

-- | Apply a Scala type to one type argument
stapply1 :: Meta.Type -> Meta.Type -> Meta.Type
stapply1 t1 t2 = stapply t1 [
  t2]

-- | Apply a Scala type to two type arguments
stapply2 :: Meta.Type -> Meta.Type -> Meta.Type -> Meta.Type
stapply2 t1 t2 t3 =
    stapply t1 [
      t2,
      t3]

-- | Create a Scala type parameter from a Hydra name
stparam :: Core.Name -> Meta.Type_Param
stparam name =

      let v = Core.unName name
      in Meta.Type_Param {
        Meta.type_ParamMods = [],
        Meta.type_ParamName = (Meta.NameValue v),
        Meta.type_ParamTparams = [],
        Meta.type_ParamTbounds = [],
        Meta.type_ParamVbounds = [],
        Meta.type_ParamCbounds = []}

-- | Create a Scala type reference by name
stref :: String -> Meta.Type
stref s = Meta.TypeRef (Meta.Type_RefName (Meta.Type_Name {
  Meta.type_NameValue = s}))

-- | Create a Scala pattern variable
svar :: Core.Name -> Meta.Pat
svar name =

      let v = Core.unName name
      in (Meta.PatVar (Meta.Pat_Var {
        Meta.pat_VarName = Meta.Data_Name {
          Meta.data_NameValue = (Meta.PredefString v)}}))

-- | Reference to scalaReservedWords from the language module
scalaReservedWords :: S.Set String
scalaReservedWords = Language.scalaReservedWords
