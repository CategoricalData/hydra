-- Note: this is an automatically generated file. Do not edit.

-- | Utility functions for constructing Scala AST nodes

module Hydra.Ext.Scala.Utils where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Scala.Language as Language
import qualified Hydra.Ext.Scala.Meta as Meta
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Math as Math
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
    Strings.cat2 (Maybes.maybe dlft (\n -> Strings.cat2 (scalaTypeName True n) ".") sname) (scalaEscapeName (Core.unName fname))

-- | Convert a Hydra name to a Scala type name
scalaTypeName :: Bool -> Core.Name -> String
scalaTypeName qualify name =
    Logic.ifElse (Logic.or qualify (Sets.member (Names.localNameOf name) scalaReservedWords)) (Core.unName name) (Names.localNameOf name)

-- | Sanitize a name for Scala: escape reserved words, replace invalid characters
scalaEscapeName :: String -> String
scalaEscapeName s =

      let sanitized = Strings.fromList (Lists.map (\c -> Logic.ifElse (Equality.equal c 39) 95 c) (Strings.toList s))
          sanitized2 = Logic.ifElse (Equality.equal sanitized "_") "_x" sanitized
          needsBackticks =
                  Logic.or (Sets.member sanitized2 scalaReservedWords) (Logic.and (Equality.gt (Strings.length sanitized2) 0) (Equality.equal (Strings.charAt (Math.sub (Strings.length sanitized2) 1) sanitized2) 95))
      in (Logic.ifElse needsBackticks (Strings.cat [
        "`",
        sanitized2,
        "`"]) sanitized2)

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
          prefix = Module.unNamespace (Maybes.fromJust (Module.qualifiedNameNamespace qname))
          local = scalaEscapeName (Module.qualifiedNameLocal qname)
      in (sname (Strings.cat2 (Strings.cat2 prefix ".") local))

-- | Apply explicit type parameters to a Scala expression (e.g. f[A, B])
sapplyTypes :: Meta.Data -> [Meta.Type] -> Meta.Data
sapplyTypes fun typeArgs =

      let typeToStr = \t -> typeToString t
          typeStrings = Lists.map typeToStr typeArgs
          typeArgStr =
                  Strings.cat [
                    "[",
                    (Strings.intercalate ", " typeStrings),
                    "]"]
      in case fun of
        Meta.DataRef v0 -> case v0 of
          Meta.Data_RefName v1 ->
            let nameStr = Meta.data_NameValue v1
                rawName = Meta.unPredefString nameStr
            in (sname (Strings.cat2 rawName typeArgStr))
          _ -> fun
        _ -> fun

-- | Convert a Scala type to its string representation
typeToString :: Meta.Type -> String
typeToString t =
    case t of
      Meta.TypeRef v0 -> case v0 of
        Meta.Type_RefName v1 -> Meta.type_NameValue v1
        _ -> "Any"
      Meta.TypeVar v0 -> Meta.type_NameValue (Meta.type_VarName v0)
      Meta.TypeFunctionType v0 -> case v0 of
        Meta.Type_FunctionTypeFunction v1 ->
          let params = Lists.map typeToString (Meta.type_FunctionParams v1)
              res = typeToString (Meta.type_FunctionRes v1)
          in (Strings.cat [
            "(",
            (Strings.intercalate ", " params),
            ") => ",
            res])
        _ -> "Any"
      Meta.TypeApply v0 ->
        let base = typeToString (Meta.type_ApplyTpe v0)
            argStrs = Lists.map typeToString (Meta.type_ApplyArgs v0)
        in (Strings.cat [
          base,
          "[",
          (Strings.intercalate ", " argStrs),
          "]"])
      _ -> "Any"

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

-- | Create a Scala type parameter from a Hydra name, capitalizing to avoid collision with value params
stparam :: Core.Name -> Meta.Type_Param
stparam name =

      let v = Formatting.capitalize (Core.unName name)
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
