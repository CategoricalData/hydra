-- Note: this is an automatically generated file. Do not edit.

-- | Utility functions for constructing Scala AST nodes

module Hydra.Ext.Scala.Utils where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Scala.Language as Language
import qualified Hydra.Ext.Scala.Syntax as Syntax
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
import qualified Hydra.Strip as Strip
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Extract the name from a type, if it is a named type
nameOfType :: t0 -> Core.Type -> Maybe Core.Name
nameOfType cx t =
    case (Strip.deannotateType t) of
      Core.TypeVariable v0 -> Just v0
      Core.TypeForall v0 -> nameOfType cx (Core.forallTypeBody v0)
      _ -> Nothing

-- | Qualify a union field name, optionally prefixing with the Scala type name
qualifyUnionFieldName :: String -> Maybe Core.Name -> Core.Name -> String
qualifyUnionFieldName dlft sname fname =
    Strings.cat2 (Maybes.maybe dlft (\n -> Strings.cat2 (scalaTypeName True n) ".") sname) (scalaEscapeName (Core.unName fname))

-- | Apply a Scala data expression to a list of arguments
sapply :: Syntax.Data -> [Syntax.Data] -> Syntax.Data
sapply fun args =
    Syntax.DataApply (Syntax.Data_Apply {
      Syntax.data_ApplyFun = fun,
      Syntax.data_ApplyArgs = args})

-- | Apply explicit type parameters to a Scala expression (e.g. f[A, B])
sapplyTypes :: Syntax.Data -> [Syntax.Type] -> Syntax.Data
sapplyTypes fun typeArgs =

      let typeToStr = \t -> typeToString t
          typeStrings = Lists.map typeToStr typeArgs
          typeArgStr =
                  Strings.cat [
                    "[",
                    (Strings.intercalate ", " typeStrings),
                    "]"]
      in case fun of
        Syntax.DataRef v0 -> case v0 of
          Syntax.Data_RefName v1 ->
            let nameStr = Syntax.data_NameValue v1
                rawName = Syntax.unPredefString nameStr
            in (sname (Strings.cat2 rawName typeArgStr))
          _ -> fun
        _ -> fun

-- | Create a Scala assignment expression
sassign :: Syntax.Data -> Syntax.Data -> Syntax.Data
sassign lhs rhs =
    Syntax.DataAssign (Syntax.Data_Assign {
      Syntax.data_AssignLhs = lhs,
      Syntax.data_AssignRhs = rhs})

-- | Sanitize a name for Scala: escape reserved words, replace invalid characters
scalaEscapeName :: String -> String
scalaEscapeName s =

      let sanitized = Strings.fromList (Lists.map (\c -> Logic.ifElse (Equality.equal c 39) 95 c) (Strings.toList s))
          sanitized2 = Logic.ifElse (Equality.equal sanitized "_") "_x" sanitized
          sanitized3 = Logic.ifElse (Equality.equal sanitized2 "toString") "toString_" sanitized2
          needsBackticks =
                  Logic.or (Sets.member sanitized3 scalaReservedWords) (Logic.and (Equality.gt (Strings.length sanitized3) 0) (Equality.equal (Strings.charAt (Math.sub (Strings.length sanitized3) 1) sanitized3) 95))
      in (Logic.ifElse needsBackticks (Strings.cat [
        "`",
        sanitized3,
        "`"]) sanitized3)

-- | Reference to scalaReservedWords from the language module
scalaReservedWords :: S.Set String
scalaReservedWords = Language.scalaReservedWords

-- | Convert a Hydra name to a Scala type name
scalaTypeName :: Bool -> Core.Name -> String
scalaTypeName qualify name =
    Logic.ifElse (Logic.or qualify (Sets.member (Names.localNameOf name) scalaReservedWords)) (Core.unName name) (Names.localNameOf name)

-- | Create a Scala lambda (function) expression
slambda :: String -> Syntax.Data -> Maybe Syntax.Type -> Syntax.Data
slambda v body sdom =
    Syntax.DataFunctionData (Syntax.Data_FunctionDataFunction (Syntax.Data_Function {
      Syntax.data_FunctionParams = [
        Syntax.Data_Param {
          Syntax.data_ParamMods = [],
          Syntax.data_ParamName = (Syntax.NameValue v),
          Syntax.data_ParamDecltpe = sdom,
          Syntax.data_ParamDefault = Nothing}],
      Syntax.data_FunctionBody = body}))

-- | Create a Scala name reference
sname :: String -> Syntax.Data
sname s = Syntax.DataRef (Syntax.Data_RefName (Syntax.Data_Name {
  Syntax.data_NameValue = (Syntax.PredefString s)}))

-- | Create a Scala primitive reference from a Hydra name
sprim :: Core.Name -> Syntax.Data
sprim name =

      let qname = Names.qualifyName name
          prefix = Module.unNamespace (Maybes.fromJust (Module.qualifiedNameNamespace qname))
          local = scalaEscapeName (Module.qualifiedNameLocal qname)
      in (sname (Strings.cat2 (Strings.cat2 prefix ".") local))

-- | Apply a Scala type to a list of type arguments
stapply :: Syntax.Type -> [Syntax.Type] -> Syntax.Type
stapply t args =
    Syntax.TypeApply (Syntax.Type_Apply {
      Syntax.type_ApplyTpe = t,
      Syntax.type_ApplyArgs = args})

-- | Apply a Scala type to one type argument
stapply1 :: Syntax.Type -> Syntax.Type -> Syntax.Type
stapply1 t1 t2 = stapply t1 [
  t2]

-- | Apply a Scala type to two type arguments
stapply2 :: Syntax.Type -> Syntax.Type -> Syntax.Type -> Syntax.Type
stapply2 t1 t2 t3 =
    stapply t1 [
      t2,
      t3]

-- | Create a Scala type parameter from a Hydra name, capitalizing to avoid collision with value params
stparam :: Core.Name -> Syntax.Type_Param
stparam name =

      let v = Formatting.capitalize (Core.unName name)
      in Syntax.Type_Param {
        Syntax.type_ParamMods = [],
        Syntax.type_ParamName = (Syntax.NameValue v),
        Syntax.type_ParamTparams = [],
        Syntax.type_ParamTbounds = [],
        Syntax.type_ParamVbounds = [],
        Syntax.type_ParamCbounds = []}

-- | Create a Scala type reference by name
stref :: String -> Syntax.Type
stref s = Syntax.TypeRef (Syntax.Type_RefName (Syntax.Type_Name {
  Syntax.type_NameValue = s}))

-- | Create a Scala pattern variable
svar :: Core.Name -> Syntax.Pat
svar name =

      let v = Core.unName name
      in (Syntax.PatVar (Syntax.Pat_Var {
        Syntax.pat_VarName = Syntax.Data_Name {
          Syntax.data_NameValue = (Syntax.PredefString v)}}))

-- | Convert a Scala type to its string representation
typeToString :: Syntax.Type -> String
typeToString t =
    case t of
      Syntax.TypeRef v0 -> case v0 of
        Syntax.Type_RefName v1 -> Syntax.type_NameValue v1
        _ -> "Any"
      Syntax.TypeVar v0 -> Syntax.type_NameValue (Syntax.type_VarName v0)
      Syntax.TypeFunctionType v0 -> case v0 of
        Syntax.Type_FunctionTypeFunction v1 ->
          let params = Lists.map typeToString (Syntax.type_FunctionParams v1)
              res = typeToString (Syntax.type_FunctionRes v1)
          in (Strings.cat [
            "(",
            (Strings.intercalate ", " params),
            ") => ",
            res])
        _ -> "Any"
      Syntax.TypeApply v0 ->
        let base = typeToString (Syntax.type_ApplyTpe v0)
            argStrs = Lists.map typeToString (Syntax.type_ApplyArgs v0)
        in (Strings.cat [
          base,
          "[",
          (Strings.intercalate ", " argStrs),
          "]"])
      _ -> "Any"
