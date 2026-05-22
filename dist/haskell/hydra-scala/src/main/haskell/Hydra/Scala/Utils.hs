-- Note: this is an automatically generated file. Do not edit.
-- | Utility functions for constructing Scala AST nodes

module Hydra.Scala.Utils where
import qualified Hydra.Core as Core
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Scala.Language as Language
import qualified Hydra.Scala.Syntax as Syntax
import qualified Hydra.Strip as Strip
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
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
    Syntax.DataApply (Syntax.ApplyData {
      Syntax.applyDataFun = fun,
      Syntax.applyDataArgs = args})
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
          Syntax.RefDataName v1 ->
            let nameStr = Syntax.nameDataValue v1
                rawName = Syntax.unPredefString nameStr
            in (sname (Strings.cat2 rawName typeArgStr))
          _ -> fun
        _ -> fun
-- | Create a Scala assignment expression
sassign :: Syntax.Data -> Syntax.Data -> Syntax.Data
sassign lhs rhs =
    Syntax.DataAssign (Syntax.AssignData {
      Syntax.assignDataLhs = lhs,
      Syntax.assignDataRhs = rhs})
-- | Sanitize a name for Scala: escape reserved words, replace invalid characters
scalaEscapeName :: String -> String
scalaEscapeName s =

      let sanitized = Strings.fromList (Lists.map (\c -> Logic.ifElse (Equality.equal c 39) 95 c) (Strings.toList s))
          sanitized2 = Logic.ifElse (Equality.equal sanitized "_") "_x" sanitized
          sanitized3 = Logic.ifElse (Equality.equal sanitized2 "toString") "toString_" sanitized2
          needsBackticks =
                  Logic.or (Sets.member sanitized3 scalaReservedWords) (Logic.and (Equality.gt (Strings.length sanitized3) 0) (Equality.equal (Maybes.fromMaybe 0 (Strings.maybeCharAt (Math.sub (Strings.length sanitized3) 1) sanitized3)) 95))
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
    Syntax.DataFunction (Syntax.FunctionData {
      Syntax.functionDataParams = [
        Syntax.ParamData {
          Syntax.paramDataMods = [],
          Syntax.paramDataName = (Syntax.NameValue v),
          Syntax.paramDataDecltpe = sdom,
          Syntax.paramDataDefault = Nothing}],
      Syntax.functionDataBody = body})
-- | Create a Scala name reference
sname :: String -> Syntax.Data
sname s = Syntax.DataRef (Syntax.RefDataName (Syntax.NameData {
  Syntax.nameDataValue = (Syntax.PredefString s)}))
-- | Create a Scala primitive reference from a Hydra name
sprim :: Core.Name -> Syntax.Data
sprim name =

      let qname = Names.qualifyName name
          prefix = Packaging.unModuleName (Maybes.fromMaybe (Packaging.ModuleName "") (Packaging.qualifiedNameModuleName qname))
          local = scalaEscapeName (Packaging.qualifiedNameLocal qname)
      in (sname (Strings.cat2 (Strings.cat2 prefix ".") local))
-- | Apply a Scala type to a list of type arguments
stapply :: Syntax.Type -> [Syntax.Type] -> Syntax.Type
stapply t args =
    Syntax.TypeApply (Syntax.ApplyType {
      Syntax.applyTypeTpe = t,
      Syntax.applyTypeArgs = args})
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
stparam :: Core.Name -> Syntax.ParamType
stparam name =

      let v = Formatting.capitalize (Core.unName name)
      in Syntax.ParamType {
        Syntax.paramTypeMods = [],
        Syntax.paramTypeName = (Syntax.NameValue v),
        Syntax.paramTypeTparams = [],
        Syntax.paramTypeTbounds = [],
        Syntax.paramTypeVbounds = [],
        Syntax.paramTypeCbounds = []}
-- | Create a Scala type reference by name
stref :: String -> Syntax.Type
stref s = Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
  Syntax.nameTypeValue = s}))
-- | Create a Scala pattern variable
svar :: Core.Name -> Syntax.Pat
svar name =

      let v = Core.unName name
      in (Syntax.PatVar (Syntax.VarPat {
        Syntax.varPatName = Syntax.NameData {
          Syntax.nameDataValue = (Syntax.PredefString v)}}))
-- | Convert a Scala type to its string representation
typeToString :: Syntax.Type -> String
typeToString t =
    case t of
      Syntax.TypeRef v0 -> case v0 of
        Syntax.RefTypeName v1 -> Syntax.nameTypeValue v1
        _ -> "Any"
      Syntax.TypeVar v0 -> Syntax.nameTypeValue (Syntax.varTypeName v0)
      Syntax.TypeFunction v0 ->
        let params = Lists.map typeToString (Syntax.functionTypeParams v0)
            res = typeToString (Syntax.functionTypeRes v0)
        in (Strings.cat [
          "(",
          (Strings.intercalate ", " params),
          ") => ",
          res])
      Syntax.TypeApply v0 ->
        let base = typeToString (Syntax.applyTypeTpe v0)
            argStrs = Lists.map typeToString (Syntax.applyTypeArgs v0)
        in (Strings.cat [
          base,
          "[",
          (Strings.intercalate ", " argStrs),
          "]"])
      _ -> "Any"
