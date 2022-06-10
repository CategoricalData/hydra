module Hydra.Impl.Haskell.Sources.Adapters.Utils (adapterUtilsModule) where

import Hydra.Core
import Hydra.Impl.Haskell.Sources.Libraries
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Terms
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Sources.Basics


(++.) :: Default a => Term a -> Term a -> Term a
l ++. r = apply (primitive _strings_cat) $ list [l, r]

(@.) :: Default a => Term a -> Term a -> Term a
l @. r = apply l r

(->.) :: Default a => String -> Term a -> Term a
v ->. body = lambda v body

const_ :: Default a => Term a -> Term a
const_ = constFunction

_eldata :: Default a1 => Element a2 -> Term a1
_eldata el = delta @. element (elementName el)

l_ :: Default a => String -> Term a -> Term a
l_ = lambda

match_ :: Name -> Type Meta -> [(FieldName, Term Meta)] -> Term Meta
match_ = standardMatch

p_ :: Default a => Name -> Term a
p_ = primitive

r_ :: Name -> [Field Meta] -> Term Meta
r_ = standardRecord

s_ :: String -> Term Meta
s_ = stringValue

v_ :: Default a => String -> Term a
v_ = variable


adapterUtilsModule :: Module Meta
adapterUtilsModule = Module adapterUtils [hydraBasicsModule]

adapterUtilsName :: GraphName
adapterUtilsName = GraphName "hydra/adapters/utils"

adapterUtils :: Graph Meta
adapterUtils = standardGraph adapterUtilsName [
  describeFloatType,
  describeIntegerType,
  describeLiteralType,
  describePrecision,
  describeType]

describeFloatType :: Element Meta
describeFloatType = standardFunction adapterUtilsName "describeFloatType"
  "Display a floating-point type as a string"
  (Types.nominal _FloatType) Types.string
  $ l_"t" $ (_eldata describePrecision @. (_eldata floatTypePrecision @. v_"t")) ++. s_" floating-point numbers"

describeIntegerType :: Element Meta
describeIntegerType = standardFunction adapterUtilsName "describeIntegerType"
  "Display an integer type as a string"
  (Types.nominal _IntegerType) Types.string
  $ l_"t" $ (_eldata describePrecision @. (_eldata integerTypePrecision @. v_"t")) ++. s_" integers"

describeLiteralType :: Element Meta
describeLiteralType = standardFunction adapterUtilsName "describeLiteralType"
  "Display a literal type as a string"
  (Types.nominal _LiteralType) Types.string $
  match_ _LiteralType Types.string [
    (_LiteralType_binary, const_ $ s_"binary strings"),
    (_LiteralType_boolean, const_ $ s_"boolean values"),
    (_LiteralType_float, _eldata describeFloatType),
    (_LiteralType_integer, _eldata describeIntegerType),
    (_LiteralType_string, const_ $ s_"character strings")]

describePrecision :: Element Meta
describePrecision = standardFunction adapterUtilsName "describePrecision"
  "Display numeric precision as a string"
  (Types.nominal _Precision) Types.string $
  match_ _Precision Types.string [
    (_Precision_arbitrary, const_ $ s_"arbitrary-precision"),
    (_Precision_bits,
      l_"bits" $ p_ _strings_cat @.
        list [
          p_ _literals_showInt32 @. v_"bits",
          s_"-bit"])]

describeType :: Element Meta
describeType = standardFunction adapterUtilsName "describeType"
  "Display a type as a string"
  (Types.universal "m" $ Types.nominal _Type) Types.string $
  lambda "typ" $ apply
    (match_ _TypeExpr Types.string [
      (_TypeExpr_literal, _eldata describeLiteralType),
      (_TypeExpr_element, l_"t" $ s_"elements containing " ++. (_eldata describeType @. v_"t")),
      (_TypeExpr_function, l_"ft" $ s_"functions from "
        ++. (_eldata describeType @. (project (Types.nominal _FunctionType) _FunctionType_domain (Types.nominal _Type) @. v_"ft"))
        ++. s_" to "
        ++. (_eldata describeType @. (project (Types.nominal _FunctionType) _FunctionType_codomain (Types.nominal _Type) @. v_"ft"))),
      (_TypeExpr_list, l_"t" $ s_"lists of " ++. (_eldata describeType @. v_"t")),
      (_TypeExpr_map, l_"mt" $ s_"maps from "
        ++. (_eldata describeType @. (project (Types.nominal _MapType) _MapType_keys (Types.nominal _Type) @. v_"mt"))
        ++. s_" to "
        ++. (_eldata describeType @. (project (Types.nominal _MapType) _MapType_values (Types.nominal _Type) @. v_"mt"))),
      (_TypeExpr_nominal, l_"name" $ s_"alias for " ++. apply (eliminateNominal _Name) (v_"name")),
      (_TypeExpr_optional, l_"ot" $ s_"optional " ++. (_eldata describeType @. v_"ot")),
      (_TypeExpr_record, const_ $ s_"records of a particular set of fields"),
      (_TypeExpr_set, l_"st" $ s_"sets of " ++. (_eldata describeType @. v_"st")),
      (_TypeExpr_union, const_ $ s_"unions of a particular set of fields"),
      (_TypeExpr_universal, const_ $ s_"polymorphic terms"),
      (_TypeExpr_variable, const_ $ s_"unspecified/parametric terms")])
    (apply (project (Types.universal "m" $ Types.nominal _Type) _Type_expr (Types.universal "m" $ Types.nominal _TypeExpr))
           $ variable "typ")

--idAdapter :: Element Meta
--idAdapter = standardFunction adapterUtilsName "idAdapter"
--  "An identity adapter for a given type"
--  (Types.nominal _Type) (TypeExprUniversal (UniversalType "m" $ ())) $
--  l_"t" $ r_ _Adapter [
--    Field _Adapter_isLossy (booleanValue False),
--    Field _Adapter_source (v_"t"),
--    Field _Adapter_target (v_"t"),
--    Field _Adapter_step (_eldata idStep)]
