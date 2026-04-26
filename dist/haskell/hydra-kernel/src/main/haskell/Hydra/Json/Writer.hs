-- Note: this is an automatically generated file. Do not edit.
-- | JSON serialization functions using the Hydra AST

module Hydra.Json.Writer where
import qualified Hydra.Ast as Ast
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | The colon operator used to separate keys and values in JSON objects
colonOp :: Ast.Op
colonOp =
    Ast.Op {
      Ast.opSymbol = (Ast.Symbol ":"),
      Ast.opPadding = Ast.Padding {
        Ast.paddingLeft = Ast.WsNone,
        Ast.paddingRight = Ast.WsSpace},
      Ast.opPrecedence = (Ast.Precedence 0),
      Ast.opAssociativity = Ast.AssociativityNone}
-- | Encode a byte (0..255) as a two-character lowercase hex string. Non-byte inputs yield placeholder '?' characters.
hexByte :: Int -> String
hexByte c =

      let nibble =
              \i -> Maybes.fromMaybe "?" (Maybes.map (\ch -> Strings.fromList (Lists.pure ch)) (Strings.maybeCharAt i "0123456789abcdef"))
          hi = nibble (Maybes.fromMaybe 0 (Math.maybeDiv c 16))
          lo = nibble (Maybes.fromMaybe 0 (Math.maybeMod c 16))
      in (Strings.cat2 hi lo)
-- | Escape and quote a string for JSON output
jsonString :: String -> String
jsonString s =

      let hexEscape = \c -> Strings.cat2 "\\u00" (hexByte c)
          escape =
                  \c -> Logic.ifElse (Equality.equal c 34) "\\\"" (Logic.ifElse (Equality.equal c 92) "\\\\" (Logic.ifElse (Equality.equal c 8) "\\b" (Logic.ifElse (Equality.equal c 12) "\\f" (Logic.ifElse (Equality.equal c 10) "\\n" (Logic.ifElse (Equality.equal c 13) "\\r" (Logic.ifElse (Equality.equal c 9) "\\t" (Logic.ifElse (Equality.lt c 32) (hexEscape c) (Strings.fromList (Lists.pure c)))))))))
          escaped = Strings.cat (Lists.map escape (Strings.toList s))
      in (Strings.cat2 (Strings.cat2 "\"" escaped) "\"")
-- | Convert a key-value pair to an AST expression
keyValueToExpr :: (String, Model.Value) -> Ast.Expr
keyValueToExpr pair =

      let key = Pairs.first pair
          value = Pairs.second pair
      in (Serialization.ifx colonOp (Serialization.cst (jsonString key)) (valueToExpr value))
-- | Serialize a JSON value to a string
printJson :: Model.Value -> String
printJson value = Serialization.printExpr (valueToExpr value)
-- | Convert a JSON value to an AST expression for serialization
valueToExpr :: Model.Value -> Ast.Expr
valueToExpr value =
    case value of
      Model.ValueArray v0 -> Serialization.bracketListAdaptive (Lists.map valueToExpr v0)
      Model.ValueBoolean v0 -> Serialization.cst (Logic.ifElse v0 "true" "false")
      Model.ValueNull -> Serialization.cst "null"
      Model.ValueNumber v0 ->
        let rounded = Literals.decimalToBigint v0
            shown = Literals.showDecimal v0
            isWhole = Equality.equal v0 (Literals.bigintToDecimal rounded)
            plain = Literals.showBigint rounded
        in (Serialization.cst (Logic.ifElse (Logic.and isWhole (Equality.lte (Strings.length plain) (Strings.length shown))) plain shown))
      Model.ValueObject v0 -> Serialization.bracesListAdaptive (Lists.map keyValueToExpr (Maps.toList v0))
      Model.ValueString v0 -> Serialization.cst (jsonString v0)
