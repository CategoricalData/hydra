-- Note: this is an automatically generated file. Do not edit.

-- | JSON serialization functions using the Hydra AST

module Hydra.Json.Writer where

import qualified Hydra.Ast as Ast
import qualified Hydra.Json as Json
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | The colon operator used to separate keys and values in JSON objects
colonOp :: Ast.Op
colonOp = Ast.Op {
  Ast.opSymbol = (Ast.Symbol ":"),
  Ast.opPadding = Ast.Padding {
    Ast.paddingLeft = Ast.WsNone,
    Ast.paddingRight = Ast.WsSpace},
  Ast.opPrecedence = (Ast.Precedence 0),
  Ast.opAssociativity = Ast.AssociativityNone}

-- | Escape and quote a string for JSON output
jsonString :: (String -> String)
jsonString s =  
  let escape = (\c -> Logic.ifElse (Equality.equal c 34) "\\\"" (Logic.ifElse (Equality.equal c 92) "\\\\" (Logic.ifElse (Equality.equal c 10) "\\n" (Logic.ifElse (Equality.equal c 13) "\\r" (Logic.ifElse (Equality.equal c 9) "\\t" (Strings.fromList (Lists.pure c)))))))
  in  
    let escaped = (Strings.cat (Lists.map escape (Strings.toList s)))
    in (Strings.cat2 (Strings.cat2 "\"" escaped) "\"")

-- | Convert a key-value pair to an AST expression
keyValueToExpr :: ((String, Json.Value) -> Ast.Expr)
keyValueToExpr pair =  
  let key = (Pairs.first pair)
  in  
    let value = (Pairs.second pair)
    in (Serialization.ifx colonOp (Serialization.cst (jsonString key)) (valueToExpr value))

-- | Serialize a JSON value to a string
printJson :: (Json.Value -> String)
printJson value = (Serialization.printExpr (valueToExpr value))

-- | Convert a JSON value to an AST expression for serialization
valueToExpr :: (Json.Value -> Ast.Expr)
valueToExpr value = ((\x -> case x of
  Json.ValueArray v1 -> (Serialization.bracketListAdaptive (Lists.map valueToExpr v1))
  Json.ValueBoolean v1 -> (Serialization.cst (Logic.ifElse v1 "true" "false"))
  Json.ValueNull -> (Serialization.cst "null")
  Json.ValueNumber v1 -> (Serialization.cst (Literals.showBigfloat v1))
  Json.ValueObject v1 -> (Serialization.bracesListAdaptive (Lists.map keyValueToExpr (Maps.toList v1)))
  Json.ValueString v1 -> (Serialization.cst (jsonString v1))) value)
