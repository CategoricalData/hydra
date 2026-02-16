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
keyValueToExpr :: ((String, Model.Value) -> Ast.Expr)
keyValueToExpr pair =  
  let key = (Pairs.first pair)
  in  
    let value = (Pairs.second pair)
    in (Serialization.ifx colonOp (Serialization.cst (jsonString key)) (valueToExpr value))

-- | Serialize a JSON value to a string
printJson :: (Model.Value -> String)
printJson value = (Serialization.printExpr (valueToExpr value))

-- | Convert a JSON value to an AST expression for serialization
valueToExpr :: (Model.Value -> Ast.Expr)
valueToExpr value = ((\x -> case x of
  Model.ValueArray v1 -> (Serialization.bracketListAdaptive (Lists.map valueToExpr v1))
  Model.ValueBoolean v1 -> (Serialization.cst (Logic.ifElse v1 "true" "false"))
  Model.ValueNull -> (Serialization.cst "null")
  Model.ValueNumber v1 ->  
    let rounded = (Literals.bigfloatToBigint v1)
    in (Serialization.cst (Logic.ifElse (Equality.equal v1 (Literals.bigintToBigfloat rounded)) (Literals.showBigint rounded) (Literals.showBigfloat v1)))
  Model.ValueObject v1 -> (Serialization.bracesListAdaptive (Lists.map keyValueToExpr (Maps.toList v1)))
  Model.ValueString v1 -> (Serialization.cst (jsonString v1))) value)
