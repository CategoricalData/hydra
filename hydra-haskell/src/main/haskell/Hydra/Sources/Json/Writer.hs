
module Hydra.Sources.Json.Writer where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.Grammars                        as Grammars
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Accessors                  as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Grammar                    as Grammar
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars       as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta      as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan         as Tarjan
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import Hydra.Ast
import qualified Hydra.Json.Model as J


jsonSerdeDefinition :: String -> TTerm a -> TBinding a
jsonSerdeDefinition = definitionInModule module_

module_ :: Module
module_ = Module ns elements
    [Serialization.ns]
    KernelTypes.kernelTypesNamespaces $
    Just "JSON serialization functions using the Hydra AST"
  where
    ns = Namespace "hydra.json.writer"
    elements = [
      toBinding colonOp,
      toBinding jsonString,
      toBinding keyValueToExpr,
      toBinding printJson,
      toBinding valueToExpr]

colonOp :: TBinding Op
colonOp = jsonSerdeDefinition "colonOp" $
  doc "The colon operator used to separate keys and values in JSON objects" $
  Ast.op
    (Ast.symbol $ string ":")
    (Ast.padding Ast.wsNone Ast.wsSpace)
    (Ast.precedence $ int32 0)
    Ast.associativityNone

-- | ASCII codepoints for characters that need escaping
quoteCode, backslashCode, backspaceCode, formfeedCode, newlineCode, returnCode, tabCode :: TTerm Int
quoteCode = int32 34      -- '"'
backslashCode = int32 92  -- '\\'
backspaceCode = int32 8   -- '\b'
formfeedCode = int32 12   -- '\f'
newlineCode = int32 10    -- '\n'
returnCode = int32 13     -- '\r'
tabCode = int32 9         -- '\t'

hexDigits :: TTerm String
hexDigits = string "0123456789abcdef"

jsonString :: TBinding (String -> String)
jsonString = jsonSerdeDefinition "jsonString" $
  doc "Escape and quote a string for JSON output" $
  "s" ~>
  -- hexEscape: encode a control character as \\u00XX
  "hexEscape" <~ ("c" ~>
    "hi" <~ Strings.fromList (Lists.pure (Strings.charAt (Math.div (var "c") (int32 16)) hexDigits)) $
    "lo" <~ Strings.fromList (Lists.pure (Strings.charAt (Math.mod (var "c") (int32 16)) hexDigits)) $
    string "\\u00" ++ var "hi" ++ var "lo") $
  -- escape function takes a codepoint (Int) and returns a String
  "escape" <~ ("c" ~>
    Logic.ifElse (Equality.equal (var "c") quoteCode)
      (string "\\\"")
      (Logic.ifElse (Equality.equal (var "c") backslashCode)
        (string "\\\\")
        (Logic.ifElse (Equality.equal (var "c") backspaceCode)
          (string "\\b")
          (Logic.ifElse (Equality.equal (var "c") formfeedCode)
            (string "\\f")
            (Logic.ifElse (Equality.equal (var "c") newlineCode)
              (string "\\n")
              (Logic.ifElse (Equality.equal (var "c") returnCode)
                (string "\\r")
                (Logic.ifElse (Equality.equal (var "c") tabCode)
                  (string "\\t")
                  -- Escape any other control character (< 0x20) as \u00XX
                  (Logic.ifElse (Equality.lt (var "c") (int32 32))
                    (var "hexEscape" @@ var "c")
                    -- Non-control character: pass through as-is
                    (Strings.fromList (Lists.pure (var "c"))))))))))) $
  "escaped" <~ Strings.cat (Lists.map (var "escape") (Strings.toList $ var "s")) $
  string "\"" ++ var "escaped" ++ string "\""

keyValueToExpr :: TBinding ((String, J.Value) -> Expr)
keyValueToExpr = jsonSerdeDefinition "keyValueToExpr" $
  doc "Convert a key-value pair to an AST expression" $
  "pair" ~>
  "key" <~ Pairs.first (var "pair") $
  "value" <~ Pairs.second (var "pair") $
  Serialization.ifx @@ colonOp
    @@ (Serialization.cst @@ (jsonString @@ var "key"))
    @@ (valueToExpr @@ var "value")

valueToExpr :: TBinding (J.Value -> Expr)
valueToExpr = jsonSerdeDefinition "valueToExpr" $
  doc "Convert a JSON value to an AST expression for serialization" $
  "value" ~>
  cases J._Value (var "value") Nothing [
    J._Value_array>>: "arr" ~>
      Serialization.bracketListAdaptive @@ (Lists.map (valueToExpr) (var "arr")),
    J._Value_boolean>>: "b" ~>
      Serialization.cst @@ (Logic.ifElse (var "b") (string "true") (string "false")),
    J._Value_null>>: constant $
      Serialization.cst @@ string "null",
    J._Value_number>>: "n" ~>
      -- For whole numbers, omit the decimal point (e.g., 15 instead of 15.0)
      "rounded" <~ Literals.bigfloatToBigint (var "n") $
      Serialization.cst @@ (Logic.ifElse
        (Equality.equal (var "n") (Literals.bigintToBigfloat $ var "rounded"))
        (Literals.showBigint $ var "rounded")
        (Literals.showBigfloat $ var "n")),
    J._Value_object>>: "obj" ~>
      Serialization.bracesListAdaptive @@ (Lists.map (keyValueToExpr) (Maps.toList $ var "obj")),
    J._Value_string>>: "s" ~>
      Serialization.cst @@ (jsonString @@ var "s")]

printJson :: TBinding (J.Value -> String)
printJson = jsonSerdeDefinition "printJson" $
  doc "Serialize a JSON value to a string" $
  "value" ~> Serialization.printExpr @@ (valueToExpr @@ var "value")
