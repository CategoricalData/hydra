-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.ast

module Hydra.Encode.Ast where
import qualified Hydra.Ast as Ast
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Optionals as Optionals
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Encoder for hydra.ast.Associativity
associativity :: Ast.Associativity -> Core.Term
associativity x =
    case x of
      Ast.AssociativityNone -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.ast.Associativity"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "none"),
          Core.fieldTerm = Core.TermUnit}})
      Ast.AssociativityLeft -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.ast.Associativity"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = Core.TermUnit}})
      Ast.AssociativityRight -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.ast.Associativity"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = Core.TermUnit}})
      Ast.AssociativityBoth -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.ast.Associativity"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "both"),
          Core.fieldTerm = Core.TermUnit}})
-- | Encoder for hydra.ast.BlockStyle
blockStyle :: Ast.BlockStyle -> Core.Term
blockStyle x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BlockStyle"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "indent"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) opt)) (Ast.blockStyleIndent x))},
        Core.Field {
          Core.fieldName = (Core.Name "newlineBeforeContent"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralBoolean x2)) (Ast.blockStyleNewlineBeforeContent x))},
        Core.Field {
          Core.fieldName = (Core.Name "newlineAfterContent"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralBoolean x2)) (Ast.blockStyleNewlineAfterContent x))}]})
-- | Encoder for hydra.ast.BracketExpr
bracketExpr :: Ast.BracketExpr -> Core.Term
bracketExpr x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BracketExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "brackets"),
          Core.fieldTerm = (brackets (Ast.bracketExprBrackets x))},
        Core.Field {
          Core.fieldName = (Core.Name "enclosed"),
          Core.fieldTerm = (expr (Ast.bracketExprEnclosed x))},
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (blockStyle (Ast.bracketExprStyle x))}]})
-- | Encoder for hydra.ast.Brackets
brackets :: Ast.Brackets -> Core.Term
brackets x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Brackets"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (symbol (Ast.bracketsOpen x))},
        Core.Field {
          Core.fieldName = (Core.Name "close"),
          Core.fieldTerm = (symbol (Ast.bracketsClose x))}]})
-- | Encoder for hydra.ast.Expr
expr :: Ast.Expr -> Core.Term
expr x =
    case x of
      Ast.ExprConst v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "const"),
          Core.fieldTerm = (symbol v0)}})
      Ast.ExprIndent v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "indent"),
          Core.fieldTerm = (indentedExpression v0)}})
      Ast.ExprOp v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (opExpr v0)}})
      Ast.ExprBrackets v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "brackets"),
          Core.fieldTerm = (bracketExpr v0)}})
      Ast.ExprSeq v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "seq"),
          Core.fieldTerm = (seqExpr v0)}})
-- | Encoder for hydra.ast.IndentStyle
indentStyle :: Ast.IndentStyle -> Core.Term
indentStyle x =
    case x of
      Ast.IndentStyleAllLines v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.ast.IndentStyle"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "allLines"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString v0))}})
      Ast.IndentStyleSubsequentLines v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.ast.IndentStyle"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "subsequentLines"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString v0))}})
-- | Encoder for hydra.ast.IndentedExpression
indentedExpression :: Ast.IndentedExpression -> Core.Term
indentedExpression x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.IndentedExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (indentStyle (Ast.indentedExpressionStyle x))},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (expr (Ast.indentedExpressionExpr x))}]})
-- | Encoder for hydra.ast.Op
op :: Ast.Op -> Core.Term
op x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Op"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbol"),
          Core.fieldTerm = (symbol (Ast.opSymbol x))},
        Core.Field {
          Core.fieldName = (Core.Name "padding"),
          Core.fieldTerm = (padding (Ast.opPadding x))},
        Core.Field {
          Core.fieldName = (Core.Name "precedence"),
          Core.fieldTerm = (precedence (Ast.opPrecedence x))},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (associativity (Ast.opAssociativity x))}]})
-- | Encoder for hydra.ast.OpExpr
opExpr :: Ast.OpExpr -> Core.Term
opExpr x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.OpExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (op (Ast.opExprOp x))},
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (expr (Ast.opExprLhs x))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (expr (Ast.opExprRhs x))}]})
-- | Encoder for hydra.ast.Padding
padding :: Ast.Padding -> Core.Term
padding x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Padding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (ws (Ast.paddingLeft x))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (ws (Ast.paddingRight x))}]})
-- | Encoder for hydra.ast.Precedence
precedence :: Ast.Precedence -> Core.Term
precedence x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ast.Precedence"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x2))) (Ast.unPrecedence x))})
-- | Encoder for hydra.ast.SeqExpr
seqExpr :: Ast.SeqExpr -> Core.Term
seqExpr x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.SeqExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (op (Ast.seqExprOp x))},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map expr xs)) (Ast.seqExprElements x))}]})
-- | Encoder for hydra.ast.Symbol
symbol :: Ast.Symbol -> Core.Term
symbol x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ast.Symbol"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Ast.unSymbol x))})
-- | Encoder for hydra.ast.Ws
ws :: Ast.Ws -> Core.Term
ws x =
    case x of
      Ast.WsNone -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "none"),
          Core.fieldTerm = Core.TermUnit}})
      Ast.WsSpace -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "space"),
          Core.fieldTerm = Core.TermUnit}})
      Ast.WsBreak -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "break"),
          Core.fieldTerm = Core.TermUnit}})
      Ast.WsBreakAndIndent v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "breakAndIndent"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString v0))}})
      Ast.WsDoubleBreak -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "doubleBreak"),
          Core.fieldTerm = Core.TermUnit}})
