-- Note: this is an automatically generated file. Do not edit.
-- | Term decoders for hydra.ast

module Hydra.Decode.Ast where
import qualified Hydra.Ast as Ast
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
associativity :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Ast.Associativity
associativity cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "none", (\input -> Eithers.map (\t -> Ast.AssociativityNone) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "left", (\input -> Eithers.map (\t -> Ast.AssociativityLeft) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "right", (\input -> Eithers.map (\t -> Ast.AssociativityRight) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "both", (\input -> Eithers.map (\t -> Ast.AssociativityBoth) (ExtractCore.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
blockStyle :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Ast.BlockStyle
blockStyle cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "indent" (ExtractCore.decodeMaybe (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_indent -> Eithers.bind (ExtractCore.requireField "newlineBeforeContent" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralBoolean v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected boolean literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_newlineBeforeContent -> Eithers.bind (ExtractCore.requireField "newlineAfterContent" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralBoolean v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected boolean literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_newlineAfterContent -> Right (Ast.BlockStyle {
          Ast.blockStyleIndent = field_indent,
          Ast.blockStyleNewlineBeforeContent = field_newlineBeforeContent,
          Ast.blockStyleNewlineAfterContent = field_newlineAfterContent})))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
bracketExpr :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Ast.BracketExpr
bracketExpr cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "brackets" brackets fieldMap cx) (\field_brackets -> Eithers.bind (ExtractCore.requireField "enclosed" expr fieldMap cx) (\field_enclosed -> Eithers.bind (ExtractCore.requireField "style" blockStyle fieldMap cx) (\field_style -> Right (Ast.BracketExpr {
          Ast.bracketExprBrackets = field_brackets,
          Ast.bracketExprEnclosed = field_enclosed,
          Ast.bracketExprStyle = field_style})))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
brackets :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Ast.Brackets
brackets cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "open" symbol fieldMap cx) (\field_open -> Eithers.bind (ExtractCore.requireField "close" symbol fieldMap cx) (\field_close -> Right (Ast.Brackets {
          Ast.bracketsOpen = field_open,
          Ast.bracketsClose = field_close}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
expr :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Ast.Expr
expr cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "const", (\input -> Eithers.map (\t -> Ast.ExprConst t) (symbol cx input))),
                      (Core.Name "indent", (\input -> Eithers.map (\t -> Ast.ExprIndent t) (indentedExpression cx input))),
                      (Core.Name "op", (\input -> Eithers.map (\t -> Ast.ExprOp t) (opExpr cx input))),
                      (Core.Name "brackets", (\input -> Eithers.map (\t -> Ast.ExprBrackets t) (bracketExpr cx input))),
                      (Core.Name "seq", (\input -> Eithers.map (\t -> Ast.ExprSeq t) (seqExpr cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
indentStyle :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Ast.IndentStyle
indentStyle cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "allLines", (\input -> Eithers.map (\t -> Ast.IndentStyleAllLines t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralString v2 -> Right v2
                          _ -> Left (Errors.DecodingError "expected string literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "subsequentLines", (\input -> Eithers.map (\t -> Ast.IndentStyleSubsequentLines t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralString v2 -> Right v2
                          _ -> Left (Errors.DecodingError "expected string literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input))))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
indentedExpression :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Ast.IndentedExpression
indentedExpression cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "style" indentStyle fieldMap cx) (\field_style -> Eithers.bind (ExtractCore.requireField "expr" expr fieldMap cx) (\field_expr -> Right (Ast.IndentedExpression {
          Ast.indentedExpressionStyle = field_style,
          Ast.indentedExpressionExpr = field_expr}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
op :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Ast.Op
op cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "symbol" symbol fieldMap cx) (\field_symbol -> Eithers.bind (ExtractCore.requireField "padding" padding fieldMap cx) (\field_padding -> Eithers.bind (ExtractCore.requireField "precedence" precedence fieldMap cx) (\field_precedence -> Eithers.bind (ExtractCore.requireField "associativity" associativity fieldMap cx) (\field_associativity -> Right (Ast.Op {
          Ast.opSymbol = field_symbol,
          Ast.opPadding = field_padding,
          Ast.opPrecedence = field_precedence,
          Ast.opAssociativity = field_associativity}))))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
opExpr :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Ast.OpExpr
opExpr cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "op" op fieldMap cx) (\field_op -> Eithers.bind (ExtractCore.requireField "lhs" expr fieldMap cx) (\field_lhs -> Eithers.bind (ExtractCore.requireField "rhs" expr fieldMap cx) (\field_rhs -> Right (Ast.OpExpr {
          Ast.opExprOp = field_op,
          Ast.opExprLhs = field_lhs,
          Ast.opExprRhs = field_rhs})))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
padding :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Ast.Padding
padding cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "left" ws fieldMap cx) (\field_left -> Eithers.bind (ExtractCore.requireField "right" ws fieldMap cx) (\field_right -> Right (Ast.Padding {
          Ast.paddingLeft = field_left,
          Ast.paddingRight = field_right}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
precedence :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Ast.Precedence
precedence cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Ast.Precedence b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralInteger v2 -> case v2 of
            Core.IntegerValueInt32 v3 -> Right v3
            _ -> Left (Errors.DecodingError "expected int32 value")
          _ -> Left (Errors.DecodingError "expected int32 literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
seqExpr :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Ast.SeqExpr
seqExpr cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "op" op fieldMap cx) (\field_op -> Eithers.bind (ExtractCore.requireField "elements" (ExtractCore.decodeList expr) fieldMap cx) (\field_elements -> Right (Ast.SeqExpr {
          Ast.seqExprOp = field_op,
          Ast.seqExprElements = field_elements}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
symbol :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Ast.Symbol
symbol cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Ast.Symbol b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
ws :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Ast.Ws
ws cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "none", (\input -> Eithers.map (\t -> Ast.WsNone) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "space", (\input -> Eithers.map (\t -> Ast.WsSpace) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "break", (\input -> Eithers.map (\t -> Ast.WsBreak) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "breakAndIndent", (\input -> Eithers.map (\t -> Ast.WsBreakAndIndent t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralString v2 -> Right v2
                          _ -> Left (Errors.DecodingError "expected string literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "doubleBreak", (\input -> Eithers.map (\t -> Ast.WsDoubleBreak) (ExtractCore.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
