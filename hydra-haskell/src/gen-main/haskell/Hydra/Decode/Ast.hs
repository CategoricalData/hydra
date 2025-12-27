-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.ast

module Hydra.Decode.Ast where

import qualified Hydra.Ast as Ast
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

associativity :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Ast.Associativity)
associativity cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "none", (\input -> Eithers.map (\t -> Ast.AssociativityNone) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "left", (\input -> Eithers.map (\t -> Ast.AssociativityLeft) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "right", (\input -> Eithers.map (\t -> Ast.AssociativityRight) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "both", (\input -> Eithers.map (\t -> Ast.AssociativityBoth) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input))))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.ast.Associativity"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

blockStyle :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Ast.BlockStyle)
blockStyle cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_indent -> Eithers.either (\err -> Left err) (\field_newlineBeforeContent -> Eithers.either (\err -> Left err) (\field_newlineAfterContent -> Right (Ast.BlockStyle {
      Ast.blockStyleIndent = field_indent,
      Ast.blockStyleNewlineBeforeContent = field_newlineBeforeContent,
      Ast.blockStyleNewlineAfterContent = field_newlineAfterContent})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "newlineAfterContent",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralBoolean v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected boolean literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "newlineAfterContent") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "newlineBeforeContent",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralBoolean v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected boolean literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "newlineBeforeContent") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "indent",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMaybe v2 -> (Eithers.mapMaybe (\raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
        Core.TermLiteral v3 -> ((\x -> case x of
          Core.LiteralString v4 -> (Right v4)
          _ -> (Left (Util.DecodingError "expected string literal"))) v3)
        _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) v2)
      _ -> (Left (Util.DecodingError "expected optional value"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "indent") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.ast.BlockStyle"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

bracketExpr :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Ast.BracketExpr)
bracketExpr cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_brackets -> Eithers.either (\err -> Left err) (\field_enclosed -> Eithers.either (\err -> Left err) (\field_style -> Right (Ast.BracketExpr {
      Ast.bracketExprBrackets = field_brackets,
      Ast.bracketExprEnclosed = field_enclosed,
      Ast.bracketExprStyle = field_style})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "style",
      " in record"]))) (\fieldTerm -> blockStyle cx fieldTerm) (Maps.lookup (Core.Name "style") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "enclosed",
      " in record"]))) (\fieldTerm -> expr cx fieldTerm) (Maps.lookup (Core.Name "enclosed") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "brackets",
      " in record"]))) (\fieldTerm -> brackets cx fieldTerm) (Maps.lookup (Core.Name "brackets") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.ast.BracketExpr"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

brackets :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Ast.Brackets)
brackets cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_open -> Eithers.either (\err -> Left err) (\field_close -> Right (Ast.Brackets {
      Ast.bracketsOpen = field_open,
      Ast.bracketsClose = field_close})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "close",
      " in record"]))) (\fieldTerm -> symbol cx fieldTerm) (Maps.lookup (Core.Name "close") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "open",
      " in record"]))) (\fieldTerm -> symbol cx fieldTerm) (Maps.lookup (Core.Name "open") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.ast.Brackets"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

expr :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Ast.Expr)
expr cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "const", (\input -> Eithers.map (\t -> Ast.ExprConst t) (symbol cx input))),
                (Core.Name "indent", (\input -> Eithers.map (\t -> Ast.ExprIndent t) (indentedExpression cx input))),
                (Core.Name "op", (\input -> Eithers.map (\t -> Ast.ExprOp t) (opExpr cx input))),
                (Core.Name "brackets", (\input -> Eithers.map (\t -> Ast.ExprBrackets t) (bracketExpr cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.ast.Expr"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

indentedExpression :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Ast.IndentedExpression)
indentedExpression cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_style -> Eithers.either (\err -> Left err) (\field_expr -> Right (Ast.IndentedExpression {
      Ast.indentedExpressionStyle = field_style,
      Ast.indentedExpressionExpr = field_expr})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "expr",
      " in record"]))) (\fieldTerm -> expr cx fieldTerm) (Maps.lookup (Core.Name "expr") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "style",
      " in record"]))) (\fieldTerm -> indentStyle cx fieldTerm) (Maps.lookup (Core.Name "style") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.ast.IndentedExpression"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

indentStyle :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Ast.IndentStyle)
indentStyle cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "allLines", (\input -> Eithers.map (\t -> Ast.IndentStyleAllLines t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralString v3 -> (Right v3)
                    _ -> (Left (Util.DecodingError "expected string literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "subsequentLines", (\input -> Eithers.map (\t -> Ast.IndentStyleSubsequentLines t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralString v3 -> (Right v3)
                    _ -> (Left (Util.DecodingError "expected string literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input))))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.ast.IndentStyle"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

op :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Ast.Op)
op cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_symbol -> Eithers.either (\err -> Left err) (\field_padding -> Eithers.either (\err -> Left err) (\field_precedence -> Eithers.either (\err -> Left err) (\field_associativity -> Right (Ast.Op {
      Ast.opSymbol = field_symbol,
      Ast.opPadding = field_padding,
      Ast.opPrecedence = field_precedence,
      Ast.opAssociativity = field_associativity})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "associativity",
      " in record"]))) (\fieldTerm -> associativity cx fieldTerm) (Maps.lookup (Core.Name "associativity") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "precedence",
      " in record"]))) (\fieldTerm -> precedence cx fieldTerm) (Maps.lookup (Core.Name "precedence") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "padding",
      " in record"]))) (\fieldTerm -> padding cx fieldTerm) (Maps.lookup (Core.Name "padding") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "symbol",
      " in record"]))) (\fieldTerm -> symbol cx fieldTerm) (Maps.lookup (Core.Name "symbol") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.ast.Op"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

opExpr :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Ast.OpExpr)
opExpr cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_op -> Eithers.either (\err -> Left err) (\field_lhs -> Eithers.either (\err -> Left err) (\field_rhs -> Right (Ast.OpExpr {
      Ast.opExprOp = field_op,
      Ast.opExprLhs = field_lhs,
      Ast.opExprRhs = field_rhs})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "rhs",
      " in record"]))) (\fieldTerm -> expr cx fieldTerm) (Maps.lookup (Core.Name "rhs") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "lhs",
      " in record"]))) (\fieldTerm -> expr cx fieldTerm) (Maps.lookup (Core.Name "lhs") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "op",
      " in record"]))) (\fieldTerm -> op cx fieldTerm) (Maps.lookup (Core.Name "op") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.ast.OpExpr"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

padding :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Ast.Padding)
padding cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_left -> Eithers.either (\err -> Left err) (\field_right -> Right (Ast.Padding {
      Ast.paddingLeft = field_left,
      Ast.paddingRight = field_right})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "right",
      " in record"]))) (\fieldTerm -> ws cx fieldTerm) (Maps.lookup (Core.Name "right") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "left",
      " in record"]))) (\fieldTerm -> ws cx fieldTerm) (Maps.lookup (Core.Name "left") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.ast.Padding"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

precedence :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Ast.Precedence)
precedence cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermWrap v1 -> (Eithers.map (\b -> Ast.Precedence b) ((\raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
    Core.TermLiteral v2 -> ((\x -> case x of
      Core.LiteralInteger v3 -> ((\x -> case x of
        Core.IntegerValueInt32 v4 -> (Right v4)
        _ -> (Left (Util.DecodingError "expected int32 value"))) v3)
      _ -> (Left (Util.DecodingError "expected int32 literal"))) v2)
    _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) (Core.wrappedTermBody v1)))
  _ -> (Left (Util.DecodingError "expected wrapped type hydra.ast.Precedence"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

symbol :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Ast.Symbol)
symbol cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermWrap v1 -> (Eithers.map (\b -> Ast.Symbol b) ((\raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
    Core.TermLiteral v2 -> ((\x -> case x of
      Core.LiteralString v3 -> (Right v3)
      _ -> (Left (Util.DecodingError "expected string literal"))) v2)
    _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) (Core.wrappedTermBody v1)))
  _ -> (Left (Util.DecodingError "expected wrapped type hydra.ast.Symbol"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

ws :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Ast.Ws)
ws cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "none", (\input -> Eithers.map (\t -> Ast.WsNone) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "space", (\input -> Eithers.map (\t -> Ast.WsSpace) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "break", (\input -> Eithers.map (\t -> Ast.WsBreak) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "breakAndIndent", (\input -> Eithers.map (\t -> Ast.WsBreakAndIndent t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralString v3 -> (Right v3)
                    _ -> (Left (Util.DecodingError "expected string literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "doubleBreak", (\input -> Eithers.map (\t -> Ast.WsDoubleBreak) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input))))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.ast.Ws"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
