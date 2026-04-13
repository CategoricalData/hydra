// Note: this is an automatically generated file. Do not edit.

package hydra.dsl;

/**
 * DSL functions for hydra.ast
 */
public interface Ast {
  static hydra.phantoms.TTerm<hydra.ast.Associativity> associativityBoth() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Associativity"), new hydra.core.Field(new hydra.core.Name("both"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Associativity> associativityLeft() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Associativity"), new hydra.core.Field(new hydra.core.Name("left"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Associativity> associativityNone() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Associativity"), new hydra.core.Field(new hydra.core.Name("none"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Associativity> associativityRight() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Associativity"), new hydra.core.Field(new hydra.core.Name("right"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.ast.BlockStyle> blockStyle(hydra.phantoms.TTerm<hydra.util.Maybe<String>> indent, hydra.phantoms.TTerm<Boolean> newlineBeforeContent, hydra.phantoms.TTerm<Boolean> newlineAfterContent) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.BlockStyle"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("indent"), (indent).value),
      new hydra.core.Field(new hydra.core.Name("newlineBeforeContent"), (newlineBeforeContent).value),
      new hydra.core.Field(new hydra.core.Name("newlineAfterContent"), (newlineAfterContent).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<String>> blockStyleIndent(hydra.phantoms.TTerm<hydra.ast.BlockStyle> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.BlockStyle"), new hydra.core.Name("indent"))), (x).value)));
  }

  static hydra.phantoms.TTerm<Boolean> blockStyleNewlineAfterContent(hydra.phantoms.TTerm<hydra.ast.BlockStyle> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.BlockStyle"), new hydra.core.Name("newlineAfterContent"))), (x).value)));
  }

  static hydra.phantoms.TTerm<Boolean> blockStyleNewlineBeforeContent(hydra.phantoms.TTerm<hydra.ast.BlockStyle> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.BlockStyle"), new hydra.core.Name("newlineBeforeContent"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.ast.BlockStyle> blockStyleWithIndent(hydra.phantoms.TTerm<hydra.ast.BlockStyle> original, hydra.phantoms.TTerm<hydra.util.Maybe<String>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.BlockStyle"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("indent"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("newlineBeforeContent"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.BlockStyle"), new hydra.core.Name("newlineBeforeContent"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("newlineAfterContent"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.BlockStyle"), new hydra.core.Name("newlineAfterContent"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.ast.BlockStyle> blockStyleWithNewlineAfterContent(hydra.phantoms.TTerm<hydra.ast.BlockStyle> original, hydra.phantoms.TTerm<Boolean> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.BlockStyle"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("indent"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.BlockStyle"), new hydra.core.Name("indent"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("newlineBeforeContent"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.BlockStyle"), new hydra.core.Name("newlineBeforeContent"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("newlineAfterContent"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.ast.BlockStyle> blockStyleWithNewlineBeforeContent(hydra.phantoms.TTerm<hydra.ast.BlockStyle> original, hydra.phantoms.TTerm<Boolean> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.BlockStyle"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("indent"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.BlockStyle"), new hydra.core.Name("indent"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("newlineBeforeContent"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("newlineAfterContent"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.BlockStyle"), new hydra.core.Name("newlineAfterContent"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.ast.BracketExpr> bracketExpr(hydra.phantoms.TTerm<hydra.ast.Brackets> brackets, hydra.phantoms.TTerm<hydra.ast.Expr> enclosed, hydra.phantoms.TTerm<hydra.ast.BlockStyle> style) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.BracketExpr"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("brackets"), (brackets).value),
      new hydra.core.Field(new hydra.core.Name("enclosed"), (enclosed).value),
      new hydra.core.Field(new hydra.core.Name("style"), (style).value)))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Brackets> bracketExprBrackets(hydra.phantoms.TTerm<hydra.ast.BracketExpr> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.BracketExpr"), new hydra.core.Name("brackets"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.ast.Expr> bracketExprEnclosed(hydra.phantoms.TTerm<hydra.ast.BracketExpr> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.BracketExpr"), new hydra.core.Name("enclosed"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.ast.BlockStyle> bracketExprStyle(hydra.phantoms.TTerm<hydra.ast.BracketExpr> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.BracketExpr"), new hydra.core.Name("style"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.ast.BracketExpr> bracketExprWithBrackets(hydra.phantoms.TTerm<hydra.ast.BracketExpr> original, hydra.phantoms.TTerm<hydra.ast.Brackets> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.BracketExpr"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("brackets"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("enclosed"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.BracketExpr"), new hydra.core.Name("enclosed"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("style"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.BracketExpr"), new hydra.core.Name("style"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.ast.BracketExpr> bracketExprWithEnclosed(hydra.phantoms.TTerm<hydra.ast.BracketExpr> original, hydra.phantoms.TTerm<hydra.ast.Expr> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.BracketExpr"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("brackets"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.BracketExpr"), new hydra.core.Name("brackets"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("enclosed"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("style"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.BracketExpr"), new hydra.core.Name("style"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.ast.BracketExpr> bracketExprWithStyle(hydra.phantoms.TTerm<hydra.ast.BracketExpr> original, hydra.phantoms.TTerm<hydra.ast.BlockStyle> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.BracketExpr"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("brackets"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.BracketExpr"), new hydra.core.Name("brackets"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("enclosed"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.BracketExpr"), new hydra.core.Name("enclosed"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("style"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Brackets> brackets(hydra.phantoms.TTerm<hydra.ast.Symbol> open, hydra.phantoms.TTerm<hydra.ast.Symbol> close) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.Brackets"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("open"), (open).value),
      new hydra.core.Field(new hydra.core.Name("close"), (close).value)))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Symbol> bracketsClose(hydra.phantoms.TTerm<hydra.ast.Brackets> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Brackets"), new hydra.core.Name("close"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.ast.Symbol> bracketsOpen(hydra.phantoms.TTerm<hydra.ast.Brackets> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Brackets"), new hydra.core.Name("open"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.ast.Brackets> bracketsWithClose(hydra.phantoms.TTerm<hydra.ast.Brackets> original, hydra.phantoms.TTerm<hydra.ast.Symbol> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.Brackets"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("open"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Brackets"), new hydra.core.Name("open"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("close"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Brackets> bracketsWithOpen(hydra.phantoms.TTerm<hydra.ast.Brackets> original, hydra.phantoms.TTerm<hydra.ast.Symbol> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.Brackets"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("open"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("close"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Brackets"), new hydra.core.Name("close"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Expr> exprBrackets(hydra.phantoms.TTerm<hydra.ast.BracketExpr> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Expr"), new hydra.core.Field(new hydra.core.Name("brackets"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Expr> exprConst(hydra.phantoms.TTerm<hydra.ast.Symbol> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Expr"), new hydra.core.Field(new hydra.core.Name("const"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Expr> exprIndent(hydra.phantoms.TTerm<hydra.ast.IndentedExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Expr"), new hydra.core.Field(new hydra.core.Name("indent"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Expr> exprOp(hydra.phantoms.TTerm<hydra.ast.OpExpr> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Expr"), new hydra.core.Field(new hydra.core.Name("op"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Expr> exprSeq(hydra.phantoms.TTerm<hydra.ast.SeqExpr> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Expr"), new hydra.core.Field(new hydra.core.Name("seq"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.ast.IndentStyle> indentStyleAllLines(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.IndentStyle"), new hydra.core.Field(new hydra.core.Name("allLines"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.ast.IndentStyle> indentStyleSubsequentLines(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.IndentStyle"), new hydra.core.Field(new hydra.core.Name("subsequentLines"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.ast.IndentedExpression> indentedExpression(hydra.phantoms.TTerm<hydra.ast.IndentStyle> style, hydra.phantoms.TTerm<hydra.ast.Expr> expr) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.IndentedExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("style"), (style).value),
      new hydra.core.Field(new hydra.core.Name("expr"), (expr).value)))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Expr> indentedExpressionExpr(hydra.phantoms.TTerm<hydra.ast.IndentedExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.IndentedExpression"), new hydra.core.Name("expr"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.ast.IndentStyle> indentedExpressionStyle(hydra.phantoms.TTerm<hydra.ast.IndentedExpression> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.IndentedExpression"), new hydra.core.Name("style"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.ast.IndentedExpression> indentedExpressionWithExpr(hydra.phantoms.TTerm<hydra.ast.IndentedExpression> original, hydra.phantoms.TTerm<hydra.ast.Expr> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.IndentedExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("style"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.IndentedExpression"), new hydra.core.Name("style"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("expr"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.ast.IndentedExpression> indentedExpressionWithStyle(hydra.phantoms.TTerm<hydra.ast.IndentedExpression> original, hydra.phantoms.TTerm<hydra.ast.IndentStyle> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.IndentedExpression"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("style"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("expr"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.IndentedExpression"), new hydra.core.Name("expr"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Op> op(hydra.phantoms.TTerm<hydra.ast.Symbol> symbol, hydra.phantoms.TTerm<hydra.ast.Padding> padding, hydra.phantoms.TTerm<hydra.ast.Precedence> precedence, hydra.phantoms.TTerm<hydra.ast.Associativity> associativity) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.Op"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("symbol"), (symbol).value),
      new hydra.core.Field(new hydra.core.Name("padding"), (padding).value),
      new hydra.core.Field(new hydra.core.Name("precedence"), (precedence).value),
      new hydra.core.Field(new hydra.core.Name("associativity"), (associativity).value)))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Associativity> opAssociativity(hydra.phantoms.TTerm<hydra.ast.Op> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Op"), new hydra.core.Name("associativity"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.ast.OpExpr> opExpr(hydra.phantoms.TTerm<hydra.ast.Op> op, hydra.phantoms.TTerm<hydra.ast.Expr> lhs, hydra.phantoms.TTerm<hydra.ast.Expr> rhs) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.OpExpr"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("op"), (op).value),
      new hydra.core.Field(new hydra.core.Name("lhs"), (lhs).value),
      new hydra.core.Field(new hydra.core.Name("rhs"), (rhs).value)))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Expr> opExprLhs(hydra.phantoms.TTerm<hydra.ast.OpExpr> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.OpExpr"), new hydra.core.Name("lhs"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.ast.Op> opExprOp(hydra.phantoms.TTerm<hydra.ast.OpExpr> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.OpExpr"), new hydra.core.Name("op"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.ast.Expr> opExprRhs(hydra.phantoms.TTerm<hydra.ast.OpExpr> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.OpExpr"), new hydra.core.Name("rhs"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.ast.OpExpr> opExprWithLhs(hydra.phantoms.TTerm<hydra.ast.OpExpr> original, hydra.phantoms.TTerm<hydra.ast.Expr> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.OpExpr"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("op"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.OpExpr"), new hydra.core.Name("op"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("lhs"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("rhs"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.OpExpr"), new hydra.core.Name("rhs"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.ast.OpExpr> opExprWithOp(hydra.phantoms.TTerm<hydra.ast.OpExpr> original, hydra.phantoms.TTerm<hydra.ast.Op> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.OpExpr"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("op"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("lhs"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.OpExpr"), new hydra.core.Name("lhs"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("rhs"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.OpExpr"), new hydra.core.Name("rhs"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.ast.OpExpr> opExprWithRhs(hydra.phantoms.TTerm<hydra.ast.OpExpr> original, hydra.phantoms.TTerm<hydra.ast.Expr> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.OpExpr"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("op"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.OpExpr"), new hydra.core.Name("op"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("lhs"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.OpExpr"), new hydra.core.Name("lhs"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("rhs"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Padding> opPadding(hydra.phantoms.TTerm<hydra.ast.Op> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Op"), new hydra.core.Name("padding"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.ast.Precedence> opPrecedence(hydra.phantoms.TTerm<hydra.ast.Op> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Op"), new hydra.core.Name("precedence"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.ast.Symbol> opSymbol(hydra.phantoms.TTerm<hydra.ast.Op> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Op"), new hydra.core.Name("symbol"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.ast.Op> opWithAssociativity(hydra.phantoms.TTerm<hydra.ast.Op> original, hydra.phantoms.TTerm<hydra.ast.Associativity> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.Op"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("symbol"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Op"), new hydra.core.Name("symbol"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("padding"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Op"), new hydra.core.Name("padding"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("precedence"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Op"), new hydra.core.Name("precedence"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("associativity"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Op> opWithPadding(hydra.phantoms.TTerm<hydra.ast.Op> original, hydra.phantoms.TTerm<hydra.ast.Padding> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.Op"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("symbol"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Op"), new hydra.core.Name("symbol"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("padding"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("precedence"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Op"), new hydra.core.Name("precedence"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("associativity"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Op"), new hydra.core.Name("associativity"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Op> opWithPrecedence(hydra.phantoms.TTerm<hydra.ast.Op> original, hydra.phantoms.TTerm<hydra.ast.Precedence> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.Op"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("symbol"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Op"), new hydra.core.Name("symbol"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("padding"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Op"), new hydra.core.Name("padding"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("precedence"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("associativity"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Op"), new hydra.core.Name("associativity"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Op> opWithSymbol(hydra.phantoms.TTerm<hydra.ast.Op> original, hydra.phantoms.TTerm<hydra.ast.Symbol> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.Op"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("symbol"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("padding"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Op"), new hydra.core.Name("padding"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("precedence"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Op"), new hydra.core.Name("precedence"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("associativity"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Op"), new hydra.core.Name("associativity"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Padding> padding(hydra.phantoms.TTerm<hydra.ast.Ws> left, hydra.phantoms.TTerm<hydra.ast.Ws> right) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.Padding"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("left"), (left).value),
      new hydra.core.Field(new hydra.core.Name("right"), (right).value)))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Ws> paddingLeft(hydra.phantoms.TTerm<hydra.ast.Padding> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Padding"), new hydra.core.Name("left"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.ast.Ws> paddingRight(hydra.phantoms.TTerm<hydra.ast.Padding> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Padding"), new hydra.core.Name("right"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.ast.Padding> paddingWithLeft(hydra.phantoms.TTerm<hydra.ast.Padding> original, hydra.phantoms.TTerm<hydra.ast.Ws> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.Padding"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("left"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("right"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Padding"), new hydra.core.Name("right"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Padding> paddingWithRight(hydra.phantoms.TTerm<hydra.ast.Padding> original, hydra.phantoms.TTerm<hydra.ast.Ws> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.Padding"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("left"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.Padding"), new hydra.core.Name("left"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("right"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Precedence> precedence(hydra.phantoms.TTerm<Integer> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.ast.Precedence"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.ast.SeqExpr> seqExpr(hydra.phantoms.TTerm<hydra.ast.Op> op, hydra.phantoms.TTerm<java.util.List<hydra.ast.Expr>> elements) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.SeqExpr"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("op"), (op).value),
      new hydra.core.Field(new hydra.core.Name("elements"), (elements).value)))));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.ast.Expr>> seqExprElements(hydra.phantoms.TTerm<hydra.ast.SeqExpr> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.SeqExpr"), new hydra.core.Name("elements"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.ast.Op> seqExprOp(hydra.phantoms.TTerm<hydra.ast.SeqExpr> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.SeqExpr"), new hydra.core.Name("op"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.ast.SeqExpr> seqExprWithElements(hydra.phantoms.TTerm<hydra.ast.SeqExpr> original, hydra.phantoms.TTerm<java.util.List<hydra.ast.Expr>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.SeqExpr"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("op"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.SeqExpr"), new hydra.core.Name("op"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("elements"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.ast.SeqExpr> seqExprWithOp(hydra.phantoms.TTerm<hydra.ast.SeqExpr> original, hydra.phantoms.TTerm<hydra.ast.Op> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.SeqExpr"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("op"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("elements"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.ast.SeqExpr"), new hydra.core.Name("elements"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Symbol> symbol(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.ast.Symbol"), (x).value)));
  }

  static hydra.phantoms.TTerm<Integer> unPrecedence(hydra.phantoms.TTerm<hydra.ast.Precedence> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.ast.Precedence")), (x).value)));
  }

  static hydra.phantoms.TTerm<String> unSymbol(hydra.phantoms.TTerm<hydra.ast.Symbol> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.ast.Symbol")), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.ast.Ws> wsBreak() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Ws"), new hydra.core.Field(new hydra.core.Name("break"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Ws> wsBreakAndIndent(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Ws"), new hydra.core.Field(new hydra.core.Name("breakAndIndent"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Ws> wsDoubleBreak() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Ws"), new hydra.core.Field(new hydra.core.Name("doubleBreak"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Ws> wsNone() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Ws"), new hydra.core.Field(new hydra.core.Name("none"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.ast.Ws> wsSpace() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Ws"), new hydra.core.Field(new hydra.core.Name("space"), new hydra.core.Term.Unit()))));
  }
}
