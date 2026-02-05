// Note: this is an automatically generated file. Do not edit.

package hydra.encode.ast;

/**
 * Term encoders for hydra.ast
 */
public interface Ast {
  static hydra.core.Term associativity(hydra.ast.Associativity v1) {
    return ((v1)).accept(new hydra.ast.Associativity.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.ast.Associativity.None y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Associativity"), new hydra.core.Field(new hydra.core.Name("none"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.ast.Associativity.Left y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Associativity"), new hydra.core.Field(new hydra.core.Name("left"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.ast.Associativity.Right y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Associativity"), new hydra.core.Field(new hydra.core.Name("right"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.ast.Associativity.Both y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Associativity"), new hydra.core.Field(new hydra.core.Name("both"), new hydra.core.Term.Unit())));
      }
    });
  }
  
  static hydra.core.Term blockStyle(hydra.ast.BlockStyle x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.BlockStyle"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("indent"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
        (java.util.function.Function<String, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.String_((x2)))),
        ((x)).indent))),
      new hydra.core.Field(new hydra.core.Name("newlineBeforeContent"), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(((x)).newlineBeforeContent))),
      new hydra.core.Field(new hydra.core.Name("newlineAfterContent"), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(((x)).newlineAfterContent))))));
  }
  
  static hydra.core.Term bracketExpr(hydra.ast.BracketExpr x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.BracketExpr"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("brackets"), hydra.encode.ast.Ast.brackets(((x)).brackets)),
      new hydra.core.Field(new hydra.core.Name("enclosed"), hydra.encode.ast.Ast.expr(((x)).enclosed)),
      new hydra.core.Field(new hydra.core.Name("style"), hydra.encode.ast.Ast.blockStyle(((x)).style)))));
  }
  
  static hydra.core.Term brackets(hydra.ast.Brackets x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.Brackets"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("open"), hydra.encode.ast.Ast.symbol(((x)).open)),
      new hydra.core.Field(new hydra.core.Name("close"), hydra.encode.ast.Ast.symbol(((x)).close)))));
  }
  
  static hydra.core.Term expr(hydra.ast.Expr v1) {
    return ((v1)).accept(new hydra.ast.Expr.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.ast.Expr.Const y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Expr"), new hydra.core.Field(new hydra.core.Name("const"), hydra.encode.ast.Ast.symbol(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.ast.Expr.Indent y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Expr"), new hydra.core.Field(new hydra.core.Name("indent"), hydra.encode.ast.Ast.indentedExpression(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.ast.Expr.Op y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Expr"), new hydra.core.Field(new hydra.core.Name("op"), hydra.encode.ast.Ast.opExpr(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.ast.Expr.Brackets y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Expr"), new hydra.core.Field(new hydra.core.Name("brackets"), hydra.encode.ast.Ast.bracketExpr(((y)).value))));
      }
    });
  }
  
  static hydra.core.Term indentedExpression(hydra.ast.IndentedExpression x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.IndentedExpression"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("style"), hydra.encode.ast.Ast.indentStyle(((x)).style)),
      new hydra.core.Field(new hydra.core.Name("expr"), hydra.encode.ast.Ast.expr(((x)).expr)))));
  }
  
  static hydra.core.Term indentStyle(hydra.ast.IndentStyle v1) {
    return ((v1)).accept(new hydra.ast.IndentStyle.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.ast.IndentStyle.AllLines y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.IndentStyle"), new hydra.core.Field(new hydra.core.Name("allLines"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((y)).value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.ast.IndentStyle.SubsequentLines y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.IndentStyle"), new hydra.core.Field(new hydra.core.Name("subsequentLines"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((y)).value)))));
      }
    });
  }
  
  static hydra.core.Term op(hydra.ast.Op x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.Op"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("symbol"), hydra.encode.ast.Ast.symbol(((x)).symbol)),
      new hydra.core.Field(new hydra.core.Name("padding"), hydra.encode.ast.Ast.padding(((x)).padding)),
      new hydra.core.Field(new hydra.core.Name("precedence"), hydra.encode.ast.Ast.precedence(((x)).precedence)),
      new hydra.core.Field(new hydra.core.Name("associativity"), hydra.encode.ast.Ast.associativity(((x)).associativity)))));
  }
  
  static hydra.core.Term opExpr(hydra.ast.OpExpr x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.OpExpr"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("op"), hydra.encode.ast.Ast.op(((x)).op)),
      new hydra.core.Field(new hydra.core.Name("lhs"), hydra.encode.ast.Ast.expr(((x)).lhs)),
      new hydra.core.Field(new hydra.core.Name("rhs"), hydra.encode.ast.Ast.expr(((x)).rhs)))));
  }
  
  static hydra.core.Term padding(hydra.ast.Padding x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.ast.Padding"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("left"), hydra.encode.ast.Ast.ws(((x)).left)),
      new hydra.core.Field(new hydra.core.Name("right"), hydra.encode.ast.Ast.ws(((x)).right)))));
  }
  
  static hydra.core.Term precedence(hydra.ast.Precedence x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.ast.Precedence"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(((x)).value)))));
  }
  
  static hydra.core.Term symbol(hydra.ast.Symbol x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.ast.Symbol"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((x)).value))));
  }
  
  static hydra.core.Term ws(hydra.ast.Ws v1) {
    return ((v1)).accept(new hydra.ast.Ws.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.ast.Ws.None y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Ws"), new hydra.core.Field(new hydra.core.Name("none"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.ast.Ws.Space y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Ws"), new hydra.core.Field(new hydra.core.Name("space"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.ast.Ws.Break y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Ws"), new hydra.core.Field(new hydra.core.Name("break"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.ast.Ws.BreakAndIndent y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Ws"), new hydra.core.Field(new hydra.core.Name("breakAndIndent"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((y)).value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.ast.Ws.DoubleBreak y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.ast.Ws"), new hydra.core.Field(new hydra.core.Name("doubleBreak"), new hydra.core.Term.Unit())));
      }
    });
  }
}
