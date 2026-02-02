// Note: this is an automatically generated file. Do not edit.

package hydra.serialization;

/**
 * Utilities for constructing generic program code ASTs, used for the serialization phase of source code generation.
 */
public interface Serialization {
  hydra.ast.Brackets angleBraces = new hydra.ast.Brackets(hydra.serialization.Serialization.sym("<"), hydra.serialization.Serialization.sym(">"));
  
  static hydra.ast.Expr angleBracesList(hydra.ast.BlockStyle style, java.util.List<hydra.ast.Expr> els) {
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.lists.Null.apply((els)),
      hydra.serialization.Serialization.cst("<>"),
      hydra.serialization.Serialization.brackets(
        (hydra.serialization.Serialization.angleBraces),
        (style),
        hydra.serialization.Serialization.commaSep(
          (style),
          (els))));
  }
  
  static hydra.ast.Expr bracesListAdaptive(java.util.List<hydra.ast.Expr> els) {
    hydra.ast.Expr inlineList = hydra.serialization.Serialization.curlyBracesList(
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
      (hydra.serialization.Serialization.inlineStyle),
      (els));
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.equality.Gt.apply(
        hydra.serialization.Serialization.expressionLength((inlineList)),
        70),
      hydra.serialization.Serialization.curlyBracesList(
        (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
        (hydra.serialization.Serialization.halfBlockStyle),
        (els)),
      (inlineList));
  }
  
  static hydra.ast.Expr bracketList(hydra.ast.BlockStyle style, java.util.List<hydra.ast.Expr> els) {
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.lists.Null.apply((els)),
      hydra.serialization.Serialization.cst("[]"),
      hydra.serialization.Serialization.brackets(
        (hydra.serialization.Serialization.squareBrackets),
        (style),
        hydra.serialization.Serialization.commaSep(
          (style),
          (els))));
  }
  
  static hydra.ast.Expr bracketListAdaptive(java.util.List<hydra.ast.Expr> els) {
    hydra.ast.Expr inlineList = hydra.serialization.Serialization.bracketList(
      (hydra.serialization.Serialization.inlineStyle),
      (els));
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.equality.Gt.apply(
        hydra.serialization.Serialization.expressionLength((inlineList)),
        70),
      hydra.serialization.Serialization.bracketList(
        (hydra.serialization.Serialization.halfBlockStyle),
        (els)),
      (inlineList));
  }
  
  static hydra.ast.Expr brackets(hydra.ast.Brackets br, hydra.ast.BlockStyle style, hydra.ast.Expr e) {
    return new hydra.ast.Expr.Brackets(new hydra.ast.BracketExpr((br), (e), (style)));
  }
  
  static hydra.ast.Expr commaSep(hydra.ast.BlockStyle v1, java.util.List<hydra.ast.Expr> v2) {
    return hydra.serialization.Serialization.symbolSep(
      ",",
      (v1),
      (v2));
  }
  
  static hydra.ast.Expr curlyBlock(hydra.ast.BlockStyle style, hydra.ast.Expr e) {
    return hydra.serialization.Serialization.curlyBracesList(
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
      (style),
      java.util.List.of((e)));
  }
  
  hydra.ast.Brackets curlyBraces = new hydra.ast.Brackets(hydra.serialization.Serialization.sym("{"), hydra.serialization.Serialization.sym("}"));
  
  static hydra.ast.Expr curlyBracesList(hydra.util.Maybe<String> msymb, hydra.ast.BlockStyle style, java.util.List<hydra.ast.Expr> els) {
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.lists.Null.apply((els)),
      hydra.serialization.Serialization.cst("{}"),
      hydra.serialization.Serialization.brackets(
        (hydra.serialization.Serialization.curlyBraces),
        (style),
        hydra.serialization.Serialization.symbolSep(
          hydra.lib.maybes.FromMaybe.apply(
            ",",
            (msymb)),
          (style),
          (els))));
  }
  
  static hydra.ast.Expr cst(String s) {
    return new hydra.ast.Expr.Const(hydra.serialization.Serialization.sym((s)));
  }
  
  static String customIndent(String idt, String s) {
    return hydra.lib.strings.Cat.apply(hydra.lib.lists.Intersperse.apply(
      "\n",
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<String, String>) (line -> hydra.lib.strings.Cat2.apply(
          (idt),
          (line))),
        hydra.lib.strings.Lines.apply((s)))));
  }
  
  static hydra.ast.Expr customIndentBlock(String idt, java.util.List<hydra.ast.Expr> els) {
    hydra.ast.Expr head = hydra.lib.lists.Head.apply((els));
    hydra.ast.Op idtOp = new hydra.ast.Op(hydra.serialization.Serialization.sym(""), new hydra.ast.Padding(new hydra.ast.Ws.Space(true), new hydra.ast.Ws.BreakAndIndent((idt))), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None(true));
    java.util.List<hydra.ast.Expr> rest = hydra.lib.lists.Tail.apply((els));
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.lists.Null.apply((els)),
      hydra.serialization.Serialization.cst(""),
      hydra.lib.logic.IfElse.apply(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply((els)),
          1),
        hydra.lib.lists.Head.apply((els)),
        hydra.serialization.Serialization.ifx(
          (idtOp),
          (head),
          hydra.serialization.Serialization.newlineSep((rest)))));
  }
  
  static hydra.ast.Expr dotSep(java.util.List<hydra.ast.Expr> v1) {
    return hydra.serialization.Serialization.sep(
      new hydra.ast.Op(hydra.serialization.Serialization.sym("."), new hydra.ast.Padding(new hydra.ast.Ws.None(true), new hydra.ast.Ws.None(true)), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None(true)),
      (v1));
  }
  
  static hydra.ast.Expr doubleNewlineSep(java.util.List<hydra.ast.Expr> v1) {
    return hydra.serialization.Serialization.sep(
      new hydra.ast.Op(hydra.serialization.Serialization.sym(""), new hydra.ast.Padding(new hydra.ast.Ws.Break(true), new hydra.ast.Ws.Break(true)), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None(true)),
      (v1));
  }
  
  String doubleSpace = "  ";
  
  static Integer expressionLength(hydra.ast.Expr e) {
    java.util.function.Function<hydra.ast.BlockStyle, Integer> blockStyleLength = (java.util.function.Function<hydra.ast.BlockStyle, Integer>) (style -> {
      Integer mindentLen = hydra.lib.maybes.Maybe.apply(
        0,
        (hydra.lib.strings.Length::apply),
        ((style)).indent);
      Integer nlAfterLen = hydra.lib.logic.IfElse.apply(
        ((style)).newlineAfterContent,
        1,
        0);
      Integer nlBeforeLen = hydra.lib.logic.IfElse.apply(
        ((style)).newlineBeforeContent,
        1,
        0);
      return hydra.lib.math.Add.apply(
        (mindentLen),
        hydra.lib.math.Add.apply(
          (nlBeforeLen),
          (nlAfterLen)));
    });
    java.util.function.Function<hydra.ast.Symbol, Integer> symbolLength = (java.util.function.Function<hydra.ast.Symbol, Integer>) (s -> hydra.lib.strings.Length.apply(((s)).value));
    java.util.function.Function<hydra.ast.Brackets, Integer> bracketsLength = (java.util.function.Function<hydra.ast.Brackets, Integer>) (brackets -> hydra.lib.math.Add.apply(
      ((symbolLength)).apply(((brackets)).open),
      ((symbolLength)).apply(((brackets)).close)));
    java.util.function.Function<hydra.ast.BracketExpr, Integer> bracketExprLength = (java.util.function.Function<hydra.ast.BracketExpr, Integer>) (be -> hydra.lib.math.Add.apply(
      ((bracketsLength)).apply(((be)).brackets),
      hydra.lib.math.Add.apply(
        hydra.serialization.Serialization.expressionLength(((be)).enclosed),
        ((blockStyleLength)).apply(((be)).style))));
    java.util.function.Function<hydra.ast.IndentedExpression, Integer> indentedExpressionLength = (java.util.function.Function<hydra.ast.IndentedExpression, Integer>) (ie -> {
      Integer baseLen = hydra.serialization.Serialization.expressionLength(((ie)).expr);
      Integer indentLen = (((ie)).style).accept(new hydra.ast.IndentStyle.PartialVisitor<>() {
        @Override
        public Integer visit(hydra.ast.IndentStyle.AllLines s) {
          return hydra.lib.strings.Length.apply(((s)).value);
        }
        
        @Override
        public Integer visit(hydra.ast.IndentStyle.SubsequentLines s) {
          return hydra.lib.strings.Length.apply(((s)).value);
        }
      });
      return hydra.lib.math.Add.apply(
        (baseLen),
        (indentLen));
    });
    java.util.function.Function<hydra.ast.Ws, Integer> wsLength = (java.util.function.Function<hydra.ast.Ws, Integer>) (ws -> ((ws)).accept(new hydra.ast.Ws.PartialVisitor<>() {
      @Override
      public Integer visit(hydra.ast.Ws.None ignored) {
        return 0;
      }
      
      @Override
      public Integer visit(hydra.ast.Ws.Space ignored) {
        return 1;
      }
      
      @Override
      public Integer visit(hydra.ast.Ws.Break ignored) {
        return 1;
      }
      
      @Override
      public Integer visit(hydra.ast.Ws.BreakAndIndent s) {
        return hydra.lib.math.Add.apply(
          1,
          hydra.lib.strings.Length.apply(((s)).value));
      }
      
      @Override
      public Integer visit(hydra.ast.Ws.DoubleBreak ignored) {
        return 2;
      }
    }));
    java.util.function.Function<hydra.ast.Op, Integer> opLength = (java.util.function.Function<hydra.ast.Op, Integer>) (op -> {
      hydra.ast.Padding padding = ((op)).padding;
      Integer leftLen = ((wsLength)).apply(((padding)).left);
      Integer rightLen = ((wsLength)).apply(((padding)).right);
      Integer symLen = ((symbolLength)).apply(((op)).symbol);
      return hydra.lib.math.Add.apply(
        (symLen),
        hydra.lib.math.Add.apply(
          (leftLen),
          (rightLen)));
    });
    java.util.function.Function<hydra.ast.OpExpr, Integer> opExprLength = (java.util.function.Function<hydra.ast.OpExpr, Integer>) (oe -> {
      Integer leftLen = hydra.serialization.Serialization.expressionLength(((oe)).lhs);
      Integer opLen = ((opLength)).apply(((oe)).op);
      Integer rightLen = hydra.serialization.Serialization.expressionLength(((oe)).rhs);
      return hydra.lib.math.Add.apply(
        (opLen),
        hydra.lib.math.Add.apply(
          (leftLen),
          (rightLen)));
    });
    return ((e)).accept(new hydra.ast.Expr.PartialVisitor<>() {
      @Override
      public Integer visit(hydra.ast.Expr.Const s) {
        return ((symbolLength)).apply(((s)).value);
      }
      
      @Override
      public Integer visit(hydra.ast.Expr.Indent ie) {
        return ((indentedExpressionLength)).apply(((ie)).value);
      }
      
      @Override
      public Integer visit(hydra.ast.Expr.Op oe) {
        return ((opExprLength)).apply(((oe)).value);
      }
      
      @Override
      public Integer visit(hydra.ast.Expr.Brackets be) {
        return ((bracketExprLength)).apply(((be)).value);
      }
    });
  }
  
  hydra.ast.BlockStyle fullBlockStyle = new hydra.ast.BlockStyle(hydra.util.Maybe.just((hydra.serialization.Serialization.doubleSpace)), true, true);
  
  hydra.ast.BlockStyle halfBlockStyle = new hydra.ast.BlockStyle(hydra.util.Maybe.just((hydra.serialization.Serialization.doubleSpace)), true, false);
  
  static hydra.ast.Expr ifx(hydra.ast.Op op, hydra.ast.Expr lhs, hydra.ast.Expr rhs) {
    return new hydra.ast.Expr.Op(new hydra.ast.OpExpr((op), (lhs), (rhs)));
  }
  
  static String indent(String v1) {
    return hydra.serialization.Serialization.customIndent(
      (hydra.serialization.Serialization.doubleSpace),
      (v1));
  }
  
  static hydra.ast.Expr indentBlock(java.util.List<hydra.ast.Expr> v1) {
    return hydra.serialization.Serialization.customIndentBlock(
      (hydra.serialization.Serialization.doubleSpace),
      (v1));
  }
  
  static hydra.ast.Expr indentSubsequentLines(String idt, hydra.ast.Expr e) {
    return new hydra.ast.Expr.Indent(new hydra.ast.IndentedExpression(new hydra.ast.IndentStyle.SubsequentLines((idt)), (e)));
  }
  
  static hydra.ast.Expr infixWs(String op, hydra.ast.Expr l, hydra.ast.Expr r) {
    return hydra.serialization.Serialization.spaceSep(java.util.List.of(
      (l),
      hydra.serialization.Serialization.cst((op)),
      (r)));
  }
  
  static hydra.ast.Expr infixWsList(String op, java.util.List<hydra.ast.Expr> opers) {
    hydra.ast.Expr opExpr = hydra.serialization.Serialization.cst((op));
    java.util.function.Function<java.util.List<hydra.ast.Expr>, java.util.function.Function<hydra.ast.Expr, java.util.List<hydra.ast.Expr>>> foldFun = (java.util.function.Function<java.util.List<hydra.ast.Expr>, java.util.function.Function<hydra.ast.Expr, java.util.List<hydra.ast.Expr>>>) (e -> (java.util.function.Function<hydra.ast.Expr, java.util.List<hydra.ast.Expr>>) (r -> hydra.lib.logic.IfElse.apply(
      hydra.lib.lists.Null.apply((e)),
      java.util.List.of((r)),
      hydra.lib.lists.Cons.apply(
        (r),
        hydra.lib.lists.Cons.apply(
          (opExpr),
          (e))))));
    return hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Foldl.apply(
      (foldFun),
      (java.util.List<hydra.ast.Expr>) (java.util.List.<hydra.ast.Expr>of()),
      hydra.lib.lists.Reverse.apply((opers))));
  }
  
  hydra.ast.BlockStyle inlineStyle = new hydra.ast.BlockStyle((hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), false, false);
  
  static hydra.ast.Expr newlineSep(java.util.List<hydra.ast.Expr> v1) {
    return hydra.serialization.Serialization.sep(
      new hydra.ast.Op(hydra.serialization.Serialization.sym(""), new hydra.ast.Padding(new hydra.ast.Ws.None(true), new hydra.ast.Ws.Break(true)), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None(true)),
      (v1));
  }
  
  hydra.ast.Padding noPadding = new hydra.ast.Padding(new hydra.ast.Ws.None(true), new hydra.ast.Ws.None(true));
  
  static hydra.ast.Expr noSep(java.util.List<hydra.ast.Expr> v1) {
    return hydra.serialization.Serialization.sep(
      new hydra.ast.Op(hydra.serialization.Serialization.sym(""), new hydra.ast.Padding(new hydra.ast.Ws.None(true), new hydra.ast.Ws.None(true)), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None(true)),
      (v1));
  }
  
  static hydra.ast.Expr num(Integer i) {
    return hydra.serialization.Serialization.cst(hydra.lib.literals.ShowInt32.apply((i)));
  }
  
  static hydra.ast.Op op(String s, Integer p, hydra.ast.Associativity assoc) {
    return new hydra.ast.Op(hydra.serialization.Serialization.sym((s)), new hydra.ast.Padding(new hydra.ast.Ws.Space(true), new hydra.ast.Ws.Space(true)), new hydra.ast.Precedence((p)), (assoc));
  }
  
  static hydra.ast.Op orOp(Boolean newlines) {
    return new hydra.ast.Op(hydra.serialization.Serialization.sym("|"), new hydra.ast.Padding(new hydra.ast.Ws.Space(true), hydra.lib.logic.IfElse.apply(
      (newlines),
      new hydra.ast.Ws.Break(true),
      new hydra.ast.Ws.Space(true))), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None(true));
  }
  
  static hydra.ast.Expr orSep(hydra.ast.BlockStyle style, java.util.List<hydra.ast.Expr> l) {
    hydra.ast.Expr h = hydra.lib.lists.Head.apply((l));
    Boolean newlines = ((style)).newlineBeforeContent;
    java.util.List<hydra.ast.Expr> r = hydra.lib.lists.Tail.apply((l));
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.lists.Null.apply((l)),
      hydra.serialization.Serialization.cst(""),
      hydra.lib.logic.IfElse.apply(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply((l)),
          1),
        hydra.lib.lists.Head.apply((l)),
        hydra.serialization.Serialization.ifx(
          hydra.serialization.Serialization.orOp((newlines)),
          (h),
          hydra.serialization.Serialization.orSep(
            (style),
            (r)))));
  }
  
  static hydra.ast.Expr parenList(Boolean newlines, java.util.List<hydra.ast.Expr> els) {
    hydra.ast.BlockStyle style = hydra.lib.logic.IfElse.apply(
      hydra.lib.logic.And.apply(
        (newlines),
        hydra.lib.equality.Gt.apply(
          hydra.lib.lists.Length.apply((els)),
          1)),
      (hydra.serialization.Serialization.halfBlockStyle),
      (hydra.serialization.Serialization.inlineStyle));
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.lists.Null.apply((els)),
      hydra.serialization.Serialization.cst("()"),
      hydra.serialization.Serialization.brackets(
        (hydra.serialization.Serialization.parentheses),
        (style),
        hydra.serialization.Serialization.commaSep(
          (style),
          (els))));
  }
  
  static hydra.ast.Expr parens(hydra.ast.Expr v1) {
    return hydra.serialization.Serialization.brackets(
      (hydra.serialization.Serialization.parentheses),
      (hydra.serialization.Serialization.inlineStyle),
      (v1));
  }
  
  hydra.ast.Brackets parentheses = new hydra.ast.Brackets(hydra.serialization.Serialization.sym("("), hydra.serialization.Serialization.sym(")"));
  
  static hydra.ast.Expr parenthesize(hydra.ast.Expr exp) {
    java.util.function.Function<hydra.ast.Associativity, Boolean> assocLeft = (java.util.function.Function<hydra.ast.Associativity, Boolean>) (a -> ((a)).accept(new hydra.ast.Associativity.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.ast.Associativity instance) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.ast.Associativity.Right ignored) {
        return false;
      }
    }));
    java.util.function.Function<hydra.ast.Associativity, Boolean> assocRight = (java.util.function.Function<hydra.ast.Associativity, Boolean>) (a -> ((a)).accept(new hydra.ast.Associativity.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.ast.Associativity instance) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.ast.Associativity.Left ignored) {
        return false;
      }
    }));
    return ((exp)).accept(new hydra.ast.Expr.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ast.Expr.Brackets bracketExpr) {
        return new hydra.ast.Expr.Brackets(new hydra.ast.BracketExpr((((bracketExpr)).value).brackets, hydra.serialization.Serialization.parenthesize((((bracketExpr)).value).enclosed), (((bracketExpr)).value).style));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ast.Expr.Const ignored) {
        return (exp);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ast.Expr.Indent indentExpr) {
        return new hydra.ast.Expr.Indent(new hydra.ast.IndentedExpression((((indentExpr)).value).style, hydra.serialization.Serialization.parenthesize((((indentExpr)).value).expr)));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ast.Expr.Op opExpr) {
        hydra.ast.Op op = (((opExpr)).value).op;
        hydra.ast.Associativity assoc = ((op)).associativity;
        hydra.ast.Expr lhs = (((opExpr)).value).lhs;
        hydra.ast.Expr lhs_ = hydra.serialization.Serialization.parenthesize((lhs));
        Integer prec = (((op)).precedence).value;
        hydra.ast.Expr lhs2 = ((lhs_)).accept(new hydra.ast.Expr.PartialVisitor<>() {
          @Override
          public hydra.ast.Expr otherwise(hydra.ast.Expr instance) {
            return (lhs_);
          }
          
          @Override
          public hydra.ast.Expr visit(hydra.ast.Expr.Op lopExpr) {
            hydra.ast.Op lop = (((lopExpr)).value).op;
            Integer lprec = (((lop)).precedence).value;
            hydra.util.Comparison comparison = hydra.lib.equality.Compare.apply(
              (prec),
              (lprec));
            hydra.ast.Associativity lassoc = ((lop)).associativity;
            return ((comparison)).accept(new hydra.util.Comparison.PartialVisitor<>() {
              @Override
              public hydra.ast.Expr visit(hydra.util.Comparison.LessThan ignored) {
                return (lhs_);
              }
              
              @Override
              public hydra.ast.Expr visit(hydra.util.Comparison.GreaterThan ignored) {
                return hydra.serialization.Serialization.parens((lhs_));
              }
              
              @Override
              public hydra.ast.Expr visit(hydra.util.Comparison.EqualTo ignored) {
                return hydra.lib.logic.IfElse.apply(
                  hydra.lib.logic.And.apply(
                    ((assocLeft)).apply((assoc)),
                    ((assocLeft)).apply((lassoc))),
                  (lhs_),
                  hydra.serialization.Serialization.parens((lhs_)));
              }
            });
          }
        });
        hydra.ast.Expr rhs = (((opExpr)).value).rhs;
        hydra.ast.Expr rhs_ = hydra.serialization.Serialization.parenthesize((rhs));
        hydra.ast.Expr rhs2 = ((rhs_)).accept(new hydra.ast.Expr.PartialVisitor<>() {
          @Override
          public hydra.ast.Expr otherwise(hydra.ast.Expr instance) {
            return (rhs_);
          }
          
          @Override
          public hydra.ast.Expr visit(hydra.ast.Expr.Op ropExpr) {
            hydra.ast.Op rop = (((ropExpr)).value).op;
            Integer rprec = (((rop)).precedence).value;
            hydra.util.Comparison comparison = hydra.lib.equality.Compare.apply(
              (prec),
              (rprec));
            hydra.ast.Associativity rassoc = ((rop)).associativity;
            return ((comparison)).accept(new hydra.util.Comparison.PartialVisitor<>() {
              @Override
              public hydra.ast.Expr visit(hydra.util.Comparison.LessThan ignored) {
                return (rhs_);
              }
              
              @Override
              public hydra.ast.Expr visit(hydra.util.Comparison.GreaterThan ignored) {
                return hydra.serialization.Serialization.parens((rhs_));
              }
              
              @Override
              public hydra.ast.Expr visit(hydra.util.Comparison.EqualTo ignored) {
                return hydra.lib.logic.IfElse.apply(
                  hydra.lib.logic.And.apply(
                    ((assocRight)).apply((assoc)),
                    ((assocRight)).apply((rassoc))),
                  (rhs_),
                  hydra.serialization.Serialization.parens((rhs_)));
              }
            });
          }
        });
        return new hydra.ast.Expr.Op(new hydra.ast.OpExpr((op), (lhs2), (rhs2)));
      }
    });
  }
  
  static hydra.ast.Expr prefix(String p, hydra.ast.Expr expr) {
    hydra.ast.Op preOp = new hydra.ast.Op(hydra.serialization.Serialization.sym((p)), new hydra.ast.Padding(new hydra.ast.Ws.None(true), new hydra.ast.Ws.None(true)), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None(true));
    return hydra.serialization.Serialization.ifx(
      (preOp),
      hydra.serialization.Serialization.cst(""),
      (expr));
  }
  
  static String printExpr(hydra.ast.Expr e) {
    java.util.function.Function<hydra.ast.Ws, java.util.function.Function<String, String>> idt = (java.util.function.Function<hydra.ast.Ws, java.util.function.Function<String, String>>) (ws -> (java.util.function.Function<String, String>) (s -> ((ws)).accept(new hydra.ast.Ws.PartialVisitor<>() {
      @Override
      public String otherwise(hydra.ast.Ws instance) {
        return (s);
      }
      
      @Override
      public String visit(hydra.ast.Ws.BreakAndIndent indentStr) {
        return hydra.serialization.Serialization.customIndent(
          ((indentStr)).value,
          (s));
      }
    })));
    java.util.function.Function<hydra.ast.Ws, String> pad = (java.util.function.Function<hydra.ast.Ws, String>) (ws -> ((ws)).accept(new hydra.ast.Ws.PartialVisitor<>() {
      @Override
      public String visit(hydra.ast.Ws.None ignored) {
        return "";
      }
      
      @Override
      public String visit(hydra.ast.Ws.Space ignored) {
        return " ";
      }
      
      @Override
      public String visit(hydra.ast.Ws.Break ignored) {
        return "\n";
      }
      
      @Override
      public String visit(hydra.ast.Ws.BreakAndIndent ignored) {
        return "\n";
      }
      
      @Override
      public String visit(hydra.ast.Ws.DoubleBreak ignored) {
        return "\n\n";
      }
    }));
    return ((e)).accept(new hydra.ast.Expr.PartialVisitor<>() {
      @Override
      public String visit(hydra.ast.Expr.Const symbol) {
        return (((symbol)).value).value;
      }
      
      @Override
      public String visit(hydra.ast.Expr.Indent indentExpr) {
        hydra.ast.Expr expr = (((indentExpr)).value).expr;
        java.util.List<String> lns = hydra.lib.strings.Lines.apply(hydra.serialization.Serialization.printExpr((expr)));
        hydra.ast.IndentStyle style = (((indentExpr)).value).style;
        java.util.List<String> ilns = ((style)).accept(new hydra.ast.IndentStyle.PartialVisitor<>() {
          @Override
          public java.util.List<String> visit(hydra.ast.IndentStyle.AllLines idt2) {
            return hydra.lib.lists.Map.apply(
              (java.util.function.Function<String, String>) (line -> hydra.lib.strings.Cat2.apply(
                ((idt2)).value,
                (line))),
              (lns));
          }
          
          @Override
          public java.util.List<String> visit(hydra.ast.IndentStyle.SubsequentLines idt2) {
            return hydra.lib.logic.IfElse.apply(
              hydra.lib.equality.Equal.apply(
                hydra.lib.lists.Length.apply((lns)),
                1),
              (lns),
              hydra.lib.lists.Cons.apply(
                hydra.lib.lists.Head.apply((lns)),
                hydra.lib.lists.Map.apply(
                  (java.util.function.Function<String, String>) (line -> hydra.lib.strings.Cat2.apply(
                    ((idt2)).value,
                    (line))),
                  hydra.lib.lists.Tail.apply((lns)))));
          }
        });
        return hydra.lib.strings.Intercalate.apply(
          "\n",
          (ilns));
      }
      
      @Override
      public String visit(hydra.ast.Expr.Op opExpr) {
        hydra.ast.Expr l = (((opExpr)).value).lhs;
        hydra.ast.Op op = (((opExpr)).value).op;
        hydra.ast.Padding padding = ((op)).padding;
        hydra.ast.Ws padl = ((padding)).left;
        String lhs = (((idt)).apply((padl))).apply(hydra.serialization.Serialization.printExpr((l)));
        hydra.ast.Ws padr = ((padding)).right;
        hydra.ast.Expr r = (((opExpr)).value).rhs;
        String rhs = (((idt)).apply((padr))).apply(hydra.serialization.Serialization.printExpr((r)));
        String sym = (((op)).symbol).value;
        return hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                (lhs),
                ((pad)).apply((padl))),
              (sym)),
            ((pad)).apply((padr))),
          (rhs));
      }
      
      @Override
      public String visit(hydra.ast.Expr.Brackets bracketExpr) {
        hydra.ast.Expr e = (((bracketExpr)).value).enclosed;
        String body = hydra.serialization.Serialization.printExpr((e));
        hydra.ast.Brackets brackets = (((bracketExpr)).value).brackets;
        hydra.ast.BlockStyle style = (((bracketExpr)).value).style;
        hydra.util.Maybe<String> doIndent = ((style)).indent;
        String ibody = hydra.lib.maybes.Maybe.apply(
          (body),
          (java.util.function.Function<String, String>) (idt2 -> hydra.serialization.Serialization.customIndent(
            (idt2),
            (body))),
          (doIndent));
        String l = (((brackets)).open).value;
        Boolean nlAfter = ((style)).newlineAfterContent;
        Boolean nlBefore = ((style)).newlineBeforeContent;
        String pre = hydra.lib.logic.IfElse.apply(
          (nlBefore),
          "\n",
          "");
        String r = (((brackets)).close).value;
        String suf = hydra.lib.logic.IfElse.apply(
          (nlAfter),
          "\n",
          "");
        return hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                (l),
                (pre)),
              (ibody)),
            (suf)),
          (r));
      }
    });
  }
  
  static hydra.ast.Expr semicolonSep(java.util.List<hydra.ast.Expr> v1) {
    return hydra.serialization.Serialization.symbolSep(
      ";",
      (hydra.serialization.Serialization.inlineStyle),
      (v1));
  }
  
  static hydra.ast.Expr sep(hydra.ast.Op op, java.util.List<hydra.ast.Expr> els) {
    hydra.ast.Expr h = hydra.lib.lists.Head.apply((els));
    java.util.List<hydra.ast.Expr> r = hydra.lib.lists.Tail.apply((els));
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.lists.Null.apply((els)),
      hydra.serialization.Serialization.cst(""),
      hydra.lib.logic.IfElse.apply(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply((els)),
          1),
        hydra.lib.lists.Head.apply((els)),
        hydra.serialization.Serialization.ifx(
          (op),
          (h),
          hydra.serialization.Serialization.sep(
            (op),
            (r)))));
  }
  
  static hydra.ast.Expr spaceSep(java.util.List<hydra.ast.Expr> v1) {
    return hydra.serialization.Serialization.sep(
      new hydra.ast.Op(hydra.serialization.Serialization.sym(""), new hydra.ast.Padding(new hydra.ast.Ws.Space(true), new hydra.ast.Ws.None(true)), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None(true)),
      (v1));
  }
  
  hydra.ast.Brackets squareBrackets = new hydra.ast.Brackets(hydra.serialization.Serialization.sym("["), hydra.serialization.Serialization.sym("]"));
  
  static hydra.ast.Symbol sym(String s) {
    return new hydra.ast.Symbol((s));
  }
  
  static hydra.ast.Expr symbolSep(String symb, hydra.ast.BlockStyle style, java.util.List<hydra.ast.Expr> l) {
    Integer breakCount = hydra.lib.lists.Length.apply(hydra.lib.lists.Filter.apply(
      (java.util.function.Function<Boolean, Boolean>) (x_ -> (x_)),
      java.util.List.of(
        ((style)).newlineBeforeContent,
        ((style)).newlineAfterContent)));
    hydra.ast.Ws break_ = hydra.lib.logic.IfElse.apply(
      hydra.lib.equality.Equal.apply(
        (breakCount),
        0),
      new hydra.ast.Ws.Space(true),
      hydra.lib.logic.IfElse.apply(
        hydra.lib.equality.Equal.apply(
          (breakCount),
          1),
        new hydra.ast.Ws.Break(true),
        new hydra.ast.Ws.DoubleBreak(true)));
    hydra.ast.Op commaOp = new hydra.ast.Op(hydra.serialization.Serialization.sym((symb)), new hydra.ast.Padding(new hydra.ast.Ws.None(true), (break_)), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None(true));
    hydra.ast.Expr h = hydra.lib.lists.Head.apply((l));
    java.util.List<hydra.ast.Expr> r = hydra.lib.lists.Tail.apply((l));
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.lists.Null.apply((l)),
      hydra.serialization.Serialization.cst(""),
      hydra.lib.logic.IfElse.apply(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply((l)),
          1),
        hydra.lib.lists.Head.apply((l)),
        hydra.serialization.Serialization.ifx(
          (commaOp),
          (h),
          hydra.serialization.Serialization.symbolSep(
            (symb),
            (style),
            (r)))));
  }
  
  static hydra.ast.Expr tabIndent(hydra.ast.Expr e) {
    return new hydra.ast.Expr.Indent(new hydra.ast.IndentedExpression(new hydra.ast.IndentStyle.AllLines("    "), (e)));
  }
  
  static hydra.ast.Expr tabIndentDoubleSpace(java.util.List<hydra.ast.Expr> exprs) {
    return hydra.serialization.Serialization.tabIndent(hydra.serialization.Serialization.doubleNewlineSep((exprs)));
  }
  
  static hydra.ast.Expr tabIndentSingleSpace(java.util.List<hydra.ast.Expr> exprs) {
    return hydra.serialization.Serialization.tabIndent(hydra.serialization.Serialization.newlineSep((exprs)));
  }
  
  static hydra.ast.Expr unsupportedType(String label) {
    return hydra.serialization.Serialization.cst(hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        "[",
        (label)),
      "]"));
  }
  
  static hydra.ast.Expr unsupportedVariant(String label, String obj) {
    return hydra.serialization.Serialization.cst(hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            "[unsupported ",
            (label)),
          ": "),
        hydra.lib.literals.ShowString.apply((obj))),
      "]"));
  }
  
  static hydra.ast.Expr withComma(hydra.ast.Expr e) {
    return hydra.serialization.Serialization.noSep(java.util.List.of(
      (e),
      hydra.serialization.Serialization.cst(",")));
  }
  
  static hydra.ast.Expr withSemi(hydra.ast.Expr e) {
    return hydra.serialization.Serialization.noSep(java.util.List.of(
      (e),
      hydra.serialization.Serialization.cst(";")));
  }
}
