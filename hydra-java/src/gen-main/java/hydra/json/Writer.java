// Note: this is an automatically generated file. Do not edit.

package hydra.json;

/**
 * JSON serialization functions using the Hydra AST
 */
public interface Writer {
  static hydra.ast.Op colonOp() {
    return new hydra.ast.Op(new hydra.ast.Symbol(":"), new hydra.ast.Padding(new hydra.ast.Ws.None(), new hydra.ast.Ws.Space()), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None());
  }

  static String jsonString(String s) {
    java.util.function.Function<Integer, String> hexEscape = (java.util.function.Function<Integer, String>) (c -> {
      hydra.util.Lazy<String> hi = new hydra.util.Lazy<>(() -> hydra.lib.strings.FromList.apply(hydra.lib.lists.Pure.apply(hydra.lib.strings.CharAt.apply(
        hydra.lib.math.Div.apply(
          c,
          16),
        "0123456789abcdef"))));
      hydra.util.Lazy<String> lo = new hydra.util.Lazy<>(() -> hydra.lib.strings.FromList.apply(hydra.lib.lists.Pure.apply(hydra.lib.strings.CharAt.apply(
        hydra.lib.math.Mod.apply(
          c,
          16),
        "0123456789abcdef"))));
      return hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          "\\u00",
          hi.get()),
        lo.get());
    });
    java.util.function.Function<Integer, String> escape = (java.util.function.Function<Integer, String>) (c -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        c,
        34),
      () -> "\\\"",
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          c,
          92),
        () -> "\\\\",
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            c,
            8),
          () -> "\\b",
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              c,
              12),
            () -> "\\f",
            () -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                c,
                10),
              () -> "\\n",
              () -> hydra.lib.logic.IfElse.lazy(
                hydra.lib.equality.Equal.apply(
                  c,
                  13),
                () -> "\\r",
                () -> hydra.lib.logic.IfElse.lazy(
                  hydra.lib.equality.Equal.apply(
                    c,
                    9),
                  () -> "\\t",
                  () -> hydra.lib.logic.IfElse.lazy(
                    hydra.lib.equality.Lt.apply(
                      c,
                      32),
                    () -> (hexEscape).apply(c),
                    () -> hydra.lib.strings.FromList.apply(hydra.lib.lists.Pure.apply(c)))))))))));
    hydra.util.Lazy<String> escaped = new hydra.util.Lazy<>(() -> hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
      escape,
      hydra.lib.strings.ToList.apply(s))));
    return hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        "\"",
        escaped.get()),
      "\"");
  }

  static hydra.ast.Expr keyValueToExpr(hydra.util.Pair<String, hydra.json.model.Value> pair) {
    hydra.util.Lazy<String> key = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
    hydra.util.Lazy<hydra.json.model.Value> value = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
    return hydra.Serialization.ifx(
      hydra.json.Writer.colonOp(),
      hydra.Serialization.cst(hydra.json.Writer.jsonString(key.get())),
      hydra.json.Writer.valueToExpr(value.get()));
  }

  static String printJson(hydra.json.model.Value value) {
    return hydra.Serialization.printExpr(hydra.json.Writer.valueToExpr(value));
  }

  static hydra.ast.Expr valueToExpr(hydra.json.model.Value value) {
    return (value).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.json.model.Value.Array arr) {
        return hydra.Serialization.bracketListAdaptive(hydra.lib.lists.Map.apply(
          hydra.json.Writer::valueToExpr,
          (arr).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.json.model.Value.Boolean_ b) {
        return hydra.Serialization.cst(hydra.lib.logic.IfElse.lazy(
          (b).value,
          () -> "true",
          () -> "false"));
      }

      @Override
      public hydra.ast.Expr visit(hydra.json.model.Value.Null ignored) {
        return hydra.Serialization.cst("null");
      }

      @Override
      public hydra.ast.Expr visit(hydra.json.model.Value.Number_ n) {
        java.math.BigInteger rounded = hydra.lib.literals.BigfloatToBigint.apply((n).value);
        String shown = hydra.lib.literals.ShowBigfloat.apply((n).value);
        return hydra.Serialization.cst(hydra.lib.logic.IfElse.lazy(
          hydra.lib.logic.And.apply(
            hydra.lib.equality.Equal.apply(
              (n).value,
              hydra.lib.literals.BigintToBigfloat.apply(rounded)),
            hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
              shown,
              "-0.0"))),
          () -> hydra.lib.literals.ShowBigint.apply(rounded),
          () -> shown));
      }

      @Override
      public hydra.ast.Expr visit(hydra.json.model.Value.Object_ obj) {
        return hydra.Serialization.bracesListAdaptive(hydra.lib.lists.Map.apply(
          hydra.json.Writer::keyValueToExpr,
          hydra.lib.maps.ToList.apply((obj).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.json.model.Value.String_ s) {
        return hydra.Serialization.cst(hydra.json.Writer.jsonString((s).value));
      }
    });
  }
}
