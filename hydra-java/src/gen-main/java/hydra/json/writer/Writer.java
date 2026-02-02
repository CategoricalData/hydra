// Note: this is an automatically generated file. Do not edit.

package hydra.json.writer;

/**
 * JSON serialization functions using the Hydra AST
 */
public interface Writer {
  hydra.ast.Op colonOp = new hydra.ast.Op(new hydra.ast.Symbol(":"), new hydra.ast.Padding(new hydra.ast.Ws.None(true), new hydra.ast.Ws.Space(true)), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None(true));
  
  static String jsonString(String s) {
    java.util.function.Function<Integer, String> escape = (java.util.function.Function<Integer, String>) (c -> hydra.lib.logic.IfElse.apply(
      hydra.lib.equality.Equal.apply(
        (c),
        34),
      "\\\"",
      hydra.lib.logic.IfElse.apply(
        hydra.lib.equality.Equal.apply(
          (c),
          92),
        "\\\\",
        hydra.lib.logic.IfElse.apply(
          hydra.lib.equality.Equal.apply(
            (c),
            10),
          "\\n",
          hydra.lib.logic.IfElse.apply(
            hydra.lib.equality.Equal.apply(
              (c),
              13),
            "\\r",
            hydra.lib.logic.IfElse.apply(
              hydra.lib.equality.Equal.apply(
                (c),
                9),
              "\\t",
              hydra.lib.strings.FromList.apply(hydra.lib.lists.Pure.apply((c)))))))));
    String escaped = hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
      (escape),
      hydra.lib.strings.ToList.apply((s))));
    return hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        "\"",
        (escaped)),
      "\"");
  }
  
  static hydra.ast.Expr keyValueToExpr(hydra.util.Tuple.Tuple2<String, hydra.json.model.Value> pair) {
    String key = hydra.lib.pairs.First.apply((pair));
    hydra.json.model.Value value = hydra.lib.pairs.Second.apply((pair));
    return hydra.serialization.Serialization.ifx(
      (hydra.json.writer.Writer.colonOp),
      hydra.serialization.Serialization.cst(hydra.json.writer.Writer.jsonString((key))),
      hydra.json.writer.Writer.valueToExpr((value)));
  }
  
  static String printJson(hydra.json.model.Value value) {
    return hydra.serialization.Serialization.printExpr(hydra.json.writer.Writer.valueToExpr((value)));
  }
  
  static hydra.ast.Expr valueToExpr(hydra.json.model.Value value) {
    return ((value)).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.json.model.Value.Array arr) {
        return hydra.serialization.Serialization.bracketListAdaptive(hydra.lib.lists.Map.apply(
          (hydra.json.writer.Writer::valueToExpr),
          ((arr)).value));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.json.model.Value.Boolean_ b) {
        return hydra.serialization.Serialization.cst(hydra.lib.logic.IfElse.apply(
          ((b)).value,
          "true",
          "false"));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.json.model.Value.Null ignored) {
        return hydra.serialization.Serialization.cst("null");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.json.model.Value.Number_ n) {
        java.math.BigInteger rounded = hydra.lib.literals.BigfloatToBigint.apply(((n)).value);
        return hydra.serialization.Serialization.cst(hydra.lib.logic.IfElse.apply(
          hydra.lib.equality.Equal.apply(
            ((n)).value,
            hydra.lib.literals.BigintToBigfloat.apply((rounded))),
          hydra.lib.literals.ShowBigint.apply((rounded)),
          hydra.lib.literals.ShowBigfloat.apply(((n)).value)));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.json.model.Value.Object_ obj) {
        return hydra.serialization.Serialization.bracesListAdaptive(hydra.lib.lists.Map.apply(
          (hydra.json.writer.Writer::keyValueToExpr),
          hydra.lib.maps.ToList.apply(((obj)).value)));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.json.model.Value.String_ s) {
        return hydra.serialization.Serialization.cst(hydra.json.writer.Writer.jsonString(((s)).value));
      }
    });
  }
}
