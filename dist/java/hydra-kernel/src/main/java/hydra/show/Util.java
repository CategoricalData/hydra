// Note: this is an automatically generated file. Do not edit.

package hydra.show;

/**
 * String representations of hydra.util types
 */
public interface Util {
  static String caseConvention(hydra.util.CaseConvention c) {
    return (c).accept(new hydra.util.CaseConvention.PartialVisitor<>() {
      @Override
      public String visit(hydra.util.CaseConvention.LowerSnake ignored) {
        return "lower_snake_case";
      }

      @Override
      public String visit(hydra.util.CaseConvention.UpperSnake ignored) {
        return "UPPER_SNAKE_CASE";
      }

      @Override
      public String visit(hydra.util.CaseConvention.Camel ignored) {
        return "camelCase";
      }

      @Override
      public String visit(hydra.util.CaseConvention.Pascal ignored) {
        return "PascalCase";
      }
    });
  }

  static String comparison(hydra.util.Comparison c) {
    return (c).accept(new hydra.util.Comparison.PartialVisitor<>() {
      @Override
      public String visit(hydra.util.Comparison.LessThan ignored) {
        return "lessThan";
      }

      @Override
      public String visit(hydra.util.Comparison.EqualTo ignored) {
        return "equalTo";
      }

      @Override
      public String visit(hydra.util.Comparison.GreaterThan ignored) {
        return "greaterThan";
      }
    });
  }
}
