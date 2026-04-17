// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class GeneralLogarithmFunction implements Serializable, Comparable<GeneralLogarithmFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.GeneralLogarithmFunction");

  public static final hydra.core.Name BASE = new hydra.core.Name("base");

  public static final hydra.core.Name ARGUMENT = new hydra.core.Name("argument");

  public final openGql.grammar.NumericValueExpression base;

  public final openGql.grammar.NumericValueExpression argument;

  public GeneralLogarithmFunction (openGql.grammar.NumericValueExpression base, openGql.grammar.NumericValueExpression argument) {
    this.base = base;
    this.argument = argument;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GeneralLogarithmFunction)) {
      return false;
    }
    GeneralLogarithmFunction o = (GeneralLogarithmFunction) other;
    return java.util.Objects.equals(
      this.base,
      o.base) && java.util.Objects.equals(
      this.argument,
      o.argument);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(base) + 3 * java.util.Objects.hashCode(argument);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(GeneralLogarithmFunction other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      base,
      other.base);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      argument,
      other.argument);
  }

  public GeneralLogarithmFunction withBase(openGql.grammar.NumericValueExpression base) {
    return new GeneralLogarithmFunction(base, argument);
  }

  public GeneralLogarithmFunction withArgument(openGql.grammar.NumericValueExpression argument) {
    return new GeneralLogarithmFunction(base, argument);
  }
}
