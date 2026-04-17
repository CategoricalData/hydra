// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SimpleWhenClause implements Serializable, Comparable<SimpleWhenClause> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SimpleWhenClause");

  public static final hydra.core.Name WHEN_OPERANDS = new hydra.core.Name("whenOperands");

  public static final hydra.core.Name RESULT = new hydra.core.Name("result");

  public final java.util.List<openGql.grammar.WhenOperand> whenOperands;

  public final openGql.grammar.Result result;

  public SimpleWhenClause (java.util.List<openGql.grammar.WhenOperand> whenOperands, openGql.grammar.Result result) {
    this.whenOperands = whenOperands;
    this.result = result;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SimpleWhenClause)) {
      return false;
    }
    SimpleWhenClause o = (SimpleWhenClause) other;
    return java.util.Objects.equals(
      this.whenOperands,
      o.whenOperands) && java.util.Objects.equals(
      this.result,
      o.result);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(whenOperands) + 3 * java.util.Objects.hashCode(result);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SimpleWhenClause other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      whenOperands,
      other.whenOperands);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      result,
      other.result);
  }

  public SimpleWhenClause withWhenOperands(java.util.List<openGql.grammar.WhenOperand> whenOperands) {
    return new SimpleWhenClause(whenOperands, result);
  }

  public SimpleWhenClause withResult(openGql.grammar.Result result) {
    return new SimpleWhenClause(whenOperands, result);
  }
}
