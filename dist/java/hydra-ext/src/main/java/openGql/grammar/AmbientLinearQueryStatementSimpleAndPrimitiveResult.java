// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class AmbientLinearQueryStatementSimpleAndPrimitiveResult implements Serializable, Comparable<AmbientLinearQueryStatementSimpleAndPrimitiveResult> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.AmbientLinearQueryStatementSimpleAndPrimitiveResult");

  public static final hydra.core.Name SIMPLE = new hydra.core.Name("simple");

  public static final hydra.core.Name PRIMITIVE_RESULT = new hydra.core.Name("primitiveResult");

  public final hydra.util.Maybe<java.util.List<openGql.grammar.SimpleQueryStatement>> simple;

  public final openGql.grammar.PrimitiveResultStatement primitiveResult;

  public AmbientLinearQueryStatementSimpleAndPrimitiveResult (hydra.util.Maybe<java.util.List<openGql.grammar.SimpleQueryStatement>> simple, openGql.grammar.PrimitiveResultStatement primitiveResult) {
    this.simple = simple;
    this.primitiveResult = primitiveResult;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AmbientLinearQueryStatementSimpleAndPrimitiveResult)) {
      return false;
    }
    AmbientLinearQueryStatementSimpleAndPrimitiveResult o = (AmbientLinearQueryStatementSimpleAndPrimitiveResult) other;
    return java.util.Objects.equals(
      this.simple,
      o.simple) && java.util.Objects.equals(
      this.primitiveResult,
      o.primitiveResult);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(simple) + 3 * java.util.Objects.hashCode(primitiveResult);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AmbientLinearQueryStatementSimpleAndPrimitiveResult other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      simple,
      other.simple);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      primitiveResult,
      other.primitiveResult);
  }

  public AmbientLinearQueryStatementSimpleAndPrimitiveResult withSimple(hydra.util.Maybe<java.util.List<openGql.grammar.SimpleQueryStatement>> simple) {
    return new AmbientLinearQueryStatementSimpleAndPrimitiveResult(simple, primitiveResult);
  }

  public AmbientLinearQueryStatementSimpleAndPrimitiveResult withPrimitiveResult(openGql.grammar.PrimitiveResultStatement primitiveResult) {
    return new AmbientLinearQueryStatementSimpleAndPrimitiveResult(simple, primitiveResult);
  }
}
