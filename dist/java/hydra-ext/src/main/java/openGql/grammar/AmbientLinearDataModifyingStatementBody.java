// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class AmbientLinearDataModifyingStatementBody implements Serializable, Comparable<AmbientLinearDataModifyingStatementBody> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.AmbientLinearDataModifyingStatementBody");

  public static final hydra.core.Name SIMPLE_ACCESS = new hydra.core.Name("simpleAccess");

  public static final hydra.core.Name PRIMITIVE_RESULT = new hydra.core.Name("primitiveResult");

  public final java.util.List<openGql.grammar.SimpleDataAccessingStatement> simpleAccess;

  public final hydra.util.Maybe<openGql.grammar.PrimitiveResultStatement> primitiveResult;

  public AmbientLinearDataModifyingStatementBody (java.util.List<openGql.grammar.SimpleDataAccessingStatement> simpleAccess, hydra.util.Maybe<openGql.grammar.PrimitiveResultStatement> primitiveResult) {
    this.simpleAccess = simpleAccess;
    this.primitiveResult = primitiveResult;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AmbientLinearDataModifyingStatementBody)) {
      return false;
    }
    AmbientLinearDataModifyingStatementBody o = (AmbientLinearDataModifyingStatementBody) other;
    return java.util.Objects.equals(
      this.simpleAccess,
      o.simpleAccess) && java.util.Objects.equals(
      this.primitiveResult,
      o.primitiveResult);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(simpleAccess) + 3 * java.util.Objects.hashCode(primitiveResult);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AmbientLinearDataModifyingStatementBody other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      simpleAccess,
      other.simpleAccess);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      primitiveResult,
      other.primitiveResult);
  }

  public AmbientLinearDataModifyingStatementBody withSimpleAccess(java.util.List<openGql.grammar.SimpleDataAccessingStatement> simpleAccess) {
    return new AmbientLinearDataModifyingStatementBody(simpleAccess, primitiveResult);
  }

  public AmbientLinearDataModifyingStatementBody withPrimitiveResult(hydra.util.Maybe<openGql.grammar.PrimitiveResultStatement> primitiveResult) {
    return new AmbientLinearDataModifyingStatementBody(simpleAccess, primitiveResult);
  }
}
