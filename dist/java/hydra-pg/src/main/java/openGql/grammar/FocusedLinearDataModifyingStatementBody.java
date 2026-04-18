// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class FocusedLinearDataModifyingStatementBody implements Serializable, Comparable<FocusedLinearDataModifyingStatementBody> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.FocusedLinearDataModifyingStatementBody");

  public static final hydra.core.Name USE_GRAPH = new hydra.core.Name("useGraph");

  public static final hydra.core.Name SIMPLE_ACCESS = new hydra.core.Name("simpleAccess");

  public static final hydra.core.Name PRIMITIVE_RESULT = new hydra.core.Name("primitiveResult");

  public final openGql.grammar.GraphExpression useGraph;

  public final java.util.List<openGql.grammar.SimpleDataAccessingStatement> simpleAccess;

  public final hydra.util.Maybe<openGql.grammar.PrimitiveResultStatement> primitiveResult;

  public FocusedLinearDataModifyingStatementBody (openGql.grammar.GraphExpression useGraph, java.util.List<openGql.grammar.SimpleDataAccessingStatement> simpleAccess, hydra.util.Maybe<openGql.grammar.PrimitiveResultStatement> primitiveResult) {
    this.useGraph = useGraph;
    this.simpleAccess = simpleAccess;
    this.primitiveResult = primitiveResult;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FocusedLinearDataModifyingStatementBody)) {
      return false;
    }
    FocusedLinearDataModifyingStatementBody o = (FocusedLinearDataModifyingStatementBody) other;
    return java.util.Objects.equals(
      this.useGraph,
      o.useGraph) && java.util.Objects.equals(
      this.simpleAccess,
      o.simpleAccess) && java.util.Objects.equals(
      this.primitiveResult,
      o.primitiveResult);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(useGraph) + 3 * java.util.Objects.hashCode(simpleAccess) + 5 * java.util.Objects.hashCode(primitiveResult);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FocusedLinearDataModifyingStatementBody other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      useGraph,
      other.useGraph);
    if (cmp != 0) {
      return cmp;
    }
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

  public FocusedLinearDataModifyingStatementBody withUseGraph(openGql.grammar.GraphExpression useGraph) {
    return new FocusedLinearDataModifyingStatementBody(useGraph, simpleAccess, primitiveResult);
  }

  public FocusedLinearDataModifyingStatementBody withSimpleAccess(java.util.List<openGql.grammar.SimpleDataAccessingStatement> simpleAccess) {
    return new FocusedLinearDataModifyingStatementBody(useGraph, simpleAccess, primitiveResult);
  }

  public FocusedLinearDataModifyingStatementBody withPrimitiveResult(hydra.util.Maybe<openGql.grammar.PrimitiveResultStatement> primitiveResult) {
    return new FocusedLinearDataModifyingStatementBody(useGraph, simpleAccess, primitiveResult);
  }
}
