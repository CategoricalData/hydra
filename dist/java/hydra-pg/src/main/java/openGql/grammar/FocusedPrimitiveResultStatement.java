// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class FocusedPrimitiveResultStatement implements Serializable, Comparable<FocusedPrimitiveResultStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.FocusedPrimitiveResultStatement");

  public static final hydra.core.Name USE_GRAPH = new hydra.core.Name("useGraph");

  public static final hydra.core.Name PRIMITIVE_RESULT = new hydra.core.Name("primitiveResult");

  public final openGql.grammar.GraphExpression useGraph;

  public final openGql.grammar.PrimitiveResultStatement primitiveResult;

  public FocusedPrimitiveResultStatement (openGql.grammar.GraphExpression useGraph, openGql.grammar.PrimitiveResultStatement primitiveResult) {
    this.useGraph = useGraph;
    this.primitiveResult = primitiveResult;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FocusedPrimitiveResultStatement)) {
      return false;
    }
    FocusedPrimitiveResultStatement o = (FocusedPrimitiveResultStatement) other;
    return java.util.Objects.equals(
      this.useGraph,
      o.useGraph) && java.util.Objects.equals(
      this.primitiveResult,
      o.primitiveResult);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(useGraph) + 3 * java.util.Objects.hashCode(primitiveResult);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FocusedPrimitiveResultStatement other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      useGraph,
      other.useGraph);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      primitiveResult,
      other.primitiveResult);
  }

  public FocusedPrimitiveResultStatement withUseGraph(openGql.grammar.GraphExpression useGraph) {
    return new FocusedPrimitiveResultStatement(useGraph, primitiveResult);
  }

  public FocusedPrimitiveResultStatement withPrimitiveResult(openGql.grammar.PrimitiveResultStatement primitiveResult) {
    return new FocusedPrimitiveResultStatement(useGraph, primitiveResult);
  }
}
