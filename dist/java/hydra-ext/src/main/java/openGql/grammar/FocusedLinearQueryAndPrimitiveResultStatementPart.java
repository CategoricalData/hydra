// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class FocusedLinearQueryAndPrimitiveResultStatementPart implements Serializable, Comparable<FocusedLinearQueryAndPrimitiveResultStatementPart> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.FocusedLinearQueryAndPrimitiveResultStatementPart");

  public static final hydra.core.Name USE_GRAPH = new hydra.core.Name("useGraph");

  public static final hydra.core.Name SIMPLE = new hydra.core.Name("simple");

  public static final hydra.core.Name PRIMITIVE_RESULT = new hydra.core.Name("primitiveResult");

  public final openGql.grammar.GraphExpression useGraph;

  public final java.util.List<openGql.grammar.SimpleQueryStatement> simple;

  public final openGql.grammar.PrimitiveResultStatement primitiveResult;

  public FocusedLinearQueryAndPrimitiveResultStatementPart (openGql.grammar.GraphExpression useGraph, java.util.List<openGql.grammar.SimpleQueryStatement> simple, openGql.grammar.PrimitiveResultStatement primitiveResult) {
    this.useGraph = useGraph;
    this.simple = simple;
    this.primitiveResult = primitiveResult;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FocusedLinearQueryAndPrimitiveResultStatementPart)) {
      return false;
    }
    FocusedLinearQueryAndPrimitiveResultStatementPart o = (FocusedLinearQueryAndPrimitiveResultStatementPart) other;
    return java.util.Objects.equals(
      this.useGraph,
      o.useGraph) && java.util.Objects.equals(
      this.simple,
      o.simple) && java.util.Objects.equals(
      this.primitiveResult,
      o.primitiveResult);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(useGraph) + 3 * java.util.Objects.hashCode(simple) + 5 * java.util.Objects.hashCode(primitiveResult);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FocusedLinearQueryAndPrimitiveResultStatementPart other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      useGraph,
      other.useGraph);
    if (cmp != 0) {
      return cmp;
    }
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

  public FocusedLinearQueryAndPrimitiveResultStatementPart withUseGraph(openGql.grammar.GraphExpression useGraph) {
    return new FocusedLinearQueryAndPrimitiveResultStatementPart(useGraph, simple, primitiveResult);
  }

  public FocusedLinearQueryAndPrimitiveResultStatementPart withSimple(java.util.List<openGql.grammar.SimpleQueryStatement> simple) {
    return new FocusedLinearQueryAndPrimitiveResultStatementPart(useGraph, simple, primitiveResult);
  }

  public FocusedLinearQueryAndPrimitiveResultStatementPart withPrimitiveResult(openGql.grammar.PrimitiveResultStatement primitiveResult) {
    return new FocusedLinearQueryAndPrimitiveResultStatementPart(useGraph, simple, primitiveResult);
  }
}
