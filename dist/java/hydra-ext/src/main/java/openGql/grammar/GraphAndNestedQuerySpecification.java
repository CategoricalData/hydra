// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class GraphAndNestedQuerySpecification implements Serializable, Comparable<GraphAndNestedQuerySpecification> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.GraphAndNestedQuerySpecification");

  public static final hydra.core.Name GRAPH_EXPRESSION = new hydra.core.Name("graphExpression");

  public static final hydra.core.Name NESTED = new hydra.core.Name("nested");

  public final openGql.grammar.GraphExpression graphExpression;

  public final openGql.grammar.ProcedureBody nested;

  public GraphAndNestedQuerySpecification (openGql.grammar.GraphExpression graphExpression, openGql.grammar.ProcedureBody nested) {
    this.graphExpression = graphExpression;
    this.nested = nested;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GraphAndNestedQuerySpecification)) {
      return false;
    }
    GraphAndNestedQuerySpecification o = (GraphAndNestedQuerySpecification) other;
    return java.util.Objects.equals(
      this.graphExpression,
      o.graphExpression) && java.util.Objects.equals(
      this.nested,
      o.nested);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(graphExpression) + 3 * java.util.Objects.hashCode(nested);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(GraphAndNestedQuerySpecification other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      graphExpression,
      other.graphExpression);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      nested,
      other.nested);
  }

  public GraphAndNestedQuerySpecification withGraphExpression(openGql.grammar.GraphExpression graphExpression) {
    return new GraphAndNestedQuerySpecification(graphExpression, nested);
  }

  public GraphAndNestedQuerySpecification withNested(openGql.grammar.ProcedureBody nested) {
    return new GraphAndNestedQuerySpecification(graphExpression, nested);
  }
}
