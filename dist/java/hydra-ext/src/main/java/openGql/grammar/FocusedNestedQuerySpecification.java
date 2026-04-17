// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class FocusedNestedQuerySpecification implements Serializable, Comparable<FocusedNestedQuerySpecification> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.FocusedNestedQuerySpecification");

  public static final hydra.core.Name USE_GRAPH = new hydra.core.Name("useGraph");

  public static final hydra.core.Name NESTED = new hydra.core.Name("nested");

  public final openGql.grammar.GraphExpression useGraph;

  public final openGql.grammar.ProcedureBody nested;

  public FocusedNestedQuerySpecification (openGql.grammar.GraphExpression useGraph, openGql.grammar.ProcedureBody nested) {
    this.useGraph = useGraph;
    this.nested = nested;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FocusedNestedQuerySpecification)) {
      return false;
    }
    FocusedNestedQuerySpecification o = (FocusedNestedQuerySpecification) other;
    return java.util.Objects.equals(
      this.useGraph,
      o.useGraph) && java.util.Objects.equals(
      this.nested,
      o.nested);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(useGraph) + 3 * java.util.Objects.hashCode(nested);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FocusedNestedQuerySpecification other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      useGraph,
      other.useGraph);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      nested,
      other.nested);
  }

  public FocusedNestedQuerySpecification withUseGraph(openGql.grammar.GraphExpression useGraph) {
    return new FocusedNestedQuerySpecification(useGraph, nested);
  }

  public FocusedNestedQuerySpecification withNested(openGql.grammar.ProcedureBody nested) {
    return new FocusedNestedQuerySpecification(useGraph, nested);
  }
}
