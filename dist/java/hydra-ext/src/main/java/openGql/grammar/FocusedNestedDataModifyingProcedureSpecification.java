// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class FocusedNestedDataModifyingProcedureSpecification implements Serializable, Comparable<FocusedNestedDataModifyingProcedureSpecification> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.FocusedNestedDataModifyingProcedureSpecification");

  public static final hydra.core.Name USE_GRAPH = new hydra.core.Name("useGraph");

  public static final hydra.core.Name NESTED_SPEC = new hydra.core.Name("nestedSpec");

  public final openGql.grammar.GraphExpression useGraph;

  public final openGql.grammar.ProcedureBody nestedSpec;

  public FocusedNestedDataModifyingProcedureSpecification (openGql.grammar.GraphExpression useGraph, openGql.grammar.ProcedureBody nestedSpec) {
    this.useGraph = useGraph;
    this.nestedSpec = nestedSpec;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FocusedNestedDataModifyingProcedureSpecification)) {
      return false;
    }
    FocusedNestedDataModifyingProcedureSpecification o = (FocusedNestedDataModifyingProcedureSpecification) other;
    return java.util.Objects.equals(
      this.useGraph,
      o.useGraph) && java.util.Objects.equals(
      this.nestedSpec,
      o.nestedSpec);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(useGraph) + 3 * java.util.Objects.hashCode(nestedSpec);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FocusedNestedDataModifyingProcedureSpecification other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      useGraph,
      other.useGraph);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      nestedSpec,
      other.nestedSpec);
  }

  public FocusedNestedDataModifyingProcedureSpecification withUseGraph(openGql.grammar.GraphExpression useGraph) {
    return new FocusedNestedDataModifyingProcedureSpecification(useGraph, nestedSpec);
  }

  public FocusedNestedDataModifyingProcedureSpecification withNestedSpec(openGql.grammar.ProcedureBody nestedSpec) {
    return new FocusedNestedDataModifyingProcedureSpecification(useGraph, nestedSpec);
  }
}
