// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class GraphVariableDefinition implements Serializable, Comparable<GraphVariableDefinition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.GraphVariableDefinition");

  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");

  public static final hydra.core.Name INITIALIZER = new hydra.core.Name("initializer");

  public final String variable;

  public final openGql.grammar.OptTypedGraphInitializer initializer;

  public GraphVariableDefinition (String variable, openGql.grammar.OptTypedGraphInitializer initializer) {
    this.variable = variable;
    this.initializer = initializer;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GraphVariableDefinition)) {
      return false;
    }
    GraphVariableDefinition o = (GraphVariableDefinition) other;
    return java.util.Objects.equals(
      this.variable,
      o.variable) && java.util.Objects.equals(
      this.initializer,
      o.initializer);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(variable) + 3 * java.util.Objects.hashCode(initializer);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(GraphVariableDefinition other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      variable,
      other.variable);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      initializer,
      other.initializer);
  }

  public GraphVariableDefinition withVariable(String variable) {
    return new GraphVariableDefinition(variable, initializer);
  }

  public GraphVariableDefinition withInitializer(openGql.grammar.OptTypedGraphInitializer initializer) {
    return new GraphVariableDefinition(variable, initializer);
  }
}
