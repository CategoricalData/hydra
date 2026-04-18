// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ValueVariableDefinition implements Serializable, Comparable<ValueVariableDefinition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ValueVariableDefinition");

  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");

  public static final hydra.core.Name INITIALIZER = new hydra.core.Name("initializer");

  public final String variable;

  public final openGql.grammar.OptTypedValueInitializer initializer;

  public ValueVariableDefinition (String variable, openGql.grammar.OptTypedValueInitializer initializer) {
    this.variable = variable;
    this.initializer = initializer;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ValueVariableDefinition)) {
      return false;
    }
    ValueVariableDefinition o = (ValueVariableDefinition) other;
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
  public int compareTo(ValueVariableDefinition other) {
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

  public ValueVariableDefinition withVariable(String variable) {
    return new ValueVariableDefinition(variable, initializer);
  }

  public ValueVariableDefinition withInitializer(openGql.grammar.OptTypedValueInitializer initializer) {
    return new ValueVariableDefinition(variable, initializer);
  }
}
