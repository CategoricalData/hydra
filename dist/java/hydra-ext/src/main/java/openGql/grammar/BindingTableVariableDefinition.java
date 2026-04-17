// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class BindingTableVariableDefinition implements Serializable, Comparable<BindingTableVariableDefinition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.BindingTableVariableDefinition");

  public static final hydra.core.Name BINDING = new hydra.core.Name("binding");

  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");

  public static final hydra.core.Name INITIALIZER = new hydra.core.Name("initializer");

  public final Boolean binding;

  public final String variable;

  public final openGql.grammar.OptTypedBindingTableInitializer initializer;

  public BindingTableVariableDefinition (Boolean binding, String variable, openGql.grammar.OptTypedBindingTableInitializer initializer) {
    this.binding = binding;
    this.variable = variable;
    this.initializer = initializer;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BindingTableVariableDefinition)) {
      return false;
    }
    BindingTableVariableDefinition o = (BindingTableVariableDefinition) other;
    return java.util.Objects.equals(
      this.binding,
      o.binding) && java.util.Objects.equals(
      this.variable,
      o.variable) && java.util.Objects.equals(
      this.initializer,
      o.initializer);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(binding) + 3 * java.util.Objects.hashCode(variable) + 5 * java.util.Objects.hashCode(initializer);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(BindingTableVariableDefinition other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      binding,
      other.binding);
    if (cmp != 0) {
      return cmp;
    }
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

  public BindingTableVariableDefinition withBinding(Boolean binding) {
    return new BindingTableVariableDefinition(binding, variable, initializer);
  }

  public BindingTableVariableDefinition withVariable(String variable) {
    return new BindingTableVariableDefinition(binding, variable, initializer);
  }

  public BindingTableVariableDefinition withInitializer(openGql.grammar.OptTypedBindingTableInitializer initializer) {
    return new BindingTableVariableDefinition(binding, variable, initializer);
  }
}
