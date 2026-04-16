// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SetPropertyItem implements Serializable, Comparable<SetPropertyItem> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SetPropertyItem");

  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");

  public static final hydra.core.Name PROPERTY_NAME = new hydra.core.Name("propertyName");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String variable;

  public final String propertyName;

  public final openGql.grammar.ValueExpression value;

  public SetPropertyItem (String variable, String propertyName, openGql.grammar.ValueExpression value) {
    this.variable = variable;
    this.propertyName = propertyName;
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SetPropertyItem)) {
      return false;
    }
    SetPropertyItem o = (SetPropertyItem) other;
    return java.util.Objects.equals(
      this.variable,
      o.variable) && java.util.Objects.equals(
      this.propertyName,
      o.propertyName) && java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(variable) + 3 * java.util.Objects.hashCode(propertyName) + 5 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SetPropertyItem other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      variable,
      other.variable);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      propertyName,
      other.propertyName);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }

  public SetPropertyItem withVariable(String variable) {
    return new SetPropertyItem(variable, propertyName, value);
  }

  public SetPropertyItem withPropertyName(String propertyName) {
    return new SetPropertyItem(variable, propertyName, value);
  }

  public SetPropertyItem withValue(openGql.grammar.ValueExpression value) {
    return new SetPropertyItem(variable, propertyName, value);
  }
}
