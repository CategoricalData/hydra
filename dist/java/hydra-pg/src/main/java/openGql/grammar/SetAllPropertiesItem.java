// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SetAllPropertiesItem implements Serializable, Comparable<SetAllPropertiesItem> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SetAllPropertiesItem");

  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");

  public static final hydra.core.Name PROPERTIES = new hydra.core.Name("properties");

  public final String variable;

  public final hydra.util.Maybe<java.util.List<openGql.grammar.PropertyKeyValuePair>> properties;

  public SetAllPropertiesItem (String variable, hydra.util.Maybe<java.util.List<openGql.grammar.PropertyKeyValuePair>> properties) {
    this.variable = variable;
    this.properties = properties;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SetAllPropertiesItem)) {
      return false;
    }
    SetAllPropertiesItem o = (SetAllPropertiesItem) other;
    return java.util.Objects.equals(
      this.variable,
      o.variable) && java.util.Objects.equals(
      this.properties,
      o.properties);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(variable) + 3 * java.util.Objects.hashCode(properties);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SetAllPropertiesItem other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      variable,
      other.variable);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      properties,
      other.properties);
  }

  public SetAllPropertiesItem withVariable(String variable) {
    return new SetAllPropertiesItem(variable, properties);
  }

  public SetAllPropertiesItem withProperties(hydra.util.Maybe<java.util.List<openGql.grammar.PropertyKeyValuePair>> properties) {
    return new SetAllPropertiesItem(variable, properties);
  }
}
