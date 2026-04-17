// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class PropertyKeyValuePair implements Serializable, Comparable<PropertyKeyValuePair> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.PropertyKeyValuePair");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String name;

  public final openGql.grammar.ValueExpression value;

  public PropertyKeyValuePair (String name, openGql.grammar.ValueExpression value) {
    this.name = name;
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyKeyValuePair)) {
      return false;
    }
    PropertyKeyValuePair o = (PropertyKeyValuePair) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PropertyKeyValuePair other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }

  public PropertyKeyValuePair withName(String name) {
    return new PropertyKeyValuePair(name, value);
  }

  public PropertyKeyValuePair withValue(openGql.grammar.ValueExpression value) {
    return new PropertyKeyValuePair(name, value);
  }
}
