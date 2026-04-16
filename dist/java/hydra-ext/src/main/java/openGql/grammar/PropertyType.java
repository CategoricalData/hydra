// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class PropertyType implements Serializable, Comparable<PropertyType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.PropertyType");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TYPED = new hydra.core.Name("typed");

  public static final hydra.core.Name VALUE_TYPE = new hydra.core.Name("valueType");

  public final String name;

  public final hydra.util.Maybe<java.lang.Void> typed;

  public final openGql.grammar.ValueType valueType;

  public PropertyType (String name, hydra.util.Maybe<java.lang.Void> typed, openGql.grammar.ValueType valueType) {
    this.name = name;
    this.typed = typed;
    this.valueType = valueType;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyType)) {
      return false;
    }
    PropertyType o = (PropertyType) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.typed,
      o.typed) && java.util.Objects.equals(
      this.valueType,
      o.valueType);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(typed) + 5 * java.util.Objects.hashCode(valueType);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PropertyType other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      typed,
      other.typed);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      valueType,
      other.valueType);
  }

  public PropertyType withName(String name) {
    return new PropertyType(name, typed, valueType);
  }

  public PropertyType withTyped(hydra.util.Maybe<java.lang.Void> typed) {
    return new PropertyType(name, typed, valueType);
  }

  public PropertyType withValueType(openGql.grammar.ValueType valueType) {
    return new PropertyType(name, typed, valueType);
  }
}
