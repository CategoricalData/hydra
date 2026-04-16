// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class FieldType implements Serializable, Comparable<FieldType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.FieldType");

  public static final hydra.core.Name FIELD_NAME = new hydra.core.Name("fieldName");

  public static final hydra.core.Name TYPED = new hydra.core.Name("typed");

  public static final hydra.core.Name VALUE_TYPE = new hydra.core.Name("valueType");

  public final String fieldName;

  public final hydra.util.Maybe<java.lang.Void> typed;

  public final openGql.grammar.ValueType valueType;

  public FieldType (String fieldName, hydra.util.Maybe<java.lang.Void> typed, openGql.grammar.ValueType valueType) {
    this.fieldName = fieldName;
    this.typed = typed;
    this.valueType = valueType;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldType)) {
      return false;
    }
    FieldType o = (FieldType) other;
    return java.util.Objects.equals(
      this.fieldName,
      o.fieldName) && java.util.Objects.equals(
      this.typed,
      o.typed) && java.util.Objects.equals(
      this.valueType,
      o.valueType);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(fieldName) + 3 * java.util.Objects.hashCode(typed) + 5 * java.util.Objects.hashCode(valueType);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FieldType other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      fieldName,
      other.fieldName);
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

  public FieldType withFieldName(String fieldName) {
    return new FieldType(fieldName, typed, valueType);
  }

  public FieldType withTyped(hydra.util.Maybe<java.lang.Void> typed) {
    return new FieldType(fieldName, typed, valueType);
  }

  public FieldType withValueType(openGql.grammar.ValueType valueType) {
    return new FieldType(fieldName, typed, valueType);
  }
}
