// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ListValueTypeAlt1 implements Serializable, Comparable<ListValueTypeAlt1> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ListValueTypeAlt1");

  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("typeName");

  public static final hydra.core.Name VALUE_TYPE = new hydra.core.Name("valueType");

  public static final hydra.core.Name MAX_LENGTH = new hydra.core.Name("maxLength");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final openGql.grammar.ListValueTypeName typeName;

  public final openGql.grammar.ValueType valueType;

  public final hydra.util.Maybe<openGql.grammar.UnsignedInteger> maxLength;

  public final Boolean notNull;

  public ListValueTypeAlt1 (openGql.grammar.ListValueTypeName typeName, openGql.grammar.ValueType valueType, hydra.util.Maybe<openGql.grammar.UnsignedInteger> maxLength, Boolean notNull) {
    this.typeName = typeName;
    this.valueType = valueType;
    this.maxLength = maxLength;
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ListValueTypeAlt1)) {
      return false;
    }
    ListValueTypeAlt1 o = (ListValueTypeAlt1) other;
    return java.util.Objects.equals(
      this.typeName,
      o.typeName) && java.util.Objects.equals(
      this.valueType,
      o.valueType) && java.util.Objects.equals(
      this.maxLength,
      o.maxLength) && java.util.Objects.equals(
      this.notNull,
      o.notNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(typeName) + 3 * java.util.Objects.hashCode(valueType) + 5 * java.util.Objects.hashCode(maxLength) + 7 * java.util.Objects.hashCode(notNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ListValueTypeAlt1 other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      typeName,
      other.typeName);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      valueType,
      other.valueType);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      maxLength,
      other.maxLength);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }

  public ListValueTypeAlt1 withTypeName(openGql.grammar.ListValueTypeName typeName) {
    return new ListValueTypeAlt1(typeName, valueType, maxLength, notNull);
  }

  public ListValueTypeAlt1 withValueType(openGql.grammar.ValueType valueType) {
    return new ListValueTypeAlt1(typeName, valueType, maxLength, notNull);
  }

  public ListValueTypeAlt1 withMaxLength(hydra.util.Maybe<openGql.grammar.UnsignedInteger> maxLength) {
    return new ListValueTypeAlt1(typeName, valueType, maxLength, notNull);
  }

  public ListValueTypeAlt1 withNotNull(Boolean notNull) {
    return new ListValueTypeAlt1(typeName, valueType, maxLength, notNull);
  }
}
