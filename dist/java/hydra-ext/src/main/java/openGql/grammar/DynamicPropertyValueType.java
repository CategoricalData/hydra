// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class DynamicPropertyValueType implements Serializable, Comparable<DynamicPropertyValueType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.DynamicPropertyValueType");

  public static final hydra.core.Name ANY = new hydra.core.Name("any");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final hydra.util.Maybe<Boolean> any;

  public final java.lang.Void property;

  public final java.lang.Void value;

  public final Boolean notNull;

  public DynamicPropertyValueType (hydra.util.Maybe<Boolean> any, java.lang.Void property, java.lang.Void value, Boolean notNull) {
    this.any = any;
    this.property = property;
    this.value = value;
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DynamicPropertyValueType)) {
      return false;
    }
    DynamicPropertyValueType o = (DynamicPropertyValueType) other;
    return java.util.Objects.equals(
      this.any,
      o.any) && java.util.Objects.equals(
      this.property,
      o.property) && java.util.Objects.equals(
      this.value,
      o.value) && java.util.Objects.equals(
      this.notNull,
      o.notNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(any) + 3 * java.util.Objects.hashCode(property) + 5 * java.util.Objects.hashCode(value) + 7 * java.util.Objects.hashCode(notNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DynamicPropertyValueType other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      any,
      other.any);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      property.hashCode(),
      other.property.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      value.hashCode(),
      other.value.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }

  public DynamicPropertyValueType withAny(hydra.util.Maybe<Boolean> any) {
    return new DynamicPropertyValueType(any, property, value, notNull);
  }

  public DynamicPropertyValueType withProperty(java.lang.Void property) {
    return new DynamicPropertyValueType(any, property, value, notNull);
  }

  public DynamicPropertyValueType withValue(java.lang.Void value) {
    return new DynamicPropertyValueType(any, property, value, notNull);
  }

  public DynamicPropertyValueType withNotNull(Boolean notNull) {
    return new DynamicPropertyValueType(any, property, value, notNull);
  }
}
