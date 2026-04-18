// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class OpenDynamicUnionType implements Serializable, Comparable<OpenDynamicUnionType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.OpenDynamicUnionType");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final Boolean value;

  public final Boolean notNull;

  public OpenDynamicUnionType (Boolean value, Boolean notNull) {
    this.value = value;
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OpenDynamicUnionType)) {
      return false;
    }
    OpenDynamicUnionType o = (OpenDynamicUnionType) other;
    return java.util.Objects.equals(
      this.value,
      o.value) && java.util.Objects.equals(
      this.notNull,
      o.notNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value) + 3 * java.util.Objects.hashCode(notNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(OpenDynamicUnionType other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      value,
      other.value);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }

  public OpenDynamicUnionType withValue(Boolean value) {
    return new OpenDynamicUnionType(value, notNull);
  }

  public OpenDynamicUnionType withNotNull(Boolean notNull) {
    return new OpenDynamicUnionType(value, notNull);
  }
}
