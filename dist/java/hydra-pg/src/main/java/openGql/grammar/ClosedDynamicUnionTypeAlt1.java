// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ClosedDynamicUnionTypeAlt1 implements Serializable, Comparable<ClosedDynamicUnionTypeAlt1> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ClosedDynamicUnionTypeAlt1");

  public static final hydra.core.Name ANY_VALUE = new hydra.core.Name("anyValue");

  public static final hydra.core.Name VALUE_TYPES = new hydra.core.Name("valueTypes");

  public final hydra.util.Maybe<Boolean> anyValue;

  public final java.util.List<openGql.grammar.ValueType> valueTypes;

  public ClosedDynamicUnionTypeAlt1 (hydra.util.Maybe<Boolean> anyValue, java.util.List<openGql.grammar.ValueType> valueTypes) {
    this.anyValue = anyValue;
    this.valueTypes = valueTypes;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ClosedDynamicUnionTypeAlt1)) {
      return false;
    }
    ClosedDynamicUnionTypeAlt1 o = (ClosedDynamicUnionTypeAlt1) other;
    return java.util.Objects.equals(
      this.anyValue,
      o.anyValue) && java.util.Objects.equals(
      this.valueTypes,
      o.valueTypes);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(anyValue) + 3 * java.util.Objects.hashCode(valueTypes);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ClosedDynamicUnionTypeAlt1 other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      anyValue,
      other.anyValue);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      valueTypes,
      other.valueTypes);
  }

  public ClosedDynamicUnionTypeAlt1 withAnyValue(hydra.util.Maybe<Boolean> anyValue) {
    return new ClosedDynamicUnionTypeAlt1(anyValue, valueTypes);
  }

  public ClosedDynamicUnionTypeAlt1 withValueTypes(java.util.List<openGql.grammar.ValueType> valueTypes) {
    return new ClosedDynamicUnionTypeAlt1(anyValue, valueTypes);
  }
}
