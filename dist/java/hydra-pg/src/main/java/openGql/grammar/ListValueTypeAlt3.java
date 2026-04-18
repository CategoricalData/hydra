// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ListValueTypeAlt3 implements Serializable, Comparable<ListValueTypeAlt3> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ListValueTypeAlt3");

  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("typeName");

  public static final hydra.core.Name MAX_LENGTH = new hydra.core.Name("maxLength");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final openGql.grammar.ListValueTypeName typeName;

  public final hydra.util.Maybe<openGql.grammar.UnsignedInteger> maxLength;

  public final Boolean notNull;

  public ListValueTypeAlt3 (openGql.grammar.ListValueTypeName typeName, hydra.util.Maybe<openGql.grammar.UnsignedInteger> maxLength, Boolean notNull) {
    this.typeName = typeName;
    this.maxLength = maxLength;
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ListValueTypeAlt3)) {
      return false;
    }
    ListValueTypeAlt3 o = (ListValueTypeAlt3) other;
    return java.util.Objects.equals(
      this.typeName,
      o.typeName) && java.util.Objects.equals(
      this.maxLength,
      o.maxLength) && java.util.Objects.equals(
      this.notNull,
      o.notNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(typeName) + 3 * java.util.Objects.hashCode(maxLength) + 5 * java.util.Objects.hashCode(notNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ListValueTypeAlt3 other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      typeName,
      other.typeName);
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

  public ListValueTypeAlt3 withTypeName(openGql.grammar.ListValueTypeName typeName) {
    return new ListValueTypeAlt3(typeName, maxLength, notNull);
  }

  public ListValueTypeAlt3 withMaxLength(hydra.util.Maybe<openGql.grammar.UnsignedInteger> maxLength) {
    return new ListValueTypeAlt3(typeName, maxLength, notNull);
  }

  public ListValueTypeAlt3 withNotNull(Boolean notNull) {
    return new ListValueTypeAlt3(typeName, maxLength, notNull);
  }
}
