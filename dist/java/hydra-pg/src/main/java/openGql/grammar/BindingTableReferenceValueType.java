// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class BindingTableReferenceValueType implements Serializable, Comparable<BindingTableReferenceValueType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.BindingTableReferenceValueType");

  public static final hydra.core.Name BINDING_TABLE_TYPE = new hydra.core.Name("bindingTableType");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final openGql.grammar.BindingTableType bindingTableType;

  public final Boolean notNull;

  public BindingTableReferenceValueType (openGql.grammar.BindingTableType bindingTableType, Boolean notNull) {
    this.bindingTableType = bindingTableType;
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BindingTableReferenceValueType)) {
      return false;
    }
    BindingTableReferenceValueType o = (BindingTableReferenceValueType) other;
    return java.util.Objects.equals(
      this.bindingTableType,
      o.bindingTableType) && java.util.Objects.equals(
      this.notNull,
      o.notNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(bindingTableType) + 3 * java.util.Objects.hashCode(notNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(BindingTableReferenceValueType other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      bindingTableType,
      other.bindingTableType);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }

  public BindingTableReferenceValueType withBindingTableType(openGql.grammar.BindingTableType bindingTableType) {
    return new BindingTableReferenceValueType(bindingTableType, notNull);
  }

  public BindingTableReferenceValueType withNotNull(Boolean notNull) {
    return new BindingTableReferenceValueType(bindingTableType, notNull);
  }
}
