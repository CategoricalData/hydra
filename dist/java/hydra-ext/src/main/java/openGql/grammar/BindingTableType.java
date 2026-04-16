// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class BindingTableType implements Serializable, Comparable<BindingTableType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.BindingTableType");

  public static final hydra.core.Name BINDING = new hydra.core.Name("binding");

  public static final hydra.core.Name FIELD_TYPES = new hydra.core.Name("fieldTypes");

  public final Boolean binding;

  public final hydra.util.Maybe<java.util.List<openGql.grammar.FieldType>> fieldTypes;

  public BindingTableType (Boolean binding, hydra.util.Maybe<java.util.List<openGql.grammar.FieldType>> fieldTypes) {
    this.binding = binding;
    this.fieldTypes = fieldTypes;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BindingTableType)) {
      return false;
    }
    BindingTableType o = (BindingTableType) other;
    return java.util.Objects.equals(
      this.binding,
      o.binding) && java.util.Objects.equals(
      this.fieldTypes,
      o.fieldTypes);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(binding) + 3 * java.util.Objects.hashCode(fieldTypes);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(BindingTableType other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      binding,
      other.binding);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      fieldTypes,
      other.fieldTypes);
  }

  public BindingTableType withBinding(Boolean binding) {
    return new BindingTableType(binding, fieldTypes);
  }

  public BindingTableType withFieldTypes(hydra.util.Maybe<java.util.List<openGql.grammar.FieldType>> fieldTypes) {
    return new BindingTableType(binding, fieldTypes);
  }
}
