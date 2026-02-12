// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ItemsPattern implements Serializable, Comparable<ItemsPattern> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ItemsPattern");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.ext.python.syntax.KeyValuePattern> value;
  
  public ItemsPattern (java.util.List<hydra.ext.python.syntax.KeyValuePattern> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ItemsPattern)) {
      return false;
    }
    ItemsPattern o = (ItemsPattern) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ItemsPattern other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
