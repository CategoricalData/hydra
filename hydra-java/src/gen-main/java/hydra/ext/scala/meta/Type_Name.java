// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_Name implements Serializable, Comparable<Type_Name> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Type_Name");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public Type_Name (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Name)) {
      return false;
    }
    Type_Name o = (Type_Name) other;
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
  public int compareTo(Type_Name other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
