// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_AnonymousName implements Serializable, Comparable<Type_AnonymousName> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Type_AnonymousName");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.lang.Void value;

  public Type_AnonymousName (java.lang.Void value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_AnonymousName)) {
      return false;
    }
    Type_AnonymousName o = (Type_AnonymousName) other;
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
  public int compareTo(Type_AnonymousName other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
