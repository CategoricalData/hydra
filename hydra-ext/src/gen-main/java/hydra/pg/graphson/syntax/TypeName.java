// Note: this is an automatically generated file. Do not edit.

package hydra.pg.graphson.syntax;

import java.io.Serializable;

public class TypeName implements Serializable, Comparable<TypeName> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.graphson.syntax.TypeName");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public TypeName (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeName)) {
      return false;
    }
    TypeName o = (TypeName) other;
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
  public int compareTo(TypeName other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
