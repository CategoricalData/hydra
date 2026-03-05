// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Classes
 */
public class Class_ implements Serializable, Comparable<Class_> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.Class");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final java.lang.Void value;
  
  public Class_ (java.lang.Void value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Class_)) {
      return false;
    }
    Class_ o = (Class_) other;
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
  public int compareTo(Class_ other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
