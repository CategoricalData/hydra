// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class AnonymousIndividual implements Serializable, Comparable<AnonymousIndividual> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.AnonymousIndividual");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final java.lang.Void value;
  
  public AnonymousIndividual (java.lang.Void value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnonymousIndividual)) {
      return false;
    }
    AnonymousIndividual o = (AnonymousIndividual) other;
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
  public int compareTo(AnonymousIndividual other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
