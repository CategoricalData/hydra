// Note: this is an automatically generated file. Do not edit.

package hydra.error;

import java.io.Serializable;

/**
 * A reference to a term (element, binding, or primitive) that is not defined
 */
public class UndefinedTermError implements Serializable, Comparable<UndefinedTermError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.UndefinedTermError");
  
  public static final hydra.core.Name NAME = new hydra.core.Name("name");
  
  /**
   * The name of the undefined term
   */
  public final hydra.core.Name name;
  
  public UndefinedTermError (hydra.core.Name name) {
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UndefinedTermError)) {
      return false;
    }
    UndefinedTermError o = (UndefinedTermError) other;
    return java.util.Objects.equals(
      this.name,
      o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UndefinedTermError other) {
    return ((Comparable) name).compareTo(other.name);
  }
}
