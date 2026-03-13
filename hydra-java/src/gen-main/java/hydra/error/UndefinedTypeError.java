// Note: this is an automatically generated file. Do not edit.

package hydra.error;

import java.io.Serializable;

/**
 * A reference to a type or type variable that is not defined
 */
public class UndefinedTypeError implements Serializable, Comparable<UndefinedTypeError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.UndefinedTypeError");
  
  public static final hydra.core.Name NAME = new hydra.core.Name("name");
  
  /**
   * The name of the undefined type
   */
  public final hydra.core.Name name;
  
  public UndefinedTypeError (hydra.core.Name name) {
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UndefinedTypeError)) {
      return false;
    }
    UndefinedTypeError o = (UndefinedTypeError) other;
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
  public int compareTo(UndefinedTypeError other) {
    return ((Comparable) name).compareTo(other.name);
  }
}
