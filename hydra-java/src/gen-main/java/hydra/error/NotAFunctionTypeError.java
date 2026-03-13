// Note: this is an automatically generated file. Do not edit.

package hydra.error;

import java.io.Serializable;

/**
 * A type that is not a function type when one was expected in an application
 */
public class NotAFunctionTypeError implements Serializable, Comparable<NotAFunctionTypeError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.NotAFunctionTypeError");
  
  public static final hydra.core.Name TYPE = new hydra.core.Name("type");
  
  /**
   * The actual type encountered
   */
  public final hydra.core.Type type;
  
  public NotAFunctionTypeError (hydra.core.Type type) {
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NotAFunctionTypeError)) {
      return false;
    }
    NotAFunctionTypeError o = (NotAFunctionTypeError) other;
    return java.util.Objects.equals(
      this.type,
      o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(type);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NotAFunctionTypeError other) {
    return ((Comparable) type).compareTo(other.type);
  }
}
