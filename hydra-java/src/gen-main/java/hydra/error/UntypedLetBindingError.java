// Note: this is an automatically generated file. Do not edit.

package hydra.error;

import java.io.Serializable;

/**
 * A let binding without a type annotation
 */
public class UntypedLetBindingError implements Serializable, Comparable<UntypedLetBindingError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.UntypedLetBindingError");
  
  public static final hydra.core.Name BINDING = new hydra.core.Name("binding");
  
  /**
   * The untyped binding
   */
  public final hydra.core.Binding binding;
  
  public UntypedLetBindingError (hydra.core.Binding binding) {
    this.binding = binding;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UntypedLetBindingError)) {
      return false;
    }
    UntypedLetBindingError o = (UntypedLetBindingError) other;
    return java.util.Objects.equals(
      this.binding,
      o.binding);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(binding);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UntypedLetBindingError other) {
    return ((Comparable) binding).compareTo(other.binding);
  }
}
