// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class VariantTypeParameters implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.VariantTypeParameters");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.ext.csharp.syntax.VariantTypeParameter> value;
  
  public VariantTypeParameters (java.util.List<hydra.ext.csharp.syntax.VariantTypeParameter> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariantTypeParameters)) {
      return false;
    }
    VariantTypeParameters o = (VariantTypeParameters) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}