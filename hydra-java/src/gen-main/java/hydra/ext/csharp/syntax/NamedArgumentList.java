// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class NamedArgumentList implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.NamedArgumentList");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.ext.csharp.syntax.NamedArgument> value;
  
  public NamedArgumentList (java.util.List<hydra.ext.csharp.syntax.NamedArgument> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NamedArgumentList)) {
      return false;
    }
    NamedArgumentList o = (NamedArgumentList) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}