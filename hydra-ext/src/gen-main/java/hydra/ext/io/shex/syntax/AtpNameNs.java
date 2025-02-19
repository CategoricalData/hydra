// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class AtpNameNs implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.io.shex.syntax.AtpNameNs");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.util.Opt<hydra.ext.io.shex.syntax.PnPrefix> value;
  
  public AtpNameNs (hydra.util.Opt<hydra.ext.io.shex.syntax.PnPrefix> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AtpNameNs)) {
      return false;
    }
    AtpNameNs o = (AtpNameNs) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}