// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shex.syntax;

import java.io.Serializable;

public class PnameNs implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/shex/syntax.PnameNs");
  
  public final hydra.util.Opt<hydra.langs.shex.syntax.PnPrefix> value;
  
  public PnameNs (hydra.util.Opt<hydra.langs.shex.syntax.PnPrefix> value) {
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PnameNs)) {
      return false;
    }
    PnameNs o = (PnameNs) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}