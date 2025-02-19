// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class TPrimaryAndGenexp implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.TPrimaryAndGenexp");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY = new hydra.core.Name("primary");
  
  public static final hydra.core.Name FIELD_NAME_GENEXP = new hydra.core.Name("genexp");
  
  public final hydra.ext.python.syntax.TPrimary primary;
  
  public final hydra.ext.python.syntax.Genexp genexp;
  
  public TPrimaryAndGenexp (hydra.ext.python.syntax.TPrimary primary, hydra.ext.python.syntax.Genexp genexp) {
    java.util.Objects.requireNonNull((primary));
    java.util.Objects.requireNonNull((genexp));
    this.primary = primary;
    this.genexp = genexp;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TPrimaryAndGenexp)) {
      return false;
    }
    TPrimaryAndGenexp o = (TPrimaryAndGenexp) (other);
    return primary.equals(o.primary) && genexp.equals(o.genexp);
  }
  
  @Override
  public int hashCode() {
    return 2 * primary.hashCode() + 3 * genexp.hashCode();
  }
  
  public TPrimaryAndGenexp withPrimary(hydra.ext.python.syntax.TPrimary primary) {
    java.util.Objects.requireNonNull((primary));
    return new TPrimaryAndGenexp(primary, genexp);
  }
  
  public TPrimaryAndGenexp withGenexp(hydra.ext.python.syntax.Genexp genexp) {
    java.util.Objects.requireNonNull((genexp));
    return new TPrimaryAndGenexp(primary, genexp);
  }
}