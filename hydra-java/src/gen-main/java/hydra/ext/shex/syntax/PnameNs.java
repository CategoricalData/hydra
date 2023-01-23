package hydra.ext.shex.syntax;

public class PnameNs {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.PnameNs");
  
  public final java.util.Optional<hydra.ext.shex.syntax.PnPrefix> value;
  
  public PnameNs (java.util.Optional<hydra.ext.shex.syntax.PnPrefix> value) {
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