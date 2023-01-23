package hydra.ext.shex.syntax;

public class AtpNameNs {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.AtpNameNs");
  
  public final java.util.Optional<hydra.ext.shex.syntax.PnPrefix> value;
  
  public AtpNameNs (java.util.Optional<hydra.ext.shex.syntax.PnPrefix> value) {
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