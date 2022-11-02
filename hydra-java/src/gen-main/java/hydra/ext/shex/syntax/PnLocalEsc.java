package hydra.ext.shex.syntax;

public class PnLocalEsc {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.PnLocalEsc");
  
  public final String regex;
  
  public PnLocalEsc (String regex) {
    this.regex = regex;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PnLocalEsc)) {
      return false;
    }
    PnLocalEsc o = (PnLocalEsc) (other);
    return regex.equals(o.regex);
  }
  
  @Override
  public int hashCode() {
    return 2 * regex.hashCode();
  }
}