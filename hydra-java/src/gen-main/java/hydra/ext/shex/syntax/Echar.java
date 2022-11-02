package hydra.ext.shex.syntax;

public class Echar {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.Echar");
  
  public final String regex;
  
  public Echar (String regex) {
    this.regex = regex;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Echar)) {
      return false;
    }
    Echar o = (Echar) (other);
    return regex.equals(o.regex);
  }
  
  @Override
  public int hashCode() {
    return 2 * regex.hashCode();
  }
}