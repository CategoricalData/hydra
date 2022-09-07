package hydra.ext.coq.syntax;

public class Ident {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Ident");
  
  public final hydra.ext.coq.syntax.String_ value;
  
  public Ident (hydra.ext.coq.syntax.String_ value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Ident)) {
      return false;
    }
    Ident o = (Ident) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}