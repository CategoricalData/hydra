package hydra.ext.coq.syntax;

public class Ident {
  public final String_ value;
  
  public Ident (String_ value) {
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