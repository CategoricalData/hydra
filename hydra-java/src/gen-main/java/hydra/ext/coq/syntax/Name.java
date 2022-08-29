package hydra.ext.coq.syntax;

public class Name {
  public final java.util.Optional<hydra.ext.coq.syntax.Ident> value;
  
  public Name (java.util.Optional<hydra.ext.coq.syntax.Ident> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Name)) {
      return false;
    }
    Name o = (Name) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}