package hydra.ext.coq.syntax;

public class Cofix {
  public final CofixBody body;
  
  public final java.util.Optional<CofixQual> qual;
  
  public Cofix (CofixBody body, java.util.Optional<CofixQual> qual) {
    this.body = body;
    this.qual = qual;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Cofix)) {
      return false;
    }
    Cofix o = (Cofix) (other);
    return body.equals(o.body) && qual.equals(o.qual);
  }
  
  @Override
  public int hashCode() {
    return 2 * body.hashCode() + 3 * qual.hashCode();
  }
  
  public Cofix withBody(CofixBody body) {
    return new Cofix(body, qual);
  }
  
  public Cofix withQual(java.util.Optional<CofixQual> qual) {
    return new Cofix(body, qual);
  }
}