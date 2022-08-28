package hydra.ext.coq.syntax;

public class Cofix {
  public final hydra.ext.coq.syntax.CofixBody body;
  
  public final java.util.Optional<hydra.ext.coq.syntax.CofixQual> qual;
  
  public Cofix (hydra.ext.coq.syntax.CofixBody body, java.util.Optional<hydra.ext.coq.syntax.CofixQual> qual) {
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
  
  public Cofix withBody(hydra.ext.coq.syntax.CofixBody body) {
    return new Cofix(body, qual);
  }
  
  public Cofix withQual(java.util.Optional<hydra.ext.coq.syntax.CofixQual> qual) {
    return new Cofix(body, qual);
  }
}