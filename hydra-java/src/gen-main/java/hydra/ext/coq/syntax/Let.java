package hydra.ext.coq.syntax;

/**
 * A let-in definition
 */
public class Let {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Let");
  
  public final hydra.ext.coq.syntax.LetBindings bindings;
  
  public final hydra.ext.coq.syntax.Term in;
  
  public Let (hydra.ext.coq.syntax.LetBindings bindings, hydra.ext.coq.syntax.Term in) {
    this.bindings = bindings;
    this.in = in;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Let)) {
      return false;
    }
    Let o = (Let) (other);
    return bindings.equals(o.bindings) && in.equals(o.in);
  }
  
  @Override
  public int hashCode() {
    return 2 * bindings.hashCode() + 3 * in.hashCode();
  }
  
  public Let withBindings(hydra.ext.coq.syntax.LetBindings bindings) {
    return new Let(bindings, in);
  }
  
  public Let withIn(hydra.ext.coq.syntax.Term in) {
    return new Let(bindings, in);
  }
}