package hydra.ext.coq.syntax;

public class LetNamed {
  public final hydra.ext.coq.syntax.LetBinder binder;
  
  public final java.util.List<hydra.ext.coq.syntax.Binder> binders;
  
  public LetNamed (hydra.ext.coq.syntax.LetBinder binder, java.util.List<hydra.ext.coq.syntax.Binder> binders) {
    this.binder = binder;
    this.binders = binders;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LetNamed)) {
      return false;
    }
    LetNamed o = (LetNamed) (other);
    return binder.equals(o.binder) && binders.equals(o.binders);
  }
  
  @Override
  public int hashCode() {
    return 2 * binder.hashCode() + 3 * binders.hashCode();
  }
  
  public LetNamed withBinder(hydra.ext.coq.syntax.LetBinder binder) {
    return new LetNamed(binder, binders);
  }
  
  public LetNamed withBinders(java.util.List<hydra.ext.coq.syntax.Binder> binders) {
    return new LetNamed(binder, binders);
  }
}