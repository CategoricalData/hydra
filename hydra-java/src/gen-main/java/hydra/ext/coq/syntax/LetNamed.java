package hydra.ext.coq.syntax;

public class LetNamed {
  public final LetBinder binder;
  
  public final java.util.List<Binder> binders;
  
  public LetNamed (LetBinder binder, java.util.List<Binder> binders) {
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
  
  public LetNamed withBinder(LetBinder binder) {
    return new LetNamed(binder, binders);
  }
  
  public LetNamed withBinders(java.util.List<Binder> binders) {
    return new LetNamed(binder, binders);
  }
}