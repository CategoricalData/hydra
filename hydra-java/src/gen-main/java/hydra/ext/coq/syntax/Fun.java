package hydra.ext.coq.syntax;

public class Fun {
  public final hydra.ext.coq.syntax.OpenBinders binders;
  
  public final hydra.ext.coq.syntax.Term body;
  
  public Fun (hydra.ext.coq.syntax.OpenBinders binders, hydra.ext.coq.syntax.Term body) {
    this.binders = binders;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Fun)) {
      return false;
    }
    Fun o = (Fun) (other);
    return binders.equals(o.binders) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * binders.hashCode() + 3 * body.hashCode();
  }
  
  public Fun withBinders(hydra.ext.coq.syntax.OpenBinders binders) {
    return new Fun(binders, body);
  }
  
  public Fun withBody(hydra.ext.coq.syntax.Term body) {
    return new Fun(binders, body);
  }
}