package hydra.ext.coq.syntax;

public class Fun {
  public final OpenBinders binders;
  
  public final Term body;
  
  public Fun (OpenBinders binders, Term body) {
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
  
  public Fun withBinders(OpenBinders binders) {
    return new Fun(binders, body);
  }
  
  public Fun withBody(Term body) {
    return new Fun(binders, body);
  }
}