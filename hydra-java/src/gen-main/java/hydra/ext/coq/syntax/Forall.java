package hydra.ext.coq.syntax;

public class Forall {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Forall");
  
  public final hydra.ext.coq.syntax.OpenBinders binders;
  
  public final hydra.ext.coq.syntax.Type type;
  
  public Forall (hydra.ext.coq.syntax.OpenBinders binders, hydra.ext.coq.syntax.Type type) {
    this.binders = binders;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Forall)) {
      return false;
    }
    Forall o = (Forall) (other);
    return binders.equals(o.binders) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * binders.hashCode() + 3 * type.hashCode();
  }
  
  public Forall withBinders(hydra.ext.coq.syntax.OpenBinders binders) {
    return new Forall(binders, type);
  }
  
  public Forall withType(hydra.ext.coq.syntax.Type type) {
    return new Forall(binders, type);
  }
}