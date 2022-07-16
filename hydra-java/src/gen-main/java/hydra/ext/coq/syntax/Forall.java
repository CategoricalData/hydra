package hydra.ext.coq.syntax;

public class Forall {
  public final OpenBinders binders;
  
  public final Type type;
  
  public Forall (OpenBinders binders, Type type) {
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
  
  public Forall withBinders(OpenBinders binders) {
    return new Forall(binders, type);
  }
  
  public Forall withType(Type type) {
    return new Forall(binders, type);
  }
}