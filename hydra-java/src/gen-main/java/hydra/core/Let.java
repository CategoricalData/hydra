package hydra.core;

/**
 * A set of (possibly recursive) 'let' bindings
 */
public class Let<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.Let");
  
  public final java.util.Map<hydra.core.Name, hydra.core.Term<M>> bindings;
  
  public final hydra.core.Term<M> environment;
  
  public Let (java.util.Map<hydra.core.Name, hydra.core.Term<M>> bindings, hydra.core.Term<M> environment) {
    this.bindings = bindings;
    this.environment = environment;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Let)) {
      return false;
    }
    Let o = (Let) (other);
    return bindings.equals(o.bindings) && environment.equals(o.environment);
  }
  
  @Override
  public int hashCode() {
    return 2 * bindings.hashCode() + 3 * environment.hashCode();
  }
  
  public Let withBindings(java.util.Map<hydra.core.Name, hydra.core.Term<M>> bindings) {
    return new Let(bindings, environment);
  }
  
  public Let withEnvironment(hydra.core.Term<M> environment) {
    return new Let(bindings, environment);
  }
}