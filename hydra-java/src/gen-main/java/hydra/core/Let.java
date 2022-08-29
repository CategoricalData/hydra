package hydra.core;

/**
 * A 'let' binding
 */
public class Let<M> {
  public final hydra.core.Variable key;
  
  public final hydra.core.Term<M> value;
  
  public final hydra.core.Term<M> environment;
  
  public Let (hydra.core.Variable key, hydra.core.Term<M> value, hydra.core.Term<M> environment) {
    this.key = key;
    this.value = value;
    this.environment = environment;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Let)) {
      return false;
    }
    Let o = (Let) (other);
    return key.equals(o.key) && value.equals(o.value) && environment.equals(o.environment);
  }
  
  @Override
  public int hashCode() {
    return 2 * key.hashCode() + 3 * value.hashCode() + 5 * environment.hashCode();
  }
  
  public Let withKey(hydra.core.Variable key) {
    return new Let(key, value, environment);
  }
  
  public Let withValue(hydra.core.Term<M> value) {
    return new Let(key, value, environment);
  }
  
  public Let withEnvironment(hydra.core.Term<M> environment) {
    return new Let(key, value, environment);
  }
}