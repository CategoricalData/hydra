package hydra.core;

/**
 * A 'let' binding
 */
public class Let<M> {
  public final Variable key;
  
  public final Term<M> value;
  
  public final Term<M> environment;
  
  public Let (Variable key, Term<M> value, Term<M> environment) {
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
  
  public Let withKey(Variable key) {
    return new Let(key, value, environment);
  }
  
  public Let withValue(Term<M> value) {
    return new Let(key, value, environment);
  }
  
  public Let withEnvironment(Term<M> environment) {
    return new Let(key, value, environment);
  }
}