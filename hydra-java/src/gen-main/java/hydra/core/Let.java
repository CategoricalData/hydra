package hydra.core;

/**
 * A 'let' binding
 */
public class Let<A> {
  public final hydra.core.Variable key;
  
  public final hydra.core.Term<A> value;
  
  public final hydra.core.Term<A> environment;
  
  /**
   * Constructs an immutable Let object
   */
  public Let(hydra.core.Variable key, hydra.core.Term<A> value, hydra.core.Term<A> environment) {
    this.key = key;
    this.value = value;
    this.environment = environment;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Let)) {
        return false;
    }
    Let o = (Let) other;
    return key.equals(o.key)
        && value.equals(o.value)
        && environment.equals(o.environment);
  }
  
  @Override
  public int hashCode() {
    return 2 * key.hashCode()
        + 3 * value.hashCode()
        + 5 * environment.hashCode();
  }
  
  /**
   * Construct a new immutable Let object in which key is overridden
   */
  public Let withKey(hydra.core.Variable key) {
    return new Let(key, value, environment);
  }
  
  /**
   * Construct a new immutable Let object in which value is overridden
   */
  public Let withValue(hydra.core.Term<A> value) {
    return new Let(key, value, environment);
  }
  
  /**
   * Construct a new immutable Let object in which environment is overridden
   */
  public Let withEnvironment(hydra.core.Term<A> environment) {
    return new Let(key, value, environment);
  }
}
