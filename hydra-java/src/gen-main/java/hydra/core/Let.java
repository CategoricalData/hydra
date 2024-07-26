// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A set of (possibly recursive) 'let' bindings
 */
public class Let implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.Let");
  
  public final java.util.Map<hydra.core.Name, hydra.core.Term> bindings;
  
  public final hydra.core.Term environment;
  
  public Let (java.util.Map<hydra.core.Name, hydra.core.Term> bindings, hydra.core.Term environment) {
    java.util.Objects.requireNonNull((bindings));
    java.util.Objects.requireNonNull((environment));
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
  
  public Let withBindings(java.util.Map<hydra.core.Name, hydra.core.Term> bindings) {
    java.util.Objects.requireNonNull((bindings));
    return new Let(bindings, environment);
  }
  
  public Let withEnvironment(hydra.core.Term environment) {
    java.util.Objects.requireNonNull((environment));
    return new Let(bindings, environment);
  }
}