// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A set of (possibly recursive) 'let' bindings together with an environment in which they are bound
 */
public class Let implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.Let");
  
  public static final hydra.core.Name FIELD_NAME_BINDINGS = new hydra.core.Name("bindings");
  
  public static final hydra.core.Name FIELD_NAME_ENVIRONMENT = new hydra.core.Name("environment");
  
  public final java.util.List<hydra.core.LetBinding> bindings;
  
  public final hydra.core.Term environment;
  
  public Let (java.util.List<hydra.core.LetBinding> bindings, hydra.core.Term environment) {
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
  
  public Let withBindings(java.util.List<hydra.core.LetBinding> bindings) {
    java.util.Objects.requireNonNull((bindings));
    return new Let(bindings, environment);
  }
  
  public Let withEnvironment(hydra.core.Term environment) {
    java.util.Objects.requireNonNull((environment));
    return new Let(bindings, environment);
  }
}