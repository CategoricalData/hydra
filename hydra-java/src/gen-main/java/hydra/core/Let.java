// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A set of (possibly recursive) 'let' bindings together with a body in which they are bound
 */
public class Let implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.Let");
  
  public static final hydra.core.Name FIELD_NAME_BINDINGS = new hydra.core.Name("bindings");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  /**
   * The list of variable bindings
   */
  public final java.util.List<hydra.core.Binding> bindings;
  
  /**
   * The body term in which the variables are bound
   */
  public final hydra.core.Term body;
  
  public Let (java.util.List<hydra.core.Binding> bindings, hydra.core.Term body) {
    java.util.Objects.requireNonNull((bindings));
    java.util.Objects.requireNonNull((body));
    this.bindings = bindings;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Let)) {
      return false;
    }
    Let o = (Let) (other);
    return bindings.equals(o.bindings) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * bindings.hashCode() + 3 * body.hashCode();
  }
  
  public Let withBindings(java.util.List<hydra.core.Binding> bindings) {
    java.util.Objects.requireNonNull((bindings));
    return new Let(bindings, body);
  }
  
  public Let withBody(hydra.core.Term body) {
    java.util.Objects.requireNonNull((body));
    return new Let(bindings, body);
  }
}
