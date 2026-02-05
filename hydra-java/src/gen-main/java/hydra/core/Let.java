// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A set of (possibly recursive) 'let' bindings together with a body in which they are bound
 */
public class Let implements Serializable, Comparable<Let> {
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
    this.bindings = bindings;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Let)) {
      return false;
    }
    Let o = (Let) (other);
    return java.util.Objects.equals(
      this.bindings,
      o.bindings) && java.util.Objects.equals(
      this.body,
      o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(bindings) + 3 * java.util.Objects.hashCode(body);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Let other) {
    int cmp = 0;
    cmp = Integer.compare(
      bindings.hashCode(),
      other.bindings.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (body)).compareTo(other.body);
  }
  
  public Let withBindings(java.util.List<hydra.core.Binding> bindings) {
    return new Let(bindings, body);
  }
  
  public Let withBody(hydra.core.Term body) {
    return new Let(bindings, body);
  }
}
