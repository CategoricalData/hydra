// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class HasTraversalTokenArgs implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.HasTraversalTokenArgs");
  
  public static final hydra.core.Name FIELD_NAME_TRAVERSAL_TOKEN = new hydra.core.Name("traversalToken");
  
  public static final hydra.core.Name FIELD_NAME_REST = new hydra.core.Name("rest");
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalTokenArgument traversalToken;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.HasTraversalTokenArgsRest rest;
  
  public HasTraversalTokenArgs (hydra.ext.org.apache.tinkerpop.gremlin.TraversalTokenArgument traversalToken, hydra.ext.org.apache.tinkerpop.gremlin.HasTraversalTokenArgsRest rest) {
    java.util.Objects.requireNonNull((traversalToken));
    java.util.Objects.requireNonNull((rest));
    this.traversalToken = traversalToken;
    this.rest = rest;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof HasTraversalTokenArgs)) {
      return false;
    }
    HasTraversalTokenArgs o = (HasTraversalTokenArgs) (other);
    return traversalToken.equals(o.traversalToken) && rest.equals(o.rest);
  }
  
  @Override
  public int hashCode() {
    return 2 * traversalToken.hashCode() + 3 * rest.hashCode();
  }
  
  public HasTraversalTokenArgs withTraversalToken(hydra.ext.org.apache.tinkerpop.gremlin.TraversalTokenArgument traversalToken) {
    java.util.Objects.requireNonNull((traversalToken));
    return new HasTraversalTokenArgs(traversalToken, rest);
  }
  
  public HasTraversalTokenArgs withRest(hydra.ext.org.apache.tinkerpop.gremlin.HasTraversalTokenArgsRest rest) {
    java.util.Objects.requireNonNull((rest));
    return new HasTraversalTokenArgs(traversalToken, rest);
  }
}