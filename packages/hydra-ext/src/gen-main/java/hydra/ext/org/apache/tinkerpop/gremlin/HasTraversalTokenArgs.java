// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class HasTraversalTokenArgs implements Serializable, Comparable<HasTraversalTokenArgs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.HasTraversalTokenArgs");

  public static final hydra.core.Name TRAVERSAL_TOKEN = new hydra.core.Name("traversalToken");

  public static final hydra.core.Name REST = new hydra.core.Name("rest");

  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalTokenArgument traversalToken;

  public final hydra.ext.org.apache.tinkerpop.gremlin.HasTraversalTokenArgsRest rest;

  public HasTraversalTokenArgs (hydra.ext.org.apache.tinkerpop.gremlin.TraversalTokenArgument traversalToken, hydra.ext.org.apache.tinkerpop.gremlin.HasTraversalTokenArgsRest rest) {
    this.traversalToken = traversalToken;
    this.rest = rest;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof HasTraversalTokenArgs)) {
      return false;
    }
    HasTraversalTokenArgs o = (HasTraversalTokenArgs) other;
    return java.util.Objects.equals(
      this.traversalToken,
      o.traversalToken) && java.util.Objects.equals(
      this.rest,
      o.rest);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(traversalToken) + 3 * java.util.Objects.hashCode(rest);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(HasTraversalTokenArgs other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      traversalToken,
      other.traversalToken);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      rest,
      other.rest);
  }

  public HasTraversalTokenArgs withTraversalToken(hydra.ext.org.apache.tinkerpop.gremlin.TraversalTokenArgument traversalToken) {
    return new HasTraversalTokenArgs(traversalToken, rest);
  }

  public HasTraversalTokenArgs withRest(hydra.ext.org.apache.tinkerpop.gremlin.HasTraversalTokenArgsRest rest) {
    return new HasTraversalTokenArgs(traversalToken, rest);
  }
}
