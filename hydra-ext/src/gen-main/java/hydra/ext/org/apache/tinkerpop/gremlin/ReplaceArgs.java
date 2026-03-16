// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class ReplaceArgs implements Serializable, Comparable<ReplaceArgs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ReplaceArgs");
  
  public static final hydra.core.Name SCOPE = new hydra.core.Name("scope");
  
  public static final hydra.core.Name FROM = new hydra.core.Name("from");
  
  public static final hydra.core.Name TO = new hydra.core.Name("to");
  
  public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> scope;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument from;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument to;
  
  public ReplaceArgs (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> scope, hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument from, hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument to) {
    this.scope = scope;
    this.from = from;
    this.to = to;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ReplaceArgs)) {
      return false;
    }
    ReplaceArgs o = (ReplaceArgs) other;
    return java.util.Objects.equals(
      this.scope,
      o.scope) && java.util.Objects.equals(
      this.from,
      o.from) && java.util.Objects.equals(
      this.to,
      o.to);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(scope) + 3 * java.util.Objects.hashCode(from) + 5 * java.util.Objects.hashCode(to);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ReplaceArgs other) {
    int cmp = 0;
    cmp = ((Comparable) scope).compareTo(other.scope);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) from).compareTo(other.from);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) to).compareTo(other.to);
  }
  
  public ReplaceArgs withScope(hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> scope) {
    return new ReplaceArgs(scope, from, to);
  }
  
  public ReplaceArgs withFrom(hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument from) {
    return new ReplaceArgs(scope, from, to);
  }
  
  public ReplaceArgs withTo(hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument to) {
    return new ReplaceArgs(scope, from, to);
  }
}
