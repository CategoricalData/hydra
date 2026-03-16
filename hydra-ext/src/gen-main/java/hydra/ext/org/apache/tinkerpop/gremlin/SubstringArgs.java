// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class SubstringArgs implements Serializable, Comparable<SubstringArgs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.SubstringArgs");
  
  public static final hydra.core.Name SCOPE = new hydra.core.Name("scope");
  
  public static final hydra.core.Name START = new hydra.core.Name("start");
  
  public static final hydra.core.Name END = new hydra.core.Name("end");
  
  public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> scope;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.IntegerArgument start;
  
  public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.IntegerArgument> end;
  
  public SubstringArgs (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> scope, hydra.ext.org.apache.tinkerpop.gremlin.IntegerArgument start, hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.IntegerArgument> end) {
    this.scope = scope;
    this.start = start;
    this.end = end;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SubstringArgs)) {
      return false;
    }
    SubstringArgs o = (SubstringArgs) other;
    return java.util.Objects.equals(
      this.scope,
      o.scope) && java.util.Objects.equals(
      this.start,
      o.start) && java.util.Objects.equals(
      this.end,
      o.end);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(scope) + 3 * java.util.Objects.hashCode(start) + 5 * java.util.Objects.hashCode(end);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SubstringArgs other) {
    int cmp = 0;
    cmp = ((Comparable) scope).compareTo(other.scope);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) start).compareTo(other.start);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) end).compareTo(other.end);
  }
  
  public SubstringArgs withScope(hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> scope) {
    return new SubstringArgs(scope, start, end);
  }
  
  public SubstringArgs withStart(hydra.ext.org.apache.tinkerpop.gremlin.IntegerArgument start) {
    return new SubstringArgs(scope, start, end);
  }
  
  public SubstringArgs withEnd(hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.IntegerArgument> end) {
    return new SubstringArgs(scope, start, end);
  }
}
