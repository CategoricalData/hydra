// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public class TailArgs implements Serializable, Comparable<TailArgs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.TailArgs");

  public static final hydra.core.Name SCOPE = new hydra.core.Name("scope");

  public static final hydra.core.Name INTEGER = new hydra.core.Name("integer");

  public final hydra.util.Maybe<hydra.tinkerpop.gremlin.TraversalScopeArgument> scope;

  public final hydra.util.Maybe<hydra.tinkerpop.gremlin.IntegerArgument> integer;

  public TailArgs (hydra.util.Maybe<hydra.tinkerpop.gremlin.TraversalScopeArgument> scope, hydra.util.Maybe<hydra.tinkerpop.gremlin.IntegerArgument> integer) {
    this.scope = scope;
    this.integer = integer;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TailArgs)) {
      return false;
    }
    TailArgs o = (TailArgs) other;
    return java.util.Objects.equals(
      this.scope,
      o.scope) && java.util.Objects.equals(
      this.integer,
      o.integer);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(scope) + 3 * java.util.Objects.hashCode(integer);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TailArgs other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      scope,
      other.scope);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      integer,
      other.integer);
  }

  public TailArgs withScope(hydra.util.Maybe<hydra.tinkerpop.gremlin.TraversalScopeArgument> scope) {
    return new TailArgs(scope, integer);
  }

  public TailArgs withInteger(hydra.util.Maybe<hydra.tinkerpop.gremlin.IntegerArgument> integer) {
    return new TailArgs(scope, integer);
  }
}
