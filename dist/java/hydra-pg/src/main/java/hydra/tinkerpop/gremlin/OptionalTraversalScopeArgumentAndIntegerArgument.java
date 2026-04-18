// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public class OptionalTraversalScopeArgumentAndIntegerArgument implements Serializable, Comparable<OptionalTraversalScopeArgumentAndIntegerArgument> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument");

  public static final hydra.core.Name SCOPE = new hydra.core.Name("scope");

  public static final hydra.core.Name LONG = new hydra.core.Name("long");

  public final hydra.util.Maybe<hydra.tinkerpop.gremlin.TraversalScopeArgument> scope;

  public final hydra.tinkerpop.gremlin.IntegerArgument long_;

  public OptionalTraversalScopeArgumentAndIntegerArgument (hydra.util.Maybe<hydra.tinkerpop.gremlin.TraversalScopeArgument> scope, hydra.tinkerpop.gremlin.IntegerArgument long_) {
    this.scope = scope;
    this.long_ = long_;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OptionalTraversalScopeArgumentAndIntegerArgument)) {
      return false;
    }
    OptionalTraversalScopeArgumentAndIntegerArgument o = (OptionalTraversalScopeArgumentAndIntegerArgument) other;
    return java.util.Objects.equals(
      this.scope,
      o.scope) && java.util.Objects.equals(
      this.long_,
      o.long_);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(scope) + 3 * java.util.Objects.hashCode(long_);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(OptionalTraversalScopeArgumentAndIntegerArgument other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      scope,
      other.scope);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      long_,
      other.long_);
  }

  public OptionalTraversalScopeArgumentAndIntegerArgument withScope(hydra.util.Maybe<hydra.tinkerpop.gremlin.TraversalScopeArgument> scope) {
    return new OptionalTraversalScopeArgumentAndIntegerArgument(scope, long_);
  }

  public OptionalTraversalScopeArgumentAndIntegerArgument withLong(hydra.tinkerpop.gremlin.IntegerArgument long_) {
    return new OptionalTraversalScopeArgumentAndIntegerArgument(scope, long_);
  }
}
