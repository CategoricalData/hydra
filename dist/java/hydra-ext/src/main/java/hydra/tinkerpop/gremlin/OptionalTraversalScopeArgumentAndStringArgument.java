// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public class OptionalTraversalScopeArgumentAndStringArgument implements Serializable, Comparable<OptionalTraversalScopeArgumentAndStringArgument> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndStringArgument");

  public static final hydra.core.Name SCOPE = new hydra.core.Name("scope");

  public static final hydra.core.Name STRING = new hydra.core.Name("string");

  public final hydra.util.Maybe<hydra.tinkerpop.gremlin.TraversalScopeArgument> scope;

  public final hydra.tinkerpop.gremlin.StringArgument string;

  public OptionalTraversalScopeArgumentAndStringArgument (hydra.util.Maybe<hydra.tinkerpop.gremlin.TraversalScopeArgument> scope, hydra.tinkerpop.gremlin.StringArgument string) {
    this.scope = scope;
    this.string = string;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OptionalTraversalScopeArgumentAndStringArgument)) {
      return false;
    }
    OptionalTraversalScopeArgumentAndStringArgument o = (OptionalTraversalScopeArgumentAndStringArgument) other;
    return java.util.Objects.equals(
      this.scope,
      o.scope) && java.util.Objects.equals(
      this.string,
      o.string);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(scope) + 3 * java.util.Objects.hashCode(string);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(OptionalTraversalScopeArgumentAndStringArgument other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      scope,
      other.scope);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      string,
      other.string);
  }

  public OptionalTraversalScopeArgumentAndStringArgument withScope(hydra.util.Maybe<hydra.tinkerpop.gremlin.TraversalScopeArgument> scope) {
    return new OptionalTraversalScopeArgumentAndStringArgument(scope, string);
  }

  public OptionalTraversalScopeArgumentAndStringArgument withString(hydra.tinkerpop.gremlin.StringArgument string) {
    return new OptionalTraversalScopeArgumentAndStringArgument(scope, string);
  }
}
