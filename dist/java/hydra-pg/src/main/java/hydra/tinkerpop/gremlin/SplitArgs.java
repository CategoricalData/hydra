// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public class SplitArgs implements Serializable, Comparable<SplitArgs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.SplitArgs");

  public static final hydra.core.Name SCOPE = new hydra.core.Name("scope");

  public static final hydra.core.Name DELIMITER = new hydra.core.Name("delimiter");

  public final hydra.util.Maybe<hydra.tinkerpop.gremlin.TraversalScopeArgument> scope;

  public final hydra.tinkerpop.gremlin.StringNullableArgument delimiter;

  public SplitArgs (hydra.util.Maybe<hydra.tinkerpop.gremlin.TraversalScopeArgument> scope, hydra.tinkerpop.gremlin.StringNullableArgument delimiter) {
    this.scope = scope;
    this.delimiter = delimiter;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SplitArgs)) {
      return false;
    }
    SplitArgs o = (SplitArgs) other;
    return java.util.Objects.equals(
      this.scope,
      o.scope) && java.util.Objects.equals(
      this.delimiter,
      o.delimiter);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(scope) + 3 * java.util.Objects.hashCode(delimiter);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SplitArgs other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      scope,
      other.scope);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      delimiter,
      other.delimiter);
  }

  public SplitArgs withScope(hydra.util.Maybe<hydra.tinkerpop.gremlin.TraversalScopeArgument> scope) {
    return new SplitArgs(scope, delimiter);
  }

  public SplitArgs withDelimiter(hydra.tinkerpop.gremlin.StringNullableArgument delimiter) {
    return new SplitArgs(scope, delimiter);
  }
}
