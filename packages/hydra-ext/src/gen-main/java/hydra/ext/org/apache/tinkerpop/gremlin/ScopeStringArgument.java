// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class ScopeStringArgument implements Serializable, Comparable<ScopeStringArgument> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ScopeStringArgument");

  public static final hydra.core.Name SCOPE = new hydra.core.Name("scope");

  public static final hydra.core.Name STRINGS = new hydra.core.Name("strings");

  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument scope;

  public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> strings;

  public ScopeStringArgument (hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument scope, java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> strings) {
    this.scope = scope;
    this.strings = strings;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ScopeStringArgument)) {
      return false;
    }
    ScopeStringArgument o = (ScopeStringArgument) other;
    return java.util.Objects.equals(
      this.scope,
      o.scope) && java.util.Objects.equals(
      this.strings,
      o.strings);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(scope) + 3 * java.util.Objects.hashCode(strings);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ScopeStringArgument other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      scope,
      other.scope);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      strings,
      other.strings);
  }

  public ScopeStringArgument withScope(hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument scope) {
    return new ScopeStringArgument(scope, strings);
  }

  public ScopeStringArgument withStrings(java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> strings) {
    return new ScopeStringArgument(scope, strings);
  }
}
