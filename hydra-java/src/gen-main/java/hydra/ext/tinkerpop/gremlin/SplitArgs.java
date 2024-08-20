// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.gremlin;

import java.io.Serializable;

public class SplitArgs implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/gremlin.SplitArgs");
  
  public static final hydra.core.Name FIELD_NAME_SCOPE = new hydra.core.Name("scope");
  
  public static final hydra.core.Name FIELD_NAME_DELIMITER = new hydra.core.Name("delimiter");
  
  public final hydra.util.Opt<hydra.ext.tinkerpop.gremlin.TraversalScopeArgument> scope;
  
  public final hydra.ext.tinkerpop.gremlin.StringNullableArgument delimiter;
  
  public SplitArgs (hydra.util.Opt<hydra.ext.tinkerpop.gremlin.TraversalScopeArgument> scope, hydra.ext.tinkerpop.gremlin.StringNullableArgument delimiter) {
    java.util.Objects.requireNonNull((scope));
    java.util.Objects.requireNonNull((delimiter));
    this.scope = scope;
    this.delimiter = delimiter;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SplitArgs)) {
      return false;
    }
    SplitArgs o = (SplitArgs) (other);
    return scope.equals(o.scope) && delimiter.equals(o.delimiter);
  }
  
  @Override
  public int hashCode() {
    return 2 * scope.hashCode() + 3 * delimiter.hashCode();
  }
  
  public SplitArgs withScope(hydra.util.Opt<hydra.ext.tinkerpop.gremlin.TraversalScopeArgument> scope) {
    java.util.Objects.requireNonNull((scope));
    return new SplitArgs(scope, delimiter);
  }
  
  public SplitArgs withDelimiter(hydra.ext.tinkerpop.gremlin.StringNullableArgument delimiter) {
    java.util.Objects.requireNonNull((delimiter));
    return new SplitArgs(scope, delimiter);
  }
}
