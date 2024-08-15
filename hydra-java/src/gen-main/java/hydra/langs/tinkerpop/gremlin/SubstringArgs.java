// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class SubstringArgs implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.SubstringArgs");
  
  public static final hydra.core.Name FIELD_NAME_SCOPE = new hydra.core.Name("scope");
  
  public static final hydra.core.Name FIELD_NAME_START = new hydra.core.Name("start");
  
  public static final hydra.core.Name FIELD_NAME_END = new hydra.core.Name("end");
  
  public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope;
  
  public final hydra.langs.tinkerpop.gremlin.IntegerArgument start;
  
  public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.IntegerArgument> end;
  
  public SubstringArgs (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope, hydra.langs.tinkerpop.gremlin.IntegerArgument start, hydra.util.Opt<hydra.langs.tinkerpop.gremlin.IntegerArgument> end) {
    java.util.Objects.requireNonNull((scope));
    java.util.Objects.requireNonNull((start));
    java.util.Objects.requireNonNull((end));
    this.scope = scope;
    this.start = start;
    this.end = end;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SubstringArgs)) {
      return false;
    }
    SubstringArgs o = (SubstringArgs) (other);
    return scope.equals(o.scope) && start.equals(o.start) && end.equals(o.end);
  }
  
  @Override
  public int hashCode() {
    return 2 * scope.hashCode() + 3 * start.hashCode() + 5 * end.hashCode();
  }
  
  public SubstringArgs withScope(hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> scope) {
    java.util.Objects.requireNonNull((scope));
    return new SubstringArgs(scope, start, end);
  }
  
  public SubstringArgs withStart(hydra.langs.tinkerpop.gremlin.IntegerArgument start) {
    java.util.Objects.requireNonNull((start));
    return new SubstringArgs(scope, start, end);
  }
  
  public SubstringArgs withEnd(hydra.util.Opt<hydra.langs.tinkerpop.gremlin.IntegerArgument> end) {
    java.util.Objects.requireNonNull((end));
    return new SubstringArgs(scope, start, end);
  }
}