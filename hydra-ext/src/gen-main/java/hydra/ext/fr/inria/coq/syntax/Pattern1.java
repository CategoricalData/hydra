// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class Pattern1 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.Pattern1");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public static final hydra.core.Name FIELD_NAME_SCOPE = new hydra.core.Name("scope");
  
  public final hydra.ext.fr.inria.coq.syntax.Pattern0 pattern;
  
  public final hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.ScopeKey> scope;
  
  public Pattern1 (hydra.ext.fr.inria.coq.syntax.Pattern0 pattern, hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.ScopeKey> scope) {
    java.util.Objects.requireNonNull((pattern));
    java.util.Objects.requireNonNull((scope));
    this.pattern = pattern;
    this.scope = scope;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pattern1)) {
      return false;
    }
    Pattern1 o = (Pattern1) (other);
    return pattern.equals(o.pattern) && scope.equals(o.scope);
  }
  
  @Override
  public int hashCode() {
    return 2 * pattern.hashCode() + 3 * scope.hashCode();
  }
  
  public Pattern1 withPattern(hydra.ext.fr.inria.coq.syntax.Pattern0 pattern) {
    java.util.Objects.requireNonNull((pattern));
    return new Pattern1(pattern, scope);
  }
  
  public Pattern1 withScope(hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.ScopeKey> scope) {
    java.util.Objects.requireNonNull((scope));
    return new Pattern1(pattern, scope);
  }
}