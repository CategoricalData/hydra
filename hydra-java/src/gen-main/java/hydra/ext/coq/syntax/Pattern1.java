package hydra.ext.coq.syntax;

public class Pattern1 {
  public final hydra.ext.coq.syntax.Pattern0 pattern;
  
  public final java.util.Optional<hydra.ext.coq.syntax.ScopeKey> scope;
  
  public Pattern1 (hydra.ext.coq.syntax.Pattern0 pattern, java.util.Optional<hydra.ext.coq.syntax.ScopeKey> scope) {
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
  
  public Pattern1 withPattern(hydra.ext.coq.syntax.Pattern0 pattern) {
    return new Pattern1(pattern, scope);
  }
  
  public Pattern1 withScope(java.util.Optional<hydra.ext.coq.syntax.ScopeKey> scope) {
    return new Pattern1(pattern, scope);
  }
}