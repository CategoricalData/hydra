package hydra.ext.coq.syntax;

public class Pattern10_Patterns {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Pattern10.Patterns");
  
  public final hydra.ext.coq.syntax.Pattern1 pattern;
  
  public final java.util.List<hydra.ext.coq.syntax.Pattern1> patterns;
  
  public Pattern10_Patterns (hydra.ext.coq.syntax.Pattern1 pattern, java.util.List<hydra.ext.coq.syntax.Pattern1> patterns) {
    this.pattern = pattern;
    this.patterns = patterns;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pattern10_Patterns)) {
      return false;
    }
    Pattern10_Patterns o = (Pattern10_Patterns) (other);
    return pattern.equals(o.pattern) && patterns.equals(o.patterns);
  }
  
  @Override
  public int hashCode() {
    return 2 * pattern.hashCode() + 3 * patterns.hashCode();
  }
  
  public Pattern10_Patterns withPattern(hydra.ext.coq.syntax.Pattern1 pattern) {
    return new Pattern10_Patterns(pattern, patterns);
  }
  
  public Pattern10_Patterns withPatterns(java.util.List<hydra.ext.coq.syntax.Pattern1> patterns) {
    return new Pattern10_Patterns(pattern, patterns);
  }
}