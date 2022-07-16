package hydra.ext.coq.syntax;

public class Pattern10_As {
  public final Pattern1 pattern;
  
  public final Name as;
  
  public Pattern10_As (Pattern1 pattern, Name as) {
    this.pattern = pattern;
    this.as = as;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pattern10_As)) {
      return false;
    }
    Pattern10_As o = (Pattern10_As) (other);
    return pattern.equals(o.pattern) && as.equals(o.as);
  }
  
  @Override
  public int hashCode() {
    return 2 * pattern.hashCode() + 3 * as.hashCode();
  }
  
  public Pattern10_As withPattern(Pattern1 pattern) {
    return new Pattern10_As(pattern, as);
  }
  
  public Pattern10_As withAs(Name as) {
    return new Pattern10_As(pattern, as);
  }
}