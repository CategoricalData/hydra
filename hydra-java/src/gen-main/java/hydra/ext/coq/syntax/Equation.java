package hydra.ext.coq.syntax;

public class Equation {
  /**
   * Note: list cannot be empty
   */
  public final java.util.List<java.util.List<Pattern>> pattern;
  
  public final Term term;
  
  public Equation (java.util.List<java.util.List<Pattern>> pattern, Term term) {
    this.pattern = pattern;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Equation)) {
      return false;
    }
    Equation o = (Equation) (other);
    return pattern.equals(o.pattern) && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * pattern.hashCode() + 3 * term.hashCode();
  }
  
  public Equation withPattern(java.util.List<java.util.List<Pattern>> pattern) {
    return new Equation(pattern, term);
  }
  
  public Equation withTerm(Term term) {
    return new Equation(pattern, term);
  }
}