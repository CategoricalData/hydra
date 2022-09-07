package hydra.ext.coq.syntax;

public class Equation {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Equation");
  
  public final java.util.List<java.util.List<hydra.ext.coq.syntax.Pattern>> pattern;
  
  public final hydra.ext.coq.syntax.Term term;
  
  public Equation (java.util.List<java.util.List<hydra.ext.coq.syntax.Pattern>> pattern, hydra.ext.coq.syntax.Term term) {
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
  
  public Equation withPattern(java.util.List<java.util.List<hydra.ext.coq.syntax.Pattern>> pattern) {
    return new Equation(pattern, term);
  }
  
  public Equation withTerm(hydra.ext.coq.syntax.Term term) {
    return new Equation(pattern, term);
  }
}