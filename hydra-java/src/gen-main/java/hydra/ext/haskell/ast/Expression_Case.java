package hydra.ext.haskell.ast;

/**
 * A case expression
 */
public class Expression_Case {
  public final Expression case_;
  
  public final java.util.List<Alternative> alternatives;
  
  public Expression_Case (Expression case_, java.util.List<Alternative> alternatives) {
    this.case_ = case_;
    this.alternatives = alternatives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Expression_Case)) {
      return false;
    }
    Expression_Case o = (Expression_Case) (other);
    return case_.equals(o.case_) && alternatives.equals(o.alternatives);
  }
  
  @Override
  public int hashCode() {
    return 2 * case_.hashCode() + 3 * alternatives.hashCode();
  }
  
  public Expression_Case withCase(Expression case_) {
    return new Expression_Case(case_, alternatives);
  }
  
  public Expression_Case withAlternatives(java.util.List<Alternative> alternatives) {
    return new Expression_Case(case_, alternatives);
  }
}