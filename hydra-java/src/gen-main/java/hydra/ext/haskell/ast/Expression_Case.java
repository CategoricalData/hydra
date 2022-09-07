package hydra.ext.haskell.ast;

/**
 * A case expression
 */
public class Expression_Case {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.Expression.Case");
  
  public final hydra.ext.haskell.ast.Expression case_;
  
  public final java.util.List<hydra.ext.haskell.ast.Alternative> alternatives;
  
  public Expression_Case (hydra.ext.haskell.ast.Expression case_, java.util.List<hydra.ext.haskell.ast.Alternative> alternatives) {
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
  
  public Expression_Case withCase(hydra.ext.haskell.ast.Expression case_) {
    return new Expression_Case(case_, alternatives);
  }
  
  public Expression_Case withAlternatives(java.util.List<hydra.ext.haskell.ast.Alternative> alternatives) {
    return new Expression_Case(case_, alternatives);
  }
}