// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A case expression
 */
public class CaseExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.CaseExpression");
  
  public static final hydra.core.Name FIELD_NAME_CASE = new hydra.core.Name("case");
  
  public static final hydra.core.Name FIELD_NAME_ALTERNATIVES = new hydra.core.Name("alternatives");
  
  public final hydra.ext.haskell.ast.Expression case_;
  
  public final java.util.List<hydra.ext.haskell.ast.Alternative> alternatives;
  
  public CaseExpression (hydra.ext.haskell.ast.Expression case_, java.util.List<hydra.ext.haskell.ast.Alternative> alternatives) {
    java.util.Objects.requireNonNull((case_));
    java.util.Objects.requireNonNull((alternatives));
    this.case_ = case_;
    this.alternatives = alternatives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CaseExpression)) {
      return false;
    }
    CaseExpression o = (CaseExpression) (other);
    return case_.equals(o.case_) && alternatives.equals(o.alternatives);
  }
  
  @Override
  public int hashCode() {
    return 2 * case_.hashCode() + 3 * alternatives.hashCode();
  }
  
  public CaseExpression withCase(hydra.ext.haskell.ast.Expression case_) {
    java.util.Objects.requireNonNull((case_));
    return new CaseExpression(case_, alternatives);
  }
  
  public CaseExpression withAlternatives(java.util.List<hydra.ext.haskell.ast.Alternative> alternatives) {
    java.util.Objects.requireNonNull((alternatives));
    return new CaseExpression(case_, alternatives);
  }
}