// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A case expression
 */
public class CaseExpression implements Serializable, Comparable<CaseExpression> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.CaseExpression");
  
  public static final hydra.core.Name FIELD_NAME_CASE = new hydra.core.Name("case");
  
  public static final hydra.core.Name FIELD_NAME_ALTERNATIVES = new hydra.core.Name("alternatives");
  
  /**
   * The expression being matched
   */
  public final hydra.ext.haskell.ast.Expression case_;
  
  /**
   * The pattern-matching alternatives
   */
  public final java.util.List<hydra.ext.haskell.ast.Alternative> alternatives;
  
  public CaseExpression (hydra.ext.haskell.ast.Expression case_, java.util.List<hydra.ext.haskell.ast.Alternative> alternatives) {
    this.case_ = case_;
    this.alternatives = alternatives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CaseExpression)) {
      return false;
    }
    CaseExpression o = (CaseExpression) (other);
    return java.util.Objects.equals(
      this.case_,
      o.case_) && java.util.Objects.equals(
      this.alternatives,
      o.alternatives);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(case_) + 3 * java.util.Objects.hashCode(alternatives);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CaseExpression other) {
    int cmp = 0;
    cmp = ((Comparable) (case_)).compareTo(other.case_);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      alternatives.hashCode(),
      other.alternatives.hashCode());
  }
  
  public CaseExpression withCase(hydra.ext.haskell.ast.Expression case_) {
    return new CaseExpression(case_, alternatives);
  }
  
  public CaseExpression withAlternatives(java.util.List<hydra.ext.haskell.ast.Alternative> alternatives) {
    return new CaseExpression(case_, alternatives);
  }
}
