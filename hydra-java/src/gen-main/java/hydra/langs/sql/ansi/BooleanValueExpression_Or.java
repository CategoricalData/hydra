// Note: this is an automatically generated file. Do not edit.

package hydra.langs.sql.ansi;

import java.io.Serializable;

public class BooleanValueExpression_Or implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/sql/ansi.BooleanValueExpression.Or");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.langs.sql.ansi.BooleanValueExpression lhs;
  
  public final hydra.langs.sql.ansi.BooleanTerm rhs;
  
  public BooleanValueExpression_Or (hydra.langs.sql.ansi.BooleanValueExpression lhs, hydra.langs.sql.ansi.BooleanTerm rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BooleanValueExpression_Or)) {
      return false;
    }
    BooleanValueExpression_Or o = (BooleanValueExpression_Or) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public BooleanValueExpression_Or withLhs(hydra.langs.sql.ansi.BooleanValueExpression lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new BooleanValueExpression_Or(lhs, rhs);
  }
  
  public BooleanValueExpression_Or withRhs(hydra.langs.sql.ansi.BooleanTerm rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new BooleanValueExpression_Or(lhs, rhs);
  }
}