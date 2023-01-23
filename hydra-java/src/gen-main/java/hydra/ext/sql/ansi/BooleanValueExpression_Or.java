package hydra.ext.sql.ansi;

public class BooleanValueExpression_Or {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.BooleanValueExpression.Or");
  
  public final hydra.ext.sql.ansi.BooleanValueExpression lhs;
  
  public final hydra.ext.sql.ansi.BooleanTerm rhs;
  
  public BooleanValueExpression_Or (hydra.ext.sql.ansi.BooleanValueExpression lhs, hydra.ext.sql.ansi.BooleanTerm rhs) {
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
  
  public BooleanValueExpression_Or withLhs(hydra.ext.sql.ansi.BooleanValueExpression lhs) {
    return new BooleanValueExpression_Or(lhs, rhs);
  }
  
  public BooleanValueExpression_Or withRhs(hydra.ext.sql.ansi.BooleanTerm rhs) {
    return new BooleanValueExpression_Or(lhs, rhs);
  }
}