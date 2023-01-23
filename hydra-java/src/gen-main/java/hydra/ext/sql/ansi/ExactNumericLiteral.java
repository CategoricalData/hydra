package hydra.ext.sql.ansi;

public class ExactNumericLiteral {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.ExactNumericLiteral");
  
  public final String value;
  
  public ExactNumericLiteral (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExactNumericLiteral)) {
      return false;
    }
    ExactNumericLiteral o = (ExactNumericLiteral) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}