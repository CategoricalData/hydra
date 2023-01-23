package hydra.ext.sql.ansi;

public class RowValueSpecialCase {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.RowValueSpecialCase");
  
  public final hydra.ext.sql.ansi.NonparenthesizedValueExpressionPrimary value;
  
  public RowValueSpecialCase (hydra.ext.sql.ansi.NonparenthesizedValueExpressionPrimary value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RowValueSpecialCase)) {
      return false;
    }
    RowValueSpecialCase o = (RowValueSpecialCase) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}