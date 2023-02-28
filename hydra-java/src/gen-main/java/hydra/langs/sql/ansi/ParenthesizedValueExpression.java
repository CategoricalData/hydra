package hydra.langs.sql.ansi;

public class ParenthesizedValueExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/sql/ansi.ParenthesizedValueExpression");
  
  public final hydra.langs.sql.ansi.ValueExpression value;
  
  public ParenthesizedValueExpression (hydra.langs.sql.ansi.ValueExpression value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParenthesizedValueExpression)) {
      return false;
    }
    ParenthesizedValueExpression o = (ParenthesizedValueExpression) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}