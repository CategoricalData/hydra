package hydra.ext.sql.ansi;

public class LeftBracketOrTrigraph {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.LeftBracketOrTrigraph");
  
  public final String value;
  
  public LeftBracketOrTrigraph (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LeftBracketOrTrigraph)) {
      return false;
    }
    LeftBracketOrTrigraph o = (LeftBracketOrTrigraph) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}