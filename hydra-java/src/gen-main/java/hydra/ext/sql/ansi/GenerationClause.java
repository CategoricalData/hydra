package hydra.ext.sql.ansi;

public class GenerationClause {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.GenerationClause");
  
  public GenerationClause () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GenerationClause)) {
      return false;
    }
    GenerationClause o = (GenerationClause) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}