package hydra.ext.sql.ansi;

public class TableConstraintDefinition {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.TableConstraintDefinition");
  
  public TableConstraintDefinition () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TableConstraintDefinition)) {
      return false;
    }
    TableConstraintDefinition o = (TableConstraintDefinition) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}