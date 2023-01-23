package hydra.ext.sql.ansi;

public class NewSpecification {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.NewSpecification");
  
  public NewSpecification () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NewSpecification)) {
      return false;
    }
    NewSpecification o = (NewSpecification) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}