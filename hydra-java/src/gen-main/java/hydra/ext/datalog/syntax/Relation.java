package hydra.ext.datalog.syntax;

public class Relation {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/datalog/syntax.Relation");
  
  public final String value;
  
  public Relation (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Relation)) {
      return false;
    }
    Relation o = (Relation) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}