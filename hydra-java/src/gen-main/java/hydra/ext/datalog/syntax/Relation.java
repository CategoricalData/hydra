package hydra.ext.datalog.syntax;

public class Relation {
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