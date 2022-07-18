package hydra.ext.datalog.syntax;

public class Variable {
  public final String value;
  
  public Variable (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Variable)) {
      return false;
    }
    Variable o = (Variable) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}