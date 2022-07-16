package hydra.ext.haskell.ast;

public class Variable {
  public final Name value;
  
  public Variable (Name value) {
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