package hydra.ext.haskell.ast;

public class RightHandSide {
  public final Expression value;
  
  public RightHandSide (Expression value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RightHandSide)) {
      return false;
    }
    RightHandSide o = (RightHandSide) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}