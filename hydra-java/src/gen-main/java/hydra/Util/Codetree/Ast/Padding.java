package hydra.util.codetree.ast;

/**
 * Left and right padding for an operator
 */
public class Padding {
  public final Ws left;
  
  public final Ws right;
  
  public Padding (Ws left, Ws right) {
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Padding)) {
      return false;
    }
    Padding o = (Padding) (other);
    return left.equals(o.left) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode();
  }
  
  public Padding withLeft(Ws left) {
    return new Padding(left, right);
  }
  
  public Padding withRight(Ws right) {
    return new Padding(left, right);
  }
}