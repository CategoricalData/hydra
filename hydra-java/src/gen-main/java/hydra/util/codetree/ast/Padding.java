package hydra.util.codetree.ast;

/**
 * Left and right padding for an operator
 */
public class Padding {
  public final hydra.util.codetree.ast.Ws left;
  
  public final hydra.util.codetree.ast.Ws right;
  
  public Padding (hydra.util.codetree.ast.Ws left, hydra.util.codetree.ast.Ws right) {
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
  
  public Padding withLeft(hydra.util.codetree.ast.Ws left) {
    return new Padding(left, right);
  }
  
  public Padding withRight(hydra.util.codetree.ast.Ws right) {
    return new Padding(left, right);
  }
}