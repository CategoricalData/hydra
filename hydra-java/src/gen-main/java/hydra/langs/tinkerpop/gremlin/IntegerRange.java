// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class IntegerRange implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.IntegerRange");
  
  public final hydra.langs.tinkerpop.gremlin.IntegerLiteral left;
  
  public final hydra.langs.tinkerpop.gremlin.IntegerLiteral right;
  
  public IntegerRange (hydra.langs.tinkerpop.gremlin.IntegerLiteral left, hydra.langs.tinkerpop.gremlin.IntegerLiteral right) {
    if (left == null) {
      throw new IllegalArgumentException("null value for 'left' argument");
    }
    if (right == null) {
      throw new IllegalArgumentException("null value for 'right' argument");
    }
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IntegerRange)) {
      return false;
    }
    IntegerRange o = (IntegerRange) (other);
    return left.equals(o.left) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode();
  }
  
  public IntegerRange withLeft(hydra.langs.tinkerpop.gremlin.IntegerLiteral left) {
    if (left == null) {
      throw new IllegalArgumentException("null value for 'left' argument");
    }
    return new IntegerRange(left, right);
  }
  
  public IntegerRange withRight(hydra.langs.tinkerpop.gremlin.IntegerLiteral right) {
    if (right == null) {
      throw new IllegalArgumentException("null value for 'right' argument");
    }
    return new IntegerRange(left, right);
  }
}