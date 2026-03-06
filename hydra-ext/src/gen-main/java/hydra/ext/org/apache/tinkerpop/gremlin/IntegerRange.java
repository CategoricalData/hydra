// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class IntegerRange implements Serializable, Comparable<IntegerRange> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.IntegerRange");
  
  public static final hydra.core.Name LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.IntegerLiteral left;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.IntegerLiteral right;
  
  public IntegerRange (hydra.ext.org.apache.tinkerpop.gremlin.IntegerLiteral left, hydra.ext.org.apache.tinkerpop.gremlin.IntegerLiteral right) {
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IntegerRange)) {
      return false;
    }
    IntegerRange o = (IntegerRange) other;
    return java.util.Objects.equals(
      this.left,
      o.left) && java.util.Objects.equals(
      this.right,
      o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(left) + 3 * java.util.Objects.hashCode(right);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(IntegerRange other) {
    int cmp = 0;
    cmp = ((Comparable) left).compareTo(other.left);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) right).compareTo(other.right);
  }
  
  public IntegerRange withLeft(hydra.ext.org.apache.tinkerpop.gremlin.IntegerLiteral left) {
    return new IntegerRange(left, right);
  }
  
  public IntegerRange withRight(hydra.ext.org.apache.tinkerpop.gremlin.IntegerLiteral right) {
    return new IntegerRange(left, right);
  }
}
