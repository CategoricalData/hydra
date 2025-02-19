// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphviz.dot;

import java.io.Serializable;

public class EqualityPair implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.graphviz.dot.EqualityPair");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.org.graphviz.dot.Id left;
  
  public final hydra.ext.org.graphviz.dot.Id right;
  
  public EqualityPair (hydra.ext.org.graphviz.dot.Id left, hydra.ext.org.graphviz.dot.Id right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((right));
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EqualityPair)) {
      return false;
    }
    EqualityPair o = (EqualityPair) (other);
    return left.equals(o.left) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode();
  }
  
  public EqualityPair withLeft(hydra.ext.org.graphviz.dot.Id left) {
    java.util.Objects.requireNonNull((left));
    return new EqualityPair(left, right);
  }
  
  public EqualityPair withRight(hydra.ext.org.graphviz.dot.Id right) {
    java.util.Objects.requireNonNull((right));
    return new EqualityPair(left, right);
  }
}