// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class StringRange implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.StringRange");
  
  public final String left;
  
  public final String right;
  
  public StringRange (String left, String right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((right));
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringRange)) {
      return false;
    }
    StringRange o = (StringRange) (other);
    return left.equals(o.left) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode();
  }
  
  public StringRange withLeft(String left) {
    java.util.Objects.requireNonNull((left));
    return new StringRange(left, right);
  }
  
  public StringRange withRight(String right) {
    java.util.Objects.requireNonNull((right));
    return new StringRange(left, right);
  }
}