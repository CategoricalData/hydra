// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class ListComprehension implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/openCypher.ListComprehension");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.ext.cypher.openCypher.FilterExpression left;
  
  public final hydra.util.Opt<hydra.ext.cypher.openCypher.Expression> right;
  
  public ListComprehension (hydra.ext.cypher.openCypher.FilterExpression left, hydra.util.Opt<hydra.ext.cypher.openCypher.Expression> right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((right));
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ListComprehension)) {
      return false;
    }
    ListComprehension o = (ListComprehension) (other);
    return left.equals(o.left) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode();
  }
  
  public ListComprehension withLeft(hydra.ext.cypher.openCypher.FilterExpression left) {
    java.util.Objects.requireNonNull((left));
    return new ListComprehension(left, right);
  }
  
  public ListComprehension withRight(hydra.util.Opt<hydra.ext.cypher.openCypher.Expression> right) {
    java.util.Objects.requireNonNull((right));
    return new ListComprehension(left, right);
  }
}
