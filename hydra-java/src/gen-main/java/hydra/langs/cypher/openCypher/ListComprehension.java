package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class ListComprehension implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.ListComprehension");
  
  public final hydra.langs.cypher.openCypher.FilterExpression left;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Expression> right;
  
  public ListComprehension (hydra.langs.cypher.openCypher.FilterExpression left, java.util.Optional<hydra.langs.cypher.openCypher.Expression> right) {
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
  
  public ListComprehension withLeft(hydra.langs.cypher.openCypher.FilterExpression left) {
    return new ListComprehension(left, right);
  }
  
  public ListComprehension withRight(java.util.Optional<hydra.langs.cypher.openCypher.Expression> right) {
    return new ListComprehension(left, right);
  }
}