package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class PropertyEquals implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.PropertyEquals");
  
  public final hydra.langs.cypher.openCypher.PropertyExpression left;
  
  public final hydra.langs.cypher.openCypher.Expression right;
  
  public PropertyEquals (hydra.langs.cypher.openCypher.PropertyExpression left, hydra.langs.cypher.openCypher.Expression right) {
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyEquals)) {
      return false;
    }
    PropertyEquals o = (PropertyEquals) (other);
    return left.equals(o.left) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode();
  }
  
  public PropertyEquals withLeft(hydra.langs.cypher.openCypher.PropertyExpression left) {
    return new PropertyEquals(left, right);
  }
  
  public PropertyEquals withRight(hydra.langs.cypher.openCypher.Expression right) {
    return new PropertyEquals(left, right);
  }
}