// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class PropertyEquals implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.PropertyEquals");
  
  public final hydra.langs.cypher.openCypher.PropertyExpression lhs;
  
  public final hydra.langs.cypher.openCypher.Expression rhs;
  
  public PropertyEquals (hydra.langs.cypher.openCypher.PropertyExpression lhs, hydra.langs.cypher.openCypher.Expression rhs) {
    if (lhs == null) {
      throw new IllegalArgumentException("null value for 'lhs' argument");
    }
    if (rhs == null) {
      throw new IllegalArgumentException("null value for 'rhs' argument");
    }
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyEquals)) {
      return false;
    }
    PropertyEquals o = (PropertyEquals) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public PropertyEquals withLhs(hydra.langs.cypher.openCypher.PropertyExpression lhs) {
    if (lhs == null) {
      throw new IllegalArgumentException("null value for 'lhs' argument");
    }
    return new PropertyEquals(lhs, rhs);
  }
  
  public PropertyEquals withRhs(hydra.langs.cypher.openCypher.Expression rhs) {
    if (rhs == null) {
      throw new IllegalArgumentException("null value for 'rhs' argument");
    }
    return new PropertyEquals(lhs, rhs);
  }
}