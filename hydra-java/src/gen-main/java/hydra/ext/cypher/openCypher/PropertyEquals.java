// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class PropertyEquals implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.PropertyEquals");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.cypher.openCypher.PropertyExpression lhs;
  
  public final hydra.ext.cypher.openCypher.Expression rhs;
  
  public PropertyEquals (hydra.ext.cypher.openCypher.PropertyExpression lhs, hydra.ext.cypher.openCypher.Expression rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
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
  
  public PropertyEquals withLhs(hydra.ext.cypher.openCypher.PropertyExpression lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new PropertyEquals(lhs, rhs);
  }
  
  public PropertyEquals withRhs(hydra.ext.cypher.openCypher.Expression rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new PropertyEquals(lhs, rhs);
  }
}