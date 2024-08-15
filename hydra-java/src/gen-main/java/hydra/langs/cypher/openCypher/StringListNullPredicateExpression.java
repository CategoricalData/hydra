// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class StringListNullPredicateExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.StringListNullPredicateExpression");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public final hydra.langs.cypher.openCypher.AddOrSubtractExpression left;
  
  public final java.util.List<hydra.langs.cypher.openCypher.StringListNullPredicateRightHandSide> right;
  
  public StringListNullPredicateExpression (hydra.langs.cypher.openCypher.AddOrSubtractExpression left, java.util.List<hydra.langs.cypher.openCypher.StringListNullPredicateRightHandSide> right) {
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((right));
    this.left = left;
    this.right = right;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringListNullPredicateExpression)) {
      return false;
    }
    StringListNullPredicateExpression o = (StringListNullPredicateExpression) (other);
    return left.equals(o.left) && right.equals(o.right);
  }
  
  @Override
  public int hashCode() {
    return 2 * left.hashCode() + 3 * right.hashCode();
  }
  
  public StringListNullPredicateExpression withLeft(hydra.langs.cypher.openCypher.AddOrSubtractExpression left) {
    java.util.Objects.requireNonNull((left));
    return new StringListNullPredicateExpression(left, right);
  }
  
  public StringListNullPredicateExpression withRight(java.util.List<hydra.langs.cypher.openCypher.StringListNullPredicateRightHandSide> right) {
    java.util.Objects.requireNonNull((right));
    return new StringListNullPredicateExpression(left, right);
  }
}