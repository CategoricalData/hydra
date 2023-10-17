package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class StringPredicateExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.StringPredicateExpression");
  
  public final hydra.langs.cypher.openCypher.StringPredicate predicate;
  
  public final hydra.langs.cypher.openCypher.AddOrSubtractExpression expression;
  
  public StringPredicateExpression (hydra.langs.cypher.openCypher.StringPredicate predicate, hydra.langs.cypher.openCypher.AddOrSubtractExpression expression) {
    this.predicate = predicate;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringPredicateExpression)) {
      return false;
    }
    StringPredicateExpression o = (StringPredicateExpression) (other);
    return predicate.equals(o.predicate) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * predicate.hashCode() + 3 * expression.hashCode();
  }
  
  public StringPredicateExpression withPredicate(hydra.langs.cypher.openCypher.StringPredicate predicate) {
    return new StringPredicateExpression(predicate, expression);
  }
  
  public StringPredicateExpression withExpression(hydra.langs.cypher.openCypher.AddOrSubtractExpression expression) {
    return new StringPredicateExpression(predicate, expression);
  }
}