// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import hydra.util.Maybe;

import java.io.Serializable;

public class CaseExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.CaseExpression");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_ALTERNATIVES = new hydra.core.Name("alternatives");
  
  public static final hydra.core.Name FIELD_NAME_ELSE = new hydra.core.Name("else");
  
  public final Maybe<Expression> expression;
  
  public final java.util.List<hydra.ext.cypher.openCypher.CaseAlternative> alternatives;
  
  public final Maybe<Expression> else_;
  
  public CaseExpression (Maybe<Expression> expression, java.util.List<hydra.ext.cypher.openCypher.CaseAlternative> alternatives, Maybe<Expression> else_) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((alternatives));
    java.util.Objects.requireNonNull((else_));
    this.expression = expression;
    this.alternatives = alternatives;
    this.else_ = else_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CaseExpression)) {
      return false;
    }
    CaseExpression o = (CaseExpression) (other);
    return expression.equals(o.expression) && alternatives.equals(o.alternatives) && else_.equals(o.else_);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * alternatives.hashCode() + 5 * else_.hashCode();
  }
  
  public CaseExpression withExpression(Maybe<Expression> expression) {
    java.util.Objects.requireNonNull((expression));
    return new CaseExpression(expression, alternatives, else_);
  }
  
  public CaseExpression withAlternatives(java.util.List<hydra.ext.cypher.openCypher.CaseAlternative> alternatives) {
    java.util.Objects.requireNonNull((alternatives));
    return new CaseExpression(expression, alternatives, else_);
  }
  
  public CaseExpression withElse(Maybe<Expression> else_) {
    java.util.Objects.requireNonNull((else_));
    return new CaseExpression(expression, alternatives, else_);
  }
}
