// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class CaseExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.CaseExpression");
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.Expression> expression;
  
  public final java.util.List<hydra.langs.cypher.openCypher.CaseAlternative> alternatives;
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.Expression> else_;
  
  public CaseExpression (hydra.util.Opt<hydra.langs.cypher.openCypher.Expression> expression, java.util.List<hydra.langs.cypher.openCypher.CaseAlternative> alternatives, hydra.util.Opt<hydra.langs.cypher.openCypher.Expression> else_) {
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
  
  public CaseExpression withExpression(hydra.util.Opt<hydra.langs.cypher.openCypher.Expression> expression) {
    java.util.Objects.requireNonNull((expression));
    return new CaseExpression(expression, alternatives, else_);
  }
  
  public CaseExpression withAlternatives(java.util.List<hydra.langs.cypher.openCypher.CaseAlternative> alternatives) {
    java.util.Objects.requireNonNull((alternatives));
    return new CaseExpression(expression, alternatives, else_);
  }
  
  public CaseExpression withElse(hydra.util.Opt<hydra.langs.cypher.openCypher.Expression> else_) {
    java.util.Objects.requireNonNull((else_));
    return new CaseExpression(expression, alternatives, else_);
  }
}