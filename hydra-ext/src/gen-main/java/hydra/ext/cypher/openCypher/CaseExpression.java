// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class CaseExpression implements Serializable, Comparable<CaseExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.CaseExpression");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public static final hydra.core.Name ALTERNATIVES = new hydra.core.Name("alternatives");

  public static final hydra.core.Name ELSE = new hydra.core.Name("else");

  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.Expression> expression;

  public final hydra.util.ConsList<hydra.ext.cypher.openCypher.CaseAlternative> alternatives;

  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.Expression> else_;

  public CaseExpression (hydra.util.Maybe<hydra.ext.cypher.openCypher.Expression> expression, hydra.util.ConsList<hydra.ext.cypher.openCypher.CaseAlternative> alternatives, hydra.util.Maybe<hydra.ext.cypher.openCypher.Expression> else_) {
    this.expression = expression;
    this.alternatives = alternatives;
    this.else_ = else_;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CaseExpression)) {
      return false;
    }
    CaseExpression o = (CaseExpression) other;
    return java.util.Objects.equals(
      this.expression,
      o.expression) && java.util.Objects.equals(
      this.alternatives,
      o.alternatives) && java.util.Objects.equals(
      this.else_,
      o.else_);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expression) + 3 * java.util.Objects.hashCode(alternatives) + 5 * java.util.Objects.hashCode(else_);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CaseExpression other) {
    int cmp = 0;
    cmp = ((Comparable) expression).compareTo(other.expression);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) alternatives).compareTo(other.alternatives);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) else_).compareTo(other.else_);
  }

  public CaseExpression withExpression(hydra.util.Maybe<hydra.ext.cypher.openCypher.Expression> expression) {
    return new CaseExpression(expression, alternatives, else_);
  }

  public CaseExpression withAlternatives(hydra.util.ConsList<hydra.ext.cypher.openCypher.CaseAlternative> alternatives) {
    return new CaseExpression(expression, alternatives, else_);
  }

  public CaseExpression withElse(hydra.util.Maybe<hydra.ext.cypher.openCypher.Expression> else_) {
    return new CaseExpression(expression, alternatives, else_);
  }
}
