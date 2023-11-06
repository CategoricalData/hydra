package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class Case implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.Case");
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Expression> expression;
  
  public final java.util.List<hydra.langs.cypher.openCypher.CaseAlternative> alternatives;
  
  public Case (java.util.Optional<hydra.langs.cypher.openCypher.Expression> expression, java.util.List<hydra.langs.cypher.openCypher.CaseAlternative> alternatives) {
    this.expression = expression;
    this.alternatives = alternatives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Case)) {
      return false;
    }
    Case o = (Case) (other);
    return expression.equals(o.expression) && alternatives.equals(o.alternatives);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * alternatives.hashCode();
  }
  
  public Case withExpression(java.util.Optional<hydra.langs.cypher.openCypher.Expression> expression) {
    return new Case(expression, alternatives);
  }
  
  public Case withAlternatives(java.util.List<hydra.langs.cypher.openCypher.CaseAlternative> alternatives) {
    return new Case(expression, alternatives);
  }
}