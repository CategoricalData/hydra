// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class CaseAlternative implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.CaseAlternative");
  
  public final hydra.langs.cypher.openCypher.Expression condition;
  
  public final hydra.langs.cypher.openCypher.Expression result;
  
  public CaseAlternative (hydra.langs.cypher.openCypher.Expression condition, hydra.langs.cypher.openCypher.Expression result) {
    if (condition == null) {
      throw new IllegalArgumentException("null value for 'condition' argument");
    }
    if (result == null) {
      throw new IllegalArgumentException("null value for 'result' argument");
    }
    this.condition = condition;
    this.result = result;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CaseAlternative)) {
      return false;
    }
    CaseAlternative o = (CaseAlternative) (other);
    return condition.equals(o.condition) && result.equals(o.result);
  }
  
  @Override
  public int hashCode() {
    return 2 * condition.hashCode() + 3 * result.hashCode();
  }
  
  public CaseAlternative withCondition(hydra.langs.cypher.openCypher.Expression condition) {
    if (condition == null) {
      throw new IllegalArgumentException("null value for 'condition' argument");
    }
    return new CaseAlternative(condition, result);
  }
  
  public CaseAlternative withResult(hydra.langs.cypher.openCypher.Expression result) {
    if (result == null) {
      throw new IllegalArgumentException("null value for 'result' argument");
    }
    return new CaseAlternative(condition, result);
  }
}