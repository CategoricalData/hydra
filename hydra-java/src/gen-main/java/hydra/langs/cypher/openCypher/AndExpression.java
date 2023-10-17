package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class AndExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.AndExpression");
  
  public final java.util.List<hydra.langs.cypher.openCypher.NotExpression> value;
  
  public AndExpression (java.util.List<hydra.langs.cypher.openCypher.NotExpression> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AndExpression)) {
      return false;
    }
    AndExpression o = (AndExpression) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}