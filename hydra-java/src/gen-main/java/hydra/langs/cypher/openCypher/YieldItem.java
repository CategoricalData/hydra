package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class YieldItem implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.YieldItem");
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.ProcedureResultField> resultField;
  
  public final hydra.langs.cypher.openCypher.Variable variable;
  
  public YieldItem (java.util.Optional<hydra.langs.cypher.openCypher.ProcedureResultField> resultField, hydra.langs.cypher.openCypher.Variable variable) {
    this.resultField = resultField;
    this.variable = variable;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof YieldItem)) {
      return false;
    }
    YieldItem o = (YieldItem) (other);
    return resultField.equals(o.resultField) && variable.equals(o.variable);
  }
  
  @Override
  public int hashCode() {
    return 2 * resultField.hashCode() + 3 * variable.hashCode();
  }
  
  public YieldItem withResultField(java.util.Optional<hydra.langs.cypher.openCypher.ProcedureResultField> resultField) {
    return new YieldItem(resultField, variable);
  }
  
  public YieldItem withVariable(hydra.langs.cypher.openCypher.Variable variable) {
    return new YieldItem(resultField, variable);
  }
}