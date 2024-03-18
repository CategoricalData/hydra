package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class YieldItem implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.YieldItem");
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.ProcedureResultField> field;
  
  public final hydra.langs.cypher.openCypher.Variable variable;
  
  public YieldItem (java.util.Optional<hydra.langs.cypher.openCypher.ProcedureResultField> field, hydra.langs.cypher.openCypher.Variable variable) {
    this.field = field;
    this.variable = variable;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof YieldItem)) {
      return false;
    }
    YieldItem o = (YieldItem) (other);
    return field.equals(o.field) && variable.equals(o.variable);
  }
  
  @Override
  public int hashCode() {
    return 2 * field.hashCode() + 3 * variable.hashCode();
  }
  
  public YieldItem withField(java.util.Optional<hydra.langs.cypher.openCypher.ProcedureResultField> field) {
    return new YieldItem(field, variable);
  }
  
  public YieldItem withVariable(hydra.langs.cypher.openCypher.Variable variable) {
    return new YieldItem(field, variable);
  }
}