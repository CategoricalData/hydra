// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class YieldItem implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.YieldItem");
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.ProcedureResultField> field;
  
  public final hydra.langs.cypher.openCypher.Variable variable;
  
  public YieldItem (hydra.util.Opt<hydra.langs.cypher.openCypher.ProcedureResultField> field, hydra.langs.cypher.openCypher.Variable variable) {
    if (field == null) {
      throw new IllegalArgumentException("null value for 'field' argument");
    }
    if (variable == null) {
      throw new IllegalArgumentException("null value for 'variable' argument");
    }
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
  
  public YieldItem withField(hydra.util.Opt<hydra.langs.cypher.openCypher.ProcedureResultField> field) {
    if (field == null) {
      throw new IllegalArgumentException("null value for 'field' argument");
    }
    return new YieldItem(field, variable);
  }
  
  public YieldItem withVariable(hydra.langs.cypher.openCypher.Variable variable) {
    if (variable == null) {
      throw new IllegalArgumentException("null value for 'variable' argument");
    }
    return new YieldItem(field, variable);
  }
}