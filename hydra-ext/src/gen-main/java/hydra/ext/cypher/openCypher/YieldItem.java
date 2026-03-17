// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class YieldItem implements Serializable, Comparable<YieldItem> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.YieldItem");

  public static final hydra.core.Name FIELD = new hydra.core.Name("field");

  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");

  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.ProcedureResultField> field;

  public final hydra.ext.cypher.openCypher.Variable variable;

  public YieldItem (hydra.util.Maybe<hydra.ext.cypher.openCypher.ProcedureResultField> field, hydra.ext.cypher.openCypher.Variable variable) {
    this.field = field;
    this.variable = variable;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof YieldItem)) {
      return false;
    }
    YieldItem o = (YieldItem) other;
    return java.util.Objects.equals(
      this.field,
      o.field) && java.util.Objects.equals(
      this.variable,
      o.variable);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(field) + 3 * java.util.Objects.hashCode(variable);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(YieldItem other) {
    int cmp = 0;
    cmp = ((Comparable) field).compareTo(other.field);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) variable).compareTo(other.variable);
  }

  public YieldItem withField(hydra.util.Maybe<hydra.ext.cypher.openCypher.ProcedureResultField> field) {
    return new YieldItem(field, variable);
  }

  public YieldItem withVariable(hydra.ext.cypher.openCypher.Variable variable) {
    return new YieldItem(field, variable);
  }
}
