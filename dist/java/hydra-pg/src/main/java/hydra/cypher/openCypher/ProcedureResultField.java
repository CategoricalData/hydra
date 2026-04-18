// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public class ProcedureResultField implements Serializable, Comparable<ProcedureResultField> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.ProcedureResultField");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public ProcedureResultField (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ProcedureResultField)) {
      return false;
    }
    ProcedureResultField o = (ProcedureResultField) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ProcedureResultField other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
