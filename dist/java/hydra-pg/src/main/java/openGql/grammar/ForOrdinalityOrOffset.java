// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ForOrdinalityOrOffset implements Serializable, Comparable<ForOrdinalityOrOffset> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ForOrdinalityOrOffset");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");

  public final openGql.grammar.OrdinalityOrOffsetType type;

  public final String variable;

  public ForOrdinalityOrOffset (openGql.grammar.OrdinalityOrOffsetType type, String variable) {
    this.type = type;
    this.variable = variable;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ForOrdinalityOrOffset)) {
      return false;
    }
    ForOrdinalityOrOffset o = (ForOrdinalityOrOffset) other;
    return java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.variable,
      o.variable);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(type) + 3 * java.util.Objects.hashCode(variable);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ForOrdinalityOrOffset other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      type,
      other.type);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      variable,
      other.variable);
  }

  public ForOrdinalityOrOffset withType(openGql.grammar.OrdinalityOrOffsetType type) {
    return new ForOrdinalityOrOffset(type, variable);
  }

  public ForOrdinalityOrOffset withVariable(String variable) {
    return new ForOrdinalityOrOffset(type, variable);
  }
}
