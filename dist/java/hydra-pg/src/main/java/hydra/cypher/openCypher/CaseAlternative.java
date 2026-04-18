// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public class CaseAlternative implements Serializable, Comparable<CaseAlternative> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.CaseAlternative");

  public static final hydra.core.Name CONDITION = new hydra.core.Name("condition");

  public static final hydra.core.Name RESULT = new hydra.core.Name("result");

  public final hydra.cypher.openCypher.Expression condition;

  public final hydra.cypher.openCypher.Expression result;

  public CaseAlternative (hydra.cypher.openCypher.Expression condition, hydra.cypher.openCypher.Expression result) {
    this.condition = condition;
    this.result = result;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CaseAlternative)) {
      return false;
    }
    CaseAlternative o = (CaseAlternative) other;
    return java.util.Objects.equals(
      this.condition,
      o.condition) && java.util.Objects.equals(
      this.result,
      o.result);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(condition) + 3 * java.util.Objects.hashCode(result);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CaseAlternative other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      condition,
      other.condition);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      result,
      other.result);
  }

  public CaseAlternative withCondition(hydra.cypher.openCypher.Expression condition) {
    return new CaseAlternative(condition, result);
  }

  public CaseAlternative withResult(hydra.cypher.openCypher.Expression result) {
    return new CaseAlternative(condition, result);
  }
}
