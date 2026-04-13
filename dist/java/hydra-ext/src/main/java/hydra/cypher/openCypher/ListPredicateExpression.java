// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public class ListPredicateExpression implements Serializable, Comparable<ListPredicateExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.ListPredicateExpression");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.cypher.openCypher.AddOrSubtractExpression value;

  public ListPredicateExpression (hydra.cypher.openCypher.AddOrSubtractExpression value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ListPredicateExpression)) {
      return false;
    }
    ListPredicateExpression o = (ListPredicateExpression) other;
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
  public int compareTo(ListPredicateExpression other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
