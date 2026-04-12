// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class PatternPredicate implements Serializable, Comparable<PatternPredicate> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.PatternPredicate");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.ext.cypher.openCypher.RelationshipsPattern value;

  public PatternPredicate (hydra.ext.cypher.openCypher.RelationshipsPattern value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PatternPredicate)) {
      return false;
    }
    PatternPredicate o = (PatternPredicate) other;
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
  public int compareTo(PatternPredicate other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
