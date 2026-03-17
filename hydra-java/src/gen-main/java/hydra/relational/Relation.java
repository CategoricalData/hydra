// Note: this is an automatically generated file. Do not edit.

package hydra.relational;

import java.io.Serializable;

/**
 * A set of distinct n-tuples; a table
 */
public class Relation<V> implements Serializable, Comparable<Relation<V>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.relational.Relation");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.util.ConsList<hydra.relational.Row<V>> value;

  public Relation (hydra.util.ConsList<hydra.relational.Row<V>> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Relation)) {
      return false;
    }
    Relation o = (Relation) other;
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
  public int compareTo(Relation other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
