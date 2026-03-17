// Note: this is an automatically generated file. Do not edit.

package hydra.pg.graphson.syntax;

import java.io.Serializable;

public class ValuePair implements Serializable, Comparable<ValuePair> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.graphson.syntax.ValuePair");

  public static final hydra.core.Name FIRST = new hydra.core.Name("first");

  public static final hydra.core.Name SECOND = new hydra.core.Name("second");

  public final hydra.pg.graphson.syntax.Value first;

  public final hydra.pg.graphson.syntax.Value second;

  public ValuePair (hydra.pg.graphson.syntax.Value first, hydra.pg.graphson.syntax.Value second) {
    this.first = first;
    this.second = second;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ValuePair)) {
      return false;
    }
    ValuePair o = (ValuePair) other;
    return java.util.Objects.equals(
      this.first,
      o.first) && java.util.Objects.equals(
      this.second,
      o.second);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(first) + 3 * java.util.Objects.hashCode(second);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ValuePair other) {
    int cmp = 0;
    cmp = ((Comparable) first).compareTo(other.first);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) second).compareTo(other.second);
  }

  public ValuePair withFirst(hydra.pg.graphson.syntax.Value first) {
    return new ValuePair(first, second);
  }

  public ValuePair withSecond(hydra.pg.graphson.syntax.Value second) {
    return new ValuePair(first, second);
  }
}
