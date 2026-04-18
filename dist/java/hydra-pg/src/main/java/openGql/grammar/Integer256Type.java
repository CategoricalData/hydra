// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class Integer256Type implements Serializable, Comparable<Integer256Type> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.Integer256Type");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final Boolean notNull;

  public Integer256Type (Boolean notNull) {
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Integer256Type)) {
      return false;
    }
    Integer256Type o = (Integer256Type) other;
    return java.util.Objects.equals(
      this.notNull,
      o.notNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(notNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Integer256Type other) {
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }
}
