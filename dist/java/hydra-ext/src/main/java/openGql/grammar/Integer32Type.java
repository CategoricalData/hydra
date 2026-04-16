// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class Integer32Type implements Serializable, Comparable<Integer32Type> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.Integer32Type");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final Boolean notNull;

  public Integer32Type (Boolean notNull) {
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Integer32Type)) {
      return false;
    }
    Integer32Type o = (Integer32Type) other;
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
  public int compareTo(Integer32Type other) {
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }
}
