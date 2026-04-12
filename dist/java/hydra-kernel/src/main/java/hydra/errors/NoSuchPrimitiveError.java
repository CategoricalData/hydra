// Note: this is an automatically generated file. Do not edit.

package hydra.errors;

import java.io.Serializable;

/**
 * No primitive function with the expected name was registered in the graph
 */
public class NoSuchPrimitiveError implements Serializable, Comparable<NoSuchPrimitiveError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.errors.NoSuchPrimitiveError");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The primitive name which was not found
   */
  public final hydra.core.Name name;

  public NoSuchPrimitiveError (hydra.core.Name name) {
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NoSuchPrimitiveError)) {
      return false;
    }
    NoSuchPrimitiveError o = (NoSuchPrimitiveError) other;
    return java.util.Objects.equals(
      this.name,
      o.name);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NoSuchPrimitiveError other) {
    return hydra.util.Comparing.compare(
      name,
      other.name);
  }
}
