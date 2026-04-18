// Note: this is an automatically generated file. Do not edit.

package hydra.error.pg;

import java.io.Serializable;

/**
 * An invalid property on a vertex or edge, identified by its key
 */
public class InvalidElementPropertyError implements Serializable, Comparable<InvalidElementPropertyError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.pg.InvalidElementPropertyError");

  public static final hydra.core.Name KEY = new hydra.core.Name("key");

  public static final hydra.core.Name ERROR = new hydra.core.Name("error");

  /**
   * The key of the invalid property
   */
  public final hydra.pg.model.PropertyKey key;

  /**
   * The specific error
   */
  public final hydra.error.pg.InvalidPropertyError error;

  public InvalidElementPropertyError (hydra.pg.model.PropertyKey key, hydra.error.pg.InvalidPropertyError error) {
    this.key = key;
    this.error = error;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InvalidElementPropertyError)) {
      return false;
    }
    InvalidElementPropertyError o = (InvalidElementPropertyError) other;
    return java.util.Objects.equals(
      this.key,
      o.key) && java.util.Objects.equals(
      this.error,
      o.error);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(key) + 3 * java.util.Objects.hashCode(error);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(InvalidElementPropertyError other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      key,
      other.key);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      error,
      other.error);
  }

  public InvalidElementPropertyError withKey(hydra.pg.model.PropertyKey key) {
    return new InvalidElementPropertyError(key, error);
  }

  public InvalidElementPropertyError withError(hydra.error.pg.InvalidPropertyError error) {
    return new InvalidElementPropertyError(key, error);
  }
}
