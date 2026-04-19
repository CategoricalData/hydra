// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A case statement with no cases and no default (optional)
 */
public class EmptyCaseStatementError implements Serializable, Comparable<EmptyCaseStatementError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.EmptyCaseStatementError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("typeName");

  /**
   * The path to the empty case statement within the term
   */
  public final hydra.paths.SubtermPath location;

  /**
   * The name of the union type being matched
   */
  public final hydra.core.Name typeName;

  public EmptyCaseStatementError (hydra.paths.SubtermPath location, hydra.core.Name typeName) {
    this.location = location;
    this.typeName = typeName;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EmptyCaseStatementError)) {
      return false;
    }
    EmptyCaseStatementError o = (EmptyCaseStatementError) other;
    return java.util.Objects.equals(
      this.location,
      o.location) && java.util.Objects.equals(
      this.typeName,
      o.typeName);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(location) + 3 * java.util.Objects.hashCode(typeName);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EmptyCaseStatementError other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      location,
      other.location);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      typeName,
      other.typeName);
  }

  public EmptyCaseStatementError withLocation(hydra.paths.SubtermPath location) {
    return new EmptyCaseStatementError(location, typeName);
  }

  public EmptyCaseStatementError withTypeName(hydra.core.Name typeName) {
    return new EmptyCaseStatementError(location, typeName);
  }
}
