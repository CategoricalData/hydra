// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A union type with duplicate field names
 */
public class DuplicateUnionTypeFieldNamesError implements Serializable, Comparable<DuplicateUnionTypeFieldNamesError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.DuplicateUnionTypeFieldNamesError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The path to the union type with duplicate fields
   */
  public final hydra.paths.SubtermPath location;

  /**
   * The duplicated field name
   */
  public final hydra.core.Name name;

  public DuplicateUnionTypeFieldNamesError (hydra.paths.SubtermPath location, hydra.core.Name name) {
    this.location = location;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DuplicateUnionTypeFieldNamesError)) {
      return false;
    }
    DuplicateUnionTypeFieldNamesError o = (DuplicateUnionTypeFieldNamesError) other;
    return java.util.Objects.equals(
      this.location,
      o.location) && java.util.Objects.equals(
      this.name,
      o.name);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(location) + 3 * java.util.Objects.hashCode(name);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DuplicateUnionTypeFieldNamesError other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      location,
      other.location);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      name,
      other.name);
  }

  public DuplicateUnionTypeFieldNamesError withLocation(hydra.paths.SubtermPath location) {
    return new DuplicateUnionTypeFieldNamesError(location, name);
  }

  public DuplicateUnionTypeFieldNamesError withName(hydra.core.Name name) {
    return new DuplicateUnionTypeFieldNamesError(location, name);
  }
}
