// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Database functions
 */
public class DatabaseFunctionFeatures implements Serializable, Comparable<DatabaseFunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.DatabaseFunctionFeatures");

  public static final hydra.core.Name DB_NAME_FROM_ELEMENT_ID = new hydra.core.Name("dbNameFromElementId");

  /**
   * The db.nameFromElementId() function. Resolves the database name from the given element id. Introduced in 5.12.
   */
  public final Boolean dbNameFromElementId;

  public DatabaseFunctionFeatures (Boolean dbNameFromElementId) {
    this.dbNameFromElementId = dbNameFromElementId;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DatabaseFunctionFeatures)) {
      return false;
    }
    DatabaseFunctionFeatures o = (DatabaseFunctionFeatures) other;
    return java.util.Objects.equals(
      this.dbNameFromElementId,
      o.dbNameFromElementId);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(dbNameFromElementId);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DatabaseFunctionFeatures other) {
    return hydra.util.Comparing.compare(
      dbNameFromElementId,
      other.dbNameFromElementId);
  }
}
