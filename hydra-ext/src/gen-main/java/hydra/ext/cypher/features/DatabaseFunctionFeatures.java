// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Database functions
 */
public class DatabaseFunctionFeatures implements Serializable, Comparable<DatabaseFunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.DatabaseFunctionFeatures");

  public static final hydra.core.Name DB_NAME_FROM_ELEMENT_ID = new hydra.core.Name("db.nameFromElementId");

  /**
   * The db.nameFromElementId() function. Resolves the database name from the given element id. Introduced in 5.12.
   */
  public final Boolean db_nameFromElementId;

  public DatabaseFunctionFeatures (Boolean db_nameFromElementId) {
    this.db_nameFromElementId = db_nameFromElementId;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DatabaseFunctionFeatures)) {
      return false;
    }
    DatabaseFunctionFeatures o = (DatabaseFunctionFeatures) other;
    return java.util.Objects.equals(
      this.db_nameFromElementId,
      o.db_nameFromElementId);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(db_nameFromElementId);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DatabaseFunctionFeatures other) {
    return ((Comparable) db_nameFromElementId).compareTo(other.db_nameFromElementId);
  }
}
