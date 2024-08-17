// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * Database functions
 */
public class DatabaseFunctionFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/features.DatabaseFunctionFeatures");
  
  public static final hydra.core.Name FIELD_NAME_DB.NAME_FROM_ELEMENT_ID = new hydra.core.Name("db.nameFromElementId");
  
  /**
   * The db.nameFromElementId() function. Resolves the database name from the given element id. Introduced in 5.12.
   */
  public final Boolean db_nameFromElementId;
  
  public DatabaseFunctionFeatures (Boolean db_nameFromElementId) {
    java.util.Objects.requireNonNull((db_nameFromElementId));
    this.db_nameFromElementId = db_nameFromElementId;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DatabaseFunctionFeatures)) {
      return false;
    }
    DatabaseFunctionFeatures o = (DatabaseFunctionFeatures) (other);
    return db_nameFromElementId.equals(o.db_nameFromElementId);
  }
  
  @Override
  public int hashCode() {
    return 2 * db_nameFromElementId.hashCode();
  }
}