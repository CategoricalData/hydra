// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Delete operations
 */
public class DeleteFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.features.DeleteFeatures");
  
  public static final hydra.core.Name FIELD_NAME_DELETE = new hydra.core.Name("delete");
  
  public static final hydra.core.Name FIELD_NAME_DETACH_DELETE = new hydra.core.Name("detachDelete");
  
  /**
   * The basic DELETE clause
   */
  public final Boolean delete;
  
  /**
   * The DETACH DELETE clause
   */
  public final Boolean detachDelete;
  
  public DeleteFeatures (Boolean delete, Boolean detachDelete) {
    java.util.Objects.requireNonNull((delete));
    java.util.Objects.requireNonNull((detachDelete));
    this.delete = delete;
    this.detachDelete = detachDelete;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DeleteFeatures)) {
      return false;
    }
    DeleteFeatures o = (DeleteFeatures) (other);
    return delete.equals(o.delete) && detachDelete.equals(o.detachDelete);
  }
  
  @Override
  public int hashCode() {
    return 2 * delete.hashCode() + 3 * detachDelete.hashCode();
  }
  
  public DeleteFeatures withDelete(Boolean delete) {
    java.util.Objects.requireNonNull((delete));
    return new DeleteFeatures(delete, detachDelete);
  }
  
  public DeleteFeatures withDetachDelete(Boolean detachDelete) {
    java.util.Objects.requireNonNull((detachDelete));
    return new DeleteFeatures(delete, detachDelete);
  }
}