// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for delete operations.
 */
public class DeleteFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.DeleteFeatures");
  
  /**
   * Whether to expect the basic DELETE clause.
   */
  public final Boolean delete;
  
  /**
   * Whether to expect the DETACH DELETE clause.
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