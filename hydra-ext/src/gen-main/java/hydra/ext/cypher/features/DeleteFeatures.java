// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Delete operations
 */
public class DeleteFeatures implements Serializable, Comparable<DeleteFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.DeleteFeatures");

  public static final hydra.core.Name DELETE = new hydra.core.Name("delete");

  public static final hydra.core.Name DETACH_DELETE = new hydra.core.Name("detachDelete");

  /**
   * The basic DELETE clause
   */
  public final Boolean delete;

  /**
   * The DETACH DELETE clause
   */
  public final Boolean detachDelete;

  public DeleteFeatures (Boolean delete, Boolean detachDelete) {
    this.delete = delete;
    this.detachDelete = detachDelete;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DeleteFeatures)) {
      return false;
    }
    DeleteFeatures o = (DeleteFeatures) other;
    return java.util.Objects.equals(
      this.delete,
      o.delete) && java.util.Objects.equals(
      this.detachDelete,
      o.detachDelete);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(delete) + 3 * java.util.Objects.hashCode(detachDelete);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DeleteFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) delete).compareTo(other.delete);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) detachDelete).compareTo(other.detachDelete);
  }

  public DeleteFeatures withDelete(Boolean delete) {
    return new DeleteFeatures(delete, detachDelete);
  }

  public DeleteFeatures withDetachDelete(Boolean detachDelete) {
    return new DeleteFeatures(delete, detachDelete);
  }
}
