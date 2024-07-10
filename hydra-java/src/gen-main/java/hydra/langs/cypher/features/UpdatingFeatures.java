// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for specific syntax related to updating data in the graph.
 */
public class UpdatingFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.UpdatingFeatures");
  
  /**
   * Whether to expect the CREATE clause.
   */
  public final Boolean create;
  
  /**
   * Whether to expect the SET clause.
   */
  public final Boolean set;
  
  /**
   * Whether to expect multi-part queries using WITH.
   */
  public final Boolean with;
  
  public UpdatingFeatures (Boolean create, Boolean set, Boolean with) {
    if (create == null) {
      throw new IllegalArgumentException("null value for 'create' argument");
    }
    if (set == null) {
      throw new IllegalArgumentException("null value for 'set' argument");
    }
    if (with == null) {
      throw new IllegalArgumentException("null value for 'with' argument");
    }
    this.create = create;
    this.set = set;
    this.with = with;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UpdatingFeatures)) {
      return false;
    }
    UpdatingFeatures o = (UpdatingFeatures) (other);
    return create.equals(o.create) && set.equals(o.set) && with.equals(o.with);
  }
  
  @Override
  public int hashCode() {
    return 2 * create.hashCode() + 3 * set.hashCode() + 5 * with.hashCode();
  }
  
  public UpdatingFeatures withCreate(Boolean create) {
    if (create == null) {
      throw new IllegalArgumentException("null value for 'create' argument");
    }
    return new UpdatingFeatures(create, set, with);
  }
  
  public UpdatingFeatures withSet(Boolean set) {
    if (set == null) {
      throw new IllegalArgumentException("null value for 'set' argument");
    }
    return new UpdatingFeatures(create, set, with);
  }
  
  public UpdatingFeatures withWith(Boolean with) {
    if (with == null) {
      throw new IllegalArgumentException("null value for 'with' argument");
    }
    return new UpdatingFeatures(create, set, with);
  }
}