// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Specific syntax related to updating data in the graph
 */
public class UpdatingFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/features.UpdatingFeatures");
  
  public static final hydra.core.Name FIELD_NAME_CREATE = new hydra.core.Name("create");
  
  public static final hydra.core.Name FIELD_NAME_SET = new hydra.core.Name("set");
  
  public static final hydra.core.Name FIELD_NAME_WITH = new hydra.core.Name("with");
  
  /**
   * The CREATE clause
   */
  public final Boolean create;
  
  /**
   * The SET clause
   */
  public final Boolean set;
  
  /**
   * Multi-part queries using WITH
   */
  public final Boolean with;
  
  public UpdatingFeatures (Boolean create, Boolean set, Boolean with) {
    java.util.Objects.requireNonNull((create));
    java.util.Objects.requireNonNull((set));
    java.util.Objects.requireNonNull((with));
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
    java.util.Objects.requireNonNull((create));
    return new UpdatingFeatures(create, set, with);
  }
  
  public UpdatingFeatures withSet(Boolean set) {
    java.util.Objects.requireNonNull((set));
    return new UpdatingFeatures(create, set, with);
  }
  
  public UpdatingFeatures withWith(Boolean with) {
    java.util.Objects.requireNonNull((with));
    return new UpdatingFeatures(create, set, with);
  }
}