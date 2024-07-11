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