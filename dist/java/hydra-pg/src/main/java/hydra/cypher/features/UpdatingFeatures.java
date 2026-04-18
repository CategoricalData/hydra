// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.features;

import java.io.Serializable;

/**
 * Specific syntax related to updating data in the graph
 */
public class UpdatingFeatures implements Serializable, Comparable<UpdatingFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.features.UpdatingFeatures");

  public static final hydra.core.Name CREATE = new hydra.core.Name("create");

  public static final hydra.core.Name SET = new hydra.core.Name("set");

  public static final hydra.core.Name WITH = new hydra.core.Name("with");

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
    this.create = create;
    this.set = set;
    this.with = with;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UpdatingFeatures)) {
      return false;
    }
    UpdatingFeatures o = (UpdatingFeatures) other;
    return java.util.Objects.equals(
      this.create,
      o.create) && java.util.Objects.equals(
      this.set,
      o.set) && java.util.Objects.equals(
      this.with,
      o.with);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(create) + 3 * java.util.Objects.hashCode(set) + 5 * java.util.Objects.hashCode(with);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UpdatingFeatures other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      create,
      other.create);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      set,
      other.set);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      with,
      other.with);
  }

  public UpdatingFeatures withCreate(Boolean create) {
    return new UpdatingFeatures(create, set, with);
  }

  public UpdatingFeatures withSet(Boolean set) {
    return new UpdatingFeatures(create, set, with);
  }

  public UpdatingFeatures withWith(Boolean with) {
    return new UpdatingFeatures(create, set, with);
  }
}
