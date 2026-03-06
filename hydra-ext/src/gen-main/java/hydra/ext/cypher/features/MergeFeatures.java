// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Merge operations
 */
public class MergeFeatures implements Serializable, Comparable<MergeFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.MergeFeatures");
  
  public static final hydra.core.Name MERGE = new hydra.core.Name("merge");
  
  public static final hydra.core.Name MERGE_ON_CREATE = new hydra.core.Name("mergeOnCreate");
  
  public static final hydra.core.Name MERGE_ON_MATCH = new hydra.core.Name("mergeOnMatch");
  
  /**
   * The basic MERGE clause
   */
  public final Boolean merge;
  
  /**
   * MERGE with the ON CREATE action
   */
  public final Boolean mergeOnCreate;
  
  /**
   * MERGE with the ON MATCH action
   */
  public final Boolean mergeOnMatch;
  
  public MergeFeatures (Boolean merge, Boolean mergeOnCreate, Boolean mergeOnMatch) {
    this.merge = merge;
    this.mergeOnCreate = mergeOnCreate;
    this.mergeOnMatch = mergeOnMatch;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MergeFeatures)) {
      return false;
    }
    MergeFeatures o = (MergeFeatures) other;
    return java.util.Objects.equals(
      this.merge,
      o.merge) && java.util.Objects.equals(
      this.mergeOnCreate,
      o.mergeOnCreate) && java.util.Objects.equals(
      this.mergeOnMatch,
      o.mergeOnMatch);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(merge) + 3 * java.util.Objects.hashCode(mergeOnCreate) + 5 * java.util.Objects.hashCode(mergeOnMatch);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MergeFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) merge).compareTo(other.merge);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) mergeOnCreate).compareTo(other.mergeOnCreate);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) mergeOnMatch).compareTo(other.mergeOnMatch);
  }
  
  public MergeFeatures withMerge(Boolean merge) {
    return new MergeFeatures(merge, mergeOnCreate, mergeOnMatch);
  }
  
  public MergeFeatures withMergeOnCreate(Boolean mergeOnCreate) {
    return new MergeFeatures(merge, mergeOnCreate, mergeOnMatch);
  }
  
  public MergeFeatures withMergeOnMatch(Boolean mergeOnMatch) {
    return new MergeFeatures(merge, mergeOnCreate, mergeOnMatch);
  }
}
