// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for merge operations.
 */
public class MergeFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.MergeFeatures");
  
  /**
   * Whether to expect the basic MERGE clause.
   */
  public final Boolean merge;
  
  /**
   * Whether to expect MERGE with the ON CREATE action.
   */
  public final Boolean mergeOnCreate;
  
  /**
   * Whether to expect MERGE with the ON MATCH action.
   */
  public final Boolean mergeOnMatch;
  
  public MergeFeatures (Boolean merge, Boolean mergeOnCreate, Boolean mergeOnMatch) {
    if (merge == null) {
      throw new IllegalArgumentException("null value for 'merge' argument");
    }
    if (mergeOnCreate == null) {
      throw new IllegalArgumentException("null value for 'mergeOnCreate' argument");
    }
    if (mergeOnMatch == null) {
      throw new IllegalArgumentException("null value for 'mergeOnMatch' argument");
    }
    this.merge = merge;
    this.mergeOnCreate = mergeOnCreate;
    this.mergeOnMatch = mergeOnMatch;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MergeFeatures)) {
      return false;
    }
    MergeFeatures o = (MergeFeatures) (other);
    return merge.equals(o.merge) && mergeOnCreate.equals(o.mergeOnCreate) && mergeOnMatch.equals(o.mergeOnMatch);
  }
  
  @Override
  public int hashCode() {
    return 2 * merge.hashCode() + 3 * mergeOnCreate.hashCode() + 5 * mergeOnMatch.hashCode();
  }
  
  public MergeFeatures withMerge(Boolean merge) {
    if (merge == null) {
      throw new IllegalArgumentException("null value for 'merge' argument");
    }
    return new MergeFeatures(merge, mergeOnCreate, mergeOnMatch);
  }
  
  public MergeFeatures withMergeOnCreate(Boolean mergeOnCreate) {
    if (mergeOnCreate == null) {
      throw new IllegalArgumentException("null value for 'mergeOnCreate' argument");
    }
    return new MergeFeatures(merge, mergeOnCreate, mergeOnMatch);
  }
  
  public MergeFeatures withMergeOnMatch(Boolean mergeOnMatch) {
    if (mergeOnMatch == null) {
      throw new IllegalArgumentException("null value for 'mergeOnMatch' argument");
    }
    return new MergeFeatures(merge, mergeOnCreate, mergeOnMatch);
  }
}