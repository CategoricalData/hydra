// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * Merge operations
 */
public class MergeFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/features.MergeFeatures");
  
  public static final hydra.core.Name FIELD_NAME_MERGE = new hydra.core.Name("merge");
  
  public static final hydra.core.Name FIELD_NAME_MERGE_ON_CREATE = new hydra.core.Name("mergeOnCreate");
  
  public static final hydra.core.Name FIELD_NAME_MERGE_ON_MATCH = new hydra.core.Name("mergeOnMatch");
  
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
    java.util.Objects.requireNonNull((merge));
    java.util.Objects.requireNonNull((mergeOnCreate));
    java.util.Objects.requireNonNull((mergeOnMatch));
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
    java.util.Objects.requireNonNull((merge));
    return new MergeFeatures(merge, mergeOnCreate, mergeOnMatch);
  }
  
  public MergeFeatures withMergeOnCreate(Boolean mergeOnCreate) {
    java.util.Objects.requireNonNull((mergeOnCreate));
    return new MergeFeatures(merge, mergeOnCreate, mergeOnMatch);
  }
  
  public MergeFeatures withMergeOnMatch(Boolean mergeOnMatch) {
    java.util.Objects.requireNonNull((mergeOnMatch));
    return new MergeFeatures(merge, mergeOnCreate, mergeOnMatch);
  }
}