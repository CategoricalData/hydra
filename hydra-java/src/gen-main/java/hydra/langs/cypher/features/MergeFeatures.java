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