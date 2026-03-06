// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Specific syntax related to reading data from the graph.
 */
public class ReadingFeatures implements Serializable, Comparable<ReadingFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.ReadingFeatures");
  
  public static final hydra.core.Name UNION = new hydra.core.Name("union");
  
  public static final hydra.core.Name UNION_ALL = new hydra.core.Name("unionAll");
  
  public static final hydra.core.Name UNWIND = new hydra.core.Name("unwind");
  
  /**
   * The UNION operator
   */
  public final Boolean union;
  
  /**
   * The UNION ALL operator
   */
  public final Boolean unionAll;
  
  /**
   * The UNWIND clause
   */
  public final Boolean unwind;
  
  public ReadingFeatures (Boolean union, Boolean unionAll, Boolean unwind) {
    this.union = union;
    this.unionAll = unionAll;
    this.unwind = unwind;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ReadingFeatures)) {
      return false;
    }
    ReadingFeatures o = (ReadingFeatures) other;
    return java.util.Objects.equals(
      this.union,
      o.union) && java.util.Objects.equals(
      this.unionAll,
      o.unionAll) && java.util.Objects.equals(
      this.unwind,
      o.unwind);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(union) + 3 * java.util.Objects.hashCode(unionAll) + 5 * java.util.Objects.hashCode(unwind);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ReadingFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) union).compareTo(other.union);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) unionAll).compareTo(other.unionAll);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) unwind).compareTo(other.unwind);
  }
  
  public ReadingFeatures withUnion(Boolean union) {
    return new ReadingFeatures(union, unionAll, unwind);
  }
  
  public ReadingFeatures withUnionAll(Boolean unionAll) {
    return new ReadingFeatures(union, unionAll, unwind);
  }
  
  public ReadingFeatures withUnwind(Boolean unwind) {
    return new ReadingFeatures(union, unionAll, unwind);
  }
}
