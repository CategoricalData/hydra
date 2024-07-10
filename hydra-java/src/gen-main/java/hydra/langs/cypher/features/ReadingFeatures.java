// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for specific syntax related to reading data from the graph..
 */
public class ReadingFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.ReadingFeatures");
  
  /**
   * Whether to expect the UNION operator.
   */
  public final Boolean union;
  
  /**
   * Whether to expect the UNION ALL operator.
   */
  public final Boolean unionAll;
  
  /**
   * Whether to expect the UNWIND clause.
   */
  public final Boolean unwind;
  
  public ReadingFeatures (Boolean union, Boolean unionAll, Boolean unwind) {
    if (union == null) {
      throw new IllegalArgumentException("null value for 'union' argument");
    }
    if (unionAll == null) {
      throw new IllegalArgumentException("null value for 'unionAll' argument");
    }
    if (unwind == null) {
      throw new IllegalArgumentException("null value for 'unwind' argument");
    }
    this.union = union;
    this.unionAll = unionAll;
    this.unwind = unwind;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ReadingFeatures)) {
      return false;
    }
    ReadingFeatures o = (ReadingFeatures) (other);
    return union.equals(o.union) && unionAll.equals(o.unionAll) && unwind.equals(o.unwind);
  }
  
  @Override
  public int hashCode() {
    return 2 * union.hashCode() + 3 * unionAll.hashCode() + 5 * unwind.hashCode();
  }
  
  public ReadingFeatures withUnion(Boolean union) {
    if (union == null) {
      throw new IllegalArgumentException("null value for 'union' argument");
    }
    return new ReadingFeatures(union, unionAll, unwind);
  }
  
  public ReadingFeatures withUnionAll(Boolean unionAll) {
    if (unionAll == null) {
      throw new IllegalArgumentException("null value for 'unionAll' argument");
    }
    return new ReadingFeatures(union, unionAll, unwind);
  }
  
  public ReadingFeatures withUnwind(Boolean unwind) {
    if (unwind == null) {
      throw new IllegalArgumentException("null value for 'unwind' argument");
    }
    return new ReadingFeatures(union, unionAll, unwind);
  }
}