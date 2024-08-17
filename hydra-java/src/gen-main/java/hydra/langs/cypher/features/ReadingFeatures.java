// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * Specific syntax related to reading data from the graph.
 */
public class ReadingFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/features.ReadingFeatures");
  
  public static final hydra.core.Name FIELD_NAME_UNION = new hydra.core.Name("union");
  
  public static final hydra.core.Name FIELD_NAME_UNION_ALL = new hydra.core.Name("unionAll");
  
  public static final hydra.core.Name FIELD_NAME_UNWIND = new hydra.core.Name("unwind");
  
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
    java.util.Objects.requireNonNull((union));
    java.util.Objects.requireNonNull((unionAll));
    java.util.Objects.requireNonNull((unwind));
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
    java.util.Objects.requireNonNull((union));
    return new ReadingFeatures(union, unionAll, unwind);
  }
  
  public ReadingFeatures withUnionAll(Boolean unionAll) {
    java.util.Objects.requireNonNull((unionAll));
    return new ReadingFeatures(union, unionAll, unwind);
  }
  
  public ReadingFeatures withUnwind(Boolean unwind) {
    java.util.Objects.requireNonNull((unwind));
    return new ReadingFeatures(union, unionAll, unwind);
  }
}