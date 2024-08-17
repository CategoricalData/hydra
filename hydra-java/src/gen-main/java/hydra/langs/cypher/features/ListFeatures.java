// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * List functionality
 */
public class ListFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/features.ListFeatures");
  
  public static final hydra.core.Name FIELD_NAME_LIST_COMPREHENSION = new hydra.core.Name("listComprehension");
  
  public static final hydra.core.Name FIELD_NAME_LIST_RANGE = new hydra.core.Name("listRange");
  
  /**
   * Basic list comprehensions
   */
  public final Boolean listComprehension;
  
  /**
   * List range comprehensions (e.g. [1..10])
   */
  public final Boolean listRange;
  
  public ListFeatures (Boolean listComprehension, Boolean listRange) {
    java.util.Objects.requireNonNull((listComprehension));
    java.util.Objects.requireNonNull((listRange));
    this.listComprehension = listComprehension;
    this.listRange = listRange;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ListFeatures)) {
      return false;
    }
    ListFeatures o = (ListFeatures) (other);
    return listComprehension.equals(o.listComprehension) && listRange.equals(o.listRange);
  }
  
  @Override
  public int hashCode() {
    return 2 * listComprehension.hashCode() + 3 * listRange.hashCode();
  }
  
  public ListFeatures withListComprehension(Boolean listComprehension) {
    java.util.Objects.requireNonNull((listComprehension));
    return new ListFeatures(listComprehension, listRange);
  }
  
  public ListFeatures withListRange(Boolean listRange) {
    java.util.Objects.requireNonNull((listRange));
    return new ListFeatures(listComprehension, listRange);
  }
}