// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.features;

import java.io.Serializable;

/**
 * List functionality
 */
public class ListFeatures implements Serializable, Comparable<ListFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.features.ListFeatures");

  public static final hydra.core.Name LIST_COMPREHENSION = new hydra.core.Name("listComprehension");

  public static final hydra.core.Name LIST_RANGE = new hydra.core.Name("listRange");

  /**
   * Basic list comprehensions
   */
  public final Boolean listComprehension;

  /**
   * List range comprehensions (e.g. [1..10])
   */
  public final Boolean listRange;

  public ListFeatures (Boolean listComprehension, Boolean listRange) {
    this.listComprehension = listComprehension;
    this.listRange = listRange;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ListFeatures)) {
      return false;
    }
    ListFeatures o = (ListFeatures) other;
    return java.util.Objects.equals(
      this.listComprehension,
      o.listComprehension) && java.util.Objects.equals(
      this.listRange,
      o.listRange);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(listComprehension) + 3 * java.util.Objects.hashCode(listRange);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ListFeatures other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      listComprehension,
      other.listComprehension);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      listRange,
      other.listRange);
  }

  public ListFeatures withListComprehension(Boolean listComprehension) {
    return new ListFeatures(listComprehension, listRange);
  }

  public ListFeatures withListRange(Boolean listRange) {
    return new ListFeatures(listComprehension, listRange);
  }
}
