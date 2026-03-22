// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Predicate functions
 */
public class PredicateFunctionFeatures implements Serializable, Comparable<PredicateFunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.PredicateFunctionFeatures");

  public static final hydra.core.Name ALL = new hydra.core.Name("all");

  public static final hydra.core.Name ANY = new hydra.core.Name("any");

  public static final hydra.core.Name EXISTS = new hydra.core.Name("exists");

  public static final hydra.core.Name IS_EMPTY = new hydra.core.Name("isEmpty");

  public static final hydra.core.Name NONE = new hydra.core.Name("none");

  public static final hydra.core.Name SINGLE = new hydra.core.Name("single");

  /**
   * The all() function. Returns true if the predicate holds for all elements in the given LIST&lt;ANY&gt;.
   */
  public final Boolean all;

  /**
   * The any() function. Returns true if the predicate holds for at least one element in the given LIST&lt;ANY&gt;.
   */
  public final Boolean any;

  /**
   * The exists() function. Returns true if a match for the pattern exists in the graph.
   */
  public final Boolean exists;

  /**
   * The isEmpty() function. Checks whether a LIST&lt;ANY&gt; is empty.; Checks whether a MAP is empty.; Checks whether a STRING is empty.
   */
  public final Boolean isEmpty;

  /**
   * The none() function. Returns true if the predicate holds for no element in the given LIST&lt;ANY&gt;.
   */
  public final Boolean none;

  /**
   * The single() function. Returns true if the predicate holds for exactly one of the elements in the given LIST&lt;ANY&gt;.
   */
  public final Boolean single;

  public PredicateFunctionFeatures (Boolean all, Boolean any, Boolean exists, Boolean isEmpty, Boolean none, Boolean single) {
    this.all = all;
    this.any = any;
    this.exists = exists;
    this.isEmpty = isEmpty;
    this.none = none;
    this.single = single;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PredicateFunctionFeatures)) {
      return false;
    }
    PredicateFunctionFeatures o = (PredicateFunctionFeatures) other;
    return java.util.Objects.equals(
      this.all,
      o.all) && java.util.Objects.equals(
      this.any,
      o.any) && java.util.Objects.equals(
      this.exists,
      o.exists) && java.util.Objects.equals(
      this.isEmpty,
      o.isEmpty) && java.util.Objects.equals(
      this.none,
      o.none) && java.util.Objects.equals(
      this.single,
      o.single);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(all) + 3 * java.util.Objects.hashCode(any) + 5 * java.util.Objects.hashCode(exists) + 7 * java.util.Objects.hashCode(isEmpty) + 11 * java.util.Objects.hashCode(none) + 13 * java.util.Objects.hashCode(single);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PredicateFunctionFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) all).compareTo(other.all);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) any).compareTo(other.any);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) exists).compareTo(other.exists);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) isEmpty).compareTo(other.isEmpty);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) none).compareTo(other.none);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) single).compareTo(other.single);
  }

  public PredicateFunctionFeatures withAll(Boolean all) {
    return new PredicateFunctionFeatures(all, any, exists, isEmpty, none, single);
  }

  public PredicateFunctionFeatures withAny(Boolean any) {
    return new PredicateFunctionFeatures(all, any, exists, isEmpty, none, single);
  }

  public PredicateFunctionFeatures withExists(Boolean exists) {
    return new PredicateFunctionFeatures(all, any, exists, isEmpty, none, single);
  }

  public PredicateFunctionFeatures withIsEmpty(Boolean isEmpty) {
    return new PredicateFunctionFeatures(all, any, exists, isEmpty, none, single);
  }

  public PredicateFunctionFeatures withNone(Boolean none) {
    return new PredicateFunctionFeatures(all, any, exists, isEmpty, none, single);
  }

  public PredicateFunctionFeatures withSingle(Boolean single) {
    return new PredicateFunctionFeatures(all, any, exists, isEmpty, none, single);
  }
}
