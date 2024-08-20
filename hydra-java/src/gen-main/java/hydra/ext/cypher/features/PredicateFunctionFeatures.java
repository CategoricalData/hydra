// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Predicate functions
 */
public class PredicateFunctionFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/features.PredicateFunctionFeatures");
  
  public static final hydra.core.Name FIELD_NAME_ALL = new hydra.core.Name("all");
  
  public static final hydra.core.Name FIELD_NAME_ANY = new hydra.core.Name("any");
  
  public static final hydra.core.Name FIELD_NAME_EXISTS = new hydra.core.Name("exists");
  
  public static final hydra.core.Name FIELD_NAME_IS_EMPTY = new hydra.core.Name("isEmpty");
  
  public static final hydra.core.Name FIELD_NAME_NONE = new hydra.core.Name("none");
  
  public static final hydra.core.Name FIELD_NAME_SINGLE = new hydra.core.Name("single");
  
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
    java.util.Objects.requireNonNull((all));
    java.util.Objects.requireNonNull((any));
    java.util.Objects.requireNonNull((exists));
    java.util.Objects.requireNonNull((isEmpty));
    java.util.Objects.requireNonNull((none));
    java.util.Objects.requireNonNull((single));
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
    PredicateFunctionFeatures o = (PredicateFunctionFeatures) (other);
    return all.equals(o.all) && any.equals(o.any) && exists.equals(o.exists) && isEmpty.equals(o.isEmpty) && none.equals(o.none) && single.equals(o.single);
  }
  
  @Override
  public int hashCode() {
    return 2 * all.hashCode() + 3 * any.hashCode() + 5 * exists.hashCode() + 7 * isEmpty.hashCode() + 11 * none.hashCode() + 13 * single.hashCode();
  }
  
  public PredicateFunctionFeatures withAll(Boolean all) {
    java.util.Objects.requireNonNull((all));
    return new PredicateFunctionFeatures(all, any, exists, isEmpty, none, single);
  }
  
  public PredicateFunctionFeatures withAny(Boolean any) {
    java.util.Objects.requireNonNull((any));
    return new PredicateFunctionFeatures(all, any, exists, isEmpty, none, single);
  }
  
  public PredicateFunctionFeatures withExists(Boolean exists) {
    java.util.Objects.requireNonNull((exists));
    return new PredicateFunctionFeatures(all, any, exists, isEmpty, none, single);
  }
  
  public PredicateFunctionFeatures withIsEmpty(Boolean isEmpty) {
    java.util.Objects.requireNonNull((isEmpty));
    return new PredicateFunctionFeatures(all, any, exists, isEmpty, none, single);
  }
  
  public PredicateFunctionFeatures withNone(Boolean none) {
    java.util.Objects.requireNonNull((none));
    return new PredicateFunctionFeatures(all, any, exists, isEmpty, none, single);
  }
  
  public PredicateFunctionFeatures withSingle(Boolean single) {
    java.util.Objects.requireNonNull((single));
    return new PredicateFunctionFeatures(all, any, exists, isEmpty, none, single);
  }
}