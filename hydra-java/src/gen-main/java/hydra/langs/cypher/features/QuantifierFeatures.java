// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for quantifier expressions.
 */
public class QuantifierFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/features.QuantifierFeatures");
  
  public static final hydra.core.Name FIELD_NAME_ALL = new hydra.core.Name("all");
  
  public static final hydra.core.Name FIELD_NAME_ANY = new hydra.core.Name("any");
  
  public static final hydra.core.Name FIELD_NAME_EXISTS = new hydra.core.Name("exists");
  
  public static final hydra.core.Name FIELD_NAME_NONE = new hydra.core.Name("none");
  
  public static final hydra.core.Name FIELD_NAME_SINGLE = new hydra.core.Name("single");
  
  /**
   * Whether to expect the ALL quantifier.
   */
  public final Boolean all;
  
  /**
   * Whether to expect the ANY quantifier.
   */
  public final Boolean any;
  
  /**
   * Whether to expect the exists() function.
   */
  public final Boolean exists;
  
  /**
   * Whether to expect the NONE quantifier.
   */
  public final Boolean none;
  
  /**
   * Whether to expect the SINGLE quantifier.
   */
  public final Boolean single;
  
  public QuantifierFeatures (Boolean all, Boolean any, Boolean exists, Boolean none, Boolean single) {
    java.util.Objects.requireNonNull((all));
    java.util.Objects.requireNonNull((any));
    java.util.Objects.requireNonNull((exists));
    java.util.Objects.requireNonNull((none));
    java.util.Objects.requireNonNull((single));
    this.all = all;
    this.any = any;
    this.exists = exists;
    this.none = none;
    this.single = single;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QuantifierFeatures)) {
      return false;
    }
    QuantifierFeatures o = (QuantifierFeatures) (other);
    return all.equals(o.all) && any.equals(o.any) && exists.equals(o.exists) && none.equals(o.none) && single.equals(o.single);
  }
  
  @Override
  public int hashCode() {
    return 2 * all.hashCode() + 3 * any.hashCode() + 5 * exists.hashCode() + 7 * none.hashCode() + 11 * single.hashCode();
  }
  
  public QuantifierFeatures withAll(Boolean all) {
    java.util.Objects.requireNonNull((all));
    return new QuantifierFeatures(all, any, exists, none, single);
  }
  
  public QuantifierFeatures withAny(Boolean any) {
    java.util.Objects.requireNonNull((any));
    return new QuantifierFeatures(all, any, exists, none, single);
  }
  
  public QuantifierFeatures withExists(Boolean exists) {
    java.util.Objects.requireNonNull((exists));
    return new QuantifierFeatures(all, any, exists, none, single);
  }
  
  public QuantifierFeatures withNone(Boolean none) {
    java.util.Objects.requireNonNull((none));
    return new QuantifierFeatures(all, any, exists, none, single);
  }
  
  public QuantifierFeatures withSingle(Boolean single) {
    java.util.Objects.requireNonNull((single));
    return new QuantifierFeatures(all, any, exists, none, single);
  }
}