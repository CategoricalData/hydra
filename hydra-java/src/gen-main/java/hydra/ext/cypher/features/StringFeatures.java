// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * String functions/keywords only found in OpenCypher
 */
public class StringFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/features.StringFeatures");
  
  public static final hydra.core.Name FIELD_NAME_CONTAINS = new hydra.core.Name("contains");
  
  public static final hydra.core.Name FIELD_NAME_ENDS_WITH = new hydra.core.Name("endsWith");
  
  public static final hydra.core.Name FIELD_NAME_IN = new hydra.core.Name("in");
  
  public static final hydra.core.Name FIELD_NAME_STARTS_WITH = new hydra.core.Name("startsWith");
  
  /**
   * The contains() function / CONTAINS
   */
  public final Boolean contains;
  
  /**
   * The endsWith() function / ENDS WITH
   */
  public final Boolean endsWith;
  
  /**
   * The in() function / IN
   */
  public final Boolean in;
  
  /**
   * The startsWith() function / STARTS WITH
   */
  public final Boolean startsWith;
  
  public StringFeatures (Boolean contains, Boolean endsWith, Boolean in, Boolean startsWith) {
    java.util.Objects.requireNonNull((contains));
    java.util.Objects.requireNonNull((endsWith));
    java.util.Objects.requireNonNull((in));
    java.util.Objects.requireNonNull((startsWith));
    this.contains = contains;
    this.endsWith = endsWith;
    this.in = in;
    this.startsWith = startsWith;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringFeatures)) {
      return false;
    }
    StringFeatures o = (StringFeatures) (other);
    return contains.equals(o.contains) && endsWith.equals(o.endsWith) && in.equals(o.in) && startsWith.equals(o.startsWith);
  }
  
  @Override
  public int hashCode() {
    return 2 * contains.hashCode() + 3 * endsWith.hashCode() + 5 * in.hashCode() + 7 * startsWith.hashCode();
  }
  
  public StringFeatures withContains(Boolean contains) {
    java.util.Objects.requireNonNull((contains));
    return new StringFeatures(contains, endsWith, in, startsWith);
  }
  
  public StringFeatures withEndsWith(Boolean endsWith) {
    java.util.Objects.requireNonNull((endsWith));
    return new StringFeatures(contains, endsWith, in, startsWith);
  }
  
  public StringFeatures withIn(Boolean in) {
    java.util.Objects.requireNonNull((in));
    return new StringFeatures(contains, endsWith, in, startsWith);
  }
  
  public StringFeatures withStartsWith(Boolean startsWith) {
    java.util.Objects.requireNonNull((startsWith));
    return new StringFeatures(contains, endsWith, in, startsWith);
  }
}