// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.helpers;

import java.io.Serializable;

/**
 * Feature flags for the target Java version
 */
public class JavaFeatures implements Serializable, Comparable<JavaFeatures> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.helpers.JavaFeatures");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_DIAMOND_OPERATOR = new hydra.core.Name("supportsDiamondOperator");
  
  /**
   * Whether the diamond operator (&lt;&gt;) is supported (Java 7+)
   */
  public final Boolean supportsDiamondOperator;
  
  public JavaFeatures (Boolean supportsDiamondOperator) {
    this.supportsDiamondOperator = supportsDiamondOperator;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof JavaFeatures)) {
      return false;
    }
    JavaFeatures o = (JavaFeatures) other;
    return java.util.Objects.equals(
      this.supportsDiamondOperator,
      o.supportsDiamondOperator);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(supportsDiamondOperator);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(JavaFeatures other) {
    return ((Comparable) supportsDiamondOperator).compareTo(other.supportsDiamondOperator);
  }
}
