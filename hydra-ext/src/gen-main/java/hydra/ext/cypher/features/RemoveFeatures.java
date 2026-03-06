// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * REMOVE operations
 */
public class RemoveFeatures implements Serializable, Comparable<RemoveFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.RemoveFeatures");
  
  public static final hydra.core.Name BY_LABEL = new hydra.core.Name("byLabel");
  
  public static final hydra.core.Name BY_PROPERTY = new hydra.core.Name("byProperty");
  
  /**
   * REMOVE Variable:NodeLabels
   */
  public final Boolean byLabel;
  
  /**
   * REMOVE PropertyExpression
   */
  public final Boolean byProperty;
  
  public RemoveFeatures (Boolean byLabel, Boolean byProperty) {
    this.byLabel = byLabel;
    this.byProperty = byProperty;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RemoveFeatures)) {
      return false;
    }
    RemoveFeatures o = (RemoveFeatures) other;
    return java.util.Objects.equals(
      this.byLabel,
      o.byLabel) && java.util.Objects.equals(
      this.byProperty,
      o.byProperty);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(byLabel) + 3 * java.util.Objects.hashCode(byProperty);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RemoveFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) byLabel).compareTo(other.byLabel);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) byProperty).compareTo(other.byProperty);
  }
  
  public RemoveFeatures withByLabel(Boolean byLabel) {
    return new RemoveFeatures(byLabel, byProperty);
  }
  
  public RemoveFeatures withByProperty(Boolean byProperty) {
    return new RemoveFeatures(byLabel, byProperty);
  }
}
