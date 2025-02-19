// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * REMOVE operations
 */
public class RemoveFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.features.RemoveFeatures");
  
  public static final hydra.core.Name FIELD_NAME_BY_LABEL = new hydra.core.Name("byLabel");
  
  public static final hydra.core.Name FIELD_NAME_BY_PROPERTY = new hydra.core.Name("byProperty");
  
  /**
   * REMOVE Variable:NodeLabels
   */
  public final Boolean byLabel;
  
  /**
   * REMOVE PropertyExpression
   */
  public final Boolean byProperty;
  
  public RemoveFeatures (Boolean byLabel, Boolean byProperty) {
    java.util.Objects.requireNonNull((byLabel));
    java.util.Objects.requireNonNull((byProperty));
    this.byLabel = byLabel;
    this.byProperty = byProperty;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RemoveFeatures)) {
      return false;
    }
    RemoveFeatures o = (RemoveFeatures) (other);
    return byLabel.equals(o.byLabel) && byProperty.equals(o.byProperty);
  }
  
  @Override
  public int hashCode() {
    return 2 * byLabel.hashCode() + 3 * byProperty.hashCode();
  }
  
  public RemoveFeatures withByLabel(Boolean byLabel) {
    java.util.Objects.requireNonNull((byLabel));
    return new RemoveFeatures(byLabel, byProperty);
  }
  
  public RemoveFeatures withByProperty(Boolean byProperty) {
    java.util.Objects.requireNonNull((byProperty));
    return new RemoveFeatures(byLabel, byProperty);
  }
}