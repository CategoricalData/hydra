// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for REMOVE operations.
 */
public class RemoveFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.RemoveFeatures");
  
  /**
   * Whether to expect REMOVE Variable:NodeLabels.
   */
  public final Boolean byLabel;
  
  /**
   * Whether to expect REMOVE PropertyExpression.
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