// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for set definitions.
 */
public class SetFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.SetFeatures");
  
  /**
   * Whether to expect defining a set using PropertyExpression = Expression.
   */
  public final Boolean propertyEquals;
  
  /**
   * Whether to expect defining a set using Variable = Expression.
   */
  public final Boolean variableEquals;
  
  /**
   * Whether to expect defining a set using Variable += Expression.
   */
  public final Boolean variablePlusEquals;
  
  /**
   * Whether to expect defining a set using Variable:NodeLabels.
   */
  public final Boolean variableWithNodeLabels;
  
  public SetFeatures (Boolean propertyEquals, Boolean variableEquals, Boolean variablePlusEquals, Boolean variableWithNodeLabels) {
    if (propertyEquals == null) {
      throw new IllegalArgumentException("null value for 'propertyEquals' argument");
    }
    if (variableEquals == null) {
      throw new IllegalArgumentException("null value for 'variableEquals' argument");
    }
    if (variablePlusEquals == null) {
      throw new IllegalArgumentException("null value for 'variablePlusEquals' argument");
    }
    if (variableWithNodeLabels == null) {
      throw new IllegalArgumentException("null value for 'variableWithNodeLabels' argument");
    }
    this.propertyEquals = propertyEquals;
    this.variableEquals = variableEquals;
    this.variablePlusEquals = variablePlusEquals;
    this.variableWithNodeLabels = variableWithNodeLabels;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SetFeatures)) {
      return false;
    }
    SetFeatures o = (SetFeatures) (other);
    return propertyEquals.equals(o.propertyEquals) && variableEquals.equals(o.variableEquals) && variablePlusEquals.equals(o.variablePlusEquals) && variableWithNodeLabels.equals(o.variableWithNodeLabels);
  }
  
  @Override
  public int hashCode() {
    return 2 * propertyEquals.hashCode() + 3 * variableEquals.hashCode() + 5 * variablePlusEquals.hashCode() + 7 * variableWithNodeLabels.hashCode();
  }
  
  public SetFeatures withPropertyEquals(Boolean propertyEquals) {
    if (propertyEquals == null) {
      throw new IllegalArgumentException("null value for 'propertyEquals' argument");
    }
    return new SetFeatures(propertyEquals, variableEquals, variablePlusEquals, variableWithNodeLabels);
  }
  
  public SetFeatures withVariableEquals(Boolean variableEquals) {
    if (variableEquals == null) {
      throw new IllegalArgumentException("null value for 'variableEquals' argument");
    }
    return new SetFeatures(propertyEquals, variableEquals, variablePlusEquals, variableWithNodeLabels);
  }
  
  public SetFeatures withVariablePlusEquals(Boolean variablePlusEquals) {
    if (variablePlusEquals == null) {
      throw new IllegalArgumentException("null value for 'variablePlusEquals' argument");
    }
    return new SetFeatures(propertyEquals, variableEquals, variablePlusEquals, variableWithNodeLabels);
  }
  
  public SetFeatures withVariableWithNodeLabels(Boolean variableWithNodeLabels) {
    if (variableWithNodeLabels == null) {
      throw new IllegalArgumentException("null value for 'variableWithNodeLabels' argument");
    }
    return new SetFeatures(propertyEquals, variableEquals, variablePlusEquals, variableWithNodeLabels);
  }
}