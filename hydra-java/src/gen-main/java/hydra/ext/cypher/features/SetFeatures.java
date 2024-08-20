// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Set definitions
 */
public class SetFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/features.SetFeatures");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY_EQUALS = new hydra.core.Name("propertyEquals");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLE_EQUALS = new hydra.core.Name("variableEquals");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLE_PLUS_EQUALS = new hydra.core.Name("variablePlusEquals");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLE_WITH_NODE_LABELS = new hydra.core.Name("variableWithNodeLabels");
  
  /**
   * Defining a set using PropertyExpression = Expression
   */
  public final Boolean propertyEquals;
  
  /**
   * Defining a set using Variable = Expression
   */
  public final Boolean variableEquals;
  
  /**
   * Defining a set using Variable += Expression
   */
  public final Boolean variablePlusEquals;
  
  /**
   * Defining a set using Variable:NodeLabels
   */
  public final Boolean variableWithNodeLabels;
  
  public SetFeatures (Boolean propertyEquals, Boolean variableEquals, Boolean variablePlusEquals, Boolean variableWithNodeLabels) {
    java.util.Objects.requireNonNull((propertyEquals));
    java.util.Objects.requireNonNull((variableEquals));
    java.util.Objects.requireNonNull((variablePlusEquals));
    java.util.Objects.requireNonNull((variableWithNodeLabels));
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
    java.util.Objects.requireNonNull((propertyEquals));
    return new SetFeatures(propertyEquals, variableEquals, variablePlusEquals, variableWithNodeLabels);
  }
  
  public SetFeatures withVariableEquals(Boolean variableEquals) {
    java.util.Objects.requireNonNull((variableEquals));
    return new SetFeatures(propertyEquals, variableEquals, variablePlusEquals, variableWithNodeLabels);
  }
  
  public SetFeatures withVariablePlusEquals(Boolean variablePlusEquals) {
    java.util.Objects.requireNonNull((variablePlusEquals));
    return new SetFeatures(propertyEquals, variableEquals, variablePlusEquals, variableWithNodeLabels);
  }
  
  public SetFeatures withVariableWithNodeLabels(Boolean variableWithNodeLabels) {
    java.util.Objects.requireNonNull((variableWithNodeLabels));
    return new SetFeatures(propertyEquals, variableEquals, variablePlusEquals, variableWithNodeLabels);
  }
}