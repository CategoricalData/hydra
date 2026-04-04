// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Set definitions
 */
public class SetFeatures implements Serializable, Comparable<SetFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.SetFeatures");

  public static final hydra.core.Name PROPERTY_EQUALS = new hydra.core.Name("propertyEquals");

  public static final hydra.core.Name VARIABLE_EQUALS = new hydra.core.Name("variableEquals");

  public static final hydra.core.Name VARIABLE_PLUS_EQUALS = new hydra.core.Name("variablePlusEquals");

  public static final hydra.core.Name VARIABLE_WITH_NODE_LABELS = new hydra.core.Name("variableWithNodeLabels");

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
    SetFeatures o = (SetFeatures) other;
    return java.util.Objects.equals(
      this.propertyEquals,
      o.propertyEquals) && java.util.Objects.equals(
      this.variableEquals,
      o.variableEquals) && java.util.Objects.equals(
      this.variablePlusEquals,
      o.variablePlusEquals) && java.util.Objects.equals(
      this.variableWithNodeLabels,
      o.variableWithNodeLabels);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(propertyEquals) + 3 * java.util.Objects.hashCode(variableEquals) + 5 * java.util.Objects.hashCode(variablePlusEquals) + 7 * java.util.Objects.hashCode(variableWithNodeLabels);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SetFeatures other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      propertyEquals,
      other.propertyEquals);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      variableEquals,
      other.variableEquals);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      variablePlusEquals,
      other.variablePlusEquals);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      variableWithNodeLabels,
      other.variableWithNodeLabels);
  }

  public SetFeatures withPropertyEquals(Boolean propertyEquals) {
    return new SetFeatures(propertyEquals, variableEquals, variablePlusEquals, variableWithNodeLabels);
  }

  public SetFeatures withVariableEquals(Boolean variableEquals) {
    return new SetFeatures(propertyEquals, variableEquals, variablePlusEquals, variableWithNodeLabels);
  }

  public SetFeatures withVariablePlusEquals(Boolean variablePlusEquals) {
    return new SetFeatures(propertyEquals, variableEquals, variablePlusEquals, variableWithNodeLabels);
  }

  public SetFeatures withVariableWithNodeLabels(Boolean variableWithNodeLabels) {
    return new SetFeatures(propertyEquals, variableEquals, variablePlusEquals, variableWithNodeLabels);
  }
}
