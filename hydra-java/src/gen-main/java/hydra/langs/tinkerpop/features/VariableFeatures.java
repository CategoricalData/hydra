// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.features;

import java.io.Serializable;

/**
 * Features for Graph.Variables.
 */
public class VariableFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/features.VariableFeatures");
  
  public static final hydra.core.Name FIELD_NAME_DATA_TYPE_FEATURES = new hydra.core.Name("dataTypeFeatures");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_VARIABLES = new hydra.core.Name("supportsVariables");
  
  public final hydra.langs.tinkerpop.features.DataTypeFeatures dataTypeFeatures;
  
  /**
   * If any of the features on Graph.Features.VariableFeatures is true then this value must be true.
   */
  public final Boolean supportsVariables;
  
  public VariableFeatures (hydra.langs.tinkerpop.features.DataTypeFeatures dataTypeFeatures, Boolean supportsVariables) {
    java.util.Objects.requireNonNull((dataTypeFeatures));
    java.util.Objects.requireNonNull((supportsVariables));
    this.dataTypeFeatures = dataTypeFeatures;
    this.supportsVariables = supportsVariables;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariableFeatures)) {
      return false;
    }
    VariableFeatures o = (VariableFeatures) (other);
    return dataTypeFeatures.equals(o.dataTypeFeatures) && supportsVariables.equals(o.supportsVariables);
  }
  
  @Override
  public int hashCode() {
    return 2 * dataTypeFeatures.hashCode() + 3 * supportsVariables.hashCode();
  }
  
  public VariableFeatures withDataTypeFeatures(hydra.langs.tinkerpop.features.DataTypeFeatures dataTypeFeatures) {
    java.util.Objects.requireNonNull((dataTypeFeatures));
    return new VariableFeatures(dataTypeFeatures, supportsVariables);
  }
  
  public VariableFeatures withSupportsVariables(Boolean supportsVariables) {
    java.util.Objects.requireNonNull((supportsVariables));
    return new VariableFeatures(dataTypeFeatures, supportsVariables);
  }
}