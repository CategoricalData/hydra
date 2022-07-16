package hydra.ext.tinkerpop.features;

/**
 * Features for Graph.Variables.
 */
public class VariableFeatures {
  public final DataTypeFeatures dataTypeFeatures;
  
  /**
   * If any of the features on Graph.Features.VariableFeatures is true then this value must be true.
   */
  public final Boolean supportsVariables;
  
  public VariableFeatures (DataTypeFeatures dataTypeFeatures, Boolean supportsVariables) {
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
  
  public VariableFeatures withDataTypeFeatures(DataTypeFeatures dataTypeFeatures) {
    return new VariableFeatures(dataTypeFeatures, supportsVariables);
  }
  
  public VariableFeatures withSupportsVariables(Boolean supportsVariables) {
    return new VariableFeatures(dataTypeFeatures, supportsVariables);
  }
}