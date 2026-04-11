// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.features;

import java.io.Serializable;

/**
 * Features for Graph.Variables.
 */
public class VariableFeatures implements Serializable, Comparable<VariableFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.features.VariableFeatures");

  public static final hydra.core.Name DATA_TYPE_FEATURES = new hydra.core.Name("dataTypeFeatures");

  public static final hydra.core.Name SUPPORTS_VARIABLES = new hydra.core.Name("supportsVariables");

  public final hydra.ext.org.apache.tinkerpop.features.DataTypeFeatures dataTypeFeatures;

  /**
   * If any of the features on Graph.Features.VariableFeatures is true then this value must be true.
   */
  public final Boolean supportsVariables;

  public VariableFeatures (hydra.ext.org.apache.tinkerpop.features.DataTypeFeatures dataTypeFeatures, Boolean supportsVariables) {
    this.dataTypeFeatures = dataTypeFeatures;
    this.supportsVariables = supportsVariables;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariableFeatures)) {
      return false;
    }
    VariableFeatures o = (VariableFeatures) other;
    return java.util.Objects.equals(
      this.dataTypeFeatures,
      o.dataTypeFeatures) && java.util.Objects.equals(
      this.supportsVariables,
      o.supportsVariables);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(dataTypeFeatures) + 3 * java.util.Objects.hashCode(supportsVariables);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(VariableFeatures other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      dataTypeFeatures,
      other.dataTypeFeatures);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      supportsVariables,
      other.supportsVariables);
  }

  public VariableFeatures withDataTypeFeatures(hydra.ext.org.apache.tinkerpop.features.DataTypeFeatures dataTypeFeatures) {
    return new VariableFeatures(dataTypeFeatures, supportsVariables);
  }

  public VariableFeatures withSupportsVariables(Boolean supportsVariables) {
    return new VariableFeatures(dataTypeFeatures, supportsVariables);
  }
}
