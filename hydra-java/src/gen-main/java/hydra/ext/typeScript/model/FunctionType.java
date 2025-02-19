// Note: this is an automatically generated file. Do not edit.

package hydra.ext.typeScript.model;

import java.io.Serializable;

public class FunctionType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.typeScript.model.FunctionType");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETERS = new hydra.core.Name("parameters");
  
  public static final hydra.core.Name FIELD_NAME_RANGE = new hydra.core.Name("range");
  
  public final java.util.List<hydra.ext.typeScript.model.Parameter> parameters;
  
  public final hydra.ext.typeScript.model.Type range;
  
  public FunctionType (java.util.List<hydra.ext.typeScript.model.Parameter> parameters, hydra.ext.typeScript.model.Type range) {
    java.util.Objects.requireNonNull((parameters));
    java.util.Objects.requireNonNull((range));
    this.parameters = parameters;
    this.range = range;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FunctionType)) {
      return false;
    }
    FunctionType o = (FunctionType) (other);
    return parameters.equals(o.parameters) && range.equals(o.range);
  }
  
  @Override
  public int hashCode() {
    return 2 * parameters.hashCode() + 3 * range.hashCode();
  }
  
  public FunctionType withParameters(java.util.List<hydra.ext.typeScript.model.Parameter> parameters) {
    java.util.Objects.requireNonNull((parameters));
    return new FunctionType(parameters, range);
  }
  
  public FunctionType withRange(hydra.ext.typeScript.model.Type range) {
    java.util.Objects.requireNonNull((range));
    return new FunctionType(parameters, range);
  }
}