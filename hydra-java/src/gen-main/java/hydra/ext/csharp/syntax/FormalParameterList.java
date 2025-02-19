// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class FormalParameterList implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.FormalParameterList");
  
  public static final hydra.core.Name FIELD_NAME_FIXED = new hydra.core.Name("fixed");
  
  public static final hydra.core.Name FIELD_NAME_ARRAY = new hydra.core.Name("array");
  
  public final java.util.List<hydra.ext.csharp.syntax.FixedParameter> fixed;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.ParameterArray> array;
  
  public FormalParameterList (java.util.List<hydra.ext.csharp.syntax.FixedParameter> fixed, hydra.util.Opt<hydra.ext.csharp.syntax.ParameterArray> array) {
    java.util.Objects.requireNonNull((fixed));
    java.util.Objects.requireNonNull((array));
    this.fixed = fixed;
    this.array = array;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FormalParameterList)) {
      return false;
    }
    FormalParameterList o = (FormalParameterList) (other);
    return fixed.equals(o.fixed) && array.equals(o.array);
  }
  
  @Override
  public int hashCode() {
    return 2 * fixed.hashCode() + 3 * array.hashCode();
  }
  
  public FormalParameterList withFixed(java.util.List<hydra.ext.csharp.syntax.FixedParameter> fixed) {
    java.util.Objects.requireNonNull((fixed));
    return new FormalParameterList(fixed, array);
  }
  
  public FormalParameterList withArray(hydra.util.Opt<hydra.ext.csharp.syntax.ParameterArray> array) {
    java.util.Objects.requireNonNull((array));
    return new FormalParameterList(fixed, array);
  }
}