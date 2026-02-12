// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class LambdaSlashNoDefault implements Serializable, Comparable<LambdaSlashNoDefault> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.LambdaSlashNoDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETERS = new hydra.core.Name("parameters");
  
  public final java.util.List<hydra.ext.python.syntax.LambdaParamNoDefault> parameters;
  
  public LambdaSlashNoDefault (java.util.List<hydra.ext.python.syntax.LambdaParamNoDefault> parameters) {
    this.parameters = parameters;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LambdaSlashNoDefault)) {
      return false;
    }
    LambdaSlashNoDefault o = (LambdaSlashNoDefault) other;
    return java.util.Objects.equals(
      this.parameters,
      o.parameters);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(parameters);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LambdaSlashNoDefault other) {
    return Integer.compare(
      parameters.hashCode(),
      other.parameters.hashCode());
  }
}
