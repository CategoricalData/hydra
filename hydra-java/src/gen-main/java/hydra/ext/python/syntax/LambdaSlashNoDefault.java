// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class LambdaSlashNoDefault implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.LambdaSlashNoDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETERS = new hydra.core.Name("parameters");
  
  public final java.util.List<hydra.ext.python.syntax.LambdaParamNoDefault> parameters;
  
  public LambdaSlashNoDefault (java.util.List<hydra.ext.python.syntax.LambdaParamNoDefault> parameters) {
    java.util.Objects.requireNonNull((parameters));
    this.parameters = parameters;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LambdaSlashNoDefault)) {
      return false;
    }
    LambdaSlashNoDefault o = (LambdaSlashNoDefault) (other);
    return parameters.equals(o.parameters);
  }
  
  @Override
  public int hashCode() {
    return 2 * parameters.hashCode();
  }
}