// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class LambdaSlashWithDefault implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.LambdaSlashWithDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_NO_DEFAULT = new hydra.core.Name("paramNoDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_WITH_DEFAULT = new hydra.core.Name("paramWithDefault");
  
  public final java.util.List<hydra.ext.python.syntax.LambdaParamNoDefault> paramNoDefault;
  
  public final java.util.List<hydra.ext.python.syntax.LambdaParamWithDefault> paramWithDefault;
  
  public LambdaSlashWithDefault (java.util.List<hydra.ext.python.syntax.LambdaParamNoDefault> paramNoDefault, java.util.List<hydra.ext.python.syntax.LambdaParamWithDefault> paramWithDefault) {
    java.util.Objects.requireNonNull((paramNoDefault));
    java.util.Objects.requireNonNull((paramWithDefault));
    this.paramNoDefault = paramNoDefault;
    this.paramWithDefault = paramWithDefault;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LambdaSlashWithDefault)) {
      return false;
    }
    LambdaSlashWithDefault o = (LambdaSlashWithDefault) (other);
    return paramNoDefault.equals(o.paramNoDefault) && paramWithDefault.equals(o.paramWithDefault);
  }
  
  @Override
  public int hashCode() {
    return 2 * paramNoDefault.hashCode() + 3 * paramWithDefault.hashCode();
  }
  
  public LambdaSlashWithDefault withParamNoDefault(java.util.List<hydra.ext.python.syntax.LambdaParamNoDefault> paramNoDefault) {
    java.util.Objects.requireNonNull((paramNoDefault));
    return new LambdaSlashWithDefault(paramNoDefault, paramWithDefault);
  }
  
  public LambdaSlashWithDefault withParamWithDefault(java.util.List<hydra.ext.python.syntax.LambdaParamWithDefault> paramWithDefault) {
    java.util.Objects.requireNonNull((paramWithDefault));
    return new LambdaSlashWithDefault(paramNoDefault, paramWithDefault);
  }
}