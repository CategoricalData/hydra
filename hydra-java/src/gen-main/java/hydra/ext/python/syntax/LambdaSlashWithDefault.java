// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class LambdaSlashWithDefault implements Serializable, Comparable<LambdaSlashWithDefault> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.python.syntax.LambdaSlashWithDefault");
  
  public static final hydra.core.Name PARAM_NO_DEFAULT = new hydra.core.Name("paramNoDefault");
  
  public static final hydra.core.Name PARAM_WITH_DEFAULT = new hydra.core.Name("paramWithDefault");
  
  public final hydra.util.ConsList<hydra.ext.python.syntax.LambdaParamNoDefault> paramNoDefault;
  
  public final hydra.util.ConsList<hydra.ext.python.syntax.LambdaParamWithDefault> paramWithDefault;
  
  public LambdaSlashWithDefault (hydra.util.ConsList<hydra.ext.python.syntax.LambdaParamNoDefault> paramNoDefault, hydra.util.ConsList<hydra.ext.python.syntax.LambdaParamWithDefault> paramWithDefault) {
    this.paramNoDefault = paramNoDefault;
    this.paramWithDefault = paramWithDefault;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LambdaSlashWithDefault)) {
      return false;
    }
    LambdaSlashWithDefault o = (LambdaSlashWithDefault) other;
    return java.util.Objects.equals(
      this.paramNoDefault,
      o.paramNoDefault) && java.util.Objects.equals(
      this.paramWithDefault,
      o.paramWithDefault);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(paramNoDefault) + 3 * java.util.Objects.hashCode(paramWithDefault);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LambdaSlashWithDefault other) {
    int cmp = 0;
    cmp = ((Comparable) paramNoDefault).compareTo(other.paramNoDefault);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) paramWithDefault).compareTo(other.paramWithDefault);
  }
  
  public LambdaSlashWithDefault withParamNoDefault(hydra.util.ConsList<hydra.ext.python.syntax.LambdaParamNoDefault> paramNoDefault) {
    return new LambdaSlashWithDefault(paramNoDefault, paramWithDefault);
  }
  
  public LambdaSlashWithDefault withParamWithDefault(hydra.util.ConsList<hydra.ext.python.syntax.LambdaParamWithDefault> paramWithDefault) {
    return new LambdaSlashWithDefault(paramNoDefault, paramWithDefault);
  }
}
