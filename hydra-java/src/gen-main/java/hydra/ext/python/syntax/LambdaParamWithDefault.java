// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class LambdaParamWithDefault implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.LambdaParamWithDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM = new hydra.core.Name("param");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT = new hydra.core.Name("default");
  
  public final hydra.ext.python.syntax.Name param;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Default> default_;
  
  public LambdaParamWithDefault (hydra.ext.python.syntax.Name param, hydra.util.Opt<hydra.ext.python.syntax.Default> default_) {
    java.util.Objects.requireNonNull((param));
    java.util.Objects.requireNonNull((default_));
    this.param = param;
    this.default_ = default_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LambdaParamWithDefault)) {
      return false;
    }
    LambdaParamWithDefault o = (LambdaParamWithDefault) (other);
    return param.equals(o.param) && default_.equals(o.default_);
  }
  
  @Override
  public int hashCode() {
    return 2 * param.hashCode() + 3 * default_.hashCode();
  }
  
  public LambdaParamWithDefault withParam(hydra.ext.python.syntax.Name param) {
    java.util.Objects.requireNonNull((param));
    return new LambdaParamWithDefault(param, default_);
  }
  
  public LambdaParamWithDefault withDefault(hydra.util.Opt<hydra.ext.python.syntax.Default> default_) {
    java.util.Objects.requireNonNull((default_));
    return new LambdaParamWithDefault(param, default_);
  }
}