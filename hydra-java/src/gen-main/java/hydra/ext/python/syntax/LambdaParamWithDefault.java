// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class LambdaParamWithDefault implements Serializable, Comparable<LambdaParamWithDefault> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.LambdaParamWithDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM = new hydra.core.Name("param");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT = new hydra.core.Name("default");
  
  public final hydra.ext.python.syntax.Name param;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Default> default_;
  
  public LambdaParamWithDefault (hydra.ext.python.syntax.Name param, hydra.util.Maybe<hydra.ext.python.syntax.Default> default_) {
    this.param = param;
    this.default_ = default_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LambdaParamWithDefault)) {
      return false;
    }
    LambdaParamWithDefault o = (LambdaParamWithDefault) other;
    return java.util.Objects.equals(
      this.param,
      o.param) && java.util.Objects.equals(
      this.default_,
      o.default_);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(param) + 3 * java.util.Objects.hashCode(default_);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LambdaParamWithDefault other) {
    int cmp = 0;
    cmp = ((Comparable) param).compareTo(other.param);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      default_.hashCode(),
      other.default_.hashCode());
  }
  
  public LambdaParamWithDefault withParam(hydra.ext.python.syntax.Name param) {
    return new LambdaParamWithDefault(param, default_);
  }
  
  public LambdaParamWithDefault withDefault(hydra.util.Maybe<hydra.ext.python.syntax.Default> default_) {
    return new LambdaParamWithDefault(param, default_);
  }
}
