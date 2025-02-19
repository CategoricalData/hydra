// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ParamWithDefaultParameters implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ParamWithDefaultParameters");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_WITH_DEFAULT = new hydra.core.Name("paramWithDefault");
  
  public static final hydra.core.Name FIELD_NAME_STAR_ETC = new hydra.core.Name("starEtc");
  
  public final java.util.List<hydra.ext.python.syntax.ParamWithDefault> paramWithDefault;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.StarEtc> starEtc;
  
  public ParamWithDefaultParameters (java.util.List<hydra.ext.python.syntax.ParamWithDefault> paramWithDefault, hydra.util.Opt<hydra.ext.python.syntax.StarEtc> starEtc) {
    java.util.Objects.requireNonNull((paramWithDefault));
    java.util.Objects.requireNonNull((starEtc));
    this.paramWithDefault = paramWithDefault;
    this.starEtc = starEtc;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParamWithDefaultParameters)) {
      return false;
    }
    ParamWithDefaultParameters o = (ParamWithDefaultParameters) (other);
    return paramWithDefault.equals(o.paramWithDefault) && starEtc.equals(o.starEtc);
  }
  
  @Override
  public int hashCode() {
    return 2 * paramWithDefault.hashCode() + 3 * starEtc.hashCode();
  }
  
  public ParamWithDefaultParameters withParamWithDefault(java.util.List<hydra.ext.python.syntax.ParamWithDefault> paramWithDefault) {
    java.util.Objects.requireNonNull((paramWithDefault));
    return new ParamWithDefaultParameters(paramWithDefault, starEtc);
  }
  
  public ParamWithDefaultParameters withStarEtc(hydra.util.Opt<hydra.ext.python.syntax.StarEtc> starEtc) {
    java.util.Objects.requireNonNull((starEtc));
    return new ParamWithDefaultParameters(paramWithDefault, starEtc);
  }
}