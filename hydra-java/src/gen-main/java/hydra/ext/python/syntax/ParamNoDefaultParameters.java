// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ParamNoDefaultParameters implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ParamNoDefaultParameters");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_NO_DEFAULT = new hydra.core.Name("paramNoDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_WITH_DEFAULT = new hydra.core.Name("paramWithDefault");
  
  public static final hydra.core.Name FIELD_NAME_STAR_ETC = new hydra.core.Name("starEtc");
  
  public final java.util.List<hydra.ext.python.syntax.ParamNoDefault> paramNoDefault;
  
  public final java.util.List<hydra.ext.python.syntax.ParamWithDefault> paramWithDefault;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.StarEtc> starEtc;
  
  public ParamNoDefaultParameters (java.util.List<hydra.ext.python.syntax.ParamNoDefault> paramNoDefault, java.util.List<hydra.ext.python.syntax.ParamWithDefault> paramWithDefault, hydra.util.Opt<hydra.ext.python.syntax.StarEtc> starEtc) {
    java.util.Objects.requireNonNull((paramNoDefault));
    java.util.Objects.requireNonNull((paramWithDefault));
    java.util.Objects.requireNonNull((starEtc));
    this.paramNoDefault = paramNoDefault;
    this.paramWithDefault = paramWithDefault;
    this.starEtc = starEtc;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParamNoDefaultParameters)) {
      return false;
    }
    ParamNoDefaultParameters o = (ParamNoDefaultParameters) (other);
    return paramNoDefault.equals(o.paramNoDefault) && paramWithDefault.equals(o.paramWithDefault) && starEtc.equals(o.starEtc);
  }
  
  @Override
  public int hashCode() {
    return 2 * paramNoDefault.hashCode() + 3 * paramWithDefault.hashCode() + 5 * starEtc.hashCode();
  }
  
  public ParamNoDefaultParameters withParamNoDefault(java.util.List<hydra.ext.python.syntax.ParamNoDefault> paramNoDefault) {
    java.util.Objects.requireNonNull((paramNoDefault));
    return new ParamNoDefaultParameters(paramNoDefault, paramWithDefault, starEtc);
  }
  
  public ParamNoDefaultParameters withParamWithDefault(java.util.List<hydra.ext.python.syntax.ParamWithDefault> paramWithDefault) {
    java.util.Objects.requireNonNull((paramWithDefault));
    return new ParamNoDefaultParameters(paramNoDefault, paramWithDefault, starEtc);
  }
  
  public ParamNoDefaultParameters withStarEtc(hydra.util.Opt<hydra.ext.python.syntax.StarEtc> starEtc) {
    java.util.Objects.requireNonNull((starEtc));
    return new ParamNoDefaultParameters(paramNoDefault, paramWithDefault, starEtc);
  }
}