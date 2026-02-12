// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ParamWithDefaultParameters implements Serializable, Comparable<ParamWithDefaultParameters> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ParamWithDefaultParameters");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_WITH_DEFAULT = new hydra.core.Name("paramWithDefault");
  
  public static final hydra.core.Name FIELD_NAME_STAR_ETC = new hydra.core.Name("starEtc");
  
  public final java.util.List<hydra.ext.python.syntax.ParamWithDefault> paramWithDefault;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.StarEtc> starEtc;
  
  public ParamWithDefaultParameters (java.util.List<hydra.ext.python.syntax.ParamWithDefault> paramWithDefault, hydra.util.Maybe<hydra.ext.python.syntax.StarEtc> starEtc) {
    this.paramWithDefault = paramWithDefault;
    this.starEtc = starEtc;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParamWithDefaultParameters)) {
      return false;
    }
    ParamWithDefaultParameters o = (ParamWithDefaultParameters) other;
    return java.util.Objects.equals(
      this.paramWithDefault,
      o.paramWithDefault) && java.util.Objects.equals(
      this.starEtc,
      o.starEtc);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(paramWithDefault) + 3 * java.util.Objects.hashCode(starEtc);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ParamWithDefaultParameters other) {
    int cmp = 0;
    cmp = Integer.compare(
      paramWithDefault.hashCode(),
      other.paramWithDefault.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      starEtc.hashCode(),
      other.starEtc.hashCode());
  }
  
  public ParamWithDefaultParameters withParamWithDefault(java.util.List<hydra.ext.python.syntax.ParamWithDefault> paramWithDefault) {
    return new ParamWithDefaultParameters(paramWithDefault, starEtc);
  }
  
  public ParamWithDefaultParameters withStarEtc(hydra.util.Maybe<hydra.ext.python.syntax.StarEtc> starEtc) {
    return new ParamWithDefaultParameters(paramWithDefault, starEtc);
  }
}
