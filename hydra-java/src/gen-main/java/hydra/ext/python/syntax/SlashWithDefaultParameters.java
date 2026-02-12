// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class SlashWithDefaultParameters implements Serializable, Comparable<SlashWithDefaultParameters> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.SlashWithDefaultParameters");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_NO_DEFAULT = new hydra.core.Name("paramNoDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_WITH_DEFAULT = new hydra.core.Name("paramWithDefault");
  
  public static final hydra.core.Name FIELD_NAME_STAR_ETC = new hydra.core.Name("starEtc");
  
  public final java.util.List<hydra.ext.python.syntax.ParamNoDefault> paramNoDefault;
  
  public final java.util.List<hydra.ext.python.syntax.ParamWithDefault> paramWithDefault;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.StarEtc> starEtc;
  
  public SlashWithDefaultParameters (java.util.List<hydra.ext.python.syntax.ParamNoDefault> paramNoDefault, java.util.List<hydra.ext.python.syntax.ParamWithDefault> paramWithDefault, hydra.util.Maybe<hydra.ext.python.syntax.StarEtc> starEtc) {
    this.paramNoDefault = paramNoDefault;
    this.paramWithDefault = paramWithDefault;
    this.starEtc = starEtc;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SlashWithDefaultParameters)) {
      return false;
    }
    SlashWithDefaultParameters o = (SlashWithDefaultParameters) other;
    return java.util.Objects.equals(
      this.paramNoDefault,
      o.paramNoDefault) && java.util.Objects.equals(
      this.paramWithDefault,
      o.paramWithDefault) && java.util.Objects.equals(
      this.starEtc,
      o.starEtc);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(paramNoDefault) + 3 * java.util.Objects.hashCode(paramWithDefault) + 5 * java.util.Objects.hashCode(starEtc);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SlashWithDefaultParameters other) {
    int cmp = 0;
    cmp = Integer.compare(
      paramNoDefault.hashCode(),
      other.paramNoDefault.hashCode());
    if (cmp != 0) {
      return cmp;
    }
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
  
  public SlashWithDefaultParameters withParamNoDefault(java.util.List<hydra.ext.python.syntax.ParamNoDefault> paramNoDefault) {
    return new SlashWithDefaultParameters(paramNoDefault, paramWithDefault, starEtc);
  }
  
  public SlashWithDefaultParameters withParamWithDefault(java.util.List<hydra.ext.python.syntax.ParamWithDefault> paramWithDefault) {
    return new SlashWithDefaultParameters(paramNoDefault, paramWithDefault, starEtc);
  }
  
  public SlashWithDefaultParameters withStarEtc(hydra.util.Maybe<hydra.ext.python.syntax.StarEtc> starEtc) {
    return new SlashWithDefaultParameters(paramNoDefault, paramWithDefault, starEtc);
  }
}
