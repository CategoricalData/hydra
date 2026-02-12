// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class SlashNoDefaultParameters implements Serializable, Comparable<SlashNoDefaultParameters> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.SlashNoDefaultParameters");
  
  public static final hydra.core.Name FIELD_NAME_SLASH = new hydra.core.Name("slash");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_NO_DEFAULT = new hydra.core.Name("paramNoDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_WITH_DEFAULT = new hydra.core.Name("paramWithDefault");
  
  public static final hydra.core.Name FIELD_NAME_STAR_ETC = new hydra.core.Name("starEtc");
  
  public final hydra.ext.python.syntax.SlashNoDefault slash;
  
  public final java.util.List<hydra.ext.python.syntax.ParamNoDefault> paramNoDefault;
  
  public final java.util.List<hydra.ext.python.syntax.ParamWithDefault> paramWithDefault;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.StarEtc> starEtc;
  
  public SlashNoDefaultParameters (hydra.ext.python.syntax.SlashNoDefault slash, java.util.List<hydra.ext.python.syntax.ParamNoDefault> paramNoDefault, java.util.List<hydra.ext.python.syntax.ParamWithDefault> paramWithDefault, hydra.util.Maybe<hydra.ext.python.syntax.StarEtc> starEtc) {
    this.slash = slash;
    this.paramNoDefault = paramNoDefault;
    this.paramWithDefault = paramWithDefault;
    this.starEtc = starEtc;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SlashNoDefaultParameters)) {
      return false;
    }
    SlashNoDefaultParameters o = (SlashNoDefaultParameters) other;
    return java.util.Objects.equals(
      this.slash,
      o.slash) && java.util.Objects.equals(
      this.paramNoDefault,
      o.paramNoDefault) && java.util.Objects.equals(
      this.paramWithDefault,
      o.paramWithDefault) && java.util.Objects.equals(
      this.starEtc,
      o.starEtc);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(slash) + 3 * java.util.Objects.hashCode(paramNoDefault) + 5 * java.util.Objects.hashCode(paramWithDefault) + 7 * java.util.Objects.hashCode(starEtc);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SlashNoDefaultParameters other) {
    int cmp = 0;
    cmp = ((Comparable) slash).compareTo(other.slash);
    if (cmp != 0) {
      return cmp;
    }
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
  
  public SlashNoDefaultParameters withSlash(hydra.ext.python.syntax.SlashNoDefault slash) {
    return new SlashNoDefaultParameters(slash, paramNoDefault, paramWithDefault, starEtc);
  }
  
  public SlashNoDefaultParameters withParamNoDefault(java.util.List<hydra.ext.python.syntax.ParamNoDefault> paramNoDefault) {
    return new SlashNoDefaultParameters(slash, paramNoDefault, paramWithDefault, starEtc);
  }
  
  public SlashNoDefaultParameters withParamWithDefault(java.util.List<hydra.ext.python.syntax.ParamWithDefault> paramWithDefault) {
    return new SlashNoDefaultParameters(slash, paramNoDefault, paramWithDefault, starEtc);
  }
  
  public SlashNoDefaultParameters withStarEtc(hydra.util.Maybe<hydra.ext.python.syntax.StarEtc> starEtc) {
    return new SlashNoDefaultParameters(slash, paramNoDefault, paramWithDefault, starEtc);
  }
}
