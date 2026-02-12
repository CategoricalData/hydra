// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class LambdaParameters implements Serializable, Comparable<LambdaParameters> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.LambdaParameters");
  
  public static final hydra.core.Name FIELD_NAME_SLASH_NO_DEFAULT = new hydra.core.Name("slashNoDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_NO_DEFAULT = new hydra.core.Name("paramNoDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_WITH_DEFAULT = new hydra.core.Name("paramWithDefault");
  
  public static final hydra.core.Name FIELD_NAME_STAR_ETC = new hydra.core.Name("starEtc");
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.LambdaSlashNoDefault> slashNoDefault;
  
  public final java.util.List<hydra.ext.python.syntax.LambdaParamNoDefault> paramNoDefault;
  
  public final java.util.List<hydra.ext.python.syntax.LambdaParamWithDefault> paramWithDefault;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.LambdaStarEtc> starEtc;
  
  public LambdaParameters (hydra.util.Maybe<hydra.ext.python.syntax.LambdaSlashNoDefault> slashNoDefault, java.util.List<hydra.ext.python.syntax.LambdaParamNoDefault> paramNoDefault, java.util.List<hydra.ext.python.syntax.LambdaParamWithDefault> paramWithDefault, hydra.util.Maybe<hydra.ext.python.syntax.LambdaStarEtc> starEtc) {
    this.slashNoDefault = slashNoDefault;
    this.paramNoDefault = paramNoDefault;
    this.paramWithDefault = paramWithDefault;
    this.starEtc = starEtc;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LambdaParameters)) {
      return false;
    }
    LambdaParameters o = (LambdaParameters) other;
    return java.util.Objects.equals(
      this.slashNoDefault,
      o.slashNoDefault) && java.util.Objects.equals(
      this.paramNoDefault,
      o.paramNoDefault) && java.util.Objects.equals(
      this.paramWithDefault,
      o.paramWithDefault) && java.util.Objects.equals(
      this.starEtc,
      o.starEtc);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(slashNoDefault) + 3 * java.util.Objects.hashCode(paramNoDefault) + 5 * java.util.Objects.hashCode(paramWithDefault) + 7 * java.util.Objects.hashCode(starEtc);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LambdaParameters other) {
    int cmp = 0;
    cmp = Integer.compare(
      slashNoDefault.hashCode(),
      other.slashNoDefault.hashCode());
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
  
  public LambdaParameters withSlashNoDefault(hydra.util.Maybe<hydra.ext.python.syntax.LambdaSlashNoDefault> slashNoDefault) {
    return new LambdaParameters(slashNoDefault, paramNoDefault, paramWithDefault, starEtc);
  }
  
  public LambdaParameters withParamNoDefault(java.util.List<hydra.ext.python.syntax.LambdaParamNoDefault> paramNoDefault) {
    return new LambdaParameters(slashNoDefault, paramNoDefault, paramWithDefault, starEtc);
  }
  
  public LambdaParameters withParamWithDefault(java.util.List<hydra.ext.python.syntax.LambdaParamWithDefault> paramWithDefault) {
    return new LambdaParameters(slashNoDefault, paramNoDefault, paramWithDefault, starEtc);
  }
  
  public LambdaParameters withStarEtc(hydra.util.Maybe<hydra.ext.python.syntax.LambdaStarEtc> starEtc) {
    return new LambdaParameters(slashNoDefault, paramNoDefault, paramWithDefault, starEtc);
  }
}
