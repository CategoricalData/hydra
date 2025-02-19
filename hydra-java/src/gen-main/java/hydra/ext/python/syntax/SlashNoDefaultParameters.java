// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class SlashNoDefaultParameters implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.SlashNoDefaultParameters");
  
  public static final hydra.core.Name FIELD_NAME_SLASH = new hydra.core.Name("slash");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_NO_DEFAULT = new hydra.core.Name("paramNoDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_WITH_DEFAULT = new hydra.core.Name("paramWithDefault");
  
  public static final hydra.core.Name FIELD_NAME_STAR_ETC = new hydra.core.Name("starEtc");
  
  public final hydra.ext.python.syntax.SlashNoDefault slash;
  
  public final java.util.List<hydra.ext.python.syntax.ParamNoDefault> paramNoDefault;
  
  public final java.util.List<hydra.ext.python.syntax.ParamWithDefault> paramWithDefault;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.StarEtc> starEtc;
  
  public SlashNoDefaultParameters (hydra.ext.python.syntax.SlashNoDefault slash, java.util.List<hydra.ext.python.syntax.ParamNoDefault> paramNoDefault, java.util.List<hydra.ext.python.syntax.ParamWithDefault> paramWithDefault, hydra.util.Opt<hydra.ext.python.syntax.StarEtc> starEtc) {
    java.util.Objects.requireNonNull((slash));
    java.util.Objects.requireNonNull((paramNoDefault));
    java.util.Objects.requireNonNull((paramWithDefault));
    java.util.Objects.requireNonNull((starEtc));
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
    SlashNoDefaultParameters o = (SlashNoDefaultParameters) (other);
    return slash.equals(o.slash) && paramNoDefault.equals(o.paramNoDefault) && paramWithDefault.equals(o.paramWithDefault) && starEtc.equals(o.starEtc);
  }
  
  @Override
  public int hashCode() {
    return 2 * slash.hashCode() + 3 * paramNoDefault.hashCode() + 5 * paramWithDefault.hashCode() + 7 * starEtc.hashCode();
  }
  
  public SlashNoDefaultParameters withSlash(hydra.ext.python.syntax.SlashNoDefault slash) {
    java.util.Objects.requireNonNull((slash));
    return new SlashNoDefaultParameters(slash, paramNoDefault, paramWithDefault, starEtc);
  }
  
  public SlashNoDefaultParameters withParamNoDefault(java.util.List<hydra.ext.python.syntax.ParamNoDefault> paramNoDefault) {
    java.util.Objects.requireNonNull((paramNoDefault));
    return new SlashNoDefaultParameters(slash, paramNoDefault, paramWithDefault, starEtc);
  }
  
  public SlashNoDefaultParameters withParamWithDefault(java.util.List<hydra.ext.python.syntax.ParamWithDefault> paramWithDefault) {
    java.util.Objects.requireNonNull((paramWithDefault));
    return new SlashNoDefaultParameters(slash, paramNoDefault, paramWithDefault, starEtc);
  }
  
  public SlashNoDefaultParameters withStarEtc(hydra.util.Opt<hydra.ext.python.syntax.StarEtc> starEtc) {
    java.util.Objects.requireNonNull((starEtc));
    return new SlashNoDefaultParameters(slash, paramNoDefault, paramWithDefault, starEtc);
  }
}