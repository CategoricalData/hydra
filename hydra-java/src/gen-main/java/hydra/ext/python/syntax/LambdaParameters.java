// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class LambdaParameters implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.LambdaParameters");
  
  public static final hydra.core.Name FIELD_NAME_SLASH_NO_DEFAULT = new hydra.core.Name("slashNoDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_NO_DEFAULT = new hydra.core.Name("paramNoDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_WITH_DEFAULT = new hydra.core.Name("paramWithDefault");
  
  public static final hydra.core.Name FIELD_NAME_STAR_ETC = new hydra.core.Name("starEtc");
  
  public final hydra.util.Opt<hydra.ext.python.syntax.LambdaSlashNoDefault> slashNoDefault;
  
  public final java.util.List<hydra.ext.python.syntax.LambdaParamNoDefault> paramNoDefault;
  
  public final java.util.List<hydra.ext.python.syntax.LambdaParamWithDefault> paramWithDefault;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.LambdaStarEtc> starEtc;
  
  public LambdaParameters (hydra.util.Opt<hydra.ext.python.syntax.LambdaSlashNoDefault> slashNoDefault, java.util.List<hydra.ext.python.syntax.LambdaParamNoDefault> paramNoDefault, java.util.List<hydra.ext.python.syntax.LambdaParamWithDefault> paramWithDefault, hydra.util.Opt<hydra.ext.python.syntax.LambdaStarEtc> starEtc) {
    java.util.Objects.requireNonNull((slashNoDefault));
    java.util.Objects.requireNonNull((paramNoDefault));
    java.util.Objects.requireNonNull((paramWithDefault));
    java.util.Objects.requireNonNull((starEtc));
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
    LambdaParameters o = (LambdaParameters) (other);
    return slashNoDefault.equals(o.slashNoDefault) && paramNoDefault.equals(o.paramNoDefault) && paramWithDefault.equals(o.paramWithDefault) && starEtc.equals(o.starEtc);
  }
  
  @Override
  public int hashCode() {
    return 2 * slashNoDefault.hashCode() + 3 * paramNoDefault.hashCode() + 5 * paramWithDefault.hashCode() + 7 * starEtc.hashCode();
  }
  
  public LambdaParameters withSlashNoDefault(hydra.util.Opt<hydra.ext.python.syntax.LambdaSlashNoDefault> slashNoDefault) {
    java.util.Objects.requireNonNull((slashNoDefault));
    return new LambdaParameters(slashNoDefault, paramNoDefault, paramWithDefault, starEtc);
  }
  
  public LambdaParameters withParamNoDefault(java.util.List<hydra.ext.python.syntax.LambdaParamNoDefault> paramNoDefault) {
    java.util.Objects.requireNonNull((paramNoDefault));
    return new LambdaParameters(slashNoDefault, paramNoDefault, paramWithDefault, starEtc);
  }
  
  public LambdaParameters withParamWithDefault(java.util.List<hydra.ext.python.syntax.LambdaParamWithDefault> paramWithDefault) {
    java.util.Objects.requireNonNull((paramWithDefault));
    return new LambdaParameters(slashNoDefault, paramNoDefault, paramWithDefault, starEtc);
  }
  
  public LambdaParameters withStarEtc(hydra.util.Opt<hydra.ext.python.syntax.LambdaStarEtc> starEtc) {
    java.util.Objects.requireNonNull((starEtc));
    return new LambdaParameters(slashNoDefault, paramNoDefault, paramWithDefault, starEtc);
  }
}