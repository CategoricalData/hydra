// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class NoDefaultStarEtc implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.NoDefaultStarEtc");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_NO_DEFAULT = new hydra.core.Name("paramNoDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_MAYBE_DEFAULT = new hydra.core.Name("paramMaybeDefault");
  
  public static final hydra.core.Name FIELD_NAME_KEYWORDS = new hydra.core.Name("keywords");
  
  public final hydra.ext.python.syntax.ParamNoDefault paramNoDefault;
  
  public final java.util.List<hydra.ext.python.syntax.ParamMaybeDefault> paramMaybeDefault;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Keywords> keywords;
  
  public NoDefaultStarEtc (hydra.ext.python.syntax.ParamNoDefault paramNoDefault, java.util.List<hydra.ext.python.syntax.ParamMaybeDefault> paramMaybeDefault, hydra.util.Opt<hydra.ext.python.syntax.Keywords> keywords) {
    java.util.Objects.requireNonNull((paramNoDefault));
    java.util.Objects.requireNonNull((paramMaybeDefault));
    java.util.Objects.requireNonNull((keywords));
    this.paramNoDefault = paramNoDefault;
    this.paramMaybeDefault = paramMaybeDefault;
    this.keywords = keywords;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NoDefaultStarEtc)) {
      return false;
    }
    NoDefaultStarEtc o = (NoDefaultStarEtc) (other);
    return paramNoDefault.equals(o.paramNoDefault) && paramMaybeDefault.equals(o.paramMaybeDefault) && keywords.equals(o.keywords);
  }
  
  @Override
  public int hashCode() {
    return 2 * paramNoDefault.hashCode() + 3 * paramMaybeDefault.hashCode() + 5 * keywords.hashCode();
  }
  
  public NoDefaultStarEtc withParamNoDefault(hydra.ext.python.syntax.ParamNoDefault paramNoDefault) {
    java.util.Objects.requireNonNull((paramNoDefault));
    return new NoDefaultStarEtc(paramNoDefault, paramMaybeDefault, keywords);
  }
  
  public NoDefaultStarEtc withParamMaybeDefault(java.util.List<hydra.ext.python.syntax.ParamMaybeDefault> paramMaybeDefault) {
    java.util.Objects.requireNonNull((paramMaybeDefault));
    return new NoDefaultStarEtc(paramNoDefault, paramMaybeDefault, keywords);
  }
  
  public NoDefaultStarEtc withKeywords(hydra.util.Opt<hydra.ext.python.syntax.Keywords> keywords) {
    java.util.Objects.requireNonNull((keywords));
    return new NoDefaultStarEtc(paramNoDefault, paramMaybeDefault, keywords);
  }
}