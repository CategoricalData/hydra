// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class CommaStarEtc implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.CommaStarEtc");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_MAYBE_DEFAULT = new hydra.core.Name("paramMaybeDefault");
  
  public static final hydra.core.Name FIELD_NAME_KEYWORDS = new hydra.core.Name("keywords");
  
  public final java.util.List<hydra.ext.python.syntax.ParamMaybeDefault> paramMaybeDefault;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Keywords> keywords;
  
  public CommaStarEtc (java.util.List<hydra.ext.python.syntax.ParamMaybeDefault> paramMaybeDefault, hydra.util.Opt<hydra.ext.python.syntax.Keywords> keywords) {
    java.util.Objects.requireNonNull((paramMaybeDefault));
    java.util.Objects.requireNonNull((keywords));
    this.paramMaybeDefault = paramMaybeDefault;
    this.keywords = keywords;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CommaStarEtc)) {
      return false;
    }
    CommaStarEtc o = (CommaStarEtc) (other);
    return paramMaybeDefault.equals(o.paramMaybeDefault) && keywords.equals(o.keywords);
  }
  
  @Override
  public int hashCode() {
    return 2 * paramMaybeDefault.hashCode() + 3 * keywords.hashCode();
  }
  
  public CommaStarEtc withParamMaybeDefault(java.util.List<hydra.ext.python.syntax.ParamMaybeDefault> paramMaybeDefault) {
    java.util.Objects.requireNonNull((paramMaybeDefault));
    return new CommaStarEtc(paramMaybeDefault, keywords);
  }
  
  public CommaStarEtc withKeywords(hydra.util.Opt<hydra.ext.python.syntax.Keywords> keywords) {
    java.util.Objects.requireNonNull((keywords));
    return new CommaStarEtc(paramMaybeDefault, keywords);
  }
}