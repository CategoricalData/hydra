// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class NoDefaultStarEtc implements Serializable, Comparable<NoDefaultStarEtc> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.NoDefaultStarEtc");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_NO_DEFAULT = new hydra.core.Name("paramNoDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_MAYBE_DEFAULT = new hydra.core.Name("paramMaybeDefault");
  
  public static final hydra.core.Name FIELD_NAME_KEYWORDS = new hydra.core.Name("keywords");
  
  public final hydra.ext.python.syntax.ParamNoDefault paramNoDefault;
  
  public final java.util.List<hydra.ext.python.syntax.ParamMaybeDefault> paramMaybeDefault;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Keywords> keywords;
  
  public NoDefaultStarEtc (hydra.ext.python.syntax.ParamNoDefault paramNoDefault, java.util.List<hydra.ext.python.syntax.ParamMaybeDefault> paramMaybeDefault, hydra.util.Maybe<hydra.ext.python.syntax.Keywords> keywords) {
    this.paramNoDefault = paramNoDefault;
    this.paramMaybeDefault = paramMaybeDefault;
    this.keywords = keywords;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NoDefaultStarEtc)) {
      return false;
    }
    NoDefaultStarEtc o = (NoDefaultStarEtc) other;
    return java.util.Objects.equals(
      this.paramNoDefault,
      o.paramNoDefault) && java.util.Objects.equals(
      this.paramMaybeDefault,
      o.paramMaybeDefault) && java.util.Objects.equals(
      this.keywords,
      o.keywords);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(paramNoDefault) + 3 * java.util.Objects.hashCode(paramMaybeDefault) + 5 * java.util.Objects.hashCode(keywords);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NoDefaultStarEtc other) {
    int cmp = 0;
    cmp = ((Comparable) paramNoDefault).compareTo(other.paramNoDefault);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      paramMaybeDefault.hashCode(),
      other.paramMaybeDefault.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      keywords.hashCode(),
      other.keywords.hashCode());
  }
  
  public NoDefaultStarEtc withParamNoDefault(hydra.ext.python.syntax.ParamNoDefault paramNoDefault) {
    return new NoDefaultStarEtc(paramNoDefault, paramMaybeDefault, keywords);
  }
  
  public NoDefaultStarEtc withParamMaybeDefault(java.util.List<hydra.ext.python.syntax.ParamMaybeDefault> paramMaybeDefault) {
    return new NoDefaultStarEtc(paramNoDefault, paramMaybeDefault, keywords);
  }
  
  public NoDefaultStarEtc withKeywords(hydra.util.Maybe<hydra.ext.python.syntax.Keywords> keywords) {
    return new NoDefaultStarEtc(paramNoDefault, paramMaybeDefault, keywords);
  }
}
