// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class CommaStarEtc implements Serializable, Comparable<CommaStarEtc> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.CommaStarEtc");

  public static final hydra.core.Name PARAM_MAYBE_DEFAULT = new hydra.core.Name("paramMaybeDefault");

  public static final hydra.core.Name KEYWORDS = new hydra.core.Name("keywords");

  public final java.util.List<hydra.python.syntax.ParamMaybeDefault> paramMaybeDefault;

  public final hydra.util.Maybe<hydra.python.syntax.Keywords> keywords;

  public CommaStarEtc (java.util.List<hydra.python.syntax.ParamMaybeDefault> paramMaybeDefault, hydra.util.Maybe<hydra.python.syntax.Keywords> keywords) {
    this.paramMaybeDefault = paramMaybeDefault;
    this.keywords = keywords;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CommaStarEtc)) {
      return false;
    }
    CommaStarEtc o = (CommaStarEtc) other;
    return java.util.Objects.equals(
      this.paramMaybeDefault,
      o.paramMaybeDefault) && java.util.Objects.equals(
      this.keywords,
      o.keywords);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(paramMaybeDefault) + 3 * java.util.Objects.hashCode(keywords);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CommaStarEtc other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      paramMaybeDefault,
      other.paramMaybeDefault);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      keywords,
      other.keywords);
  }

  public CommaStarEtc withParamMaybeDefault(java.util.List<hydra.python.syntax.ParamMaybeDefault> paramMaybeDefault) {
    return new CommaStarEtc(paramMaybeDefault, keywords);
  }

  public CommaStarEtc withKeywords(hydra.util.Maybe<hydra.python.syntax.Keywords> keywords) {
    return new CommaStarEtc(paramMaybeDefault, keywords);
  }
}
