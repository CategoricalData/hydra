// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class NoDefaultStarAnnotationStarEtc implements Serializable, Comparable<NoDefaultStarAnnotationStarEtc> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.NoDefaultStarAnnotationStarEtc");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_NO_DEFAULT_STAR_ANNOTATION = new hydra.core.Name("paramNoDefaultStarAnnotation");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_MAYBE_DEFAULT = new hydra.core.Name("paramMaybeDefault");
  
  public static final hydra.core.Name FIELD_NAME_KEYWORDS = new hydra.core.Name("keywords");
  
  public final hydra.ext.python.syntax.ParamNoDefaultStarAnnotation paramNoDefaultStarAnnotation;
  
  public final java.util.List<hydra.ext.python.syntax.ParamMaybeDefault> paramMaybeDefault;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Keywords> keywords;
  
  public NoDefaultStarAnnotationStarEtc (hydra.ext.python.syntax.ParamNoDefaultStarAnnotation paramNoDefaultStarAnnotation, java.util.List<hydra.ext.python.syntax.ParamMaybeDefault> paramMaybeDefault, hydra.util.Maybe<hydra.ext.python.syntax.Keywords> keywords) {
    this.paramNoDefaultStarAnnotation = paramNoDefaultStarAnnotation;
    this.paramMaybeDefault = paramMaybeDefault;
    this.keywords = keywords;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NoDefaultStarAnnotationStarEtc)) {
      return false;
    }
    NoDefaultStarAnnotationStarEtc o = (NoDefaultStarAnnotationStarEtc) other;
    return java.util.Objects.equals(
      this.paramNoDefaultStarAnnotation,
      o.paramNoDefaultStarAnnotation) && java.util.Objects.equals(
      this.paramMaybeDefault,
      o.paramMaybeDefault) && java.util.Objects.equals(
      this.keywords,
      o.keywords);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(paramNoDefaultStarAnnotation) + 3 * java.util.Objects.hashCode(paramMaybeDefault) + 5 * java.util.Objects.hashCode(keywords);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NoDefaultStarAnnotationStarEtc other) {
    int cmp = 0;
    cmp = ((Comparable) paramNoDefaultStarAnnotation).compareTo(other.paramNoDefaultStarAnnotation);
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
  
  public NoDefaultStarAnnotationStarEtc withParamNoDefaultStarAnnotation(hydra.ext.python.syntax.ParamNoDefaultStarAnnotation paramNoDefaultStarAnnotation) {
    return new NoDefaultStarAnnotationStarEtc(paramNoDefaultStarAnnotation, paramMaybeDefault, keywords);
  }
  
  public NoDefaultStarAnnotationStarEtc withParamMaybeDefault(java.util.List<hydra.ext.python.syntax.ParamMaybeDefault> paramMaybeDefault) {
    return new NoDefaultStarAnnotationStarEtc(paramNoDefaultStarAnnotation, paramMaybeDefault, keywords);
  }
  
  public NoDefaultStarAnnotationStarEtc withKeywords(hydra.util.Maybe<hydra.ext.python.syntax.Keywords> keywords) {
    return new NoDefaultStarAnnotationStarEtc(paramNoDefaultStarAnnotation, paramMaybeDefault, keywords);
  }
}
