// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class NoDefaultStarAnnotationStarEtc implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.NoDefaultStarAnnotationStarEtc");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_NO_DEFAULT_STAR_ANNOTATION = new hydra.core.Name("paramNoDefaultStarAnnotation");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_MAYBE_DEFAULT = new hydra.core.Name("paramMaybeDefault");
  
  public static final hydra.core.Name FIELD_NAME_KEYWORDS = new hydra.core.Name("keywords");
  
  public final hydra.ext.python.syntax.ParamNoDefaultStarAnnotation paramNoDefaultStarAnnotation;
  
  public final java.util.List<hydra.ext.python.syntax.ParamMaybeDefault> paramMaybeDefault;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Keywords> keywords;
  
  public NoDefaultStarAnnotationStarEtc (hydra.ext.python.syntax.ParamNoDefaultStarAnnotation paramNoDefaultStarAnnotation, java.util.List<hydra.ext.python.syntax.ParamMaybeDefault> paramMaybeDefault, hydra.util.Opt<hydra.ext.python.syntax.Keywords> keywords) {
    java.util.Objects.requireNonNull((paramNoDefaultStarAnnotation));
    java.util.Objects.requireNonNull((paramMaybeDefault));
    java.util.Objects.requireNonNull((keywords));
    this.paramNoDefaultStarAnnotation = paramNoDefaultStarAnnotation;
    this.paramMaybeDefault = paramMaybeDefault;
    this.keywords = keywords;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NoDefaultStarAnnotationStarEtc)) {
      return false;
    }
    NoDefaultStarAnnotationStarEtc o = (NoDefaultStarAnnotationStarEtc) (other);
    return paramNoDefaultStarAnnotation.equals(o.paramNoDefaultStarAnnotation) && paramMaybeDefault.equals(o.paramMaybeDefault) && keywords.equals(o.keywords);
  }
  
  @Override
  public int hashCode() {
    return 2 * paramNoDefaultStarAnnotation.hashCode() + 3 * paramMaybeDefault.hashCode() + 5 * keywords.hashCode();
  }
  
  public NoDefaultStarAnnotationStarEtc withParamNoDefaultStarAnnotation(hydra.ext.python.syntax.ParamNoDefaultStarAnnotation paramNoDefaultStarAnnotation) {
    java.util.Objects.requireNonNull((paramNoDefaultStarAnnotation));
    return new NoDefaultStarAnnotationStarEtc(paramNoDefaultStarAnnotation, paramMaybeDefault, keywords);
  }
  
  public NoDefaultStarAnnotationStarEtc withParamMaybeDefault(java.util.List<hydra.ext.python.syntax.ParamMaybeDefault> paramMaybeDefault) {
    java.util.Objects.requireNonNull((paramMaybeDefault));
    return new NoDefaultStarAnnotationStarEtc(paramNoDefaultStarAnnotation, paramMaybeDefault, keywords);
  }
  
  public NoDefaultStarAnnotationStarEtc withKeywords(hydra.util.Opt<hydra.ext.python.syntax.Keywords> keywords) {
    java.util.Objects.requireNonNull((keywords));
    return new NoDefaultStarAnnotationStarEtc(paramNoDefaultStarAnnotation, paramMaybeDefault, keywords);
  }
}