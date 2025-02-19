// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ParamNoDefaultStarAnnotation implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ParamNoDefaultStarAnnotation");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_STAR_ANNOTATION = new hydra.core.Name("paramStarAnnotation");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_COMMENT = new hydra.core.Name("typeComment");
  
  public final hydra.ext.python.syntax.ParamStarAnnotation paramStarAnnotation;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.TypeComment> typeComment;
  
  public ParamNoDefaultStarAnnotation (hydra.ext.python.syntax.ParamStarAnnotation paramStarAnnotation, hydra.util.Opt<hydra.ext.python.syntax.TypeComment> typeComment) {
    java.util.Objects.requireNonNull((paramStarAnnotation));
    java.util.Objects.requireNonNull((typeComment));
    this.paramStarAnnotation = paramStarAnnotation;
    this.typeComment = typeComment;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParamNoDefaultStarAnnotation)) {
      return false;
    }
    ParamNoDefaultStarAnnotation o = (ParamNoDefaultStarAnnotation) (other);
    return paramStarAnnotation.equals(o.paramStarAnnotation) && typeComment.equals(o.typeComment);
  }
  
  @Override
  public int hashCode() {
    return 2 * paramStarAnnotation.hashCode() + 3 * typeComment.hashCode();
  }
  
  public ParamNoDefaultStarAnnotation withParamStarAnnotation(hydra.ext.python.syntax.ParamStarAnnotation paramStarAnnotation) {
    java.util.Objects.requireNonNull((paramStarAnnotation));
    return new ParamNoDefaultStarAnnotation(paramStarAnnotation, typeComment);
  }
  
  public ParamNoDefaultStarAnnotation withTypeComment(hydra.util.Opt<hydra.ext.python.syntax.TypeComment> typeComment) {
    java.util.Objects.requireNonNull((typeComment));
    return new ParamNoDefaultStarAnnotation(paramStarAnnotation, typeComment);
  }
}