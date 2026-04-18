// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class ParamNoDefaultStarAnnotation implements Serializable, Comparable<ParamNoDefaultStarAnnotation> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.ParamNoDefaultStarAnnotation");

  public static final hydra.core.Name PARAM_STAR_ANNOTATION = new hydra.core.Name("paramStarAnnotation");

  public static final hydra.core.Name TYPE_COMMENT = new hydra.core.Name("typeComment");

  public final hydra.python.syntax.ParamStarAnnotation paramStarAnnotation;

  public final hydra.util.Maybe<hydra.python.syntax.TypeComment> typeComment;

  public ParamNoDefaultStarAnnotation (hydra.python.syntax.ParamStarAnnotation paramStarAnnotation, hydra.util.Maybe<hydra.python.syntax.TypeComment> typeComment) {
    this.paramStarAnnotation = paramStarAnnotation;
    this.typeComment = typeComment;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParamNoDefaultStarAnnotation)) {
      return false;
    }
    ParamNoDefaultStarAnnotation o = (ParamNoDefaultStarAnnotation) other;
    return java.util.Objects.equals(
      this.paramStarAnnotation,
      o.paramStarAnnotation) && java.util.Objects.equals(
      this.typeComment,
      o.typeComment);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(paramStarAnnotation) + 3 * java.util.Objects.hashCode(typeComment);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ParamNoDefaultStarAnnotation other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      paramStarAnnotation,
      other.paramStarAnnotation);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      typeComment,
      other.typeComment);
  }

  public ParamNoDefaultStarAnnotation withParamStarAnnotation(hydra.python.syntax.ParamStarAnnotation paramStarAnnotation) {
    return new ParamNoDefaultStarAnnotation(paramStarAnnotation, typeComment);
  }

  public ParamNoDefaultStarAnnotation withTypeComment(hydra.util.Maybe<hydra.python.syntax.TypeComment> typeComment) {
    return new ParamNoDefaultStarAnnotation(paramStarAnnotation, typeComment);
  }
}
