// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class ParamStarAnnotation implements Serializable, Comparable<ParamStarAnnotation> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.ParamStarAnnotation");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name ANNOTATION = new hydra.core.Name("annotation");

  public final hydra.python.syntax.Name name;

  public final hydra.python.syntax.StarAnnotation annotation;

  public ParamStarAnnotation (hydra.python.syntax.Name name, hydra.python.syntax.StarAnnotation annotation) {
    this.name = name;
    this.annotation = annotation;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParamStarAnnotation)) {
      return false;
    }
    ParamStarAnnotation o = (ParamStarAnnotation) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.annotation,
      o.annotation);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(annotation);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ParamStarAnnotation other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      annotation,
      other.annotation);
  }

  public ParamStarAnnotation withName(hydra.python.syntax.Name name) {
    return new ParamStarAnnotation(name, annotation);
  }

  public ParamStarAnnotation withAnnotation(hydra.python.syntax.StarAnnotation annotation) {
    return new ParamStarAnnotation(name, annotation);
  }
}
