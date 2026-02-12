// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ParamStarAnnotation implements Serializable, Comparable<ParamStarAnnotation> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ParamStarAnnotation");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATION = new hydra.core.Name("annotation");
  
  public final hydra.ext.python.syntax.Name name;
  
  public final hydra.ext.python.syntax.StarAnnotation annotation;
  
  public ParamStarAnnotation (hydra.ext.python.syntax.Name name, hydra.ext.python.syntax.StarAnnotation annotation) {
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
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) annotation).compareTo(other.annotation);
  }
  
  public ParamStarAnnotation withName(hydra.ext.python.syntax.Name name) {
    return new ParamStarAnnotation(name, annotation);
  }
  
  public ParamStarAnnotation withAnnotation(hydra.ext.python.syntax.StarAnnotation annotation) {
    return new ParamStarAnnotation(name, annotation);
  }
}
