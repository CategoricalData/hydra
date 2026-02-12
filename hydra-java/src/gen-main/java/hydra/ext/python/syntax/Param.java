// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Param implements Serializable, Comparable<Param> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Param");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATION = new hydra.core.Name("annotation");
  
  public final hydra.ext.python.syntax.Name name;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Annotation> annotation;
  
  public Param (hydra.ext.python.syntax.Name name, hydra.util.Maybe<hydra.ext.python.syntax.Annotation> annotation) {
    this.name = name;
    this.annotation = annotation;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Param)) {
      return false;
    }
    Param o = (Param) other;
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
  public int compareTo(Param other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      annotation.hashCode(),
      other.annotation.hashCode());
  }
  
  public Param withName(hydra.ext.python.syntax.Name name) {
    return new Param(name, annotation);
  }
  
  public Param withAnnotation(hydra.util.Maybe<hydra.ext.python.syntax.Annotation> annotation) {
    return new Param(name, annotation);
  }
}
