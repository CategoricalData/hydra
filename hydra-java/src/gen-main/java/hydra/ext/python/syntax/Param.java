// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Param implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Param");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATION = new hydra.core.Name("annotation");
  
  public final hydra.ext.python.syntax.Name name;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Annotation> annotation;
  
  public Param (hydra.ext.python.syntax.Name name, hydra.util.Opt<hydra.ext.python.syntax.Annotation> annotation) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((annotation));
    this.name = name;
    this.annotation = annotation;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Param)) {
      return false;
    }
    Param o = (Param) (other);
    return name.equals(o.name) && annotation.equals(o.annotation);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * annotation.hashCode();
  }
  
  public Param withName(hydra.ext.python.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new Param(name, annotation);
  }
  
  public Param withAnnotation(hydra.util.Opt<hydra.ext.python.syntax.Annotation> annotation) {
    java.util.Objects.requireNonNull((annotation));
    return new Param(name, annotation);
  }
}