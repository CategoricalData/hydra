// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class Annotation implements Serializable, Comparable<Annotation> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.Annotation");

  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations;

  public final hydra.ext.org.w3.owl.syntax.AnnotationProperty property;

  public final hydra.ext.org.w3.owl.syntax.AnnotationValue value;

  public Annotation (hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.AnnotationProperty property, hydra.ext.org.w3.owl.syntax.AnnotationValue value) {
    this.annotations = annotations;
    this.property = property;
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Annotation)) {
      return false;
    }
    Annotation o = (Annotation) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.property,
      o.property) && java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(property) + 5 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Annotation other) {
    int cmp = 0;
    cmp = ((Comparable) annotations).compareTo(other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) property).compareTo(other.property);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) value).compareTo(other.value);
  }

  public Annotation withAnnotations(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new Annotation(annotations, property, value);
  }

  public Annotation withProperty(hydra.ext.org.w3.owl.syntax.AnnotationProperty property) {
    return new Annotation(annotations, property, value);
  }

  public Annotation withValue(hydra.ext.org.w3.owl.syntax.AnnotationValue value) {
    return new Annotation(annotations, property, value);
  }
}
