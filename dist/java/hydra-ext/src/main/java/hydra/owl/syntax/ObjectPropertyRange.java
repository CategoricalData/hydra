// Note: this is an automatically generated file. Do not edit.

package hydra.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Object_Property_Range
 */
public class ObjectPropertyRange implements Serializable, Comparable<ObjectPropertyRange> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.owl.syntax.ObjectPropertyRange");

  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public static final hydra.core.Name RANGE = new hydra.core.Name("range");

  public final java.util.List<hydra.owl.syntax.Annotation> annotations;

  public final hydra.owl.syntax.ObjectPropertyExpression property;

  public final hydra.owl.syntax.ClassExpression range;

  public ObjectPropertyRange (java.util.List<hydra.owl.syntax.Annotation> annotations, hydra.owl.syntax.ObjectPropertyExpression property, hydra.owl.syntax.ClassExpression range) {
    this.annotations = annotations;
    this.property = property;
    this.range = range;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectPropertyRange)) {
      return false;
    }
    ObjectPropertyRange o = (ObjectPropertyRange) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.property,
      o.property) && java.util.Objects.equals(
      this.range,
      o.range);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(property) + 5 * java.util.Objects.hashCode(range);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ObjectPropertyRange other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      annotations,
      other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      property,
      other.property);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      range,
      other.range);
  }

  public ObjectPropertyRange withAnnotations(java.util.List<hydra.owl.syntax.Annotation> annotations) {
    return new ObjectPropertyRange(annotations, property, range);
  }

  public ObjectPropertyRange withProperty(hydra.owl.syntax.ObjectPropertyExpression property) {
    return new ObjectPropertyRange(annotations, property, range);
  }

  public ObjectPropertyRange withRange(hydra.owl.syntax.ClassExpression range) {
    return new ObjectPropertyRange(annotations, property, range);
  }
}
