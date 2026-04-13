// Note: this is an automatically generated file. Do not edit.

package hydra.owl.syntax;

import java.io.Serializable;

public class InverseObjectProperties implements Serializable, Comparable<InverseObjectProperties> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.owl.syntax.InverseObjectProperties");

  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");

  public static final hydra.core.Name PROPERTY1 = new hydra.core.Name("property1");

  public static final hydra.core.Name PROPERTY2 = new hydra.core.Name("property2");

  public final java.util.List<hydra.owl.syntax.Annotation> annotations;

  public final hydra.owl.syntax.ObjectPropertyExpression property1;

  public final hydra.owl.syntax.ObjectPropertyExpression property2;

  public InverseObjectProperties (java.util.List<hydra.owl.syntax.Annotation> annotations, hydra.owl.syntax.ObjectPropertyExpression property1, hydra.owl.syntax.ObjectPropertyExpression property2) {
    this.annotations = annotations;
    this.property1 = property1;
    this.property2 = property2;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InverseObjectProperties)) {
      return false;
    }
    InverseObjectProperties o = (InverseObjectProperties) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.property1,
      o.property1) && java.util.Objects.equals(
      this.property2,
      o.property2);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(property1) + 5 * java.util.Objects.hashCode(property2);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(InverseObjectProperties other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      annotations,
      other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      property1,
      other.property1);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      property2,
      other.property2);
  }

  public InverseObjectProperties withAnnotations(java.util.List<hydra.owl.syntax.Annotation> annotations) {
    return new InverseObjectProperties(annotations, property1, property2);
  }

  public InverseObjectProperties withProperty1(hydra.owl.syntax.ObjectPropertyExpression property1) {
    return new InverseObjectProperties(annotations, property1, property2);
  }

  public InverseObjectProperties withProperty2(hydra.owl.syntax.ObjectPropertyExpression property2) {
    return new InverseObjectProperties(annotations, property1, property2);
  }
}
