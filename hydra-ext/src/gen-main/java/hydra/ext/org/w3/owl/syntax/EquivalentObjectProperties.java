// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class EquivalentObjectProperties implements Serializable, Comparable<EquivalentObjectProperties> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.EquivalentObjectProperties");

  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");

  public static final hydra.core.Name PROPERTIES = new hydra.core.Name("properties");

  public final java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations;

  public final java.util.List<hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression> properties;

  public EquivalentObjectProperties (java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations, java.util.List<hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression> properties) {
    this.annotations = annotations;
    this.properties = properties;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EquivalentObjectProperties)) {
      return false;
    }
    EquivalentObjectProperties o = (EquivalentObjectProperties) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.properties,
      o.properties);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(properties);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EquivalentObjectProperties other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      annotations,
      other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      properties,
      other.properties);
  }

  public EquivalentObjectProperties withAnnotations(java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new EquivalentObjectProperties(annotations, properties);
  }

  public EquivalentObjectProperties withProperties(java.util.List<hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression> properties) {
    return new EquivalentObjectProperties(annotations, properties);
  }
}
