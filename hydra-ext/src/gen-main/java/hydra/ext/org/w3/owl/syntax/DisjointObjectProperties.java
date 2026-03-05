// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class DisjointObjectProperties implements Serializable, Comparable<DisjointObjectProperties> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.DisjointObjectProperties");
  
  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name PROPERTIES = new hydra.core.Name("properties");
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression> properties;
  
  public DisjointObjectProperties (java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations, java.util.List<hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression> properties) {
    this.annotations = annotations;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DisjointObjectProperties)) {
      return false;
    }
    DisjointObjectProperties o = (DisjointObjectProperties) other;
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
  public int compareTo(DisjointObjectProperties other) {
    int cmp = 0;
    cmp = Integer.compare(
      annotations.hashCode(),
      other.annotations.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      properties.hashCode(),
      other.properties.hashCode());
  }
  
  public DisjointObjectProperties withAnnotations(java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new DisjointObjectProperties(annotations, properties);
  }
  
  public DisjointObjectProperties withProperties(java.util.List<hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression> properties) {
    return new DisjointObjectProperties(annotations, properties);
  }
}
