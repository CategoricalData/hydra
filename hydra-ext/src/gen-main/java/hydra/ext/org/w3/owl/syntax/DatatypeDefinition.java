// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class DatatypeDefinition implements Serializable, Comparable<DatatypeDefinition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.DatatypeDefinition");

  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");

  public static final hydra.core.Name DATATYPE = new hydra.core.Name("datatype");

  public static final hydra.core.Name RANGE = new hydra.core.Name("range");

  public final java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations;

  public final hydra.ext.org.w3.owl.syntax.Datatype datatype;

  public final hydra.ext.org.w3.owl.syntax.DataRange range;

  public DatatypeDefinition (java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.Datatype datatype, hydra.ext.org.w3.owl.syntax.DataRange range) {
    this.annotations = annotations;
    this.datatype = datatype;
    this.range = range;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DatatypeDefinition)) {
      return false;
    }
    DatatypeDefinition o = (DatatypeDefinition) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.datatype,
      o.datatype) && java.util.Objects.equals(
      this.range,
      o.range);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(datatype) + 5 * java.util.Objects.hashCode(range);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DatatypeDefinition other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      annotations,
      other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      datatype,
      other.datatype);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      range,
      other.range);
  }

  public DatatypeDefinition withAnnotations(java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new DatatypeDefinition(annotations, datatype, range);
  }

  public DatatypeDefinition withDatatype(hydra.ext.org.w3.owl.syntax.Datatype datatype) {
    return new DatatypeDefinition(annotations, datatype, range);
  }

  public DatatypeDefinition withRange(hydra.ext.org.w3.owl.syntax.DataRange range) {
    return new DatatypeDefinition(annotations, datatype, range);
  }
}
