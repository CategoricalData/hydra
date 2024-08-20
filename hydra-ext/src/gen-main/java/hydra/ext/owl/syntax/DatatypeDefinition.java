// Note: this is an automatically generated file. Do not edit.

package hydra.ext.owl.syntax;

import java.io.Serializable;

public class DatatypeDefinition implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/owl/syntax.DatatypeDefinition");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_DATATYPE = new hydra.core.Name("datatype");
  
  public static final hydra.core.Name FIELD_NAME_RANGE = new hydra.core.Name("range");
  
  public final java.util.List<hydra.ext.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.owl.syntax.Datatype datatype;
  
  public final hydra.ext.owl.syntax.DataRange range;
  
  public DatatypeDefinition (java.util.List<hydra.ext.owl.syntax.Annotation> annotations, hydra.ext.owl.syntax.Datatype datatype, hydra.ext.owl.syntax.DataRange range) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((datatype));
    java.util.Objects.requireNonNull((range));
    this.annotations = annotations;
    this.datatype = datatype;
    this.range = range;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DatatypeDefinition)) {
      return false;
    }
    DatatypeDefinition o = (DatatypeDefinition) (other);
    return annotations.equals(o.annotations) && datatype.equals(o.datatype) && range.equals(o.range);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * datatype.hashCode() + 5 * range.hashCode();
  }
  
  public DatatypeDefinition withAnnotations(java.util.List<hydra.ext.owl.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new DatatypeDefinition(annotations, datatype, range);
  }
  
  public DatatypeDefinition withDatatype(hydra.ext.owl.syntax.Datatype datatype) {
    java.util.Objects.requireNonNull((datatype));
    return new DatatypeDefinition(annotations, datatype, range);
  }
  
  public DatatypeDefinition withRange(hydra.ext.owl.syntax.DataRange range) {
    java.util.Objects.requireNonNull((range));
    return new DatatypeDefinition(annotations, datatype, range);
  }
}
