// Note: this is an automatically generated file. Do not edit.

package hydra.langs.owl.syntax;

import java.io.Serializable;

public class DatatypeDefinition implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/owl/syntax.DatatypeDefinition");
  
  public final java.util.List<hydra.langs.owl.syntax.Annotation> annotations;
  
  public final hydra.langs.owl.syntax.Datatype datatype;
  
  public final hydra.langs.owl.syntax.DataRange range;
  
  public DatatypeDefinition (java.util.List<hydra.langs.owl.syntax.Annotation> annotations, hydra.langs.owl.syntax.Datatype datatype, hydra.langs.owl.syntax.DataRange range) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    if (datatype == null) {
      throw new IllegalArgumentException("null value for 'datatype' argument");
    }
    if (range == null) {
      throw new IllegalArgumentException("null value for 'range' argument");
    }
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
  
  public DatatypeDefinition withAnnotations(java.util.List<hydra.langs.owl.syntax.Annotation> annotations) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    return new DatatypeDefinition(annotations, datatype, range);
  }
  
  public DatatypeDefinition withDatatype(hydra.langs.owl.syntax.Datatype datatype) {
    if (datatype == null) {
      throw new IllegalArgumentException("null value for 'datatype' argument");
    }
    return new DatatypeDefinition(annotations, datatype, range);
  }
  
  public DatatypeDefinition withRange(hydra.langs.owl.syntax.DataRange range) {
    if (range == null) {
      throw new IllegalArgumentException("null value for 'range' argument");
    }
    return new DatatypeDefinition(annotations, datatype, range);
  }
}