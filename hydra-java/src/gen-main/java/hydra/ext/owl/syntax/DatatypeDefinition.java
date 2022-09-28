package hydra.ext.owl.syntax;

public class DatatypeDefinition {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.DatatypeDefinition");
  
  public final java.util.List<hydra.ext.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.owl.syntax.Datatype datatype;
  
  public final hydra.ext.owl.syntax.DataRange range;
  
  public DatatypeDefinition (java.util.List<hydra.ext.owl.syntax.Annotation> annotations, hydra.ext.owl.syntax.Datatype datatype, hydra.ext.owl.syntax.DataRange range) {
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
    return new DatatypeDefinition(annotations, datatype, range);
  }
  
  public DatatypeDefinition withDatatype(hydra.ext.owl.syntax.Datatype datatype) {
    return new DatatypeDefinition(annotations, datatype, range);
  }
  
  public DatatypeDefinition withRange(hydra.ext.owl.syntax.DataRange range) {
    return new DatatypeDefinition(annotations, datatype, range);
  }
}