package hydra.ext.pegasus.pdl;

public class NamedSchema {
  public final hydra.ext.pegasus.pdl.QualifiedName qualifiedName;
  
  public final hydra.ext.pegasus.pdl.NamedSchema_Type type;
  
  public final hydra.ext.pegasus.pdl.Annotations annotations;
  
  public NamedSchema (hydra.ext.pegasus.pdl.QualifiedName qualifiedName, hydra.ext.pegasus.pdl.NamedSchema_Type type, hydra.ext.pegasus.pdl.Annotations annotations) {
    this.qualifiedName = qualifiedName;
    this.type = type;
    this.annotations = annotations;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NamedSchema)) {
      return false;
    }
    NamedSchema o = (NamedSchema) (other);
    return qualifiedName.equals(o.qualifiedName) && type.equals(o.type) && annotations.equals(o.annotations);
  }
  
  @Override
  public int hashCode() {
    return 2 * qualifiedName.hashCode() + 3 * type.hashCode() + 5 * annotations.hashCode();
  }
  
  public NamedSchema withQualifiedName(hydra.ext.pegasus.pdl.QualifiedName qualifiedName) {
    return new NamedSchema(qualifiedName, type, annotations);
  }
  
  public NamedSchema withType(hydra.ext.pegasus.pdl.NamedSchema_Type type) {
    return new NamedSchema(qualifiedName, type, annotations);
  }
  
  public NamedSchema withAnnotations(hydra.ext.pegasus.pdl.Annotations annotations) {
    return new NamedSchema(qualifiedName, type, annotations);
  }
}