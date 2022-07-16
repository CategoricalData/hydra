package hydra.ext.pegasus.pdl;

public class NamedSchema {
  public final QualifiedName qualifiedName;
  
  public final NamedSchema_Type type;
  
  public final Annotations annotations;
  
  public NamedSchema (QualifiedName qualifiedName, NamedSchema_Type type, Annotations annotations) {
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
  
  public NamedSchema withQualifiedName(QualifiedName qualifiedName) {
    return new NamedSchema(qualifiedName, type, annotations);
  }
  
  public NamedSchema withType(NamedSchema_Type type) {
    return new NamedSchema(qualifiedName, type, annotations);
  }
  
  public NamedSchema withAnnotations(Annotations annotations) {
    return new NamedSchema(qualifiedName, type, annotations);
  }
}