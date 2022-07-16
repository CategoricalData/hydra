package hydra.ext.pegasus.pdl;

public class QualifiedName {
  public final Name name;
  
  public final java.util.Optional<Namespace> namespace;
  
  public QualifiedName (Name name, java.util.Optional<Namespace> namespace) {
    this.name = name;
    this.namespace = namespace;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QualifiedName)) {
      return false;
    }
    QualifiedName o = (QualifiedName) (other);
    return name.equals(o.name) && namespace.equals(o.namespace);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * namespace.hashCode();
  }
  
  public QualifiedName withName(Name name) {
    return new QualifiedName(name, namespace);
  }
  
  public QualifiedName withNamespace(java.util.Optional<Namespace> namespace) {
    return new QualifiedName(name, namespace);
  }
}