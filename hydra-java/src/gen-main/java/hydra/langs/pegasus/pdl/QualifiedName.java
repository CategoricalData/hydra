// Note: this is an automatically generated file. Do not edit.

package hydra.langs.pegasus.pdl;

import java.io.Serializable;

public class QualifiedName implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/pegasus/pdl.QualifiedName");
  
  public final hydra.langs.pegasus.pdl.Name name;
  
  public final java.util.Optional<hydra.langs.pegasus.pdl.Namespace> namespace;
  
  public QualifiedName (hydra.langs.pegasus.pdl.Name name, java.util.Optional<hydra.langs.pegasus.pdl.Namespace> namespace) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (namespace == null) {
      throw new IllegalArgumentException("null value for 'namespace' argument");
    }
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
  
  public QualifiedName withName(hydra.langs.pegasus.pdl.Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new QualifiedName(name, namespace);
  }
  
  public QualifiedName withNamespace(java.util.Optional<hydra.langs.pegasus.pdl.Namespace> namespace) {
    if (namespace == null) {
      throw new IllegalArgumentException("null value for 'namespace' argument");
    }
    return new QualifiedName(name, namespace);
  }
}