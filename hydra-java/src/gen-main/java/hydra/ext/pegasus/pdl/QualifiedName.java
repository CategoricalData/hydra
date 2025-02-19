// Note: this is an automatically generated file. Do not edit.

package hydra.ext.pegasus.pdl;

import java.io.Serializable;

public class QualifiedName implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.pegasus.pdl.QualifiedName");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_NAMESPACE = new hydra.core.Name("namespace");
  
  public final hydra.ext.pegasus.pdl.Name name;
  
  public final hydra.util.Opt<hydra.ext.pegasus.pdl.Namespace> namespace;
  
  public QualifiedName (hydra.ext.pegasus.pdl.Name name, hydra.util.Opt<hydra.ext.pegasus.pdl.Namespace> namespace) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((namespace));
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
  
  public QualifiedName withName(hydra.ext.pegasus.pdl.Name name) {
    java.util.Objects.requireNonNull((name));
    return new QualifiedName(name, namespace);
  }
  
  public QualifiedName withNamespace(hydra.util.Opt<hydra.ext.pegasus.pdl.Namespace> namespace) {
    java.util.Objects.requireNonNull((namespace));
    return new QualifiedName(name, namespace);
  }
}