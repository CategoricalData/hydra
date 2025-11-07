// Note: this is an automatically generated file. Do not edit.

package hydra.module;

import java.io.Serializable;

/**
 * A qualified name consisting of an optional namespace together with a mandatory local name
 */
public class QualifiedName implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.module.QualifiedName");
  
  public static final hydra.core.Name FIELD_NAME_NAMESPACE = new hydra.core.Name("namespace");
  
  public static final hydra.core.Name FIELD_NAME_LOCAL = new hydra.core.Name("local");
  
  /**
   * The optional namespace
   */
  public final hydra.util.Opt<hydra.module.Namespace> namespace;
  
  /**
   * The local name
   */
  public final String local;
  
  public QualifiedName (hydra.util.Opt<hydra.module.Namespace> namespace, String local) {
    java.util.Objects.requireNonNull((namespace));
    java.util.Objects.requireNonNull((local));
    this.namespace = namespace;
    this.local = local;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QualifiedName)) {
      return false;
    }
    QualifiedName o = (QualifiedName) (other);
    return namespace.equals(o.namespace) && local.equals(o.local);
  }
  
  @Override
  public int hashCode() {
    return 2 * namespace.hashCode() + 3 * local.hashCode();
  }
  
  public QualifiedName withNamespace(hydra.util.Opt<hydra.module.Namespace> namespace) {
    java.util.Objects.requireNonNull((namespace));
    return new QualifiedName(namespace, local);
  }
  
  public QualifiedName withLocal(String local) {
    java.util.Objects.requireNonNull((local));
    return new QualifiedName(namespace, local);
  }
}
