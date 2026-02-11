// Note: this is an automatically generated file. Do not edit.

package hydra.module;

import java.io.Serializable;

/**
 * A qualified name consisting of an optional namespace together with a mandatory local name
 */
public class QualifiedName implements Serializable, Comparable<QualifiedName> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.module.QualifiedName");
  
  public static final hydra.core.Name FIELD_NAME_NAMESPACE = new hydra.core.Name("namespace");
  
  public static final hydra.core.Name FIELD_NAME_LOCAL = new hydra.core.Name("local");
  
  /**
   * The optional namespace
   */
  public final hydra.util.Maybe<hydra.module.Namespace> namespace;
  
  /**
   * The local name
   */
  public final String local;
  
  public QualifiedName (hydra.util.Maybe<hydra.module.Namespace> namespace, String local) {
    this.namespace = namespace;
    this.local = local;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QualifiedName)) {
      return false;
    }
    QualifiedName o = (QualifiedName) other;
    return java.util.Objects.equals(
      this.namespace,
      o.namespace) && java.util.Objects.equals(
      this.local,
      o.local);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(namespace) + 3 * java.util.Objects.hashCode(local);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(QualifiedName other) {
    int cmp = 0;
    cmp = Integer.compare(
      namespace.hashCode(),
      other.namespace.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) local).compareTo(other.local);
  }
  
  public QualifiedName withNamespace(hydra.util.Maybe<hydra.module.Namespace> namespace) {
    return new QualifiedName(namespace, local);
  }
  
  public QualifiedName withLocal(String local) {
    return new QualifiedName(namespace, local);
  }
}
