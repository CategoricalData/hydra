// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class QualifiedName implements Serializable, Comparable<QualifiedName> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.QualifiedName");
  
  public static final hydra.core.Name NAMESPACE = new hydra.core.Name("namespace");
  
  public static final hydra.core.Name LOCAL = new hydra.core.Name("local");
  
  public final String namespace;
  
  public final String local;
  
  public QualifiedName (String namespace, String local) {
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
    cmp = ((Comparable) namespace).compareTo(other.namespace);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) local).compareTo(other.local);
  }
  
  public QualifiedName withNamespace(String namespace) {
    return new QualifiedName(namespace, local);
  }
  
  public QualifiedName withLocal(String local) {
    return new QualifiedName(namespace, local);
  }
}
