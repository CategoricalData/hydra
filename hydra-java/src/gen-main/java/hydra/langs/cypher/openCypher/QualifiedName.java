package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class QualifiedName implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.QualifiedName");
  
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
    QualifiedName o = (QualifiedName) (other);
    return namespace.equals(o.namespace) && local.equals(o.local);
  }
  
  @Override
  public int hashCode() {
    return 2 * namespace.hashCode() + 3 * local.hashCode();
  }
  
  public QualifiedName withNamespace(String namespace) {
    return new QualifiedName(namespace, local);
  }
  
  public QualifiedName withLocal(String local) {
    return new QualifiedName(namespace, local);
  }
}