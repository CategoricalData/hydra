package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class ProcedureName implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.ProcedureName");
  
  public final hydra.langs.cypher.openCypher.Namespace namespace;
  
  public final String name;
  
  public ProcedureName (hydra.langs.cypher.openCypher.Namespace namespace, String name) {
    this.namespace = namespace;
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ProcedureName)) {
      return false;
    }
    ProcedureName o = (ProcedureName) (other);
    return namespace.equals(o.namespace) && name.equals(o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * namespace.hashCode() + 3 * name.hashCode();
  }
  
  public ProcedureName withNamespace(hydra.langs.cypher.openCypher.Namespace namespace) {
    return new ProcedureName(namespace, name);
  }
  
  public ProcedureName withName(String name) {
    return new ProcedureName(namespace, name);
  }
}