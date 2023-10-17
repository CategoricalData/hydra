package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class FunctionName implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.FunctionName");
  
  public final hydra.langs.cypher.openCypher.Namespace namespace;
  
  public final String name;
  
  public FunctionName (hydra.langs.cypher.openCypher.Namespace namespace, String name) {
    this.namespace = namespace;
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FunctionName)) {
      return false;
    }
    FunctionName o = (FunctionName) (other);
    return namespace.equals(o.namespace) && name.equals(o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * namespace.hashCode() + 3 * name.hashCode();
  }
  
  public FunctionName withNamespace(hydra.langs.cypher.openCypher.Namespace namespace) {
    return new FunctionName(namespace, name);
  }
  
  public FunctionName withName(String name) {
    return new FunctionName(namespace, name);
  }
}