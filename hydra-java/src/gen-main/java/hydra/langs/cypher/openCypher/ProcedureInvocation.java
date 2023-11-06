package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class ProcedureInvocation implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.ProcedureInvocation");
  
  public final hydra.langs.cypher.openCypher.ProcedureName name;
  
  public final java.util.Optional<java.util.List<hydra.langs.cypher.openCypher.Expression>> arguments;
  
  public ProcedureInvocation (hydra.langs.cypher.openCypher.ProcedureName name, java.util.Optional<java.util.List<hydra.langs.cypher.openCypher.Expression>> arguments) {
    this.name = name;
    this.arguments = arguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ProcedureInvocation)) {
      return false;
    }
    ProcedureInvocation o = (ProcedureInvocation) (other);
    return name.equals(o.name) && arguments.equals(o.arguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * arguments.hashCode();
  }
  
  public ProcedureInvocation withName(hydra.langs.cypher.openCypher.ProcedureName name) {
    return new ProcedureInvocation(name, arguments);
  }
  
  public ProcedureInvocation withArguments(java.util.Optional<java.util.List<hydra.langs.cypher.openCypher.Expression>> arguments) {
    return new ProcedureInvocation(name, arguments);
  }
}