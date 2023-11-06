package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class InQueryCall implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.InQueryCall");
  
  public final hydra.langs.cypher.openCypher.ProcedureInvocation invocation;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.YieldItems> yield;
  
  public InQueryCall (hydra.langs.cypher.openCypher.ProcedureInvocation invocation, java.util.Optional<hydra.langs.cypher.openCypher.YieldItems> yield) {
    this.invocation = invocation;
    this.yield = yield;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InQueryCall)) {
      return false;
    }
    InQueryCall o = (InQueryCall) (other);
    return invocation.equals(o.invocation) && yield.equals(o.yield);
  }
  
  @Override
  public int hashCode() {
    return 2 * invocation.hashCode() + 3 * yield.hashCode();
  }
  
  public InQueryCall withInvocation(hydra.langs.cypher.openCypher.ProcedureInvocation invocation) {
    return new InQueryCall(invocation, yield);
  }
  
  public InQueryCall withYield(java.util.Optional<hydra.langs.cypher.openCypher.YieldItems> yield) {
    return new InQueryCall(invocation, yield);
  }
}