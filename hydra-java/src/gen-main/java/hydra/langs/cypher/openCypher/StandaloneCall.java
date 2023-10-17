package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class StandaloneCall implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.StandaloneCall");
  
  public final hydra.langs.cypher.openCypher.ProcedureInvocation invocation;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.YieldExpression> yield;
  
  public StandaloneCall (hydra.langs.cypher.openCypher.ProcedureInvocation invocation, java.util.Optional<hydra.langs.cypher.openCypher.YieldExpression> yield) {
    this.invocation = invocation;
    this.yield = yield;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StandaloneCall)) {
      return false;
    }
    StandaloneCall o = (StandaloneCall) (other);
    return invocation.equals(o.invocation) && yield.equals(o.yield);
  }
  
  @Override
  public int hashCode() {
    return 2 * invocation.hashCode() + 3 * yield.hashCode();
  }
  
  public StandaloneCall withInvocation(hydra.langs.cypher.openCypher.ProcedureInvocation invocation) {
    return new StandaloneCall(invocation, yield);
  }
  
  public StandaloneCall withYield(java.util.Optional<hydra.langs.cypher.openCypher.YieldExpression> yield) {
    return new StandaloneCall(invocation, yield);
  }
}