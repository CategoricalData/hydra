package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class StandaloneCall implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.StandaloneCall");
  
  public final hydra.langs.cypher.openCypher.ProcedureInvocation call;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.YieldItems> yieldItems;
  
  public StandaloneCall (hydra.langs.cypher.openCypher.ProcedureInvocation call, java.util.Optional<hydra.langs.cypher.openCypher.YieldItems> yieldItems) {
    this.call = call;
    this.yieldItems = yieldItems;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StandaloneCall)) {
      return false;
    }
    StandaloneCall o = (StandaloneCall) (other);
    return call.equals(o.call) && yieldItems.equals(o.yieldItems);
  }
  
  @Override
  public int hashCode() {
    return 2 * call.hashCode() + 3 * yieldItems.hashCode();
  }
  
  public StandaloneCall withCall(hydra.langs.cypher.openCypher.ProcedureInvocation call) {
    return new StandaloneCall(call, yieldItems);
  }
  
  public StandaloneCall withYieldItems(java.util.Optional<hydra.langs.cypher.openCypher.YieldItems> yieldItems) {
    return new StandaloneCall(call, yieldItems);
  }
}