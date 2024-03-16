package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class InQueryCall implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.InQueryCall");
  
  public final hydra.langs.cypher.openCypher.ExplicitProcedureInvocation call;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.YieldItems> yieldItems;
  
  public InQueryCall (hydra.langs.cypher.openCypher.ExplicitProcedureInvocation call, java.util.Optional<hydra.langs.cypher.openCypher.YieldItems> yieldItems) {
    this.call = call;
    this.yieldItems = yieldItems;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InQueryCall)) {
      return false;
    }
    InQueryCall o = (InQueryCall) (other);
    return call.equals(o.call) && yieldItems.equals(o.yieldItems);
  }
  
  @Override
  public int hashCode() {
    return 2 * call.hashCode() + 3 * yieldItems.hashCode();
  }
  
  public InQueryCall withCall(hydra.langs.cypher.openCypher.ExplicitProcedureInvocation call) {
    return new InQueryCall(call, yieldItems);
  }
  
  public InQueryCall withYieldItems(java.util.Optional<hydra.langs.cypher.openCypher.YieldItems> yieldItems) {
    return new InQueryCall(call, yieldItems);
  }
}