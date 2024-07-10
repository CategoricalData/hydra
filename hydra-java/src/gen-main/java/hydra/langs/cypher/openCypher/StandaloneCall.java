// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class StandaloneCall implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.StandaloneCall");
  
  public final hydra.langs.cypher.openCypher.ProcedureInvocation call;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.StarOrYieldItems> yieldItems;
  
  public StandaloneCall (hydra.langs.cypher.openCypher.ProcedureInvocation call, java.util.Optional<hydra.langs.cypher.openCypher.StarOrYieldItems> yieldItems) {
    if (call == null) {
      throw new IllegalArgumentException("null value for 'call' argument");
    }
    if (yieldItems == null) {
      throw new IllegalArgumentException("null value for 'yieldItems' argument");
    }
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
    if (call == null) {
      throw new IllegalArgumentException("null value for 'call' argument");
    }
    return new StandaloneCall(call, yieldItems);
  }
  
  public StandaloneCall withYieldItems(java.util.Optional<hydra.langs.cypher.openCypher.StarOrYieldItems> yieldItems) {
    if (yieldItems == null) {
      throw new IllegalArgumentException("null value for 'yieldItems' argument");
    }
    return new StandaloneCall(call, yieldItems);
  }
}