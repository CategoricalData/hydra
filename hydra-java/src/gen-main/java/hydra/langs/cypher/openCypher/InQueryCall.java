// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class InQueryCall implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.InQueryCall");
  
  public final hydra.langs.cypher.openCypher.ExplicitProcedureInvocation call;
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.YieldItems> yieldItems;
  
  public InQueryCall (hydra.langs.cypher.openCypher.ExplicitProcedureInvocation call, hydra.util.Opt<hydra.langs.cypher.openCypher.YieldItems> yieldItems) {
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
    if (call == null) {
      throw new IllegalArgumentException("null value for 'call' argument");
    }
    return new InQueryCall(call, yieldItems);
  }
  
  public InQueryCall withYieldItems(hydra.util.Opt<hydra.langs.cypher.openCypher.YieldItems> yieldItems) {
    if (yieldItems == null) {
      throw new IllegalArgumentException("null value for 'yieldItems' argument");
    }
    return new InQueryCall(call, yieldItems);
  }
}