// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class InQueryCall implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.InQueryCall");
  
  public static final hydra.core.Name FIELD_NAME_CALL = new hydra.core.Name("call");
  
  public static final hydra.core.Name FIELD_NAME_YIELD_ITEMS = new hydra.core.Name("yieldItems");
  
  public final hydra.ext.cypher.openCypher.ExplicitProcedureInvocation call;
  
  public final hydra.util.Opt<hydra.ext.cypher.openCypher.YieldItems> yieldItems;
  
  public InQueryCall (hydra.ext.cypher.openCypher.ExplicitProcedureInvocation call, hydra.util.Opt<hydra.ext.cypher.openCypher.YieldItems> yieldItems) {
    java.util.Objects.requireNonNull((call));
    java.util.Objects.requireNonNull((yieldItems));
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
  
  public InQueryCall withCall(hydra.ext.cypher.openCypher.ExplicitProcedureInvocation call) {
    java.util.Objects.requireNonNull((call));
    return new InQueryCall(call, yieldItems);
  }
  
  public InQueryCall withYieldItems(hydra.util.Opt<hydra.ext.cypher.openCypher.YieldItems> yieldItems) {
    java.util.Objects.requireNonNull((yieldItems));
    return new InQueryCall(call, yieldItems);
  }
}
