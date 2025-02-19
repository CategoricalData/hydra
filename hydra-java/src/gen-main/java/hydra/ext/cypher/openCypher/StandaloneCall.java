// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class StandaloneCall implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.StandaloneCall");
  
  public static final hydra.core.Name FIELD_NAME_CALL = new hydra.core.Name("call");
  
  public static final hydra.core.Name FIELD_NAME_YIELD_ITEMS = new hydra.core.Name("yieldItems");
  
  public final hydra.ext.cypher.openCypher.ProcedureInvocation call;
  
  public final hydra.util.Opt<hydra.ext.cypher.openCypher.StarOrYieldItems> yieldItems;
  
  public StandaloneCall (hydra.ext.cypher.openCypher.ProcedureInvocation call, hydra.util.Opt<hydra.ext.cypher.openCypher.StarOrYieldItems> yieldItems) {
    java.util.Objects.requireNonNull((call));
    java.util.Objects.requireNonNull((yieldItems));
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
  
  public StandaloneCall withCall(hydra.ext.cypher.openCypher.ProcedureInvocation call) {
    java.util.Objects.requireNonNull((call));
    return new StandaloneCall(call, yieldItems);
  }
  
  public StandaloneCall withYieldItems(hydra.util.Opt<hydra.ext.cypher.openCypher.StarOrYieldItems> yieldItems) {
    java.util.Objects.requireNonNull((yieldItems));
    return new StandaloneCall(call, yieldItems);
  }
}