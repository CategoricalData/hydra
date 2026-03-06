// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class StandaloneCall implements Serializable, Comparable<StandaloneCall> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.StandaloneCall");
  
  public static final hydra.core.Name CALL = new hydra.core.Name("call");
  
  public static final hydra.core.Name YIELD_ITEMS = new hydra.core.Name("yieldItems");
  
  public final hydra.ext.cypher.openCypher.ProcedureInvocation call;
  
  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.StarOrYieldItems> yieldItems;
  
  public StandaloneCall (hydra.ext.cypher.openCypher.ProcedureInvocation call, hydra.util.Maybe<hydra.ext.cypher.openCypher.StarOrYieldItems> yieldItems) {
    this.call = call;
    this.yieldItems = yieldItems;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StandaloneCall)) {
      return false;
    }
    StandaloneCall o = (StandaloneCall) other;
    return java.util.Objects.equals(
      this.call,
      o.call) && java.util.Objects.equals(
      this.yieldItems,
      o.yieldItems);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(call) + 3 * java.util.Objects.hashCode(yieldItems);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(StandaloneCall other) {
    int cmp = 0;
    cmp = ((Comparable) call).compareTo(other.call);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      yieldItems.hashCode(),
      other.yieldItems.hashCode());
  }
  
  public StandaloneCall withCall(hydra.ext.cypher.openCypher.ProcedureInvocation call) {
    return new StandaloneCall(call, yieldItems);
  }
  
  public StandaloneCall withYieldItems(hydra.util.Maybe<hydra.ext.cypher.openCypher.StarOrYieldItems> yieldItems) {
    return new StandaloneCall(call, yieldItems);
  }
}
