// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class InQueryCall implements Serializable, Comparable<InQueryCall> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.InQueryCall");

  public static final hydra.core.Name CALL = new hydra.core.Name("call");

  public static final hydra.core.Name YIELD_ITEMS = new hydra.core.Name("yieldItems");

  public final hydra.ext.cypher.openCypher.ExplicitProcedureInvocation call;

  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.YieldItems> yieldItems;

  public InQueryCall (hydra.ext.cypher.openCypher.ExplicitProcedureInvocation call, hydra.util.Maybe<hydra.ext.cypher.openCypher.YieldItems> yieldItems) {
    this.call = call;
    this.yieldItems = yieldItems;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InQueryCall)) {
      return false;
    }
    InQueryCall o = (InQueryCall) other;
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
  public int compareTo(InQueryCall other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      call,
      other.call);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      yieldItems,
      other.yieldItems);
  }

  public InQueryCall withCall(hydra.ext.cypher.openCypher.ExplicitProcedureInvocation call) {
    return new InQueryCall(call, yieldItems);
  }

  public InQueryCall withYieldItems(hydra.util.Maybe<hydra.ext.cypher.openCypher.YieldItems> yieldItems) {
    return new InQueryCall(call, yieldItems);
  }
}
