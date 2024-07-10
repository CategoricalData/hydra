// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for procedure calls.
 */
public class ProcedureCallFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.ProcedureCallFeatures");
  
  /**
   * Whether to expect CALL within a query.
   */
  public final Boolean inQueryCall;
  
  /**
   * Whether to expect standalone / top-level CALL.
   */
  public final Boolean standaloneCall;
  
  /**
   * Whether to expect the YIELD clause in CALL.
   */
  public final Boolean yield;
  
  public ProcedureCallFeatures (Boolean inQueryCall, Boolean standaloneCall, Boolean yield) {
    if (inQueryCall == null) {
      throw new IllegalArgumentException("null value for 'inQueryCall' argument");
    }
    if (standaloneCall == null) {
      throw new IllegalArgumentException("null value for 'standaloneCall' argument");
    }
    if (yield == null) {
      throw new IllegalArgumentException("null value for 'yield' argument");
    }
    this.inQueryCall = inQueryCall;
    this.standaloneCall = standaloneCall;
    this.yield = yield;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ProcedureCallFeatures)) {
      return false;
    }
    ProcedureCallFeatures o = (ProcedureCallFeatures) (other);
    return inQueryCall.equals(o.inQueryCall) && standaloneCall.equals(o.standaloneCall) && yield.equals(o.yield);
  }
  
  @Override
  public int hashCode() {
    return 2 * inQueryCall.hashCode() + 3 * standaloneCall.hashCode() + 5 * yield.hashCode();
  }
  
  public ProcedureCallFeatures withInQueryCall(Boolean inQueryCall) {
    if (inQueryCall == null) {
      throw new IllegalArgumentException("null value for 'inQueryCall' argument");
    }
    return new ProcedureCallFeatures(inQueryCall, standaloneCall, yield);
  }
  
  public ProcedureCallFeatures withStandaloneCall(Boolean standaloneCall) {
    if (standaloneCall == null) {
      throw new IllegalArgumentException("null value for 'standaloneCall' argument");
    }
    return new ProcedureCallFeatures(inQueryCall, standaloneCall, yield);
  }
  
  public ProcedureCallFeatures withYield(Boolean yield) {
    if (yield == null) {
      throw new IllegalArgumentException("null value for 'yield' argument");
    }
    return new ProcedureCallFeatures(inQueryCall, standaloneCall, yield);
  }
}