// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Procedure calls
 */
public class ProcedureCallFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/features.ProcedureCallFeatures");
  
  public static final hydra.core.Name FIELD_NAME_IN_QUERY_CALL = new hydra.core.Name("inQueryCall");
  
  public static final hydra.core.Name FIELD_NAME_STANDALONE_CALL = new hydra.core.Name("standaloneCall");
  
  public static final hydra.core.Name FIELD_NAME_YIELD = new hydra.core.Name("yield");
  
  /**
   * CALL within a query
   */
  public final Boolean inQueryCall;
  
  /**
   * Standalone / top-level CALL
   */
  public final Boolean standaloneCall;
  
  /**
   * The YIELD clause in CALL
   */
  public final Boolean yield;
  
  public ProcedureCallFeatures (Boolean inQueryCall, Boolean standaloneCall, Boolean yield) {
    java.util.Objects.requireNonNull((inQueryCall));
    java.util.Objects.requireNonNull((standaloneCall));
    java.util.Objects.requireNonNull((yield));
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
    java.util.Objects.requireNonNull((inQueryCall));
    return new ProcedureCallFeatures(inQueryCall, standaloneCall, yield);
  }
  
  public ProcedureCallFeatures withStandaloneCall(Boolean standaloneCall) {
    java.util.Objects.requireNonNull((standaloneCall));
    return new ProcedureCallFeatures(inQueryCall, standaloneCall, yield);
  }
  
  public ProcedureCallFeatures withYield(Boolean yield) {
    java.util.Objects.requireNonNull((yield));
    return new ProcedureCallFeatures(inQueryCall, standaloneCall, yield);
  }
}