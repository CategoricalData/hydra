// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.features;

import java.io.Serializable;

/**
 * Procedure calls
 */
public class ProcedureCallFeatures implements Serializable, Comparable<ProcedureCallFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.features.ProcedureCallFeatures");

  public static final hydra.core.Name IN_QUERY_CALL = new hydra.core.Name("inQueryCall");

  public static final hydra.core.Name STANDALONE_CALL = new hydra.core.Name("standaloneCall");

  public static final hydra.core.Name YIELD = new hydra.core.Name("yield");

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
    this.inQueryCall = inQueryCall;
    this.standaloneCall = standaloneCall;
    this.yield = yield;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ProcedureCallFeatures)) {
      return false;
    }
    ProcedureCallFeatures o = (ProcedureCallFeatures) other;
    return java.util.Objects.equals(
      this.inQueryCall,
      o.inQueryCall) && java.util.Objects.equals(
      this.standaloneCall,
      o.standaloneCall) && java.util.Objects.equals(
      this.yield,
      o.yield);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(inQueryCall) + 3 * java.util.Objects.hashCode(standaloneCall) + 5 * java.util.Objects.hashCode(yield);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ProcedureCallFeatures other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      inQueryCall,
      other.inQueryCall);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      standaloneCall,
      other.standaloneCall);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      yield,
      other.yield);
  }

  public ProcedureCallFeatures withInQueryCall(Boolean inQueryCall) {
    return new ProcedureCallFeatures(inQueryCall, standaloneCall, yield);
  }

  public ProcedureCallFeatures withStandaloneCall(Boolean standaloneCall) {
    return new ProcedureCallFeatures(inQueryCall, standaloneCall, yield);
  }

  public ProcedureCallFeatures withYield(Boolean yield) {
    return new ProcedureCallFeatures(inQueryCall, standaloneCall, yield);
  }
}
