// Note: this is an automatically generated file. Do not edit.

package hydra.langs.sql.ansi;

import java.io.Serializable;

public class BooleanTest_Sequence_Option implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/sql/ansi.BooleanTest.Sequence.Option");
  
  public static final hydra.core.Name FIELD_NAME_N_O_T = new hydra.core.Name("nOT");
  
  public static final hydra.core.Name FIELD_NAME_TRUTH_VALUE = new hydra.core.Name("truthValue");
  
  public final hydra.util.Opt<java.lang.Void> nOT;
  
  public final hydra.langs.sql.ansi.TruthValue truthValue;
  
  public BooleanTest_Sequence_Option (hydra.util.Opt<java.lang.Void> nOT, hydra.langs.sql.ansi.TruthValue truthValue) {
    java.util.Objects.requireNonNull((nOT));
    java.util.Objects.requireNonNull((truthValue));
    this.nOT = nOT;
    this.truthValue = truthValue;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BooleanTest_Sequence_Option)) {
      return false;
    }
    BooleanTest_Sequence_Option o = (BooleanTest_Sequence_Option) (other);
    return nOT.equals(o.nOT) && truthValue.equals(o.truthValue);
  }
  
  @Override
  public int hashCode() {
    return 2 * nOT.hashCode() + 3 * truthValue.hashCode();
  }
  
  public BooleanTest_Sequence_Option withNOT(hydra.util.Opt<java.lang.Void> nOT) {
    java.util.Objects.requireNonNull((nOT));
    return new BooleanTest_Sequence_Option(nOT, truthValue);
  }
  
  public BooleanTest_Sequence_Option withTruthValue(hydra.langs.sql.ansi.TruthValue truthValue) {
    java.util.Objects.requireNonNull((truthValue));
    return new BooleanTest_Sequence_Option(nOT, truthValue);
  }
}