package hydra.ext.sql.ansi;

public class BooleanTest {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.BooleanTest");
  
  public final hydra.ext.sql.ansi.BooleanPrimary booleanPrimary;
  
  public final java.util.Optional<hydra.ext.sql.ansi.BooleanTest_Sequence_Option> sequence;
  
  public BooleanTest (hydra.ext.sql.ansi.BooleanPrimary booleanPrimary, java.util.Optional<hydra.ext.sql.ansi.BooleanTest_Sequence_Option> sequence) {
    this.booleanPrimary = booleanPrimary;
    this.sequence = sequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BooleanTest)) {
      return false;
    }
    BooleanTest o = (BooleanTest) (other);
    return booleanPrimary.equals(o.booleanPrimary) && sequence.equals(o.sequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * booleanPrimary.hashCode() + 3 * sequence.hashCode();
  }
  
  public BooleanTest withBooleanPrimary(hydra.ext.sql.ansi.BooleanPrimary booleanPrimary) {
    return new BooleanTest(booleanPrimary, sequence);
  }
  
  public BooleanTest withSequence(java.util.Optional<hydra.ext.sql.ansi.BooleanTest_Sequence_Option> sequence) {
    return new BooleanTest(booleanPrimary, sequence);
  }
}