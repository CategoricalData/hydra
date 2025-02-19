// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public class BooleanTest implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.ansi.sql.syntax.BooleanTest");
  
  public static final hydra.core.Name FIELD_NAME_BOOLEAN_PRIMARY = new hydra.core.Name("booleanPrimary");
  
  public static final hydra.core.Name FIELD_NAME_SEQUENCE = new hydra.core.Name("sequence");
  
  public final hydra.ext.org.ansi.sql.syntax.BooleanPrimary booleanPrimary;
  
  public final hydra.util.Opt<hydra.ext.org.ansi.sql.syntax.BooleanTest_Sequence_Option> sequence;
  
  public BooleanTest (hydra.ext.org.ansi.sql.syntax.BooleanPrimary booleanPrimary, hydra.util.Opt<hydra.ext.org.ansi.sql.syntax.BooleanTest_Sequence_Option> sequence) {
    java.util.Objects.requireNonNull((booleanPrimary));
    java.util.Objects.requireNonNull((sequence));
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
  
  public BooleanTest withBooleanPrimary(hydra.ext.org.ansi.sql.syntax.BooleanPrimary booleanPrimary) {
    java.util.Objects.requireNonNull((booleanPrimary));
    return new BooleanTest(booleanPrimary, sequence);
  }
  
  public BooleanTest withSequence(hydra.util.Opt<hydra.ext.org.ansi.sql.syntax.BooleanTest_Sequence_Option> sequence) {
    java.util.Objects.requireNonNull((sequence));
    return new BooleanTest(booleanPrimary, sequence);
  }
}