// Note: this is an automatically generated file. Do not edit.

package hydra.langs.sql.ansi;

import java.io.Serializable;

public class ExactNumericType_Numeric_Option implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/sql/ansi.ExactNumericType.Numeric.Option");
  
  public final hydra.langs.sql.ansi.Precision precision;
  
  public final hydra.util.Opt<hydra.langs.sql.ansi.Scale> sequence;
  
  public ExactNumericType_Numeric_Option (hydra.langs.sql.ansi.Precision precision, hydra.util.Opt<hydra.langs.sql.ansi.Scale> sequence) {
    if (precision == null) {
      throw new IllegalArgumentException("null value for 'precision' argument");
    }
    if (sequence == null) {
      throw new IllegalArgumentException("null value for 'sequence' argument");
    }
    this.precision = precision;
    this.sequence = sequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExactNumericType_Numeric_Option)) {
      return false;
    }
    ExactNumericType_Numeric_Option o = (ExactNumericType_Numeric_Option) (other);
    return precision.equals(o.precision) && sequence.equals(o.sequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * precision.hashCode() + 3 * sequence.hashCode();
  }
  
  public ExactNumericType_Numeric_Option withPrecision(hydra.langs.sql.ansi.Precision precision) {
    if (precision == null) {
      throw new IllegalArgumentException("null value for 'precision' argument");
    }
    return new ExactNumericType_Numeric_Option(precision, sequence);
  }
  
  public ExactNumericType_Numeric_Option withSequence(hydra.util.Opt<hydra.langs.sql.ansi.Scale> sequence) {
    if (sequence == null) {
      throw new IllegalArgumentException("null value for 'sequence' argument");
    }
    return new ExactNumericType_Numeric_Option(precision, sequence);
  }
}