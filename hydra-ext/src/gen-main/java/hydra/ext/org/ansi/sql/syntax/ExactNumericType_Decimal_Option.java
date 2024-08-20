// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public class ExactNumericType_Decimal_Option implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/ansi/sql/syntax.ExactNumericType.Decimal.Option");
  
  public static final hydra.core.Name FIELD_NAME_PRECISION = new hydra.core.Name("precision");
  
  public static final hydra.core.Name FIELD_NAME_SEQUENCE = new hydra.core.Name("sequence");
  
  public final hydra.ext.org.ansi.sql.syntax.Precision precision;
  
  public final hydra.util.Opt<hydra.ext.org.ansi.sql.syntax.Scale> sequence;
  
  public ExactNumericType_Decimal_Option (hydra.ext.org.ansi.sql.syntax.Precision precision, hydra.util.Opt<hydra.ext.org.ansi.sql.syntax.Scale> sequence) {
    java.util.Objects.requireNonNull((precision));
    java.util.Objects.requireNonNull((sequence));
    this.precision = precision;
    this.sequence = sequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExactNumericType_Decimal_Option)) {
      return false;
    }
    ExactNumericType_Decimal_Option o = (ExactNumericType_Decimal_Option) (other);
    return precision.equals(o.precision) && sequence.equals(o.sequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * precision.hashCode() + 3 * sequence.hashCode();
  }
  
  public ExactNumericType_Decimal_Option withPrecision(hydra.ext.org.ansi.sql.syntax.Precision precision) {
    java.util.Objects.requireNonNull((precision));
    return new ExactNumericType_Decimal_Option(precision, sequence);
  }
  
  public ExactNumericType_Decimal_Option withSequence(hydra.util.Opt<hydra.ext.org.ansi.sql.syntax.Scale> sequence) {
    java.util.Objects.requireNonNull((sequence));
    return new ExactNumericType_Decimal_Option(precision, sequence);
  }
}