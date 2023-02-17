package hydra.ext.sql.ansi;

public class ExactNumericType_Dec_Option {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.ExactNumericType.Dec.Option");
  
  public final hydra.ext.sql.ansi.Precision precision;
  
  public final java.util.Optional<hydra.ext.sql.ansi.Scale> sequence;
  
  public ExactNumericType_Dec_Option (hydra.ext.sql.ansi.Precision precision, java.util.Optional<hydra.ext.sql.ansi.Scale> sequence) {
    this.precision = precision;
    this.sequence = sequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExactNumericType_Dec_Option)) {
      return false;
    }
    ExactNumericType_Dec_Option o = (ExactNumericType_Dec_Option) (other);
    return precision.equals(o.precision) && sequence.equals(o.sequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * precision.hashCode() + 3 * sequence.hashCode();
  }
  
  public ExactNumericType_Dec_Option withPrecision(hydra.ext.sql.ansi.Precision precision) {
    return new ExactNumericType_Dec_Option(precision, sequence);
  }
  
  public ExactNumericType_Dec_Option withSequence(java.util.Optional<hydra.ext.sql.ansi.Scale> sequence) {
    return new ExactNumericType_Dec_Option(precision, sequence);
  }
}