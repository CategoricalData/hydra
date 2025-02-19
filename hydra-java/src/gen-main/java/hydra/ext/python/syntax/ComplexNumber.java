// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ComplexNumber implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ComplexNumber");
  
  public static final hydra.core.Name FIELD_NAME_REAL = new hydra.core.Name("real");
  
  public static final hydra.core.Name FIELD_NAME_PLUS_OR_MINUS = new hydra.core.Name("plusOrMinus");
  
  public static final hydra.core.Name FIELD_NAME_IMAGINARY = new hydra.core.Name("imaginary");
  
  public final hydra.ext.python.syntax.SignedRealNumber real;
  
  public final hydra.ext.python.syntax.PlusOrMinus plusOrMinus;
  
  public final hydra.ext.python.syntax.ImaginaryNumber imaginary;
  
  public ComplexNumber (hydra.ext.python.syntax.SignedRealNumber real, hydra.ext.python.syntax.PlusOrMinus plusOrMinus, hydra.ext.python.syntax.ImaginaryNumber imaginary) {
    java.util.Objects.requireNonNull((real));
    java.util.Objects.requireNonNull((plusOrMinus));
    java.util.Objects.requireNonNull((imaginary));
    this.real = real;
    this.plusOrMinus = plusOrMinus;
    this.imaginary = imaginary;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ComplexNumber)) {
      return false;
    }
    ComplexNumber o = (ComplexNumber) (other);
    return real.equals(o.real) && plusOrMinus.equals(o.plusOrMinus) && imaginary.equals(o.imaginary);
  }
  
  @Override
  public int hashCode() {
    return 2 * real.hashCode() + 3 * plusOrMinus.hashCode() + 5 * imaginary.hashCode();
  }
  
  public ComplexNumber withReal(hydra.ext.python.syntax.SignedRealNumber real) {
    java.util.Objects.requireNonNull((real));
    return new ComplexNumber(real, plusOrMinus, imaginary);
  }
  
  public ComplexNumber withPlusOrMinus(hydra.ext.python.syntax.PlusOrMinus plusOrMinus) {
    java.util.Objects.requireNonNull((plusOrMinus));
    return new ComplexNumber(real, plusOrMinus, imaginary);
  }
  
  public ComplexNumber withImaginary(hydra.ext.python.syntax.ImaginaryNumber imaginary) {
    java.util.Objects.requireNonNull((imaginary));
    return new ComplexNumber(real, plusOrMinus, imaginary);
  }
}