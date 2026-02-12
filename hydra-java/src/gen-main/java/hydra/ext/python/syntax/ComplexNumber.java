// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ComplexNumber implements Serializable, Comparable<ComplexNumber> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ComplexNumber");
  
  public static final hydra.core.Name FIELD_NAME_REAL = new hydra.core.Name("real");
  
  public static final hydra.core.Name FIELD_NAME_PLUS_OR_MINUS = new hydra.core.Name("plusOrMinus");
  
  public static final hydra.core.Name FIELD_NAME_IMAGINARY = new hydra.core.Name("imaginary");
  
  public final hydra.ext.python.syntax.SignedRealNumber real;
  
  public final hydra.ext.python.syntax.PlusOrMinus plusOrMinus;
  
  public final hydra.ext.python.syntax.ImaginaryNumber imaginary;
  
  public ComplexNumber (hydra.ext.python.syntax.SignedRealNumber real, hydra.ext.python.syntax.PlusOrMinus plusOrMinus, hydra.ext.python.syntax.ImaginaryNumber imaginary) {
    this.real = real;
    this.plusOrMinus = plusOrMinus;
    this.imaginary = imaginary;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ComplexNumber)) {
      return false;
    }
    ComplexNumber o = (ComplexNumber) other;
    return java.util.Objects.equals(
      this.real,
      o.real) && java.util.Objects.equals(
      this.plusOrMinus,
      o.plusOrMinus) && java.util.Objects.equals(
      this.imaginary,
      o.imaginary);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(real) + 3 * java.util.Objects.hashCode(plusOrMinus) + 5 * java.util.Objects.hashCode(imaginary);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ComplexNumber other) {
    int cmp = 0;
    cmp = ((Comparable) real).compareTo(other.real);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) plusOrMinus).compareTo(other.plusOrMinus);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) imaginary).compareTo(other.imaginary);
  }
  
  public ComplexNumber withReal(hydra.ext.python.syntax.SignedRealNumber real) {
    return new ComplexNumber(real, plusOrMinus, imaginary);
  }
  
  public ComplexNumber withPlusOrMinus(hydra.ext.python.syntax.PlusOrMinus plusOrMinus) {
    return new ComplexNumber(real, plusOrMinus, imaginary);
  }
  
  public ComplexNumber withImaginary(hydra.ext.python.syntax.ImaginaryNumber imaginary) {
    return new ComplexNumber(real, plusOrMinus, imaginary);
  }
}
