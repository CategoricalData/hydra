// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class ComplexNumber implements Serializable, Comparable<ComplexNumber> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.ComplexNumber");

  public static final hydra.core.Name REAL = new hydra.core.Name("real");

  public static final hydra.core.Name PLUS_OR_MINUS = new hydra.core.Name("plusOrMinus");

  public static final hydra.core.Name IMAGINARY = new hydra.core.Name("imaginary");

  public final hydra.python.syntax.SignedRealNumber real;

  public final hydra.python.syntax.PlusOrMinus plusOrMinus;

  public final hydra.python.syntax.ImaginaryNumber imaginary;

  public ComplexNumber (hydra.python.syntax.SignedRealNumber real, hydra.python.syntax.PlusOrMinus plusOrMinus, hydra.python.syntax.ImaginaryNumber imaginary) {
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
    cmp = hydra.util.Comparing.compare(
      real,
      other.real);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      plusOrMinus,
      other.plusOrMinus);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      imaginary,
      other.imaginary);
  }

  public ComplexNumber withReal(hydra.python.syntax.SignedRealNumber real) {
    return new ComplexNumber(real, plusOrMinus, imaginary);
  }

  public ComplexNumber withPlusOrMinus(hydra.python.syntax.PlusOrMinus plusOrMinus) {
    return new ComplexNumber(real, plusOrMinus, imaginary);
  }

  public ComplexNumber withImaginary(hydra.python.syntax.ImaginaryNumber imaginary) {
    return new ComplexNumber(real, plusOrMinus, imaginary);
  }
}
