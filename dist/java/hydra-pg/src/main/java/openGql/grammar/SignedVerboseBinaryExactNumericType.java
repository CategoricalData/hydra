// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SignedVerboseBinaryExactNumericType implements Serializable, Comparable<SignedVerboseBinaryExactNumericType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SignedVerboseBinaryExactNumericType");

  public static final hydra.core.Name SIGNED = new hydra.core.Name("signed");

  public static final hydra.core.Name VERBOSE_TYPE = new hydra.core.Name("verboseType");

  public final Boolean signed;

  public final openGql.grammar.VerboseBinaryExactNumericType verboseType;

  public SignedVerboseBinaryExactNumericType (Boolean signed, openGql.grammar.VerboseBinaryExactNumericType verboseType) {
    this.signed = signed;
    this.verboseType = verboseType;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SignedVerboseBinaryExactNumericType)) {
      return false;
    }
    SignedVerboseBinaryExactNumericType o = (SignedVerboseBinaryExactNumericType) other;
    return java.util.Objects.equals(
      this.signed,
      o.signed) && java.util.Objects.equals(
      this.verboseType,
      o.verboseType);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(signed) + 3 * java.util.Objects.hashCode(verboseType);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SignedVerboseBinaryExactNumericType other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      signed,
      other.signed);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      verboseType,
      other.verboseType);
  }

  public SignedVerboseBinaryExactNumericType withSigned(Boolean signed) {
    return new SignedVerboseBinaryExactNumericType(signed, verboseType);
  }

  public SignedVerboseBinaryExactNumericType withVerboseType(openGql.grammar.VerboseBinaryExactNumericType verboseType) {
    return new SignedVerboseBinaryExactNumericType(signed, verboseType);
  }
}
