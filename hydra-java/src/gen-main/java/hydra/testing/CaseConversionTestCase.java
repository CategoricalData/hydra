// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which checks that strings are converted between different case conventions correctly
 */
public class CaseConversionTestCase implements Serializable, Comparable<CaseConversionTestCase> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.CaseConversionTestCase");
  
  public static final hydra.core.Name FIELD_NAME_FROM_CONVENTION = new hydra.core.Name("fromConvention");
  
  public static final hydra.core.Name FIELD_NAME_TO_CONVENTION = new hydra.core.Name("toConvention");
  
  public static final hydra.core.Name FIELD_NAME_FROM_STRING = new hydra.core.Name("fromString");
  
  public static final hydra.core.Name FIELD_NAME_TO_STRING = new hydra.core.Name("toString");
  
  /**
   * The source case convention
   */
  public final hydra.util.CaseConvention fromConvention;
  
  /**
   * The target case convention
   */
  public final hydra.util.CaseConvention toConvention;
  
  /**
   * The input string
   */
  public final String fromString;
  
  /**
   * The expected output string
   */
  public final String toString;
  
  public CaseConversionTestCase (hydra.util.CaseConvention fromConvention, hydra.util.CaseConvention toConvention, String fromString, String toString) {
    this.fromConvention = fromConvention;
    this.toConvention = toConvention;
    this.fromString = fromString;
    this.toString = toString;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CaseConversionTestCase)) {
      return false;
    }
    CaseConversionTestCase o = (CaseConversionTestCase) (other);
    return java.util.Objects.equals(
      this.fromConvention,
      o.fromConvention) && java.util.Objects.equals(
      this.toConvention,
      o.toConvention) && java.util.Objects.equals(
      this.fromString,
      o.fromString) && java.util.Objects.equals(
      this.toString,
      o.toString);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(fromConvention) + 3 * java.util.Objects.hashCode(toConvention) + 5 * java.util.Objects.hashCode(fromString) + 7 * java.util.Objects.hashCode(toString);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CaseConversionTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) (fromConvention)).compareTo(other.fromConvention);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (toConvention)).compareTo(other.toConvention);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (fromString)).compareTo(other.fromString);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (toString)).compareTo(other.toString);
  }
  
  public CaseConversionTestCase withFromConvention(hydra.util.CaseConvention fromConvention) {
    return new CaseConversionTestCase(fromConvention, toConvention, fromString, toString);
  }
  
  public CaseConversionTestCase withToConvention(hydra.util.CaseConvention toConvention) {
    return new CaseConversionTestCase(fromConvention, toConvention, fromString, toString);
  }
  
  public CaseConversionTestCase withFromString(String fromString) {
    return new CaseConversionTestCase(fromConvention, toConvention, fromString, toString);
  }
  
  public CaseConversionTestCase withToString(String toString) {
    return new CaseConversionTestCase(fromConvention, toConvention, fromString, toString);
  }
}
