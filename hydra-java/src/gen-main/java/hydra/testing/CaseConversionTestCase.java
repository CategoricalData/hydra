// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which checks that strings are converted between different case conventions correctly
 */
public class CaseConversionTestCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.CaseConversionTestCase");
  
  public static final hydra.core.Name FIELD_NAME_FROM_CONVENTION = new hydra.core.Name("fromConvention");
  
  public static final hydra.core.Name FIELD_NAME_TO_CONVENTION = new hydra.core.Name("toConvention");
  
  public static final hydra.core.Name FIELD_NAME_FROM_STRING = new hydra.core.Name("fromString");
  
  public static final hydra.core.Name FIELD_NAME_TO_STRING = new hydra.core.Name("toString");
  
  public final hydra.mantle.CaseConvention fromConvention;
  
  public final hydra.mantle.CaseConvention toConvention;
  
  public final String fromString;
  
  public final String toString;
  
  public CaseConversionTestCase (hydra.mantle.CaseConvention fromConvention, hydra.mantle.CaseConvention toConvention, String fromString, String toString) {
    java.util.Objects.requireNonNull((fromConvention));
    java.util.Objects.requireNonNull((toConvention));
    java.util.Objects.requireNonNull((fromString));
    java.util.Objects.requireNonNull((toString));
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
    return fromConvention.equals(o.fromConvention) && toConvention.equals(o.toConvention) && fromString.equals(o.fromString) && toString.equals(o.toString);
  }
  
  @Override
  public int hashCode() {
    return 2 * fromConvention.hashCode() + 3 * toConvention.hashCode() + 5 * fromString.hashCode() + 7 * toString.hashCode();
  }
  
  public CaseConversionTestCase withFromConvention(hydra.mantle.CaseConvention fromConvention) {
    java.util.Objects.requireNonNull((fromConvention));
    return new CaseConversionTestCase(fromConvention, toConvention, fromString, toString);
  }
  
  public CaseConversionTestCase withToConvention(hydra.mantle.CaseConvention toConvention) {
    java.util.Objects.requireNonNull((toConvention));
    return new CaseConversionTestCase(fromConvention, toConvention, fromString, toString);
  }
  
  public CaseConversionTestCase withFromString(String fromString) {
    java.util.Objects.requireNonNull((fromString));
    return new CaseConversionTestCase(fromConvention, toConvention, fromString, toString);
  }
  
  public CaseConversionTestCase withToString(String toString) {
    java.util.Objects.requireNonNull((toString));
    return new CaseConversionTestCase(fromConvention, toConvention, fromString, toString);
  }
}