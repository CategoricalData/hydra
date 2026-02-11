// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case for round-trip encoding/decoding using the Either-based JSON functions. Encodes a term, then decodes it back, verifying the result equals the original.
 */
public class JsonRoundtripTestCase implements Serializable, Comparable<JsonRoundtripTestCase> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.JsonRoundtripTestCase");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  /**
   * The Hydra type for encoding/decoding
   */
  public final hydra.core.Type type;
  
  /**
   * The Hydra term to round-trip
   */
  public final hydra.core.Term term;
  
  public JsonRoundtripTestCase (hydra.core.Type type, hydra.core.Term term) {
    this.type = type;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof JsonRoundtripTestCase)) {
      return false;
    }
    JsonRoundtripTestCase o = (JsonRoundtripTestCase) other;
    return java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.term,
      o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(type) + 3 * java.util.Objects.hashCode(term);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(JsonRoundtripTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) type).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) term).compareTo(other.term);
  }
  
  public JsonRoundtripTestCase withType(hydra.core.Type type) {
    return new JsonRoundtripTestCase(type, term);
  }
  
  public JsonRoundtripTestCase withTerm(hydra.core.Term term) {
    return new JsonRoundtripTestCase(type, term);
  }
}
