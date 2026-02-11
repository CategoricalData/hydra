// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which encodes a Hydra term to JSON using a type-directed coder, and verifies that decoding produces the original term (round-trip)
 */
public class JsonCoderTestCase implements Serializable, Comparable<JsonCoderTestCase> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.JsonCoderTestCase");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public static final hydra.core.Name FIELD_NAME_JSON = new hydra.core.Name("json");
  
  /**
   * The Hydra type that determines how the term is encoded/decoded
   */
  public final hydra.core.Type type;
  
  /**
   * The Hydra term to encode
   */
  public final hydra.core.Term term;
  
  /**
   * The expected JSON value
   */
  public final hydra.json.model.Value json;
  
  public JsonCoderTestCase (hydra.core.Type type, hydra.core.Term term, hydra.json.model.Value json) {
    this.type = type;
    this.term = term;
    this.json = json;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof JsonCoderTestCase)) {
      return false;
    }
    JsonCoderTestCase o = (JsonCoderTestCase) other;
    return java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.term,
      o.term) && java.util.Objects.equals(
      this.json,
      o.json);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(type) + 3 * java.util.Objects.hashCode(term) + 5 * java.util.Objects.hashCode(json);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(JsonCoderTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) type).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) term).compareTo(other.term);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) json).compareTo(other.json);
  }
  
  public JsonCoderTestCase withType(hydra.core.Type type) {
    return new JsonCoderTestCase(type, term, json);
  }
  
  public JsonCoderTestCase withTerm(hydra.core.Term term) {
    return new JsonCoderTestCase(type, term, json);
  }
  
  public JsonCoderTestCase withJson(hydra.json.model.Value json) {
    return new JsonCoderTestCase(type, term, json);
  }
}
