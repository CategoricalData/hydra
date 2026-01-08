// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which encodes a Hydra term to JSON using a type-directed coder, and verifies that decoding produces the original term (round-trip)
 */
public class JsonCoderTestCase implements Serializable {
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
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((term));
    java.util.Objects.requireNonNull((json));
    this.type = type;
    this.term = term;
    this.json = json;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof JsonCoderTestCase)) {
      return false;
    }
    JsonCoderTestCase o = (JsonCoderTestCase) (other);
    return type.equals(o.type) && term.equals(o.term) && json.equals(o.json);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * term.hashCode() + 5 * json.hashCode();
  }
  
  public JsonCoderTestCase withType(hydra.core.Type type) {
    java.util.Objects.requireNonNull((type));
    return new JsonCoderTestCase(type, term, json);
  }
  
  public JsonCoderTestCase withTerm(hydra.core.Term term) {
    java.util.Objects.requireNonNull((term));
    return new JsonCoderTestCase(type, term, json);
  }
  
  public JsonCoderTestCase withJson(hydra.json.model.Value json) {
    java.util.Objects.requireNonNull((json));
    return new JsonCoderTestCase(type, term, json);
  }
}
