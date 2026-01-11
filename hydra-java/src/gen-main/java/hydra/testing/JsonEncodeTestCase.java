// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case for the Either-based JSON encoder. Takes an input term and expected result (Either String Value).
 */
public class JsonEncodeTestCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.JsonEncodeTestCase");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public static final hydra.core.Name FIELD_NAME_EXPECTED = new hydra.core.Name("expected");
  
  /**
   * The Hydra term to encode
   */
  public final hydra.core.Term term;
  
  /**
   * The expected result: Left for error, Right for encoded JSON
   */
  public final hydra.util.Either<String, hydra.json.model.Value> expected;
  
  public JsonEncodeTestCase (hydra.core.Term term, hydra.util.Either<String, hydra.json.model.Value> expected) {
    java.util.Objects.requireNonNull((term));
    java.util.Objects.requireNonNull((expected));
    this.term = term;
    this.expected = expected;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof JsonEncodeTestCase)) {
      return false;
    }
    JsonEncodeTestCase o = (JsonEncodeTestCase) (other);
    return term.equals(o.term) && expected.equals(o.expected);
  }
  
  @Override
  public int hashCode() {
    return 2 * term.hashCode() + 3 * expected.hashCode();
  }
  
  public JsonEncodeTestCase withTerm(hydra.core.Term term) {
    java.util.Objects.requireNonNull((term));
    return new JsonEncodeTestCase(term, expected);
  }
  
  public JsonEncodeTestCase withExpected(hydra.util.Either<String, hydra.json.model.Value> expected) {
    java.util.Objects.requireNonNull((expected));
    return new JsonEncodeTestCase(term, expected);
  }
}
