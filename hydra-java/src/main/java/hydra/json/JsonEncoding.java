package hydra.json;

import hydra.core.Name;
import hydra.json.Value;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import hydra.util.Maybe;

import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;


/**
 * Encoding functions for Hydra's native JSON values.
 */
public abstract class JsonEncoding {

  /**
   * Encode a boolean value to JSON.
   *
   * @param b the boolean value to encode
   * @return the JSON value
   */
  protected static Value toJson(boolean b) {
    return new Value.Boolean_(b);
  }

  /**
   * Encode a double value to JSON.
   *
   * @param d the double value to encode
   * @return the JSON value
   */
  protected static Value toJson(double d) {
    return new Value.Number_(BigDecimal.valueOf(d));
  }

  /**
   * Encode an integer value to JSON.
   *
   * @param i the integer value to encode
   * @return the JSON value
   */
  protected static Value toJson(int i) {
    return new Value.Number_(BigDecimal.valueOf(i));
  }

  /**
   * Encode a string value to JSON.
   *
   * @param value the string value to encode
   * @return the JSON value
   */
  protected static Value toJson(String value) {
    return new Value.String_(value);
  }

  /**
   * Encode a list to JSON.
   *
   * @param <L> the element type
   * @param list the list to encode
   * @param mapping the function to map each element to a JSON value
   * @return the JSON value
   */
  protected static <L> Value toJson(List<L> list, Function<L, Value> mapping) {
    return new Value.Array(list.stream().map(mapping).collect(Collectors.toList()));
  }

  /**
   * Encode an optional value to JSON.
   *
   * @param <L> the element type
   * @param opt the optional value to encode
   * @param mapping the function to map the element to a JSON value
   * @return the JSON value, or null if the optional is empty
   */
  protected static <L> Value toJson(Maybe<L> opt, Function<L, Value> mapping) {
    return opt.map(mapping).orElse(null);
  }

  /**
   * Encode a list of strings to JSON.
   *
   * @param strings the list of strings to encode
   * @return the JSON value
   */
  protected static Value toJson(List<String> strings) {
    return toJson(strings, JsonEncoding::toJson);
  }

  /**
   * Create an empty JSON object (unit value).
   *
   * @return the empty JSON object
   */
  protected static Value unit() {
    return new Value.Object_(Collections.emptyMap());
  }

  /**
   * Create a JSON variant with a unit (empty object) value.
   *
   * @param key the variant key
   * @return the JSON variant
   */
  protected static Value unitVariant(String key) {
    return variant(key, unit());
  }

  /**
   * Create a JSON variant with a unit (empty object) value.
   *
   * @param key the variant key
   * @return the JSON variant
   */
  protected static Value unitVariant(Name key) {
    return variant(key.value, unit());
  }

  /**
   * Create a JSON variant (single-entry object).
   *
   * @param key the variant key
   * @param value the variant value
   * @return the JSON variant
   */
  protected static Value variant(String key, Value value) {
    return new ObjectBuilder().put(key, value).build();
  }

  /**
   * Create a JSON variant (single-entry object).
   *
   * @param key the variant key
   * @param value the variant value
   * @return the JSON variant
   */
  protected static Value variant(Name key, Value value) {
    return new ObjectBuilder().put(key.value, value).build();
  }

  /**
   * Builder class for constructing JSON objects.
   */
  public static class ObjectBuilder {
    // Note: use LinkedHashMap for the sake of predictable ordering
    private final Map<String, Value> map = new LinkedHashMap<>();

    /**
     * Add a boolean field to the object.
     *
     * @param key the field key
     * @param value the boolean value
     * @return this builder
     */
    public ObjectBuilder put(String key, boolean value) {
      return put(key, toJson(value));
    }

    /**
     * Add a boolean field to the object.
     *
     * @param key the field key
     * @param value the boolean value
     * @return this builder
     */
    public ObjectBuilder put(Name key, boolean value) {
      return put(key.value, value);
    }

    /**
     * Add a double field to the object.
     *
     * @param key the field key
     * @param value the double value
     * @return this builder
     */
    public ObjectBuilder put(String key, double value) {
      return put(key, toJson(value));
    }

    /**
     * Add a double field to the object.
     *
     * @param key the field key
     * @param value the double value
     * @return this builder
     */
    public ObjectBuilder put(Name key, double value) {
      return put(key.value, value);
    }

    /**
     * Add an integer field to the object.
     *
     * @param key the field key
     * @param value the integer value
     * @return this builder
     */
    public ObjectBuilder put(String key, int value) {
      return put(key, toJson(value));
    }

    /**
     * Add an integer field to the object.
     *
     * @param key the field key
     * @param value the integer value
     * @return this builder
     */
    public ObjectBuilder put(Name key, int value) {
      return put(key.value, value);
    }

    /**
     * Add a string field to the object.
     *
     * @param key the field key
     * @param value the string value
     * @return this builder
     */
    public ObjectBuilder put(String key, String value) {
      return put(key, toJson(value));
    }

    /**
     * Add a string field to the object.
     *
     * @param key the field key
     * @param value the string value
     * @return this builder
     */
    public ObjectBuilder put(Name key, String value) {
      return put(key.value, value);
    }

    /**
     * Add a JSON value field to the object.
     *
     * @param key the field key
     * @param value the JSON value (null values are ignored)
     * @return this builder
     */
    public ObjectBuilder put(String key, Value value) {
      if (value != null) {
        map.put(key, value);
      }
      return this;
    }

    /**
     * Add a JSON value field to the object.
     *
     * @param key the field key
     * @param value the JSON value (null values are ignored)
     * @return this builder
     */
    public ObjectBuilder put(Name key, Value value) {
      return put(key.value, value);
    }

    /**
     * Add a custom field to the object with a mapping function.
     *
     * @param <L> the element type
     * @param key the field key
     * @param value the value to encode
     * @param mapping the function to map the value to a JSON value
     * @return this builder
     */
    public <L> ObjectBuilder put(String key, L value, Function<L, Value> mapping) {
      return put(key, mapping.apply(value));
    }

    /**
     * Add a custom field to the object with a mapping function.
     *
     * @param <L> the element type
     * @param key the field key
     * @param value the value to encode
     * @param mapping the function to map the value to a JSON value
     * @return this builder
     */
    public <L> ObjectBuilder put(Name key, L value, Function<L, Value> mapping) {
      return put(key.value, value, mapping);
    }

    /**
     * Add a list field to the object.
     *
     * @param <L> the element type
     * @param key the field key
     * @param value the list value
     * @param mapping the function to map each element to a JSON value
     * @return this builder
     */
    public <L> ObjectBuilder putList(String key, List<L> value, Function<L, Value> mapping) {
      return put(key, toJson(value, mapping));
    }

    /**
     * Add a list field to the object.
     *
     * @param <L> the element type
     * @param key the field key
     * @param value the list value
     * @param mapping the function to map each element to a JSON value
     * @return this builder
     */
    public <L> ObjectBuilder putList(Name key, List<L> value, Function<L, Value> mapping) {
      return putList(key.value, value, mapping);
    }

    /**
     * Add an optional boolean field to the object.
     *
     * @param key the field key
     * @param value the optional boolean value
     * @return this builder
     */
    public ObjectBuilder putOpt(String key, Maybe<Boolean> value) {
      return putOpt(key, value, JsonEncoding::toJson);
    }

    /**
     * Add an optional boolean field to the object.
     *
     * @param key the field key
     * @param value the optional boolean value
     * @return this builder
     */
    public ObjectBuilder putOpt(Name key, Maybe<Boolean> value) {
      return putOpt(key, value, JsonEncoding::toJson);
    }

    /**
     * Add an optional field to the object.
     *
     * @param <L> the element type
     * @param key the field key
     * @param value the optional value
     * @param mapping the function to map the value to a JSON value
     * @return this builder
     */
    public <L> ObjectBuilder putOpt(String key, Maybe<L> value, Function<L, Value> mapping) {
      return put(key, toJson(value, mapping));
    }

    /**
     * Add an optional field to the object.
     *
     * @param <L> the element type
     * @param key the field key
     * @param value the optional value
     * @param mapping the function to map the value to a JSON value
     * @return this builder
     */
    public <L> ObjectBuilder putOpt(Name key, Maybe<L> value, Function<L, Value> mapping) {
      return putOpt(key.value, value, mapping);
    }

    /**
     * Add an optional set field to the object.
     *
     * @param <L> the element type
     * @param key the field key
     * @param values the optional set value
     * @param mapping the function to map each element to a JSON value
     * @return this builder
     */
    public <L> ObjectBuilder putOptSet(String key, Maybe<Set<L>> values, Function<L, Value> mapping) {
      return put(key, toJson(new ArrayList<>(values.orElse(Collections.emptySet())), mapping));
    }

    /**
     * Add an optional set field to the object.
     *
     * @param <L> the element type
     * @param key the field key
     * @param values the optional set value
     * @param mapping the function to map each element to a JSON value
     * @return this builder
     */
    public <L> ObjectBuilder putOptSet(Name key, Maybe<Set<L>> values, Function<L, Value> mapping) {
      return putOptSet(key.value, values, mapping);
    }

    /**
     * Build the JSON object.
     *
     * @return the JSON object value
     */
    public Value build() {
      return new Value.Object_(map);
    }
  }
}
