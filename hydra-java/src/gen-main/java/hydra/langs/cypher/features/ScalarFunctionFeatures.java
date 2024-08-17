// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * Scalar functions
 */
public class ScalarFunctionFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/features.ScalarFunctionFeatures");
  
  public static final hydra.core.Name FIELD_NAME_CHAR_LENGTH = new hydra.core.Name("char_length");
  
  public static final hydra.core.Name FIELD_NAME_CHARACTER_LENGTH = new hydra.core.Name("character_length");
  
  public static final hydra.core.Name FIELD_NAME_COALESCE = new hydra.core.Name("coalesce");
  
  public static final hydra.core.Name FIELD_NAME_ELEMENT_ID = new hydra.core.Name("elementId");
  
  public static final hydra.core.Name FIELD_NAME_END_NODE = new hydra.core.Name("endNode");
  
  public static final hydra.core.Name FIELD_NAME_HEAD = new hydra.core.Name("head");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_LAST = new hydra.core.Name("last");
  
  public static final hydra.core.Name FIELD_NAME_LENGTH = new hydra.core.Name("length");
  
  public static final hydra.core.Name FIELD_NAME_NULL_IF = new hydra.core.Name("nullIf");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTIES = new hydra.core.Name("properties");
  
  public static final hydra.core.Name FIELD_NAME_RANDOM_U_U_I_D = new hydra.core.Name("randomUUID");
  
  public static final hydra.core.Name FIELD_NAME_SIZE = new hydra.core.Name("size");
  
  public static final hydra.core.Name FIELD_NAME_START_NODE = new hydra.core.Name("startNode");
  
  public static final hydra.core.Name FIELD_NAME_TO_BOOLEAN = new hydra.core.Name("toBoolean");
  
  public static final hydra.core.Name FIELD_NAME_TO_BOOLEAN_OR_NULL = new hydra.core.Name("toBooleanOrNull");
  
  public static final hydra.core.Name FIELD_NAME_TO_FLOAT = new hydra.core.Name("toFloat");
  
  public static final hydra.core.Name FIELD_NAME_TO_FLOAT_OR_NULL = new hydra.core.Name("toFloatOrNull");
  
  public static final hydra.core.Name FIELD_NAME_TO_INTEGER = new hydra.core.Name("toInteger");
  
  public static final hydra.core.Name FIELD_NAME_TO_INTEGER_OR_NULL = new hydra.core.Name("toIntegerOrNull");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_VALUE_TYPE = new hydra.core.Name("valueType");
  
  /**
   * The char_length() function. Returns the number of Unicode characters in a STRING.
   */
  public final Boolean char_length;
  
  /**
   * The character_length() function. Returns the number of Unicode characters in a STRING.
   */
  public final Boolean character_length;
  
  /**
   * The coalesce() function. Returns the first non-null value in a list of expressions.
   */
  public final Boolean coalesce;
  
  /**
   * The elementId() function. Returns a node identifier, unique within a specific transaction and DBMS.; Returns a relationship identifier, unique within a specific transaction and DBMS.
   */
  public final Boolean elementId;
  
  /**
   * The endNode() function. Returns a relationship identifier, unique within a specific transaction and DBMS.
   */
  public final Boolean endNode;
  
  /**
   * The head() function. Returns the first element in a LIST&lt;ANY&gt;.
   */
  public final Boolean head;
  
  /**
   * The id() function. [Deprecated] Returns the id of a NODE. Replaced by elementId().; [Deprecated] Returns the id of a RELATIONSHIP. Replaced by elementId().
   */
  public final Boolean id;
  
  /**
   * The last() function. Returns the last element in a LIST&lt;ANY&gt;.
   */
  public final Boolean last;
  
  /**
   * The length() function. Returns the length of a PATH.
   */
  public final Boolean length;
  
  /**
   * The nullIf() function. Returns null if the two given parameters are equivalent, otherwise returns the value of the first parameter.
   */
  public final Boolean nullIf;
  
  /**
   * The properties() function. Returns a MAP containing all the properties of a MAP.; Returns a MAP containing all the properties of a NODE.; Returns a MAP containing all the properties of a RELATIONSHIP.
   */
  public final Boolean properties;
  
  /**
   * The randomUUID() function. Generates a random UUID.
   */
  public final Boolean randomUUID;
  
  /**
   * The size() function. Returns the number of items in a LIST&lt;ANY&gt;.; Returns the number of Unicode characters in a STRING.
   */
  public final Boolean size;
  
  /**
   * The startNode() function. Returns the start NODE of a RELATIONSHIP.
   */
  public final Boolean startNode;
  
  /**
   * The toBoolean() function. Converts a STRING value to a BOOLEAN value.; Converts a BOOLEAN value to a BOOLEAN value.; Converts an INTEGER value to a BOOLEAN value.
   */
  public final Boolean toBoolean;
  
  /**
   * The toBooleanOrNull() function. Converts a value to a BOOLEAN value, or null if the value cannot be converted.
   */
  public final Boolean toBooleanOrNull;
  
  /**
   * The toFloat() function. Converts an INTEGER value to a FLOAT value.; Converts a STRING value to a FLOAT value.
   */
  public final Boolean toFloat;
  
  /**
   * The toFloatOrNull() function. Converts a value to a FLOAT value, or null if the value cannot be converted.
   */
  public final Boolean toFloatOrNull;
  
  /**
   * The toInteger() function. Converts a FLOAT value to an INTEGER value.; Converts a BOOLEAN value to an INTEGER value.; Converts a STRING value to an INTEGER value.
   */
  public final Boolean toInteger;
  
  /**
   * The toIntegerOrNull() function. Converts a value to an INTEGER value, or null if the value cannot be converted.
   */
  public final Boolean toIntegerOrNull;
  
  /**
   * The type() function. Returns a STRING representation of the RELATIONSHIP type.
   */
  public final Boolean type;
  
  /**
   * The valueType() function. Returns a STRING representation of the most precise value type that the given expression evaluates to.
   */
  public final Boolean valueType;
  
  public ScalarFunctionFeatures (Boolean char_length, Boolean character_length, Boolean coalesce, Boolean elementId, Boolean endNode, Boolean head, Boolean id, Boolean last, Boolean length, Boolean nullIf, Boolean properties, Boolean randomUUID, Boolean size, Boolean startNode, Boolean toBoolean, Boolean toBooleanOrNull, Boolean toFloat, Boolean toFloatOrNull, Boolean toInteger, Boolean toIntegerOrNull, Boolean type, Boolean valueType) {
    java.util.Objects.requireNonNull((char_length));
    java.util.Objects.requireNonNull((character_length));
    java.util.Objects.requireNonNull((coalesce));
    java.util.Objects.requireNonNull((elementId));
    java.util.Objects.requireNonNull((endNode));
    java.util.Objects.requireNonNull((head));
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((last));
    java.util.Objects.requireNonNull((length));
    java.util.Objects.requireNonNull((nullIf));
    java.util.Objects.requireNonNull((properties));
    java.util.Objects.requireNonNull((randomUUID));
    java.util.Objects.requireNonNull((size));
    java.util.Objects.requireNonNull((startNode));
    java.util.Objects.requireNonNull((toBoolean));
    java.util.Objects.requireNonNull((toBooleanOrNull));
    java.util.Objects.requireNonNull((toFloat));
    java.util.Objects.requireNonNull((toFloatOrNull));
    java.util.Objects.requireNonNull((toInteger));
    java.util.Objects.requireNonNull((toIntegerOrNull));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((valueType));
    this.char_length = char_length;
    this.character_length = character_length;
    this.coalesce = coalesce;
    this.elementId = elementId;
    this.endNode = endNode;
    this.head = head;
    this.id = id;
    this.last = last;
    this.length = length;
    this.nullIf = nullIf;
    this.properties = properties;
    this.randomUUID = randomUUID;
    this.size = size;
    this.startNode = startNode;
    this.toBoolean = toBoolean;
    this.toBooleanOrNull = toBooleanOrNull;
    this.toFloat = toFloat;
    this.toFloatOrNull = toFloatOrNull;
    this.toInteger = toInteger;
    this.toIntegerOrNull = toIntegerOrNull;
    this.type = type;
    this.valueType = valueType;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ScalarFunctionFeatures)) {
      return false;
    }
    ScalarFunctionFeatures o = (ScalarFunctionFeatures) (other);
    return char_length.equals(o.char_length) && character_length.equals(o.character_length) && coalesce.equals(o.coalesce) && elementId.equals(o.elementId) && endNode.equals(o.endNode) && head.equals(o.head) && id.equals(o.id) && last.equals(o.last) && length.equals(o.length) && nullIf.equals(o.nullIf) && properties.equals(o.properties) && randomUUID.equals(o.randomUUID) && size.equals(o.size) && startNode.equals(o.startNode) && toBoolean.equals(o.toBoolean) && toBooleanOrNull.equals(o.toBooleanOrNull) && toFloat.equals(o.toFloat) && toFloatOrNull.equals(o.toFloatOrNull) && toInteger.equals(o.toInteger) && toIntegerOrNull.equals(o.toIntegerOrNull) && type.equals(o.type) && valueType.equals(o.valueType);
  }
  
  @Override
  public int hashCode() {
    return 2 * char_length.hashCode() + 3 * character_length.hashCode() + 5 * coalesce.hashCode() + 7 * elementId.hashCode() + 11 * endNode.hashCode() + 13 * head.hashCode() + 17 * id.hashCode() + 19 * last.hashCode() + 23 * length.hashCode() + 29 * nullIf.hashCode() + 31 * properties.hashCode() + 37 * randomUUID.hashCode() + 41 * size.hashCode() + 43 * startNode.hashCode() + 47 * toBoolean.hashCode() + 53 * toBooleanOrNull.hashCode() + 59 * toFloat.hashCode() + 61 * toFloatOrNull.hashCode() + 67 * toInteger.hashCode() + 71 * toIntegerOrNull.hashCode() + 2 * type.hashCode() + 3 * valueType.hashCode();
  }
  
  public ScalarFunctionFeatures withChar_length(Boolean char_length) {
    java.util.Objects.requireNonNull((char_length));
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withCharacter_length(Boolean character_length) {
    java.util.Objects.requireNonNull((character_length));
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withCoalesce(Boolean coalesce) {
    java.util.Objects.requireNonNull((coalesce));
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withElementId(Boolean elementId) {
    java.util.Objects.requireNonNull((elementId));
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withEndNode(Boolean endNode) {
    java.util.Objects.requireNonNull((endNode));
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withHead(Boolean head) {
    java.util.Objects.requireNonNull((head));
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withId(Boolean id) {
    java.util.Objects.requireNonNull((id));
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withLast(Boolean last) {
    java.util.Objects.requireNonNull((last));
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withLength(Boolean length) {
    java.util.Objects.requireNonNull((length));
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withNullIf(Boolean nullIf) {
    java.util.Objects.requireNonNull((nullIf));
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withProperties(Boolean properties) {
    java.util.Objects.requireNonNull((properties));
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withRandomUUID(Boolean randomUUID) {
    java.util.Objects.requireNonNull((randomUUID));
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withSize(Boolean size) {
    java.util.Objects.requireNonNull((size));
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withStartNode(Boolean startNode) {
    java.util.Objects.requireNonNull((startNode));
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withToBoolean(Boolean toBoolean) {
    java.util.Objects.requireNonNull((toBoolean));
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withToBooleanOrNull(Boolean toBooleanOrNull) {
    java.util.Objects.requireNonNull((toBooleanOrNull));
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withToFloat(Boolean toFloat) {
    java.util.Objects.requireNonNull((toFloat));
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withToFloatOrNull(Boolean toFloatOrNull) {
    java.util.Objects.requireNonNull((toFloatOrNull));
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withToInteger(Boolean toInteger) {
    java.util.Objects.requireNonNull((toInteger));
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withToIntegerOrNull(Boolean toIntegerOrNull) {
    java.util.Objects.requireNonNull((toIntegerOrNull));
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withType(Boolean type) {
    java.util.Objects.requireNonNull((type));
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withValueType(Boolean valueType) {
    java.util.Objects.requireNonNull((valueType));
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
}