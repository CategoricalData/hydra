// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Scalar functions
 */
public class ScalarFunctionFeatures implements Serializable, Comparable<ScalarFunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.ScalarFunctionFeatures");
  
  public static final hydra.core.Name CHAR_LENGTH = new hydra.core.Name("char_length");
  
  public static final hydra.core.Name CHARACTER_LENGTH = new hydra.core.Name("character_length");
  
  public static final hydra.core.Name COALESCE = new hydra.core.Name("coalesce");
  
  public static final hydra.core.Name ELEMENT_ID = new hydra.core.Name("elementId");
  
  public static final hydra.core.Name END_NODE = new hydra.core.Name("endNode");
  
  public static final hydra.core.Name HEAD = new hydra.core.Name("head");
  
  public static final hydra.core.Name ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name LAST = new hydra.core.Name("last");
  
  public static final hydra.core.Name LENGTH = new hydra.core.Name("length");
  
  public static final hydra.core.Name NULL_IF = new hydra.core.Name("nullIf");
  
  public static final hydra.core.Name PROPERTIES = new hydra.core.Name("properties");
  
  public static final hydra.core.Name RANDOM_U_U_I_D = new hydra.core.Name("randomUUID");
  
  public static final hydra.core.Name SIZE = new hydra.core.Name("size");
  
  public static final hydra.core.Name START_NODE = new hydra.core.Name("startNode");
  
  public static final hydra.core.Name TO_BOOLEAN = new hydra.core.Name("toBoolean");
  
  public static final hydra.core.Name TO_BOOLEAN_OR_NULL = new hydra.core.Name("toBooleanOrNull");
  
  public static final hydra.core.Name TO_FLOAT = new hydra.core.Name("toFloat");
  
  public static final hydra.core.Name TO_FLOAT_OR_NULL = new hydra.core.Name("toFloatOrNull");
  
  public static final hydra.core.Name TO_INTEGER = new hydra.core.Name("toInteger");
  
  public static final hydra.core.Name TO_INTEGER_OR_NULL = new hydra.core.Name("toIntegerOrNull");
  
  public static final hydra.core.Name TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name VALUE_TYPE = new hydra.core.Name("valueType");
  
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
    ScalarFunctionFeatures o = (ScalarFunctionFeatures) other;
    return java.util.Objects.equals(
      this.char_length,
      o.char_length) && java.util.Objects.equals(
      this.character_length,
      o.character_length) && java.util.Objects.equals(
      this.coalesce,
      o.coalesce) && java.util.Objects.equals(
      this.elementId,
      o.elementId) && java.util.Objects.equals(
      this.endNode,
      o.endNode) && java.util.Objects.equals(
      this.head,
      o.head) && java.util.Objects.equals(
      this.id,
      o.id) && java.util.Objects.equals(
      this.last,
      o.last) && java.util.Objects.equals(
      this.length,
      o.length) && java.util.Objects.equals(
      this.nullIf,
      o.nullIf) && java.util.Objects.equals(
      this.properties,
      o.properties) && java.util.Objects.equals(
      this.randomUUID,
      o.randomUUID) && java.util.Objects.equals(
      this.size,
      o.size) && java.util.Objects.equals(
      this.startNode,
      o.startNode) && java.util.Objects.equals(
      this.toBoolean,
      o.toBoolean) && java.util.Objects.equals(
      this.toBooleanOrNull,
      o.toBooleanOrNull) && java.util.Objects.equals(
      this.toFloat,
      o.toFloat) && java.util.Objects.equals(
      this.toFloatOrNull,
      o.toFloatOrNull) && java.util.Objects.equals(
      this.toInteger,
      o.toInteger) && java.util.Objects.equals(
      this.toIntegerOrNull,
      o.toIntegerOrNull) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.valueType,
      o.valueType);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(char_length) + 3 * java.util.Objects.hashCode(character_length) + 5 * java.util.Objects.hashCode(coalesce) + 7 * java.util.Objects.hashCode(elementId) + 11 * java.util.Objects.hashCode(endNode) + 13 * java.util.Objects.hashCode(head) + 17 * java.util.Objects.hashCode(id) + 19 * java.util.Objects.hashCode(last) + 23 * java.util.Objects.hashCode(length) + 29 * java.util.Objects.hashCode(nullIf) + 31 * java.util.Objects.hashCode(properties) + 37 * java.util.Objects.hashCode(randomUUID) + 41 * java.util.Objects.hashCode(size) + 43 * java.util.Objects.hashCode(startNode) + 47 * java.util.Objects.hashCode(toBoolean) + 53 * java.util.Objects.hashCode(toBooleanOrNull) + 59 * java.util.Objects.hashCode(toFloat) + 61 * java.util.Objects.hashCode(toFloatOrNull) + 67 * java.util.Objects.hashCode(toInteger) + 71 * java.util.Objects.hashCode(toIntegerOrNull);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ScalarFunctionFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) char_length).compareTo(other.char_length);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) character_length).compareTo(other.character_length);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) coalesce).compareTo(other.coalesce);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) elementId).compareTo(other.elementId);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) endNode).compareTo(other.endNode);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) head).compareTo(other.head);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) id).compareTo(other.id);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) last).compareTo(other.last);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) length).compareTo(other.length);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) nullIf).compareTo(other.nullIf);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) properties).compareTo(other.properties);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) randomUUID).compareTo(other.randomUUID);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) size).compareTo(other.size);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) startNode).compareTo(other.startNode);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) toBoolean).compareTo(other.toBoolean);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) toBooleanOrNull).compareTo(other.toBooleanOrNull);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) toFloat).compareTo(other.toFloat);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) toFloatOrNull).compareTo(other.toFloatOrNull);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) toInteger).compareTo(other.toInteger);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) toIntegerOrNull).compareTo(other.toIntegerOrNull);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) type).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) valueType).compareTo(other.valueType);
  }
  
  public ScalarFunctionFeatures withChar_length(Boolean char_length) {
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withCharacter_length(Boolean character_length) {
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withCoalesce(Boolean coalesce) {
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withElementId(Boolean elementId) {
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withEndNode(Boolean endNode) {
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withHead(Boolean head) {
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withId(Boolean id) {
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withLast(Boolean last) {
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withLength(Boolean length) {
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withNullIf(Boolean nullIf) {
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withProperties(Boolean properties) {
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withRandomUUID(Boolean randomUUID) {
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withSize(Boolean size) {
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withStartNode(Boolean startNode) {
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withToBoolean(Boolean toBoolean) {
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withToBooleanOrNull(Boolean toBooleanOrNull) {
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withToFloat(Boolean toFloat) {
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withToFloatOrNull(Boolean toFloatOrNull) {
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withToInteger(Boolean toInteger) {
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withToIntegerOrNull(Boolean toIntegerOrNull) {
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withType(Boolean type) {
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
  
  public ScalarFunctionFeatures withValueType(Boolean valueType) {
    return new ScalarFunctionFeatures(char_length, character_length, coalesce, elementId, endNode, head, id, last, length, nullIf, properties, randomUUID, size, startNode, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull, type, valueType);
  }
}
