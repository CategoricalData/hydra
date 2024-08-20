// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.parquet.format;

import java.io.Serializable;

/**
 * Represents a element inside a schema definition.
 * - if it is a group (inner node) then type is undefined and num_children is defined
 * - if it is a primitive type (leaf) then type is defined and num_children is undefined
 * the nodes are listed in depth first traversal order.
 */
public class SchemaElement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/parquet/format.SchemaElement");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_LENGTH = new hydra.core.Name("typeLength");
  
  public static final hydra.core.Name FIELD_NAME_REPETITION_TYPE = new hydra.core.Name("repetitionType");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_NUM_CHILDREN = new hydra.core.Name("numChildren");
  
  public static final hydra.core.Name FIELD_NAME_FIELD_ID = new hydra.core.Name("fieldId");
  
  public static final hydra.core.Name FIELD_NAME_LOGICAL_TYPE = new hydra.core.Name("logicalType");
  
  /**
   * Data type for this field. Not set if the current element is a non-leaf node
   */
  public final hydra.util.Opt<hydra.ext.org.apache.parquet.format.Type> type;
  
  /**
   * If type is FIXED_LEN_BYTE_ARRAY, this is the byte length of the values. Otherwise, if specified, this is the maximum bit length to store any of the values. (e.g. a low cardinality INT col could have this set to 3).  Note that this is in the schema, and therefore fixed for the entire file.
   */
  public final hydra.util.Opt<Integer> typeLength;
  
  /**
   * repetition of the field. The root of the schema does not have a repetition_type. All other nodes must have one
   */
  public final hydra.util.Opt<hydra.ext.org.apache.parquet.format.FieldRepetitionType> repetitionType;
  
  /**
   * Name of the field in the schema
   */
  public final String name;
  
  /**
   * Nested fields.  Since thrift does not support nested fields, the nesting is flattened to a single list by a depth-first traversal. The children count is used to construct the nested relationship. This field is not set when the element is a primitive type
   */
  public final hydra.util.Opt<Integer> numChildren;
  
  /**
   * When the original schema supports field ids, this will save the original field id in the parquet schema
   */
  public final hydra.util.Opt<Integer> fieldId;
  
  /**
   * The logical type of this SchemaElement. LogicalType replaces ConvertedType, but ConvertedType is still required for some logical types to ensure forward-compatibility in format v1.
   */
  public final hydra.util.Opt<hydra.ext.org.apache.parquet.format.LogicalType> logicalType;
  
  public SchemaElement (hydra.util.Opt<hydra.ext.org.apache.parquet.format.Type> type, hydra.util.Opt<Integer> typeLength, hydra.util.Opt<hydra.ext.org.apache.parquet.format.FieldRepetitionType> repetitionType, String name, hydra.util.Opt<Integer> numChildren, hydra.util.Opt<Integer> fieldId, hydra.util.Opt<hydra.ext.org.apache.parquet.format.LogicalType> logicalType) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((typeLength));
    java.util.Objects.requireNonNull((repetitionType));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((numChildren));
    java.util.Objects.requireNonNull((fieldId));
    java.util.Objects.requireNonNull((logicalType));
    this.type = type;
    this.typeLength = typeLength;
    this.repetitionType = repetitionType;
    this.name = name;
    this.numChildren = numChildren;
    this.fieldId = fieldId;
    this.logicalType = logicalType;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SchemaElement)) {
      return false;
    }
    SchemaElement o = (SchemaElement) (other);
    return type.equals(o.type) && typeLength.equals(o.typeLength) && repetitionType.equals(o.repetitionType) && name.equals(o.name) && numChildren.equals(o.numChildren) && fieldId.equals(o.fieldId) && logicalType.equals(o.logicalType);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * typeLength.hashCode() + 5 * repetitionType.hashCode() + 7 * name.hashCode() + 11 * numChildren.hashCode() + 13 * fieldId.hashCode() + 17 * logicalType.hashCode();
  }
  
  public SchemaElement withType(hydra.util.Opt<hydra.ext.org.apache.parquet.format.Type> type) {
    java.util.Objects.requireNonNull((type));
    return new SchemaElement(type, typeLength, repetitionType, name, numChildren, fieldId, logicalType);
  }
  
  public SchemaElement withTypeLength(hydra.util.Opt<Integer> typeLength) {
    java.util.Objects.requireNonNull((typeLength));
    return new SchemaElement(type, typeLength, repetitionType, name, numChildren, fieldId, logicalType);
  }
  
  public SchemaElement withRepetitionType(hydra.util.Opt<hydra.ext.org.apache.parquet.format.FieldRepetitionType> repetitionType) {
    java.util.Objects.requireNonNull((repetitionType));
    return new SchemaElement(type, typeLength, repetitionType, name, numChildren, fieldId, logicalType);
  }
  
  public SchemaElement withName(String name) {
    java.util.Objects.requireNonNull((name));
    return new SchemaElement(type, typeLength, repetitionType, name, numChildren, fieldId, logicalType);
  }
  
  public SchemaElement withNumChildren(hydra.util.Opt<Integer> numChildren) {
    java.util.Objects.requireNonNull((numChildren));
    return new SchemaElement(type, typeLength, repetitionType, name, numChildren, fieldId, logicalType);
  }
  
  public SchemaElement withFieldId(hydra.util.Opt<Integer> fieldId) {
    java.util.Objects.requireNonNull((fieldId));
    return new SchemaElement(type, typeLength, repetitionType, name, numChildren, fieldId, logicalType);
  }
  
  public SchemaElement withLogicalType(hydra.util.Opt<hydra.ext.org.apache.parquet.format.LogicalType> logicalType) {
    java.util.Objects.requireNonNull((logicalType));
    return new SchemaElement(type, typeLength, repetitionType, name, numChildren, fieldId, logicalType);
  }
}