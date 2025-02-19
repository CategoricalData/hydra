// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.azure.dtld;

import java.io.Serializable;

/**
 * An Enum describes a data type with a set of named labels that map to values. The values in an Enum can be either integers or strings, but the labels are always strings.
 */
public class Schema_Enum implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.com.microsoft.azure.dtld.Schema_Enum");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_ENUM_VALUES = new hydra.core.Name("enumValues");
  
  public static final hydra.core.Name FIELD_NAME_VALUE_SCHEMA = new hydra.core.Name("valueSchema");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_COMMENT = new hydra.core.Name("comment");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_DISPLAY_NAME = new hydra.core.Name("displayName");
  
  /**
   * Enum
   */
  public final hydra.ext.com.microsoft.azure.dtld.Iri type;
  
  /**
   * A set of enum value and label mappings
   */
  public final java.util.List<hydra.ext.com.microsoft.azure.dtld.EnumValue> enumValues;
  
  /**
   * The data type for the enum values. All enum values must be of the same type.
   */
  public final hydra.ext.com.microsoft.azure.dtld.IntegerOrString valueSchema;
  
  /**
   * The ID of the enum. If no @id is provided, the digital twin interface processor will assign one.
   */
  public final hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id;
  
  /**
   * A comment for model authors
   */
  public final hydra.util.Opt<String> comment;
  
  /**
   * A localizable description for display
   */
  public final hydra.util.Opt<String> description;
  
  /**
   * A localizable name for display
   */
  public final hydra.util.Opt<String> displayName;
  
  public Schema_Enum (hydra.ext.com.microsoft.azure.dtld.Iri type, java.util.List<hydra.ext.com.microsoft.azure.dtld.EnumValue> enumValues, hydra.ext.com.microsoft.azure.dtld.IntegerOrString valueSchema, hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id, hydra.util.Opt<String> comment, hydra.util.Opt<String> description, hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((enumValues));
    java.util.Objects.requireNonNull((valueSchema));
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((comment));
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((displayName));
    this.type = type;
    this.enumValues = enumValues;
    this.valueSchema = valueSchema;
    this.id = id;
    this.comment = comment;
    this.description = description;
    this.displayName = displayName;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Schema_Enum)) {
      return false;
    }
    Schema_Enum o = (Schema_Enum) (other);
    return type.equals(o.type) && enumValues.equals(o.enumValues) && valueSchema.equals(o.valueSchema) && id.equals(o.id) && comment.equals(o.comment) && description.equals(o.description) && displayName.equals(o.displayName);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * enumValues.hashCode() + 5 * valueSchema.hashCode() + 7 * id.hashCode() + 11 * comment.hashCode() + 13 * description.hashCode() + 17 * displayName.hashCode();
  }
  
  public Schema_Enum withType(hydra.ext.com.microsoft.azure.dtld.Iri type) {
    java.util.Objects.requireNonNull((type));
    return new Schema_Enum(type, enumValues, valueSchema, id, comment, description, displayName);
  }
  
  public Schema_Enum withEnumValues(java.util.List<hydra.ext.com.microsoft.azure.dtld.EnumValue> enumValues) {
    java.util.Objects.requireNonNull((enumValues));
    return new Schema_Enum(type, enumValues, valueSchema, id, comment, description, displayName);
  }
  
  public Schema_Enum withValueSchema(hydra.ext.com.microsoft.azure.dtld.IntegerOrString valueSchema) {
    java.util.Objects.requireNonNull((valueSchema));
    return new Schema_Enum(type, enumValues, valueSchema, id, comment, description, displayName);
  }
  
  public Schema_Enum withId(hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id) {
    java.util.Objects.requireNonNull((id));
    return new Schema_Enum(type, enumValues, valueSchema, id, comment, description, displayName);
  }
  
  public Schema_Enum withComment(hydra.util.Opt<String> comment) {
    java.util.Objects.requireNonNull((comment));
    return new Schema_Enum(type, enumValues, valueSchema, id, comment, description, displayName);
  }
  
  public Schema_Enum withDescription(hydra.util.Opt<String> description) {
    java.util.Objects.requireNonNull((description));
    return new Schema_Enum(type, enumValues, valueSchema, id, comment, description, displayName);
  }
  
  public Schema_Enum withDisplayName(hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((displayName));
    return new Schema_Enum(type, enumValues, valueSchema, id, comment, description, displayName);
  }
}