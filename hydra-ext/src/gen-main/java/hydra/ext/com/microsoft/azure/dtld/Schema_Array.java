// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.azure.dtld;

import java.io.Serializable;

/**
 * An Array describes an indexable data type where each element is of the same schema. An Array elements' schema can itself be a primitive or complex schema.
 */
public class Schema_Array implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.com.microsoft.azure.dtld.Schema_Array");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_ELEMENT_SCHEMA = new hydra.core.Name("elementSchema");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_COMMENT = new hydra.core.Name("comment");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_DISPLAY_NAME = new hydra.core.Name("displayName");
  
  /**
   * This must be 'Array'
   */
  public final hydra.ext.com.microsoft.azure.dtld.Iri type;
  
  /**
   * The data type of the array elements
   */
  public final hydra.ext.com.microsoft.azure.dtld.Schema elementSchema;
  
  /**
   * The ID of the array. If no @id is provided, the digital twin interface processor will assign one.
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
  
  public Schema_Array (hydra.ext.com.microsoft.azure.dtld.Iri type, hydra.ext.com.microsoft.azure.dtld.Schema elementSchema, hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id, hydra.util.Opt<String> comment, hydra.util.Opt<String> description, hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((elementSchema));
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((comment));
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((displayName));
    this.type = type;
    this.elementSchema = elementSchema;
    this.id = id;
    this.comment = comment;
    this.description = description;
    this.displayName = displayName;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Schema_Array)) {
      return false;
    }
    Schema_Array o = (Schema_Array) (other);
    return type.equals(o.type) && elementSchema.equals(o.elementSchema) && id.equals(o.id) && comment.equals(o.comment) && description.equals(o.description) && displayName.equals(o.displayName);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * elementSchema.hashCode() + 5 * id.hashCode() + 7 * comment.hashCode() + 11 * description.hashCode() + 13 * displayName.hashCode();
  }
  
  public Schema_Array withType(hydra.ext.com.microsoft.azure.dtld.Iri type) {
    java.util.Objects.requireNonNull((type));
    return new Schema_Array(type, elementSchema, id, comment, description, displayName);
  }
  
  public Schema_Array withElementSchema(hydra.ext.com.microsoft.azure.dtld.Schema elementSchema) {
    java.util.Objects.requireNonNull((elementSchema));
    return new Schema_Array(type, elementSchema, id, comment, description, displayName);
  }
  
  public Schema_Array withId(hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id) {
    java.util.Objects.requireNonNull((id));
    return new Schema_Array(type, elementSchema, id, comment, description, displayName);
  }
  
  public Schema_Array withComment(hydra.util.Opt<String> comment) {
    java.util.Objects.requireNonNull((comment));
    return new Schema_Array(type, elementSchema, id, comment, description, displayName);
  }
  
  public Schema_Array withDescription(hydra.util.Opt<String> description) {
    java.util.Objects.requireNonNull((description));
    return new Schema_Array(type, elementSchema, id, comment, description, displayName);
  }
  
  public Schema_Array withDisplayName(hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((displayName));
    return new Schema_Array(type, elementSchema, id, comment, description, displayName);
  }
}