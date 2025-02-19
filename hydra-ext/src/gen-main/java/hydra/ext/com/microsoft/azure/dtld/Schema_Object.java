// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.azure.dtld;

import java.io.Serializable;

/**
 * An Object describes a data type made up of named fields (like a struct in C). The fields in an Object map can be primitive or complex schemas.
 */
public class Schema_Object implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.com.microsoft.azure.dtld.Schema_Object");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_FIELDS = new hydra.core.Name("fields");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_COMMENT = new hydra.core.Name("comment");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_DISPLAY_NAME = new hydra.core.Name("displayName");
  
  /**
   * Object
   */
  public final hydra.ext.com.microsoft.azure.dtld.Iri type;
  
  /**
   * A set of field descriptions, one for each field in the Object
   */
  public final java.util.Set<hydra.ext.com.microsoft.azure.dtld.Field> fields;
  
  /**
   * The ID of the object. If no @id is provided, the digital twin interface processor will assign one.
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
  
  public Schema_Object (hydra.ext.com.microsoft.azure.dtld.Iri type, java.util.Set<hydra.ext.com.microsoft.azure.dtld.Field> fields, hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id, hydra.util.Opt<String> comment, hydra.util.Opt<String> description, hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((fields));
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((comment));
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((displayName));
    this.type = type;
    this.fields = fields;
    this.id = id;
    this.comment = comment;
    this.description = description;
    this.displayName = displayName;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Schema_Object)) {
      return false;
    }
    Schema_Object o = (Schema_Object) (other);
    return type.equals(o.type) && fields.equals(o.fields) && id.equals(o.id) && comment.equals(o.comment) && description.equals(o.description) && displayName.equals(o.displayName);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * fields.hashCode() + 5 * id.hashCode() + 7 * comment.hashCode() + 11 * description.hashCode() + 13 * displayName.hashCode();
  }
  
  public Schema_Object withType(hydra.ext.com.microsoft.azure.dtld.Iri type) {
    java.util.Objects.requireNonNull((type));
    return new Schema_Object(type, fields, id, comment, description, displayName);
  }
  
  public Schema_Object withFields(java.util.Set<hydra.ext.com.microsoft.azure.dtld.Field> fields) {
    java.util.Objects.requireNonNull((fields));
    return new Schema_Object(type, fields, id, comment, description, displayName);
  }
  
  public Schema_Object withId(hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id) {
    java.util.Objects.requireNonNull((id));
    return new Schema_Object(type, fields, id, comment, description, displayName);
  }
  
  public Schema_Object withComment(hydra.util.Opt<String> comment) {
    java.util.Objects.requireNonNull((comment));
    return new Schema_Object(type, fields, id, comment, description, displayName);
  }
  
  public Schema_Object withDescription(hydra.util.Opt<String> description) {
    java.util.Objects.requireNonNull((description));
    return new Schema_Object(type, fields, id, comment, description, displayName);
  }
  
  public Schema_Object withDisplayName(hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((displayName));
    return new Schema_Object(type, fields, id, comment, description, displayName);
  }
}