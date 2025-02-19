// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.azure.dtld;

import java.io.Serializable;

/**
 * Within an interface definition, complex schemas may be defined for reusability across Telemetry, Properties, and Commands. This is designed to promote readability and improved maintenance because schemas that are reused can be defined once (per interface). Interface schemas are defined in the schemas property of an interface.
 */
public class Schema_Interface implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.com.microsoft.azure.dtld.Schema_Interface");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_COMMENT = new hydra.core.Name("comment");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_DISPLAY_NAME = new hydra.core.Name("displayName");
  
  /**
   * The globally unique identifier for the schema
   */
  public final hydra.ext.com.microsoft.azure.dtld.Dtmi id;
  
  /**
   * The type of complex schema. This must refer to one of the complex schema classes (Array, Enum, Map, or Object).
   */
  public final hydra.ext.com.microsoft.azure.dtld.Schema_Interface_Type type;
  
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
  
  public Schema_Interface (hydra.ext.com.microsoft.azure.dtld.Dtmi id, hydra.ext.com.microsoft.azure.dtld.Schema_Interface_Type type, hydra.util.Opt<String> comment, hydra.util.Opt<String> description, hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((comment));
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((displayName));
    this.id = id;
    this.type = type;
    this.comment = comment;
    this.description = description;
    this.displayName = displayName;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Schema_Interface)) {
      return false;
    }
    Schema_Interface o = (Schema_Interface) (other);
    return id.equals(o.id) && type.equals(o.type) && comment.equals(o.comment) && description.equals(o.description) && displayName.equals(o.displayName);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * type.hashCode() + 5 * comment.hashCode() + 7 * description.hashCode() + 11 * displayName.hashCode();
  }
  
  public Schema_Interface withId(hydra.ext.com.microsoft.azure.dtld.Dtmi id) {
    java.util.Objects.requireNonNull((id));
    return new Schema_Interface(id, type, comment, description, displayName);
  }
  
  public Schema_Interface withType(hydra.ext.com.microsoft.azure.dtld.Schema_Interface_Type type) {
    java.util.Objects.requireNonNull((type));
    return new Schema_Interface(id, type, comment, description, displayName);
  }
  
  public Schema_Interface withComment(hydra.util.Opt<String> comment) {
    java.util.Objects.requireNonNull((comment));
    return new Schema_Interface(id, type, comment, description, displayName);
  }
  
  public Schema_Interface withDescription(hydra.util.Opt<String> description) {
    java.util.Objects.requireNonNull((description));
    return new Schema_Interface(id, type, comment, description, displayName);
  }
  
  public Schema_Interface withDisplayName(hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((displayName));
    return new Schema_Interface(id, type, comment, description, displayName);
  }
}