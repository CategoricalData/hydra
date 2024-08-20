// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.azure.dtld;

import java.io.Serializable;

/**
 * A Field describes a field in an Object.
 */
public class Field implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/com/microsoft/azure/dtld.Field");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_SCHEMA = new hydra.core.Name("schema");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_COMMENT = new hydra.core.Name("comment");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_DISPLAY_NAME = new hydra.core.Name("displayName");
  
  /**
   * The 'programming' name of the field. The name may only contain the characters a-z, A-Z, 0-9, and underscore, and must match this regular expression ^[a-zA-Z](?:[a-zA-Z0-9_]*[a-zA-Z0-9])?$.
   */
  public final String name;
  
  /**
   * The data type of the field
   */
  public final hydra.ext.com.microsoft.azure.dtld.Schema schema;
  
  /**
   * The ID of the field. If no @id is provided, the digital twin interface processor will assign one.
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
  
  public Field (String name, hydra.ext.com.microsoft.azure.dtld.Schema schema, hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id, hydra.util.Opt<String> comment, hydra.util.Opt<String> description, hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((schema));
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((comment));
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((displayName));
    this.name = name;
    this.schema = schema;
    this.id = id;
    this.comment = comment;
    this.description = description;
    this.displayName = displayName;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Field)) {
      return false;
    }
    Field o = (Field) (other);
    return name.equals(o.name) && schema.equals(o.schema) && id.equals(o.id) && comment.equals(o.comment) && description.equals(o.description) && displayName.equals(o.displayName);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * schema.hashCode() + 5 * id.hashCode() + 7 * comment.hashCode() + 11 * description.hashCode() + 13 * displayName.hashCode();
  }
  
  public Field withName(String name) {
    java.util.Objects.requireNonNull((name));
    return new Field(name, schema, id, comment, description, displayName);
  }
  
  public Field withSchema(hydra.ext.com.microsoft.azure.dtld.Schema schema) {
    java.util.Objects.requireNonNull((schema));
    return new Field(name, schema, id, comment, description, displayName);
  }
  
  public Field withId(hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id) {
    java.util.Objects.requireNonNull((id));
    return new Field(name, schema, id, comment, description, displayName);
  }
  
  public Field withComment(hydra.util.Opt<String> comment) {
    java.util.Objects.requireNonNull((comment));
    return new Field(name, schema, id, comment, description, displayName);
  }
  
  public Field withDescription(hydra.util.Opt<String> description) {
    java.util.Objects.requireNonNull((description));
    return new Field(name, schema, id, comment, description, displayName);
  }
  
  public Field withDisplayName(hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((displayName));
    return new Field(name, schema, id, comment, description, displayName);
  }
}