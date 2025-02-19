// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.azure.dtld;

import java.io.Serializable;

/**
 * A MapKey describes the key in a Map. The schema of a MapKey must be string.
 */
public class MapKey implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.com.microsoft.azure.dtld.MapKey");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_SCHEMA = new hydra.core.Name("schema");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_COMMENT = new hydra.core.Name("comment");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_DISPLAY_NAME = new hydra.core.Name("displayName");
  
  /**
   * The 'programming' name of the map's key. The name may only contain the characters a-z, A-Z, 0-9, and underscore, and must match this regular expression ^[a-zA-Z](?:[a-zA-Z0-9_]*[a-zA-Z0-9])?$.
   */
  public final String name;
  
  /**
   * The data type of the map's key
   */
  public final hydra.ext.com.microsoft.azure.dtld.Schema schema;
  
  /**
   * The ID of the map key. If no @id is provided, the digital twin interface processor will assign one.
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
  
  public MapKey (String name, hydra.ext.com.microsoft.azure.dtld.Schema schema, hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id, hydra.util.Opt<String> comment, hydra.util.Opt<String> description, hydra.util.Opt<String> displayName) {
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
    if (!(other instanceof MapKey)) {
      return false;
    }
    MapKey o = (MapKey) (other);
    return name.equals(o.name) && schema.equals(o.schema) && id.equals(o.id) && comment.equals(o.comment) && description.equals(o.description) && displayName.equals(o.displayName);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * schema.hashCode() + 5 * id.hashCode() + 7 * comment.hashCode() + 11 * description.hashCode() + 13 * displayName.hashCode();
  }
  
  public MapKey withName(String name) {
    java.util.Objects.requireNonNull((name));
    return new MapKey(name, schema, id, comment, description, displayName);
  }
  
  public MapKey withSchema(hydra.ext.com.microsoft.azure.dtld.Schema schema) {
    java.util.Objects.requireNonNull((schema));
    return new MapKey(name, schema, id, comment, description, displayName);
  }
  
  public MapKey withId(hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id) {
    java.util.Objects.requireNonNull((id));
    return new MapKey(name, schema, id, comment, description, displayName);
  }
  
  public MapKey withComment(hydra.util.Opt<String> comment) {
    java.util.Objects.requireNonNull((comment));
    return new MapKey(name, schema, id, comment, description, displayName);
  }
  
  public MapKey withDescription(hydra.util.Opt<String> description) {
    java.util.Objects.requireNonNull((description));
    return new MapKey(name, schema, id, comment, description, displayName);
  }
  
  public MapKey withDisplayName(hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((displayName));
    return new MapKey(name, schema, id, comment, description, displayName);
  }
}