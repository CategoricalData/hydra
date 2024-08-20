// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.azure.dtld;

import java.io.Serializable;

/**
 * A Map describes a data type of key-value pairs where the values share the same schema. The key in a Map must be a string. The values in a Map can be any schema.
 */
public class Schema_Map implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/com/microsoft/azure/dtld.Schema.Map");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_MAP_KEY = new hydra.core.Name("mapKey");
  
  public static final hydra.core.Name FIELD_NAME_MAP_VALUE = new hydra.core.Name("mapValue");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_COMMENT = new hydra.core.Name("comment");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_DISPLAY_NAME = new hydra.core.Name("displayName");
  
  /**
   * Map
   */
  public final hydra.ext.com.microsoft.azure.dtld.Iri type;
  
  /**
   * A description of the keys in the map
   */
  public final hydra.ext.com.microsoft.azure.dtld.MapKey mapKey;
  
  /**
   * A description of the values in the map
   */
  public final hydra.ext.com.microsoft.azure.dtld.MapValue mapValue;
  
  /**
   * The ID of the map. If no @id is provided, the digital twin interface processor will assign one.
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
  
  public Schema_Map (hydra.ext.com.microsoft.azure.dtld.Iri type, hydra.ext.com.microsoft.azure.dtld.MapKey mapKey, hydra.ext.com.microsoft.azure.dtld.MapValue mapValue, hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id, hydra.util.Opt<String> comment, hydra.util.Opt<String> description, hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((mapKey));
    java.util.Objects.requireNonNull((mapValue));
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((comment));
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((displayName));
    this.type = type;
    this.mapKey = mapKey;
    this.mapValue = mapValue;
    this.id = id;
    this.comment = comment;
    this.description = description;
    this.displayName = displayName;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Schema_Map)) {
      return false;
    }
    Schema_Map o = (Schema_Map) (other);
    return type.equals(o.type) && mapKey.equals(o.mapKey) && mapValue.equals(o.mapValue) && id.equals(o.id) && comment.equals(o.comment) && description.equals(o.description) && displayName.equals(o.displayName);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * mapKey.hashCode() + 5 * mapValue.hashCode() + 7 * id.hashCode() + 11 * comment.hashCode() + 13 * description.hashCode() + 17 * displayName.hashCode();
  }
  
  public Schema_Map withType(hydra.ext.com.microsoft.azure.dtld.Iri type) {
    java.util.Objects.requireNonNull((type));
    return new Schema_Map(type, mapKey, mapValue, id, comment, description, displayName);
  }
  
  public Schema_Map withMapKey(hydra.ext.com.microsoft.azure.dtld.MapKey mapKey) {
    java.util.Objects.requireNonNull((mapKey));
    return new Schema_Map(type, mapKey, mapValue, id, comment, description, displayName);
  }
  
  public Schema_Map withMapValue(hydra.ext.com.microsoft.azure.dtld.MapValue mapValue) {
    java.util.Objects.requireNonNull((mapValue));
    return new Schema_Map(type, mapKey, mapValue, id, comment, description, displayName);
  }
  
  public Schema_Map withId(hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id) {
    java.util.Objects.requireNonNull((id));
    return new Schema_Map(type, mapKey, mapValue, id, comment, description, displayName);
  }
  
  public Schema_Map withComment(hydra.util.Opt<String> comment) {
    java.util.Objects.requireNonNull((comment));
    return new Schema_Map(type, mapKey, mapValue, id, comment, description, displayName);
  }
  
  public Schema_Map withDescription(hydra.util.Opt<String> description) {
    java.util.Objects.requireNonNull((description));
    return new Schema_Map(type, mapKey, mapValue, id, comment, description, displayName);
  }
  
  public Schema_Map withDisplayName(hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((displayName));
    return new Schema_Map(type, mapKey, mapValue, id, comment, description, displayName);
  }
}