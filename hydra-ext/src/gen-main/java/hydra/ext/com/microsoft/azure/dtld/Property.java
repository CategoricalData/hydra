// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.azure.dtld;

import java.io.Serializable;

/**
 * A Property describes the read-only and read/write state of any digital twin. For example, a device serial number may be a read-only property, the desired temperature on a thermostat may be a read-write property; and the name of a room may be a read-write property.
 */
public class Property implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/com/microsoft/azure/dtld.Property");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_SCHEMA = new hydra.core.Name("schema");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_COMMENT = new hydra.core.Name("comment");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_DISPLAY_NAME = new hydra.core.Name("displayName");
  
  public static final hydra.core.Name FIELD_NAME_UNIT = new hydra.core.Name("unit");
  
  public static final hydra.core.Name FIELD_NAME_WRITABLE = new hydra.core.Name("writable");
  
  /**
   * This must at least be 'Property'. It can also include a semantic type.
   */
  public final hydra.ext.com.microsoft.azure.dtld.Iri type;
  
  /**
   * The 'programming' name of the property. The name may only contain the characters a-z, A-Z, 0-9, and underscore, and must match this regular expression ^[a-zA-Z](?:[a-zA-Z0-9_]*[a-zA-Z0-9])?$..
   */
  public final String name;
  
  /**
   * The data type of the Property
   */
  public final hydra.ext.com.microsoft.azure.dtld.Schema schema;
  
  /**
   * The ID of the property. If no @id is provided, the digital twin interface processor will assign one.
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
  
  /**
   * The unit type of the property. A semantic type is required for the unit property to be available.
   */
  public final hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Unit> unit;
  
  /**
   * A boolean value that indicates whether the property is writable by an external source, such as an application, or not. The default value is false (read-only).
   */
  public final hydra.util.Opt<Boolean> writable;
  
  public Property (hydra.ext.com.microsoft.azure.dtld.Iri type, String name, hydra.ext.com.microsoft.azure.dtld.Schema schema, hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id, hydra.util.Opt<String> comment, hydra.util.Opt<String> description, hydra.util.Opt<String> displayName, hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Unit> unit, hydra.util.Opt<Boolean> writable) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((schema));
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((comment));
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((displayName));
    java.util.Objects.requireNonNull((unit));
    java.util.Objects.requireNonNull((writable));
    this.type = type;
    this.name = name;
    this.schema = schema;
    this.id = id;
    this.comment = comment;
    this.description = description;
    this.displayName = displayName;
    this.unit = unit;
    this.writable = writable;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Property)) {
      return false;
    }
    Property o = (Property) (other);
    return type.equals(o.type) && name.equals(o.name) && schema.equals(o.schema) && id.equals(o.id) && comment.equals(o.comment) && description.equals(o.description) && displayName.equals(o.displayName) && unit.equals(o.unit) && writable.equals(o.writable);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * name.hashCode() + 5 * schema.hashCode() + 7 * id.hashCode() + 11 * comment.hashCode() + 13 * description.hashCode() + 17 * displayName.hashCode() + 19 * unit.hashCode() + 23 * writable.hashCode();
  }
  
  public Property withType(hydra.ext.com.microsoft.azure.dtld.Iri type) {
    java.util.Objects.requireNonNull((type));
    return new Property(type, name, schema, id, comment, description, displayName, unit, writable);
  }
  
  public Property withName(String name) {
    java.util.Objects.requireNonNull((name));
    return new Property(type, name, schema, id, comment, description, displayName, unit, writable);
  }
  
  public Property withSchema(hydra.ext.com.microsoft.azure.dtld.Schema schema) {
    java.util.Objects.requireNonNull((schema));
    return new Property(type, name, schema, id, comment, description, displayName, unit, writable);
  }
  
  public Property withId(hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id) {
    java.util.Objects.requireNonNull((id));
    return new Property(type, name, schema, id, comment, description, displayName, unit, writable);
  }
  
  public Property withComment(hydra.util.Opt<String> comment) {
    java.util.Objects.requireNonNull((comment));
    return new Property(type, name, schema, id, comment, description, displayName, unit, writable);
  }
  
  public Property withDescription(hydra.util.Opt<String> description) {
    java.util.Objects.requireNonNull((description));
    return new Property(type, name, schema, id, comment, description, displayName, unit, writable);
  }
  
  public Property withDisplayName(hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((displayName));
    return new Property(type, name, schema, id, comment, description, displayName, unit, writable);
  }
  
  public Property withUnit(hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Unit> unit) {
    java.util.Objects.requireNonNull((unit));
    return new Property(type, name, schema, id, comment, description, displayName, unit, writable);
  }
  
  public Property withWritable(hydra.util.Opt<Boolean> writable) {
    java.util.Objects.requireNonNull((writable));
    return new Property(type, name, schema, id, comment, description, displayName, unit, writable);
  }
}