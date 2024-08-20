// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.azure.dtld;

import java.io.Serializable;

/**
 * Telemetry describes the data emitted by any digital twin, whether the data is a regular stream of sensor readings or a computed stream of data, such as occupancy, or an occasional error or information message.
 */
public class Telemetry implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/com/microsoft/azure/dtld.Telemetry");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_SCHEMA = new hydra.core.Name("schema");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_COMMENT = new hydra.core.Name("comment");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_DISPLAY_NAME = new hydra.core.Name("displayName");
  
  public static final hydra.core.Name FIELD_NAME_UNIT = new hydra.core.Name("unit");
  
  /**
   * This must be at least 'Telemetry'. It can also include a semantic type
   */
  public final hydra.ext.com.microsoft.azure.dtld.Iri type;
  
  /**
   * The 'programming' name of the telemetry. The name may only contain the characters a-z, A-Z, 0-9, and underscore, and must match this regular expression ^[a-zA-Z](?:[a-zA-Z0-9_]*[a-zA-Z0-9])?$..
   */
  public final String name;
  
  /**
   * The data type of the Telemetry
   */
  public final hydra.ext.com.microsoft.azure.dtld.Schema schema;
  
  /**
   * The ID of the telemetry. If no @id is provided, the digital twin interface processor will assign one.
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
   * The unit type of the Telemetry. A semantic type is required for the unit property to be available.
   */
  public final hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Unit> unit;
  
  public Telemetry (hydra.ext.com.microsoft.azure.dtld.Iri type, String name, hydra.ext.com.microsoft.azure.dtld.Schema schema, hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id, hydra.util.Opt<String> comment, hydra.util.Opt<String> description, hydra.util.Opt<String> displayName, hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Unit> unit) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((schema));
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((comment));
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((displayName));
    java.util.Objects.requireNonNull((unit));
    this.type = type;
    this.name = name;
    this.schema = schema;
    this.id = id;
    this.comment = comment;
    this.description = description;
    this.displayName = displayName;
    this.unit = unit;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Telemetry)) {
      return false;
    }
    Telemetry o = (Telemetry) (other);
    return type.equals(o.type) && name.equals(o.name) && schema.equals(o.schema) && id.equals(o.id) && comment.equals(o.comment) && description.equals(o.description) && displayName.equals(o.displayName) && unit.equals(o.unit);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * name.hashCode() + 5 * schema.hashCode() + 7 * id.hashCode() + 11 * comment.hashCode() + 13 * description.hashCode() + 17 * displayName.hashCode() + 19 * unit.hashCode();
  }
  
  public Telemetry withType(hydra.ext.com.microsoft.azure.dtld.Iri type) {
    java.util.Objects.requireNonNull((type));
    return new Telemetry(type, name, schema, id, comment, description, displayName, unit);
  }
  
  public Telemetry withName(String name) {
    java.util.Objects.requireNonNull((name));
    return new Telemetry(type, name, schema, id, comment, description, displayName, unit);
  }
  
  public Telemetry withSchema(hydra.ext.com.microsoft.azure.dtld.Schema schema) {
    java.util.Objects.requireNonNull((schema));
    return new Telemetry(type, name, schema, id, comment, description, displayName, unit);
  }
  
  public Telemetry withId(hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id) {
    java.util.Objects.requireNonNull((id));
    return new Telemetry(type, name, schema, id, comment, description, displayName, unit);
  }
  
  public Telemetry withComment(hydra.util.Opt<String> comment) {
    java.util.Objects.requireNonNull((comment));
    return new Telemetry(type, name, schema, id, comment, description, displayName, unit);
  }
  
  public Telemetry withDescription(hydra.util.Opt<String> description) {
    java.util.Objects.requireNonNull((description));
    return new Telemetry(type, name, schema, id, comment, description, displayName, unit);
  }
  
  public Telemetry withDisplayName(hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((displayName));
    return new Telemetry(type, name, schema, id, comment, description, displayName, unit);
  }
  
  public Telemetry withUnit(hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Unit> unit) {
    java.util.Objects.requireNonNull((unit));
    return new Telemetry(type, name, schema, id, comment, description, displayName, unit);
  }
}