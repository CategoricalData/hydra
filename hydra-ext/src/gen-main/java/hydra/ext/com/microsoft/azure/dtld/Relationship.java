// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.azure.dtld;

import java.io.Serializable;

/**
 * A Relationship describes a link to another digital twin and enables graphs of digital twins to be created. Relationships are different from Components because they describe a link to a separate digital twin.
 */
public class Relationship implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/com/microsoft/azure/dtld.Relationship");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_COMMENT = new hydra.core.Name("comment");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_DISPLAY_NAME = new hydra.core.Name("displayName");
  
  public static final hydra.core.Name FIELD_NAME_MAX_MULTIPLICITY = new hydra.core.Name("maxMultiplicity");
  
  public static final hydra.core.Name FIELD_NAME_MIN_MULTIPLICITY = new hydra.core.Name("minMultiplicity");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTIES = new hydra.core.Name("properties");
  
  public static final hydra.core.Name FIELD_NAME_TARGET = new hydra.core.Name("target");
  
  public static final hydra.core.Name FIELD_NAME_WRITABLE = new hydra.core.Name("writable");
  
  /**
   * This must be 'Relationship'
   */
  public final hydra.ext.com.microsoft.azure.dtld.Iri type;
  
  /**
   * The 'programming' name of the relationship. The name may only contain the characters a-z, A-Z, 0-9, and underscore, and must match this regular expression ^[a-zA-Z](?:[a-zA-Z0-9_]*[a-zA-Z0-9])?$.
   */
  public final String name;
  
  /**
   * The ID of the relationship description. If no @id is provided, the digital twin interface processor will assign one.
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
   * The maximum multiplicity for the target of the relationship. The default value is infinite (there may be an unlimited number of relationship instances for this relationship).
   */
  public final hydra.util.Opt<Integer> maxMultiplicity;
  
  /**
   * The minimum multiplicity for the target of the relationship. The default value is 0 (this relationship is permitted to have no instances). In DTDL v2, minMultiplicity must always be 0.
   */
  public final hydra.util.Opt<Integer> minMultiplicity;
  
  /**
   * A set of Properties that define relationship-specific state
   */
  public final hydra.util.Opt<java.util.Set<hydra.ext.com.microsoft.azure.dtld.Property>> properties;
  
  /**
   * An interface ID. The default value (when target is not specified) is that the target may be any interface.
   */
  public final hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Interface> target;
  
  /**
   * A boolean value that indicates whether the relationship is writable by an external source, such as an application, or not. The default value is false (read-only).
   */
  public final hydra.util.Opt<Boolean> writable;
  
  public Relationship (hydra.ext.com.microsoft.azure.dtld.Iri type, String name, hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id, hydra.util.Opt<String> comment, hydra.util.Opt<String> description, hydra.util.Opt<String> displayName, hydra.util.Opt<Integer> maxMultiplicity, hydra.util.Opt<Integer> minMultiplicity, hydra.util.Opt<java.util.Set<hydra.ext.com.microsoft.azure.dtld.Property>> properties, hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Interface> target, hydra.util.Opt<Boolean> writable) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((comment));
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((displayName));
    java.util.Objects.requireNonNull((maxMultiplicity));
    java.util.Objects.requireNonNull((minMultiplicity));
    java.util.Objects.requireNonNull((properties));
    java.util.Objects.requireNonNull((target));
    java.util.Objects.requireNonNull((writable));
    this.type = type;
    this.name = name;
    this.id = id;
    this.comment = comment;
    this.description = description;
    this.displayName = displayName;
    this.maxMultiplicity = maxMultiplicity;
    this.minMultiplicity = minMultiplicity;
    this.properties = properties;
    this.target = target;
    this.writable = writable;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Relationship)) {
      return false;
    }
    Relationship o = (Relationship) (other);
    return type.equals(o.type) && name.equals(o.name) && id.equals(o.id) && comment.equals(o.comment) && description.equals(o.description) && displayName.equals(o.displayName) && maxMultiplicity.equals(o.maxMultiplicity) && minMultiplicity.equals(o.minMultiplicity) && properties.equals(o.properties) && target.equals(o.target) && writable.equals(o.writable);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * name.hashCode() + 5 * id.hashCode() + 7 * comment.hashCode() + 11 * description.hashCode() + 13 * displayName.hashCode() + 17 * maxMultiplicity.hashCode() + 19 * minMultiplicity.hashCode() + 23 * properties.hashCode() + 29 * target.hashCode() + 31 * writable.hashCode();
  }
  
  public Relationship withType(hydra.ext.com.microsoft.azure.dtld.Iri type) {
    java.util.Objects.requireNonNull((type));
    return new Relationship(type, name, id, comment, description, displayName, maxMultiplicity, minMultiplicity, properties, target, writable);
  }
  
  public Relationship withName(String name) {
    java.util.Objects.requireNonNull((name));
    return new Relationship(type, name, id, comment, description, displayName, maxMultiplicity, minMultiplicity, properties, target, writable);
  }
  
  public Relationship withId(hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id) {
    java.util.Objects.requireNonNull((id));
    return new Relationship(type, name, id, comment, description, displayName, maxMultiplicity, minMultiplicity, properties, target, writable);
  }
  
  public Relationship withComment(hydra.util.Opt<String> comment) {
    java.util.Objects.requireNonNull((comment));
    return new Relationship(type, name, id, comment, description, displayName, maxMultiplicity, minMultiplicity, properties, target, writable);
  }
  
  public Relationship withDescription(hydra.util.Opt<String> description) {
    java.util.Objects.requireNonNull((description));
    return new Relationship(type, name, id, comment, description, displayName, maxMultiplicity, minMultiplicity, properties, target, writable);
  }
  
  public Relationship withDisplayName(hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((displayName));
    return new Relationship(type, name, id, comment, description, displayName, maxMultiplicity, minMultiplicity, properties, target, writable);
  }
  
  public Relationship withMaxMultiplicity(hydra.util.Opt<Integer> maxMultiplicity) {
    java.util.Objects.requireNonNull((maxMultiplicity));
    return new Relationship(type, name, id, comment, description, displayName, maxMultiplicity, minMultiplicity, properties, target, writable);
  }
  
  public Relationship withMinMultiplicity(hydra.util.Opt<Integer> minMultiplicity) {
    java.util.Objects.requireNonNull((minMultiplicity));
    return new Relationship(type, name, id, comment, description, displayName, maxMultiplicity, minMultiplicity, properties, target, writable);
  }
  
  public Relationship withProperties(hydra.util.Opt<java.util.Set<hydra.ext.com.microsoft.azure.dtld.Property>> properties) {
    java.util.Objects.requireNonNull((properties));
    return new Relationship(type, name, id, comment, description, displayName, maxMultiplicity, minMultiplicity, properties, target, writable);
  }
  
  public Relationship withTarget(hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Interface> target) {
    java.util.Objects.requireNonNull((target));
    return new Relationship(type, name, id, comment, description, displayName, maxMultiplicity, minMultiplicity, properties, target, writable);
  }
  
  public Relationship withWritable(hydra.util.Opt<Boolean> writable) {
    java.util.Objects.requireNonNull((writable));
    return new Relationship(type, name, id, comment, description, displayName, maxMultiplicity, minMultiplicity, properties, target, writable);
  }
}