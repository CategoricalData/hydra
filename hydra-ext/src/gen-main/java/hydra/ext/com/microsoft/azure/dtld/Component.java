// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.azure.dtld;

import java.io.Serializable;

/**
 * Components enable interfaces to be composed of other interfaces. Components are different from relationships because they describe contents that are directly part of the interface. (A relationship describes a link between two interfaces.)
 */
public class Component implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/com/microsoft/azure/dtld.Component");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_SCHEMA = new hydra.core.Name("schema");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_COMMENT = new hydra.core.Name("comment");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_DISPLAY_NAME = new hydra.core.Name("displayName");
  
  /**
   * This must be 'Component'
   */
  public final hydra.ext.com.microsoft.azure.dtld.Iri type;
  
  /**
   * The 'programming' name of the component. The name may only contain the characters a-z, A-Z, 0-9, and underscore, and must match this regular expression ^[a-zA-Z](?:[a-zA-Z0-9_]*[a-zA-Z0-9])?$.
   */
  public final String name;
  
  /**
   * The data type of the component
   */
  public final hydra.ext.com.microsoft.azure.dtld.Interface schema;
  
  /**
   * The ID of the component. If no @id is provided, the digital twin interface processor will assign one.
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
  
  public Component (hydra.ext.com.microsoft.azure.dtld.Iri type, String name, hydra.ext.com.microsoft.azure.dtld.Interface schema, hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id, hydra.util.Opt<String> comment, hydra.util.Opt<String> description, hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((schema));
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((comment));
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((displayName));
    this.type = type;
    this.name = name;
    this.schema = schema;
    this.id = id;
    this.comment = comment;
    this.description = description;
    this.displayName = displayName;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Component)) {
      return false;
    }
    Component o = (Component) (other);
    return type.equals(o.type) && name.equals(o.name) && schema.equals(o.schema) && id.equals(o.id) && comment.equals(o.comment) && description.equals(o.description) && displayName.equals(o.displayName);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * name.hashCode() + 5 * schema.hashCode() + 7 * id.hashCode() + 11 * comment.hashCode() + 13 * description.hashCode() + 17 * displayName.hashCode();
  }
  
  public Component withType(hydra.ext.com.microsoft.azure.dtld.Iri type) {
    java.util.Objects.requireNonNull((type));
    return new Component(type, name, schema, id, comment, description, displayName);
  }
  
  public Component withName(String name) {
    java.util.Objects.requireNonNull((name));
    return new Component(type, name, schema, id, comment, description, displayName);
  }
  
  public Component withSchema(hydra.ext.com.microsoft.azure.dtld.Interface schema) {
    java.util.Objects.requireNonNull((schema));
    return new Component(type, name, schema, id, comment, description, displayName);
  }
  
  public Component withId(hydra.util.Opt<hydra.ext.com.microsoft.azure.dtld.Dtmi> id) {
    java.util.Objects.requireNonNull((id));
    return new Component(type, name, schema, id, comment, description, displayName);
  }
  
  public Component withComment(hydra.util.Opt<String> comment) {
    java.util.Objects.requireNonNull((comment));
    return new Component(type, name, schema, id, comment, description, displayName);
  }
  
  public Component withDescription(hydra.util.Opt<String> description) {
    java.util.Objects.requireNonNull((description));
    return new Component(type, name, schema, id, comment, description, displayName);
  }
  
  public Component withDisplayName(hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((displayName));
    return new Component(type, name, schema, id, comment, description, displayName);
  }
}