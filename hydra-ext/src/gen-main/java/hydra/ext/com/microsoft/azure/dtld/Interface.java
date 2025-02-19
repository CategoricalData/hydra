// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.azure.dtld;

import java.io.Serializable;

public class Interface implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.com.microsoft.azure.dtld.Interface");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_CONTEXT = new hydra.core.Name("context");
  
  public static final hydra.core.Name FIELD_NAME_COMMENT = new hydra.core.Name("comment");
  
  public static final hydra.core.Name FIELD_NAME_CONTENTS = new hydra.core.Name("contents");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_DISPLAY_NAME = new hydra.core.Name("displayName");
  
  public static final hydra.core.Name FIELD_NAME_EXTENDS = new hydra.core.Name("extends");
  
  public static final hydra.core.Name FIELD_NAME_SCHEMAS = new hydra.core.Name("schemas");
  
  /**
   * A digital twin model identifier for the interface
   */
  public final hydra.ext.com.microsoft.azure.dtld.Dtmi id;
  
  /**
   * This must be 'Interface'
   */
  public final hydra.ext.com.microsoft.azure.dtld.Iri type;
  
  /**
   * The context to use when processing this interface. For this version, it must be set to 'dtmi:dtdl:context;2'
   */
  public final hydra.ext.com.microsoft.azure.dtld.Iri context;
  
  /**
   * A comment for model authors
   */
  public final hydra.util.Opt<String> comment;
  
  /**
   * A set of objects that define the contents (Telemetry, Properties, Commands, Relationships, and/or Components) of this interface
   */
  public final hydra.util.Opt<java.util.Set<hydra.ext.com.microsoft.azure.dtld.Interface_Contents>> contents;
  
  /**
   * A localizable description for display
   */
  public final hydra.util.Opt<String> description;
  
  /**
   * A localizable name for display
   */
  public final hydra.util.Opt<String> displayName;
  
  /**
   * A set of DTMIs that refer to interfaces this interface inherits from. Interfaces can inherit from multiple interfaces.
   */
  public final hydra.util.Opt<java.util.Set<hydra.ext.com.microsoft.azure.dtld.Interface>> extends_;
  
  /**
   * A set of IRIs or objects that refer to the reusable schemas within this interface.
   */
  public final hydra.util.Opt<java.util.Set<hydra.ext.com.microsoft.azure.dtld.Schema_Interface>> schemas;
  
  public Interface (hydra.ext.com.microsoft.azure.dtld.Dtmi id, hydra.ext.com.microsoft.azure.dtld.Iri type, hydra.ext.com.microsoft.azure.dtld.Iri context, hydra.util.Opt<String> comment, hydra.util.Opt<java.util.Set<hydra.ext.com.microsoft.azure.dtld.Interface_Contents>> contents, hydra.util.Opt<String> description, hydra.util.Opt<String> displayName, hydra.util.Opt<java.util.Set<hydra.ext.com.microsoft.azure.dtld.Interface>> extends_, hydra.util.Opt<java.util.Set<hydra.ext.com.microsoft.azure.dtld.Schema_Interface>> schemas) {
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((context));
    java.util.Objects.requireNonNull((comment));
    java.util.Objects.requireNonNull((contents));
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((displayName));
    java.util.Objects.requireNonNull((extends_));
    java.util.Objects.requireNonNull((schemas));
    this.id = id;
    this.type = type;
    this.context = context;
    this.comment = comment;
    this.contents = contents;
    this.description = description;
    this.displayName = displayName;
    this.extends_ = extends_;
    this.schemas = schemas;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Interface)) {
      return false;
    }
    Interface o = (Interface) (other);
    return id.equals(o.id) && type.equals(o.type) && context.equals(o.context) && comment.equals(o.comment) && contents.equals(o.contents) && description.equals(o.description) && displayName.equals(o.displayName) && extends_.equals(o.extends_) && schemas.equals(o.schemas);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * type.hashCode() + 5 * context.hashCode() + 7 * comment.hashCode() + 11 * contents.hashCode() + 13 * description.hashCode() + 17 * displayName.hashCode() + 19 * extends_.hashCode() + 23 * schemas.hashCode();
  }
  
  public Interface withId(hydra.ext.com.microsoft.azure.dtld.Dtmi id) {
    java.util.Objects.requireNonNull((id));
    return new Interface(id, type, context, comment, contents, description, displayName, extends_, schemas);
  }
  
  public Interface withType(hydra.ext.com.microsoft.azure.dtld.Iri type) {
    java.util.Objects.requireNonNull((type));
    return new Interface(id, type, context, comment, contents, description, displayName, extends_, schemas);
  }
  
  public Interface withContext(hydra.ext.com.microsoft.azure.dtld.Iri context) {
    java.util.Objects.requireNonNull((context));
    return new Interface(id, type, context, comment, contents, description, displayName, extends_, schemas);
  }
  
  public Interface withComment(hydra.util.Opt<String> comment) {
    java.util.Objects.requireNonNull((comment));
    return new Interface(id, type, context, comment, contents, description, displayName, extends_, schemas);
  }
  
  public Interface withContents(hydra.util.Opt<java.util.Set<hydra.ext.com.microsoft.azure.dtld.Interface_Contents>> contents) {
    java.util.Objects.requireNonNull((contents));
    return new Interface(id, type, context, comment, contents, description, displayName, extends_, schemas);
  }
  
  public Interface withDescription(hydra.util.Opt<String> description) {
    java.util.Objects.requireNonNull((description));
    return new Interface(id, type, context, comment, contents, description, displayName, extends_, schemas);
  }
  
  public Interface withDisplayName(hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((displayName));
    return new Interface(id, type, context, comment, contents, description, displayName, extends_, schemas);
  }
  
  public Interface withExtends(hydra.util.Opt<java.util.Set<hydra.ext.com.microsoft.azure.dtld.Interface>> extends_) {
    java.util.Objects.requireNonNull((extends_));
    return new Interface(id, type, context, comment, contents, description, displayName, extends_, schemas);
  }
  
  public Interface withSchemas(hydra.util.Opt<java.util.Set<hydra.ext.com.microsoft.azure.dtld.Schema_Interface>> schemas) {
    java.util.Objects.requireNonNull((schemas));
    return new Interface(id, type, context, comment, contents, description, displayName, extends_, schemas);
  }
}