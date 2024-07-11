// Note: this is an automatically generated file. Do not edit.

package hydra.langs.avro.schema;

import java.io.Serializable;

public class Field implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/avro/schema.Field");
  
  /**
   * a JSON string providing the name of the field
   */
  public final String name;
  
  /**
   * a JSON string describing this field for users
   */
  public final hydra.util.Opt<String> doc;
  
  /**
   * a schema
   */
  public final hydra.langs.avro.schema.Schema type;
  
  /**
   * default value for this field, only used when reading instances that lack the field for schema evolution purposes
   */
  public final hydra.util.Opt<hydra.json.Value> default_;
  
  /**
   * specifies how this field impacts sort ordering of this record
   */
  public final hydra.util.Opt<hydra.langs.avro.schema.Order> order;
  
  /**
   * a JSON array of strings, providing alternate names for this field
   */
  public final hydra.util.Opt<java.util.List<String>> aliases;
  
  /**
   * Any additional key/value pairs attached to the field
   */
  public final java.util.Map<String, hydra.json.Value> annotations;
  
  public Field (String name, hydra.util.Opt<String> doc, hydra.langs.avro.schema.Schema type, hydra.util.Opt<hydra.json.Value> default_, hydra.util.Opt<hydra.langs.avro.schema.Order> order, hydra.util.Opt<java.util.List<String>> aliases, java.util.Map<String, hydra.json.Value> annotations) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (doc == null) {
      throw new IllegalArgumentException("null value for 'doc' argument");
    }
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    if (default_ == null) {
      throw new IllegalArgumentException("null value for 'default' argument");
    }
    if (order == null) {
      throw new IllegalArgumentException("null value for 'order' argument");
    }
    if (aliases == null) {
      throw new IllegalArgumentException("null value for 'aliases' argument");
    }
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    this.name = name;
    this.doc = doc;
    this.type = type;
    this.default_ = default_;
    this.order = order;
    this.aliases = aliases;
    this.annotations = annotations;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Field)) {
      return false;
    }
    Field o = (Field) (other);
    return name.equals(o.name) && doc.equals(o.doc) && type.equals(o.type) && default_.equals(o.default_) && order.equals(o.order) && aliases.equals(o.aliases) && annotations.equals(o.annotations);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * doc.hashCode() + 5 * type.hashCode() + 7 * default_.hashCode() + 11 * order.hashCode() + 13 * aliases.hashCode() + 17 * annotations.hashCode();
  }
  
  public Field withName(String name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new Field(name, doc, type, default_, order, aliases, annotations);
  }
  
  public Field withDoc(hydra.util.Opt<String> doc) {
    if (doc == null) {
      throw new IllegalArgumentException("null value for 'doc' argument");
    }
    return new Field(name, doc, type, default_, order, aliases, annotations);
  }
  
  public Field withType(hydra.langs.avro.schema.Schema type) {
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    return new Field(name, doc, type, default_, order, aliases, annotations);
  }
  
  public Field withDefault(hydra.util.Opt<hydra.json.Value> default_) {
    if (default_ == null) {
      throw new IllegalArgumentException("null value for 'default' argument");
    }
    return new Field(name, doc, type, default_, order, aliases, annotations);
  }
  
  public Field withOrder(hydra.util.Opt<hydra.langs.avro.schema.Order> order) {
    if (order == null) {
      throw new IllegalArgumentException("null value for 'order' argument");
    }
    return new Field(name, doc, type, default_, order, aliases, annotations);
  }
  
  public Field withAliases(hydra.util.Opt<java.util.List<String>> aliases) {
    if (aliases == null) {
      throw new IllegalArgumentException("null value for 'aliases' argument");
    }
    return new Field(name, doc, type, default_, order, aliases, annotations);
  }
  
  public Field withAnnotations(java.util.Map<String, hydra.json.Value> annotations) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    return new Field(name, doc, type, default_, order, aliases, annotations);
  }
}