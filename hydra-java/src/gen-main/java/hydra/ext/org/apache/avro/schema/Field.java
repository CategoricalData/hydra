// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.avro.schema;

import java.io.Serializable;

public class Field implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/avro/schema.Field");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_DOC = new hydra.core.Name("doc");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT = new hydra.core.Name("default");
  
  public static final hydra.core.Name FIELD_NAME_ORDER = new hydra.core.Name("order");
  
  public static final hydra.core.Name FIELD_NAME_ALIASES = new hydra.core.Name("aliases");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
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
  public final hydra.ext.org.apache.avro.schema.Schema type;
  
  /**
   * default value for this field, only used when reading instances that lack the field for schema evolution purposes
   */
  public final hydra.util.Opt<hydra.json.Value> default_;
  
  /**
   * specifies how this field impacts sort ordering of this record
   */
  public final hydra.util.Opt<hydra.ext.org.apache.avro.schema.Order> order;
  
  /**
   * a JSON array of strings, providing alternate names for this field
   */
  public final hydra.util.Opt<java.util.List<String>> aliases;
  
  /**
   * Any additional key/value pairs attached to the field
   */
  public final java.util.Map<String, hydra.json.Value> annotations;
  
  public Field (String name, hydra.util.Opt<String> doc, hydra.ext.org.apache.avro.schema.Schema type, hydra.util.Opt<hydra.json.Value> default_, hydra.util.Opt<hydra.ext.org.apache.avro.schema.Order> order, hydra.util.Opt<java.util.List<String>> aliases, java.util.Map<String, hydra.json.Value> annotations) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((doc));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((default_));
    java.util.Objects.requireNonNull((order));
    java.util.Objects.requireNonNull((aliases));
    java.util.Objects.requireNonNull((annotations));
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
    java.util.Objects.requireNonNull((name));
    return new Field(name, doc, type, default_, order, aliases, annotations);
  }
  
  public Field withDoc(hydra.util.Opt<String> doc) {
    java.util.Objects.requireNonNull((doc));
    return new Field(name, doc, type, default_, order, aliases, annotations);
  }
  
  public Field withType(hydra.ext.org.apache.avro.schema.Schema type) {
    java.util.Objects.requireNonNull((type));
    return new Field(name, doc, type, default_, order, aliases, annotations);
  }
  
  public Field withDefault(hydra.util.Opt<hydra.json.Value> default_) {
    java.util.Objects.requireNonNull((default_));
    return new Field(name, doc, type, default_, order, aliases, annotations);
  }
  
  public Field withOrder(hydra.util.Opt<hydra.ext.org.apache.avro.schema.Order> order) {
    java.util.Objects.requireNonNull((order));
    return new Field(name, doc, type, default_, order, aliases, annotations);
  }
  
  public Field withAliases(hydra.util.Opt<java.util.List<String>> aliases) {
    java.util.Objects.requireNonNull((aliases));
    return new Field(name, doc, type, default_, order, aliases, annotations);
  }
  
  public Field withAnnotations(java.util.Map<String, hydra.json.Value> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new Field(name, doc, type, default_, order, aliases, annotations);
  }
}
