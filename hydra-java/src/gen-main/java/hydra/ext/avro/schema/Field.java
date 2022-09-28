package hydra.ext.avro.schema;

public class Field {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/avro/schema.Field");
  
  /**
   * a JSON string providing the name of the field
   */
  public final String name;
  
  /**
   * a JSON string describing this field for users
   */
  public final java.util.Optional<String> doc;
  
  /**
   * a schema
   */
  public final hydra.ext.avro.schema.Schema type;
  
  /**
   * default value for this field, only used when reading instances that lack the field for schema evolution purposes
   */
  public final java.util.Optional<hydra.ext.json.model.Value> default_;
  
  /**
   * specifies how this field impacts sort ordering of this record
   */
  public final java.util.Optional<hydra.ext.avro.schema.Order> order;
  
  /**
   * a JSON array of strings, providing alternate names for this field
   */
  public final java.util.Optional<java.util.List<String>> aliases;
  
  public Field (String name, java.util.Optional<String> doc, hydra.ext.avro.schema.Schema type, java.util.Optional<hydra.ext.json.model.Value> default_, java.util.Optional<hydra.ext.avro.schema.Order> order, java.util.Optional<java.util.List<String>> aliases) {
    this.name = name;
    this.doc = doc;
    this.type = type;
    this.default_ = default_;
    this.order = order;
    this.aliases = aliases;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Field)) {
      return false;
    }
    Field o = (Field) (other);
    return name.equals(o.name) && doc.equals(o.doc) && type.equals(o.type) && default_.equals(o.default_) && order.equals(o.order) && aliases.equals(o.aliases);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * doc.hashCode() + 5 * type.hashCode() + 7 * default_.hashCode() + 11 * order.hashCode() + 13 * aliases.hashCode();
  }
  
  public Field withName(String name) {
    return new Field(name, doc, type, default_, order, aliases);
  }
  
  public Field withDoc(java.util.Optional<String> doc) {
    return new Field(name, doc, type, default_, order, aliases);
  }
  
  public Field withType(hydra.ext.avro.schema.Schema type) {
    return new Field(name, doc, type, default_, order, aliases);
  }
  
  public Field withDefault(java.util.Optional<hydra.ext.json.model.Value> default_) {
    return new Field(name, doc, type, default_, order, aliases);
  }
  
  public Field withOrder(java.util.Optional<hydra.ext.avro.schema.Order> order) {
    return new Field(name, doc, type, default_, order, aliases);
  }
  
  public Field withAliases(java.util.Optional<java.util.List<String>> aliases) {
    return new Field(name, doc, type, default_, order, aliases);
  }
}