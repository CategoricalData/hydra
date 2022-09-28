package hydra.ext.avro.schema;

public class Named {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/avro/schema.Named");
  
  /**
   * a string naming this schema
   */
  public final String name;
  
  /**
   * a string that qualifies the name
   */
  public final java.util.Optional<String> namespace;
  
  /**
   * a JSON array of strings, providing alternate names for this schema
   */
  public final java.util.Optional<java.util.List<String>> aliases;
  
  /**
   * a JSON string providing documentation to the user of this schema
   */
  public final java.util.Optional<String> doc;
  
  public final hydra.ext.avro.schema.Named type;
  
  public Named (String name, java.util.Optional<String> namespace, java.util.Optional<java.util.List<String>> aliases, java.util.Optional<String> doc, hydra.ext.avro.schema.Named type) {
    this.name = name;
    this.namespace = namespace;
    this.aliases = aliases;
    this.doc = doc;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Named)) {
      return false;
    }
    Named o = (Named) (other);
    return name.equals(o.name) && namespace.equals(o.namespace) && aliases.equals(o.aliases) && doc.equals(o.doc) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * namespace.hashCode() + 5 * aliases.hashCode() + 7 * doc.hashCode() + 11 * type.hashCode();
  }
  
  public Named withName(String name) {
    return new Named(name, namespace, aliases, doc, type);
  }
  
  public Named withNamespace(java.util.Optional<String> namespace) {
    return new Named(name, namespace, aliases, doc, type);
  }
  
  public Named withAliases(java.util.Optional<java.util.List<String>> aliases) {
    return new Named(name, namespace, aliases, doc, type);
  }
  
  public Named withDoc(java.util.Optional<String> doc) {
    return new Named(name, namespace, aliases, doc, type);
  }
  
  public Named withType(hydra.ext.avro.schema.Named type) {
    return new Named(name, namespace, aliases, doc, type);
  }
}