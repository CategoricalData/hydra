// Note: this is an automatically generated file. Do not edit.

package hydra.langs.avro.schema;

import java.io.Serializable;

public class Named implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/avro/schema.Named");
  
  /**
   * a string naming this schema
   */
  public final String name;
  
  /**
   * a string that qualifies the name
   */
  public final hydra.util.Opt<String> namespace;
  
  /**
   * a JSON array of strings, providing alternate names for this schema
   */
  public final hydra.util.Opt<java.util.List<String>> aliases;
  
  /**
   * a JSON string providing documentation to the user of this schema
   */
  public final hydra.util.Opt<String> doc;
  
  public final hydra.langs.avro.schema.NamedType type;
  
  /**
   * Any additional key/value pairs attached to the type
   */
  public final java.util.Map<String, hydra.json.Value> annotations;
  
  public Named (String name, hydra.util.Opt<String> namespace, hydra.util.Opt<java.util.List<String>> aliases, hydra.util.Opt<String> doc, hydra.langs.avro.schema.NamedType type, java.util.Map<String, hydra.json.Value> annotations) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (namespace == null) {
      throw new IllegalArgumentException("null value for 'namespace' argument");
    }
    if (aliases == null) {
      throw new IllegalArgumentException("null value for 'aliases' argument");
    }
    if (doc == null) {
      throw new IllegalArgumentException("null value for 'doc' argument");
    }
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    this.name = name;
    this.namespace = namespace;
    this.aliases = aliases;
    this.doc = doc;
    this.type = type;
    this.annotations = annotations;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Named)) {
      return false;
    }
    Named o = (Named) (other);
    return name.equals(o.name) && namespace.equals(o.namespace) && aliases.equals(o.aliases) && doc.equals(o.doc) && type.equals(o.type) && annotations.equals(o.annotations);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * namespace.hashCode() + 5 * aliases.hashCode() + 7 * doc.hashCode() + 11 * type.hashCode() + 13 * annotations.hashCode();
  }
  
  public Named withName(String name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new Named(name, namespace, aliases, doc, type, annotations);
  }
  
  public Named withNamespace(hydra.util.Opt<String> namespace) {
    if (namespace == null) {
      throw new IllegalArgumentException("null value for 'namespace' argument");
    }
    return new Named(name, namespace, aliases, doc, type, annotations);
  }
  
  public Named withAliases(hydra.util.Opt<java.util.List<String>> aliases) {
    if (aliases == null) {
      throw new IllegalArgumentException("null value for 'aliases' argument");
    }
    return new Named(name, namespace, aliases, doc, type, annotations);
  }
  
  public Named withDoc(hydra.util.Opt<String> doc) {
    if (doc == null) {
      throw new IllegalArgumentException("null value for 'doc' argument");
    }
    return new Named(name, namespace, aliases, doc, type, annotations);
  }
  
  public Named withType(hydra.langs.avro.schema.NamedType type) {
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    return new Named(name, namespace, aliases, doc, type, annotations);
  }
  
  public Named withAnnotations(java.util.Map<String, hydra.json.Value> annotations) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    return new Named(name, namespace, aliases, doc, type, annotations);
  }
}