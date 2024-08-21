// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.avro.schema;

import java.io.Serializable;

public class Named implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/avro/schema.Named");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_NAMESPACE = new hydra.core.Name("namespace");
  
  public static final hydra.core.Name FIELD_NAME_ALIASES = new hydra.core.Name("aliases");
  
  public static final hydra.core.Name FIELD_NAME_DOC = new hydra.core.Name("doc");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
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
  
  public final hydra.ext.org.apache.avro.schema.NamedType type;
  
  /**
   * Any additional key/value pairs attached to the type
   */
  public final java.util.Map<String, hydra.json.Value> annotations;
  
  public Named (String name, hydra.util.Opt<String> namespace, hydra.util.Opt<java.util.List<String>> aliases, hydra.util.Opt<String> doc, hydra.ext.org.apache.avro.schema.NamedType type, java.util.Map<String, hydra.json.Value> annotations) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((namespace));
    java.util.Objects.requireNonNull((aliases));
    java.util.Objects.requireNonNull((doc));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((annotations));
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
    java.util.Objects.requireNonNull((name));
    return new Named(name, namespace, aliases, doc, type, annotations);
  }
  
  public Named withNamespace(hydra.util.Opt<String> namespace) {
    java.util.Objects.requireNonNull((namespace));
    return new Named(name, namespace, aliases, doc, type, annotations);
  }
  
  public Named withAliases(hydra.util.Opt<java.util.List<String>> aliases) {
    java.util.Objects.requireNonNull((aliases));
    return new Named(name, namespace, aliases, doc, type, annotations);
  }
  
  public Named withDoc(hydra.util.Opt<String> doc) {
    java.util.Objects.requireNonNull((doc));
    return new Named(name, namespace, aliases, doc, type, annotations);
  }
  
  public Named withType(hydra.ext.org.apache.avro.schema.NamedType type) {
    java.util.Objects.requireNonNull((type));
    return new Named(name, namespace, aliases, doc, type, annotations);
  }
  
  public Named withAnnotations(java.util.Map<String, hydra.json.Value> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new Named(name, namespace, aliases, doc, type, annotations);
  }
}
