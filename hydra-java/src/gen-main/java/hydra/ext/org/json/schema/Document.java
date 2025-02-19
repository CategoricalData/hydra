// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.json.schema;

import java.io.Serializable;

public class Document implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.json.schema.Document");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_DEFINITIONS = new hydra.core.Name("definitions");
  
  public static final hydra.core.Name FIELD_NAME_ROOT = new hydra.core.Name("root");
  
  public final hydra.util.Opt<String> id;
  
  public final hydra.util.Opt<java.util.Map<hydra.ext.org.json.schema.Keyword, hydra.ext.org.json.schema.Schema>> definitions;
  
  public final hydra.ext.org.json.schema.Schema root;
  
  public Document (hydra.util.Opt<String> id, hydra.util.Opt<java.util.Map<hydra.ext.org.json.schema.Keyword, hydra.ext.org.json.schema.Schema>> definitions, hydra.ext.org.json.schema.Schema root) {
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((definitions));
    java.util.Objects.requireNonNull((root));
    this.id = id;
    this.definitions = definitions;
    this.root = root;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Document)) {
      return false;
    }
    Document o = (Document) (other);
    return id.equals(o.id) && definitions.equals(o.definitions) && root.equals(o.root);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * definitions.hashCode() + 5 * root.hashCode();
  }
  
  public Document withId(hydra.util.Opt<String> id) {
    java.util.Objects.requireNonNull((id));
    return new Document(id, definitions, root);
  }
  
  public Document withDefinitions(hydra.util.Opt<java.util.Map<hydra.ext.org.json.schema.Keyword, hydra.ext.org.json.schema.Schema>> definitions) {
    java.util.Objects.requireNonNull((definitions));
    return new Document(id, definitions, root);
  }
  
  public Document withRoot(hydra.ext.org.json.schema.Schema root) {
    java.util.Objects.requireNonNull((root));
    return new Document(id, definitions, root);
  }
}