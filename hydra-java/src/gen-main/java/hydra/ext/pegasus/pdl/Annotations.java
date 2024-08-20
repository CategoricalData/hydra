// Note: this is an automatically generated file. Do not edit.

package hydra.ext.pegasus.pdl;

import java.io.Serializable;

/**
 * Annotations which can be applied to record fields, aliased union members, enum symbols, or named schemas
 */
public class Annotations implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/pegasus/pdl.Annotations");
  
  public static final hydra.core.Name FIELD_NAME_DOC = new hydra.core.Name("doc");
  
  public static final hydra.core.Name FIELD_NAME_DEPRECATED = new hydra.core.Name("deprecated");
  
  public final hydra.util.Opt<String> doc;
  
  public final Boolean deprecated;
  
  public Annotations (hydra.util.Opt<String> doc, Boolean deprecated) {
    java.util.Objects.requireNonNull((doc));
    java.util.Objects.requireNonNull((deprecated));
    this.doc = doc;
    this.deprecated = deprecated;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Annotations)) {
      return false;
    }
    Annotations o = (Annotations) (other);
    return doc.equals(o.doc) && deprecated.equals(o.deprecated);
  }
  
  @Override
  public int hashCode() {
    return 2 * doc.hashCode() + 3 * deprecated.hashCode();
  }
  
  public Annotations withDoc(hydra.util.Opt<String> doc) {
    java.util.Objects.requireNonNull((doc));
    return new Annotations(doc, deprecated);
  }
  
  public Annotations withDeprecated(Boolean deprecated) {
    java.util.Objects.requireNonNull((deprecated));
    return new Annotations(doc, deprecated);
  }
}
