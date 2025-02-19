// Note: this is an automatically generated file. Do not edit.

package hydra.ext.dev.osv.schema;

import java.io.Serializable;

public class Reference implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.dev.osv.schema.Reference");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_URL = new hydra.core.Name("url");
  
  public final hydra.ext.dev.osv.schema.ReferenceType type;
  
  public final hydra.ext.dev.osv.schema.Url url;
  
  public Reference (hydra.ext.dev.osv.schema.ReferenceType type, hydra.ext.dev.osv.schema.Url url) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((url));
    this.type = type;
    this.url = url;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Reference)) {
      return false;
    }
    Reference o = (Reference) (other);
    return type.equals(o.type) && url.equals(o.url);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * url.hashCode();
  }
  
  public Reference withType(hydra.ext.dev.osv.schema.ReferenceType type) {
    java.util.Objects.requireNonNull((type));
    return new Reference(type, url);
  }
  
  public Reference withUrl(hydra.ext.dev.osv.schema.Url url) {
    java.util.Objects.requireNonNull((url));
    return new Reference(type, url);
  }
}