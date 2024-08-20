// Note: this is an automatically generated file. Do not edit.

package hydra.ext.dev.osv.schema;

import java.io.Serializable;

public class Package_ implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/dev/osv/schema.Package");
  
  public static final hydra.core.Name FIELD_NAME_ECOSYSTEM = new hydra.core.Name("ecosystem");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_PURL = new hydra.core.Name("purl");
  
  public final hydra.ext.dev.osv.schema.Ecosystem ecosystem;
  
  public final String name;
  
  public final hydra.util.Opt<hydra.ext.dev.osv.schema.Url> purl;
  
  public Package_ (hydra.ext.dev.osv.schema.Ecosystem ecosystem, String name, hydra.util.Opt<hydra.ext.dev.osv.schema.Url> purl) {
    java.util.Objects.requireNonNull((ecosystem));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((purl));
    this.ecosystem = ecosystem;
    this.name = name;
    this.purl = purl;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Package_)) {
      return false;
    }
    Package_ o = (Package_) (other);
    return ecosystem.equals(o.ecosystem) && name.equals(o.name) && purl.equals(o.purl);
  }
  
  @Override
  public int hashCode() {
    return 2 * ecosystem.hashCode() + 3 * name.hashCode() + 5 * purl.hashCode();
  }
  
  public Package_ withEcosystem(hydra.ext.dev.osv.schema.Ecosystem ecosystem) {
    java.util.Objects.requireNonNull((ecosystem));
    return new Package_(ecosystem, name, purl);
  }
  
  public Package_ withName(String name) {
    java.util.Objects.requireNonNull((name));
    return new Package_(ecosystem, name, purl);
  }
  
  public Package_ withPurl(hydra.util.Opt<hydra.ext.dev.osv.schema.Url> purl) {
    java.util.Objects.requireNonNull((purl));
    return new Package_(ecosystem, name, purl);
  }
}