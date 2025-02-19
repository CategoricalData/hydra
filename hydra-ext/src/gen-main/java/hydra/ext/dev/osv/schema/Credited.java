// Note: this is an automatically generated file. Do not edit.

package hydra.ext.dev.osv.schema;

import java.io.Serializable;

public class Credited implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.dev.osv.schema.Credited");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_CONTACT = new hydra.core.Name("contact");
  
  public final String name;
  
  public final hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.Url>> contact;
  
  public Credited (String name, hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.Url>> contact) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((contact));
    this.name = name;
    this.contact = contact;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Credited)) {
      return false;
    }
    Credited o = (Credited) (other);
    return name.equals(o.name) && contact.equals(o.contact);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * contact.hashCode();
  }
  
  public Credited withName(String name) {
    java.util.Objects.requireNonNull((name));
    return new Credited(name, contact);
  }
  
  public Credited withContact(hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.Url>> contact) {
    java.util.Objects.requireNonNull((contact));
    return new Credited(name, contact);
  }
}