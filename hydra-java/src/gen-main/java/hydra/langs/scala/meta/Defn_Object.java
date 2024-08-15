// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Defn_Object implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/scala/meta.Defn.Object");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public final hydra.langs.scala.meta.Data_Name name;
  
  public Defn_Object (hydra.langs.scala.meta.Data_Name name) {
    java.util.Objects.requireNonNull((name));
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Defn_Object)) {
      return false;
    }
    Defn_Object o = (Defn_Object) (other);
    return name.equals(o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode();
  }
}