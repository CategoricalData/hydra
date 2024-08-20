// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Importee_Rename implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Importee.Rename");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_RENAME = new hydra.core.Name("rename");
  
  public final hydra.ext.scala.meta.Name name;
  
  public final hydra.ext.scala.meta.Name rename;
  
  public Importee_Rename (hydra.ext.scala.meta.Name name, hydra.ext.scala.meta.Name rename) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((rename));
    this.name = name;
    this.rename = rename;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Importee_Rename)) {
      return false;
    }
    Importee_Rename o = (Importee_Rename) (other);
    return name.equals(o.name) && rename.equals(o.rename);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * rename.hashCode();
  }
  
  public Importee_Rename withName(hydra.ext.scala.meta.Name name) {
    java.util.Objects.requireNonNull((name));
    return new Importee_Rename(name, rename);
  }
  
  public Importee_Rename withRename(hydra.ext.scala.meta.Name rename) {
    java.util.Objects.requireNonNull((rename));
    return new Importee_Rename(name, rename);
  }
}
