// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Importee_Rename implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Importee.Rename");
  
  public final hydra.langs.scala.meta.Name name;
  
  public final hydra.langs.scala.meta.Name rename;
  
  public Importee_Rename (hydra.langs.scala.meta.Name name, hydra.langs.scala.meta.Name rename) {
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
  
  public Importee_Rename withName(hydra.langs.scala.meta.Name name) {
    java.util.Objects.requireNonNull((name));
    return new Importee_Rename(name, rename);
  }
  
  public Importee_Rename withRename(hydra.langs.scala.meta.Name rename) {
    java.util.Objects.requireNonNull((rename));
    return new Importee_Rename(name, rename);
  }
}