// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Import implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Import");
  
  public static final hydra.core.Name FIELD_NAME_IMPORTERS = new hydra.core.Name("importers");
  
  public final java.util.List<hydra.ext.scala.meta.Importer> importers;
  
  public Import (java.util.List<hydra.ext.scala.meta.Importer> importers) {
    java.util.Objects.requireNonNull((importers));
    this.importers = importers;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Import)) {
      return false;
    }
    Import o = (Import) (other);
    return importers.equals(o.importers);
  }
  
  @Override
  public int hashCode() {
    return 2 * importers.hashCode();
  }
}
