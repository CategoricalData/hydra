// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Importer implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Importer");
  
  public static final hydra.core.Name FIELD_NAME_REF = new hydra.core.Name("ref");
  
  public static final hydra.core.Name FIELD_NAME_IMPORTEES = new hydra.core.Name("importees");
  
  public final hydra.ext.scala.meta.Data_Ref ref;
  
  public final java.util.List<hydra.ext.scala.meta.Importee> importees;
  
  public Importer (hydra.ext.scala.meta.Data_Ref ref, java.util.List<hydra.ext.scala.meta.Importee> importees) {
    java.util.Objects.requireNonNull((ref));
    java.util.Objects.requireNonNull((importees));
    this.ref = ref;
    this.importees = importees;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Importer)) {
      return false;
    }
    Importer o = (Importer) (other);
    return ref.equals(o.ref) && importees.equals(o.importees);
  }
  
  @Override
  public int hashCode() {
    return 2 * ref.hashCode() + 3 * importees.hashCode();
  }
  
  public Importer withRef(hydra.ext.scala.meta.Data_Ref ref) {
    java.util.Objects.requireNonNull((ref));
    return new Importer(ref, importees);
  }
  
  public Importer withImportees(java.util.List<hydra.ext.scala.meta.Importee> importees) {
    java.util.Objects.requireNonNull((importees));
    return new Importer(ref, importees);
  }
}