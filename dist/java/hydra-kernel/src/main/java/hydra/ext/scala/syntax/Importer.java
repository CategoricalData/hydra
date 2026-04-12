// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Importer implements Serializable, Comparable<Importer> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Importer");

  public static final hydra.core.Name REF = new hydra.core.Name("ref");

  public static final hydra.core.Name IMPORTEES = new hydra.core.Name("importees");

  public final hydra.ext.scala.syntax.Data_Ref ref;

  public final java.util.List<hydra.ext.scala.syntax.Importee> importees;

  public Importer (hydra.ext.scala.syntax.Data_Ref ref, java.util.List<hydra.ext.scala.syntax.Importee> importees) {
    this.ref = ref;
    this.importees = importees;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Importer)) {
      return false;
    }
    Importer o = (Importer) other;
    return java.util.Objects.equals(
      this.ref,
      o.ref) && java.util.Objects.equals(
      this.importees,
      o.importees);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(ref) + 3 * java.util.Objects.hashCode(importees);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Importer other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      ref,
      other.ref);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      importees,
      other.importees);
  }

  public Importer withRef(hydra.ext.scala.syntax.Data_Ref ref) {
    return new Importer(ref, importees);
  }

  public Importer withImportees(java.util.List<hydra.ext.scala.syntax.Importee> importees) {
    return new Importer(ref, importees);
  }
}
