// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Import implements Serializable, Comparable<Import> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Import");

  public static final hydra.core.Name IMPORTERS = new hydra.core.Name("importers");

  public final java.util.List<hydra.ext.scala.syntax.Importer> importers;

  public Import (java.util.List<hydra.ext.scala.syntax.Importer> importers) {
    this.importers = importers;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Import)) {
      return false;
    }
    Import o = (Import) other;
    return java.util.Objects.equals(
      this.importers,
      o.importers);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(importers);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Import other) {
    return hydra.util.Comparing.compare(
      importers,
      other.importers);
  }
}
