// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Export implements Serializable, Comparable<Export> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Export");

  public static final hydra.core.Name IMPORTERS = new hydra.core.Name("importers");

  public final java.util.List<hydra.scala.syntax.Importer> importers;

  public Export (java.util.List<hydra.scala.syntax.Importer> importers) {
    this.importers = importers;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Export)) {
      return false;
    }
    Export o = (Export) other;
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
  public int compareTo(Export other) {
    return hydra.util.Comparing.compare(
      importers,
      other.importers);
  }
}
