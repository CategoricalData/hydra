// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Export implements Serializable, Comparable<Export> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Export");

  public static final hydra.core.Name IMPORTERS = new hydra.core.Name("importers");

  public final hydra.util.ConsList<hydra.ext.scala.meta.Importer> importers;

  public Export (hydra.util.ConsList<hydra.ext.scala.meta.Importer> importers) {
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
    return ((Comparable) importers).compareTo(other.importers);
  }
}
