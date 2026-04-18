// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A named import specifier (import {x as y} from ...)
 */
public class ImportSpecifier implements Serializable, Comparable<ImportSpecifier> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.ImportSpecifier");

  public static final hydra.core.Name IMPORTED = new hydra.core.Name("imported");

  public static final hydra.core.Name LOCAL = new hydra.core.Name("local");

  public final hydra.javaScript.syntax.Identifier imported;

  public final hydra.javaScript.syntax.Identifier local;

  public ImportSpecifier (hydra.javaScript.syntax.Identifier imported, hydra.javaScript.syntax.Identifier local) {
    this.imported = imported;
    this.local = local;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ImportSpecifier)) {
      return false;
    }
    ImportSpecifier o = (ImportSpecifier) other;
    return java.util.Objects.equals(
      this.imported,
      o.imported) && java.util.Objects.equals(
      this.local,
      o.local);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(imported) + 3 * java.util.Objects.hashCode(local);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ImportSpecifier other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      imported,
      other.imported);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      local,
      other.local);
  }

  public ImportSpecifier withImported(hydra.javaScript.syntax.Identifier imported) {
    return new ImportSpecifier(imported, local);
  }

  public ImportSpecifier withLocal(hydra.javaScript.syntax.Identifier local) {
    return new ImportSpecifier(imported, local);
  }
}
