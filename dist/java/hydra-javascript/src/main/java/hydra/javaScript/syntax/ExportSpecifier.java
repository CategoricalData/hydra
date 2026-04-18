// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * An export specifier (x as y)
 */
public class ExportSpecifier implements Serializable, Comparable<ExportSpecifier> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.ExportSpecifier");

  public static final hydra.core.Name LOCAL = new hydra.core.Name("local");

  public static final hydra.core.Name EXPORTED = new hydra.core.Name("exported");

  public final hydra.javaScript.syntax.Identifier local;

  public final hydra.javaScript.syntax.Identifier exported;

  public ExportSpecifier (hydra.javaScript.syntax.Identifier local, hydra.javaScript.syntax.Identifier exported) {
    this.local = local;
    this.exported = exported;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExportSpecifier)) {
      return false;
    }
    ExportSpecifier o = (ExportSpecifier) other;
    return java.util.Objects.equals(
      this.local,
      o.local) && java.util.Objects.equals(
      this.exported,
      o.exported);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(local) + 3 * java.util.Objects.hashCode(exported);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ExportSpecifier other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      local,
      other.local);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      exported,
      other.exported);
  }

  public ExportSpecifier withLocal(hydra.javaScript.syntax.Identifier local) {
    return new ExportSpecifier(local, exported);
  }

  public ExportSpecifier withExported(hydra.javaScript.syntax.Identifier exported) {
    return new ExportSpecifier(local, exported);
  }
}
