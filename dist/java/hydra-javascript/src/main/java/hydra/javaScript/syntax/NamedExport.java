// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * Named exports (export {x, y as z})
 */
public class NamedExport implements Serializable, Comparable<NamedExport> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.NamedExport");

  public static final hydra.core.Name SPECIFIERS = new hydra.core.Name("specifiers");

  public static final hydra.core.Name SOURCE = new hydra.core.Name("source");

  public final java.util.List<hydra.javaScript.syntax.ExportSpecifier> specifiers;

  public final hydra.util.Maybe<hydra.javaScript.syntax.StringLiteral> source;

  public NamedExport (java.util.List<hydra.javaScript.syntax.ExportSpecifier> specifiers, hydra.util.Maybe<hydra.javaScript.syntax.StringLiteral> source) {
    this.specifiers = specifiers;
    this.source = source;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NamedExport)) {
      return false;
    }
    NamedExport o = (NamedExport) other;
    return java.util.Objects.equals(
      this.specifiers,
      o.specifiers) && java.util.Objects.equals(
      this.source,
      o.source);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(specifiers) + 3 * java.util.Objects.hashCode(source);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NamedExport other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      specifiers,
      other.specifiers);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      source,
      other.source);
  }

  public NamedExport withSpecifiers(java.util.List<hydra.javaScript.syntax.ExportSpecifier> specifiers) {
    return new NamedExport(specifiers, source);
  }

  public NamedExport withSource(hydra.util.Maybe<hydra.javaScript.syntax.StringLiteral> source) {
    return new NamedExport(specifiers, source);
  }
}
