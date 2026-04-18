// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * Export all declaration (export * from ...)
 */
public class ExportAllDeclaration implements Serializable, Comparable<ExportAllDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.ExportAllDeclaration");

  public static final hydra.core.Name EXPORTED = new hydra.core.Name("exported");

  public static final hydra.core.Name SOURCE = new hydra.core.Name("source");

  public final hydra.util.Maybe<hydra.javaScript.syntax.Identifier> exported;

  public final hydra.javaScript.syntax.StringLiteral source;

  public ExportAllDeclaration (hydra.util.Maybe<hydra.javaScript.syntax.Identifier> exported, hydra.javaScript.syntax.StringLiteral source) {
    this.exported = exported;
    this.source = source;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExportAllDeclaration)) {
      return false;
    }
    ExportAllDeclaration o = (ExportAllDeclaration) other;
    return java.util.Objects.equals(
      this.exported,
      o.exported) && java.util.Objects.equals(
      this.source,
      o.source);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(exported) + 3 * java.util.Objects.hashCode(source);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ExportAllDeclaration other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      exported,
      other.exported);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      source,
      other.source);
  }

  public ExportAllDeclaration withExported(hydra.util.Maybe<hydra.javaScript.syntax.Identifier> exported) {
    return new ExportAllDeclaration(exported, source);
  }

  public ExportAllDeclaration withSource(hydra.javaScript.syntax.StringLiteral source) {
    return new ExportAllDeclaration(exported, source);
  }
}
