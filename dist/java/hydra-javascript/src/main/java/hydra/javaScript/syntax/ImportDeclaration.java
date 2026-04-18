// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * An import declaration
 */
public class ImportDeclaration implements Serializable, Comparable<ImportDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.ImportDeclaration");

  public static final hydra.core.Name SPECIFIERS = new hydra.core.Name("specifiers");

  public static final hydra.core.Name SOURCE = new hydra.core.Name("source");

  /**
   * What to import
   */
  public final java.util.List<hydra.javaScript.syntax.ImportClause> specifiers;

  /**
   * The module to import from
   */
  public final hydra.javaScript.syntax.StringLiteral source;

  public ImportDeclaration (java.util.List<hydra.javaScript.syntax.ImportClause> specifiers, hydra.javaScript.syntax.StringLiteral source) {
    this.specifiers = specifiers;
    this.source = source;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ImportDeclaration)) {
      return false;
    }
    ImportDeclaration o = (ImportDeclaration) other;
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
  public int compareTo(ImportDeclaration other) {
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

  public ImportDeclaration withSpecifiers(java.util.List<hydra.javaScript.syntax.ImportClause> specifiers) {
    return new ImportDeclaration(specifiers, source);
  }

  public ImportDeclaration withSource(hydra.javaScript.syntax.StringLiteral source) {
    return new ImportDeclaration(specifiers, source);
  }
}
