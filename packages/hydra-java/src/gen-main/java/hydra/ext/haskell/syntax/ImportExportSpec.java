// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.syntax;

import java.io.Serializable;

/**
 * An import or export specification
 */
public class ImportExportSpec implements Serializable, Comparable<ImportExportSpec> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.haskell.syntax.ImportExportSpec");

  public static final hydra.core.Name MODIFIER = new hydra.core.Name("modifier");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name SUBSPEC = new hydra.core.Name("subspec");

  /**
   * Optional import modifier
   */
  public final hydra.util.Maybe<hydra.ext.haskell.syntax.ImportModifier> modifier;

  /**
   * The name being imported or exported
   */
  public final hydra.ext.haskell.syntax.Name name;

  /**
   * Optional subspecification
   */
  public final hydra.util.Maybe<hydra.ext.haskell.syntax.SubspecImportExportSpec> subspec;

  public ImportExportSpec (hydra.util.Maybe<hydra.ext.haskell.syntax.ImportModifier> modifier, hydra.ext.haskell.syntax.Name name, hydra.util.Maybe<hydra.ext.haskell.syntax.SubspecImportExportSpec> subspec) {
    this.modifier = modifier;
    this.name = name;
    this.subspec = subspec;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ImportExportSpec)) {
      return false;
    }
    ImportExportSpec o = (ImportExportSpec) other;
    return java.util.Objects.equals(
      this.modifier,
      o.modifier) && java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.subspec,
      o.subspec);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(modifier) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(subspec);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ImportExportSpec other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      modifier,
      other.modifier);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      subspec,
      other.subspec);
  }

  public ImportExportSpec withModifier(hydra.util.Maybe<hydra.ext.haskell.syntax.ImportModifier> modifier) {
    return new ImportExportSpec(modifier, name, subspec);
  }

  public ImportExportSpec withName(hydra.ext.haskell.syntax.Name name) {
    return new ImportExportSpec(modifier, name, subspec);
  }

  public ImportExportSpec withSubspec(hydra.util.Maybe<hydra.ext.haskell.syntax.SubspecImportExportSpec> subspec) {
    return new ImportExportSpec(modifier, name, subspec);
  }
}
