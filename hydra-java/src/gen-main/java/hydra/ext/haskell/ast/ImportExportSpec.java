// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * An import or export specification
 */
public class ImportExportSpec implements Serializable, Comparable<ImportExportSpec> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.ImportExportSpec");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIER = new hydra.core.Name("modifier");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_SUBSPEC = new hydra.core.Name("subspec");
  
  /**
   * Optional import modifier
   */
  public final hydra.util.Maybe<hydra.ext.haskell.ast.ImportModifier> modifier;
  
  /**
   * The name being imported or exported
   */
  public final hydra.ext.haskell.ast.Name name;
  
  /**
   * Optional subspecification
   */
  public final hydra.util.Maybe<hydra.ext.haskell.ast.SubspecImportExportSpec> subspec;
  
  public ImportExportSpec (hydra.util.Maybe<hydra.ext.haskell.ast.ImportModifier> modifier, hydra.ext.haskell.ast.Name name, hydra.util.Maybe<hydra.ext.haskell.ast.SubspecImportExportSpec> subspec) {
    this.modifier = modifier;
    this.name = name;
    this.subspec = subspec;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ImportExportSpec)) {
      return false;
    }
    ImportExportSpec o = (ImportExportSpec) (other);
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
    cmp = Integer.compare(
      modifier.hashCode(),
      other.modifier.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (name)).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      subspec.hashCode(),
      other.subspec.hashCode());
  }
  
  public ImportExportSpec withModifier(hydra.util.Maybe<hydra.ext.haskell.ast.ImportModifier> modifier) {
    return new ImportExportSpec(modifier, name, subspec);
  }
  
  public ImportExportSpec withName(hydra.ext.haskell.ast.Name name) {
    return new ImportExportSpec(modifier, name, subspec);
  }
  
  public ImportExportSpec withSubspec(hydra.util.Maybe<hydra.ext.haskell.ast.SubspecImportExportSpec> subspec) {
    return new ImportExportSpec(modifier, name, subspec);
  }
}
