// Note: this is an automatically generated file. Do not edit.

package hydra.langs.haskell.ast;

import java.io.Serializable;

/**
 * An import or export specification
 */
public class ImportExportSpec implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/haskell/ast.ImportExportSpec");
  
  public final java.util.Optional<hydra.langs.haskell.ast.ImportModifier> modifier;
  
  public final hydra.langs.haskell.ast.Name name;
  
  public final java.util.Optional<hydra.langs.haskell.ast.ImportExportSpec_Subspec> subspec;
  
  public ImportExportSpec (java.util.Optional<hydra.langs.haskell.ast.ImportModifier> modifier, hydra.langs.haskell.ast.Name name, java.util.Optional<hydra.langs.haskell.ast.ImportExportSpec_Subspec> subspec) {
    if (modifier == null) {
      throw new IllegalArgumentException("null value for 'modifier' argument");
    }
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (subspec == null) {
      throw new IllegalArgumentException("null value for 'subspec' argument");
    }
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
    return modifier.equals(o.modifier) && name.equals(o.name) && subspec.equals(o.subspec);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifier.hashCode() + 3 * name.hashCode() + 5 * subspec.hashCode();
  }
  
  public ImportExportSpec withModifier(java.util.Optional<hydra.langs.haskell.ast.ImportModifier> modifier) {
    if (modifier == null) {
      throw new IllegalArgumentException("null value for 'modifier' argument");
    }
    return new ImportExportSpec(modifier, name, subspec);
  }
  
  public ImportExportSpec withName(hydra.langs.haskell.ast.Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new ImportExportSpec(modifier, name, subspec);
  }
  
  public ImportExportSpec withSubspec(java.util.Optional<hydra.langs.haskell.ast.ImportExportSpec_Subspec> subspec) {
    if (subspec == null) {
      throw new IllegalArgumentException("null value for 'subspec' argument");
    }
    return new ImportExportSpec(modifier, name, subspec);
  }
}