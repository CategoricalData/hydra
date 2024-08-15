// Note: this is an automatically generated file. Do not edit.

package hydra.langs.haskell.ast;

import java.io.Serializable;

/**
 * An import or export specification
 */
public class ImportExportSpec implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/haskell/ast.ImportExportSpec");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIER = new hydra.core.Name("modifier");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_SUBSPEC = new hydra.core.Name("subspec");
  
  public final hydra.util.Opt<hydra.langs.haskell.ast.ImportModifier> modifier;
  
  public final hydra.langs.haskell.ast.Name name;
  
  public final hydra.util.Opt<hydra.langs.haskell.ast.ImportExportSpec_Subspec> subspec;
  
  public ImportExportSpec (hydra.util.Opt<hydra.langs.haskell.ast.ImportModifier> modifier, hydra.langs.haskell.ast.Name name, hydra.util.Opt<hydra.langs.haskell.ast.ImportExportSpec_Subspec> subspec) {
    java.util.Objects.requireNonNull((modifier));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((subspec));
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
  
  public ImportExportSpec withModifier(hydra.util.Opt<hydra.langs.haskell.ast.ImportModifier> modifier) {
    java.util.Objects.requireNonNull((modifier));
    return new ImportExportSpec(modifier, name, subspec);
  }
  
  public ImportExportSpec withName(hydra.langs.haskell.ast.Name name) {
    java.util.Objects.requireNonNull((name));
    return new ImportExportSpec(modifier, name, subspec);
  }
  
  public ImportExportSpec withSubspec(hydra.util.Opt<hydra.langs.haskell.ast.ImportExportSpec_Subspec> subspec) {
    java.util.Objects.requireNonNull((subspec));
    return new ImportExportSpec(modifier, name, subspec);
  }
}