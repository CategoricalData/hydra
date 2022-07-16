package hydra.ext.haskell.ast;

/**
 * An import or export specification
 */
public class ImportExportSpec {
  public final java.util.Optional<ImportModifier> modifier;
  
  public final Name name;
  
  public final java.util.Optional<ImportExportSpec_Subspec> subspec;
  
  public ImportExportSpec (java.util.Optional<ImportModifier> modifier, Name name, java.util.Optional<ImportExportSpec_Subspec> subspec) {
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
  
  public ImportExportSpec withModifier(java.util.Optional<ImportModifier> modifier) {
    return new ImportExportSpec(modifier, name, subspec);
  }
  
  public ImportExportSpec withName(Name name) {
    return new ImportExportSpec(modifier, name, subspec);
  }
  
  public ImportExportSpec withSubspec(java.util.Optional<ImportExportSpec_Subspec> subspec) {
    return new ImportExportSpec(modifier, name, subspec);
  }
}