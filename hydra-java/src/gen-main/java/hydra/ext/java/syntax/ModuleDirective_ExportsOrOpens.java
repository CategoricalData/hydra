package hydra.ext.java.syntax;

public class ModuleDirective_ExportsOrOpens {
  public final PackageName package_;
  
  /**
   * At least one module
   */
  public final java.util.List<ModuleName> modules;
  
  public ModuleDirective_ExportsOrOpens (PackageName package_, java.util.List<ModuleName> modules) {
    this.package_ = package_;
    this.modules = modules;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ModuleDirective_ExportsOrOpens)) {
      return false;
    }
    ModuleDirective_ExportsOrOpens o = (ModuleDirective_ExportsOrOpens) (other);
    return package_.equals(o.package_) && modules.equals(o.modules);
  }
  
  @Override
  public int hashCode() {
    return 2 * package_.hashCode() + 3 * modules.hashCode();
  }
  
  public ModuleDirective_ExportsOrOpens withPackage(PackageName package_) {
    return new ModuleDirective_ExportsOrOpens(package_, modules);
  }
  
  public ModuleDirective_ExportsOrOpens withModules(java.util.List<ModuleName> modules) {
    return new ModuleDirective_ExportsOrOpens(package_, modules);
  }
}