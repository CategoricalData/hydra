// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ModuleDirective_ExportsOrOpens implements Serializable, Comparable<ModuleDirective_ExportsOrOpens> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ModuleDirective_ExportsOrOpens");
  
  public static final hydra.core.Name FIELD_NAME_PACKAGE = new hydra.core.Name("package");
  
  public static final hydra.core.Name FIELD_NAME_MODULES = new hydra.core.Name("modules");
  
  public final hydra.ext.java.syntax.PackageName package_;
  
  /**
   * At least one module
   */
  public final java.util.List<hydra.ext.java.syntax.ModuleName> modules;
  
  public ModuleDirective_ExportsOrOpens (hydra.ext.java.syntax.PackageName package_, java.util.List<hydra.ext.java.syntax.ModuleName> modules) {
    this.package_ = package_;
    this.modules = modules;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ModuleDirective_ExportsOrOpens)) {
      return false;
    }
    ModuleDirective_ExportsOrOpens o = (ModuleDirective_ExportsOrOpens) other;
    return java.util.Objects.equals(
      this.package_,
      o.package_) && java.util.Objects.equals(
      this.modules,
      o.modules);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(package_) + 3 * java.util.Objects.hashCode(modules);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ModuleDirective_ExportsOrOpens other) {
    int cmp = 0;
    cmp = ((Comparable) package_).compareTo(other.package_);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      modules.hashCode(),
      other.modules.hashCode());
  }
  
  public ModuleDirective_ExportsOrOpens withPackage(hydra.ext.java.syntax.PackageName package_) {
    return new ModuleDirective_ExportsOrOpens(package_, modules);
  }
  
  public ModuleDirective_ExportsOrOpens withModules(java.util.List<hydra.ext.java.syntax.ModuleName> modules) {
    return new ModuleDirective_ExportsOrOpens(package_, modules);
  }
}
