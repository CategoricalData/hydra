// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ModuleDirective_Requires implements Serializable, Comparable<ModuleDirective_Requires> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ModuleDirective_Requires");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_MODULE = new hydra.core.Name("module");
  
  public final java.util.List<hydra.ext.java.syntax.RequiresModifier> modifiers;
  
  public final hydra.ext.java.syntax.ModuleName module;
  
  public ModuleDirective_Requires (java.util.List<hydra.ext.java.syntax.RequiresModifier> modifiers, hydra.ext.java.syntax.ModuleName module) {
    this.modifiers = modifiers;
    this.module = module;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ModuleDirective_Requires)) {
      return false;
    }
    ModuleDirective_Requires o = (ModuleDirective_Requires) other;
    return java.util.Objects.equals(
      this.modifiers,
      o.modifiers) && java.util.Objects.equals(
      this.module,
      o.module);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(modifiers) + 3 * java.util.Objects.hashCode(module);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ModuleDirective_Requires other) {
    int cmp = 0;
    cmp = Integer.compare(
      modifiers.hashCode(),
      other.modifiers.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) module).compareTo(other.module);
  }
  
  public ModuleDirective_Requires withModifiers(java.util.List<hydra.ext.java.syntax.RequiresModifier> modifiers) {
    return new ModuleDirective_Requires(modifiers, module);
  }
  
  public ModuleDirective_Requires withModule(hydra.ext.java.syntax.ModuleName module) {
    return new ModuleDirective_Requires(modifiers, module);
  }
}
