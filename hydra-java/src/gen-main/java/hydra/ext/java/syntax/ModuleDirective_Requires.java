package hydra.ext.java.syntax;

public class ModuleDirective_Requires {
  public final java.util.List<RequiresModifier> modifiers;
  
  public final ModuleName module;
  
  public ModuleDirective_Requires (java.util.List<RequiresModifier> modifiers, ModuleName module) {
    this.modifiers = modifiers;
    this.module = module;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ModuleDirective_Requires)) {
      return false;
    }
    ModuleDirective_Requires o = (ModuleDirective_Requires) (other);
    return modifiers.equals(o.modifiers) && module.equals(o.module);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * module.hashCode();
  }
  
  public ModuleDirective_Requires withModifiers(java.util.List<RequiresModifier> modifiers) {
    return new ModuleDirective_Requires(modifiers, module);
  }
  
  public ModuleDirective_Requires withModule(ModuleName module) {
    return new ModuleDirective_Requires(modifiers, module);
  }
}