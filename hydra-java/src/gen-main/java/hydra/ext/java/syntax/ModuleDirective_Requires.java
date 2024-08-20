// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ModuleDirective_Requires implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.ModuleDirective.Requires");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_MODULE = new hydra.core.Name("module");
  
  public final java.util.List<hydra.ext.java.syntax.RequiresModifier> modifiers;
  
  public final hydra.ext.java.syntax.ModuleName module;
  
  public ModuleDirective_Requires (java.util.List<hydra.ext.java.syntax.RequiresModifier> modifiers, hydra.ext.java.syntax.ModuleName module) {
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((module));
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
  
  public ModuleDirective_Requires withModifiers(java.util.List<hydra.ext.java.syntax.RequiresModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new ModuleDirective_Requires(modifiers, module);
  }
  
  public ModuleDirective_Requires withModule(hydra.ext.java.syntax.ModuleName module) {
    java.util.Objects.requireNonNull((module));
    return new ModuleDirective_Requires(modifiers, module);
  }
}
