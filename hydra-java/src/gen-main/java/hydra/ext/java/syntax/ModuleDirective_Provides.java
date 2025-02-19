// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ModuleDirective_Provides implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ModuleDirective_Provides");
  
  public static final hydra.core.Name FIELD_NAME_TO = new hydra.core.Name("to");
  
  public static final hydra.core.Name FIELD_NAME_WITH = new hydra.core.Name("with");
  
  public final hydra.ext.java.syntax.TypeName to;
  
  /**
   * At least one type
   */
  public final java.util.List<hydra.ext.java.syntax.TypeName> with;
  
  public ModuleDirective_Provides (hydra.ext.java.syntax.TypeName to, java.util.List<hydra.ext.java.syntax.TypeName> with) {
    java.util.Objects.requireNonNull((to));
    java.util.Objects.requireNonNull((with));
    this.to = to;
    this.with = with;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ModuleDirective_Provides)) {
      return false;
    }
    ModuleDirective_Provides o = (ModuleDirective_Provides) (other);
    return to.equals(o.to) && with.equals(o.with);
  }
  
  @Override
  public int hashCode() {
    return 2 * to.hashCode() + 3 * with.hashCode();
  }
  
  public ModuleDirective_Provides withTo(hydra.ext.java.syntax.TypeName to) {
    java.util.Objects.requireNonNull((to));
    return new ModuleDirective_Provides(to, with);
  }
  
  public ModuleDirective_Provides withWith(java.util.List<hydra.ext.java.syntax.TypeName> with) {
    java.util.Objects.requireNonNull((with));
    return new ModuleDirective_Provides(to, with);
  }
}