package hydra.ext.java.syntax;

public class ModuleDirective_Provides {
  public final TypeName to;
  
  /**
   * At least one type
   */
  public final java.util.List<TypeName> with;
  
  public ModuleDirective_Provides (TypeName to, java.util.List<TypeName> with) {
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
  
  public ModuleDirective_Provides withTo(TypeName to) {
    return new ModuleDirective_Provides(to, with);
  }
  
  public ModuleDirective_Provides withWith(java.util.List<TypeName> with) {
    return new ModuleDirective_Provides(to, with);
  }
}