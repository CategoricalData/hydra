package hydra.ext.java.syntax;

public class ModuleName {
  public final Identifier identifier;
  
  public final java.util.Optional<ModuleName> name;
  
  public ModuleName (Identifier identifier, java.util.Optional<ModuleName> name) {
    this.identifier = identifier;
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ModuleName)) {
      return false;
    }
    ModuleName o = (ModuleName) (other);
    return identifier.equals(o.identifier) && name.equals(o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * identifier.hashCode() + 3 * name.hashCode();
  }
  
  public ModuleName withIdentifier(Identifier identifier) {
    return new ModuleName(identifier, name);
  }
  
  public ModuleName withName(java.util.Optional<ModuleName> name) {
    return new ModuleName(identifier, name);
  }
}