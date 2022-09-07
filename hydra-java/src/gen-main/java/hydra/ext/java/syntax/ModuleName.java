package hydra.ext.java.syntax;

public class ModuleName {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.ModuleName");
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public final java.util.Optional<hydra.ext.java.syntax.ModuleName> name;
  
  public ModuleName (hydra.ext.java.syntax.Identifier identifier, java.util.Optional<hydra.ext.java.syntax.ModuleName> name) {
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
  
  public ModuleName withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    return new ModuleName(identifier, name);
  }
  
  public ModuleName withName(java.util.Optional<hydra.ext.java.syntax.ModuleName> name) {
    return new ModuleName(identifier, name);
  }
}