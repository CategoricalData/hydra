// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ModuleName implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ModuleName");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public final hydra.util.Opt<hydra.ext.java.syntax.ModuleName> name;
  
  public ModuleName (hydra.ext.java.syntax.Identifier identifier, hydra.util.Opt<hydra.ext.java.syntax.ModuleName> name) {
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((name));
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
    java.util.Objects.requireNonNull((identifier));
    return new ModuleName(identifier, name);
  }
  
  public ModuleName withName(hydra.util.Opt<hydra.ext.java.syntax.ModuleName> name) {
    java.util.Objects.requireNonNull((name));
    return new ModuleName(identifier, name);
  }
}