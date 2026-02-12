// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ModuleName implements Serializable, Comparable<ModuleName> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ModuleName");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.ModuleName> name;
  
  public ModuleName (hydra.ext.java.syntax.Identifier identifier, hydra.util.Maybe<hydra.ext.java.syntax.ModuleName> name) {
    this.identifier = identifier;
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ModuleName)) {
      return false;
    }
    ModuleName o = (ModuleName) other;
    return java.util.Objects.equals(
      this.identifier,
      o.identifier) && java.util.Objects.equals(
      this.name,
      o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(identifier) + 3 * java.util.Objects.hashCode(name);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ModuleName other) {
    int cmp = 0;
    cmp = ((Comparable) identifier).compareTo(other.identifier);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      name.hashCode(),
      other.name.hashCode());
  }
  
  public ModuleName withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    return new ModuleName(identifier, name);
  }
  
  public ModuleName withName(hydra.util.Maybe<hydra.ext.java.syntax.ModuleName> name) {
    return new ModuleName(identifier, name);
  }
}
