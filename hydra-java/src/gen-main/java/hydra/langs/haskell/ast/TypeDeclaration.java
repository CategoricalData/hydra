// Note: this is an automatically generated file. Do not edit.

package hydra.langs.haskell.ast;

import java.io.Serializable;

public class TypeDeclaration implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/haskell/ast.TypeDeclaration");
  
  public final hydra.langs.haskell.ast.DeclarationHead name;
  
  public final hydra.langs.haskell.ast.Type type;
  
  public TypeDeclaration (hydra.langs.haskell.ast.DeclarationHead name, hydra.langs.haskell.ast.Type type) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    this.name = name;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeDeclaration)) {
      return false;
    }
    TypeDeclaration o = (TypeDeclaration) (other);
    return name.equals(o.name) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * type.hashCode();
  }
  
  public TypeDeclaration withName(hydra.langs.haskell.ast.DeclarationHead name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new TypeDeclaration(name, type);
  }
  
  public TypeDeclaration withType(hydra.langs.haskell.ast.Type type) {
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    return new TypeDeclaration(name, type);
  }
}