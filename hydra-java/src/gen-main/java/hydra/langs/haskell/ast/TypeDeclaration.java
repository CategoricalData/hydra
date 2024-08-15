// Note: this is an automatically generated file. Do not edit.

package hydra.langs.haskell.ast;

import java.io.Serializable;

public class TypeDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/haskell/ast.TypeDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public final hydra.langs.haskell.ast.DeclarationHead name;
  
  public final hydra.langs.haskell.ast.Type type;
  
  public TypeDeclaration (hydra.langs.haskell.ast.DeclarationHead name, hydra.langs.haskell.ast.Type type) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((type));
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
    java.util.Objects.requireNonNull((name));
    return new TypeDeclaration(name, type);
  }
  
  public TypeDeclaration withType(hydra.langs.haskell.ast.Type type) {
    java.util.Objects.requireNonNull((type));
    return new TypeDeclaration(name, type);
  }
}