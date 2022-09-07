package hydra.ext.haskell.ast;

/**
 * A field (name/type pair)
 */
public class Field {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.Field");
  
  public final hydra.ext.haskell.ast.Name name;
  
  public final hydra.ext.haskell.ast.Type type;
  
  public Field (hydra.ext.haskell.ast.Name name, hydra.ext.haskell.ast.Type type) {
    this.name = name;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Field)) {
      return false;
    }
    Field o = (Field) (other);
    return name.equals(o.name) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * type.hashCode();
  }
  
  public Field withName(hydra.ext.haskell.ast.Name name) {
    return new Field(name, type);
  }
  
  public Field withType(hydra.ext.haskell.ast.Type type) {
    return new Field(name, type);
  }
}