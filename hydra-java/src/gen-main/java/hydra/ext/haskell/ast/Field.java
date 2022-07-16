package hydra.ext.haskell.ast;

/**
 * A field (name/type pair)
 */
public class Field {
  public final Name name;
  
  public final Type type;
  
  public Field (Name name, Type type) {
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
  
  public Field withName(Name name) {
    return new Field(name, type);
  }
  
  public Field withType(Type type) {
    return new Field(name, type);
  }
}