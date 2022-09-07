package hydra.ext.haskell.ast;

/**
 * A field name and value
 */
public class FieldUpdate {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.FieldUpdate");
  
  public final hydra.ext.haskell.ast.Name name;
  
  public final hydra.ext.haskell.ast.Expression value;
  
  public FieldUpdate (hydra.ext.haskell.ast.Name name, hydra.ext.haskell.ast.Expression value) {
    this.name = name;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldUpdate)) {
      return false;
    }
    FieldUpdate o = (FieldUpdate) (other);
    return name.equals(o.name) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * value.hashCode();
  }
  
  public FieldUpdate withName(hydra.ext.haskell.ast.Name name) {
    return new FieldUpdate(name, value);
  }
  
  public FieldUpdate withValue(hydra.ext.haskell.ast.Expression value) {
    return new FieldUpdate(name, value);
  }
}