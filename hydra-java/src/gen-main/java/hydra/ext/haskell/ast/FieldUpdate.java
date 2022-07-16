package hydra.ext.haskell.ast;

/**
 * A field name and value
 */
public class FieldUpdate {
  public final Name name;
  
  public final Expression value;
  
  public FieldUpdate (Name name, Expression value) {
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
  
  public FieldUpdate withName(Name name) {
    return new FieldUpdate(name, value);
  }
  
  public FieldUpdate withValue(Expression value) {
    return new FieldUpdate(name, value);
  }
}