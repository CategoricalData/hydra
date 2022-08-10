package hydra.core;

/**
 * An instance of a union type; i.e. a string-indexed generalization of inl() or inr()
 */
public class Union<M> {
  public final Name typeName;
  
  public final Field<M> field;
  
  public Union (Name typeName, Field<M> field) {
    this.typeName = typeName;
    this.field = field;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Union)) {
      return false;
    }
    Union o = (Union) (other);
    return typeName.equals(o.typeName) && field.equals(o.field);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeName.hashCode() + 3 * field.hashCode();
  }
  
  public Union withTypeName(Name typeName) {
    return new Union(typeName, field);
  }
  
  public Union withField(Field<M> field) {
    return new Union(typeName, field);
  }
}