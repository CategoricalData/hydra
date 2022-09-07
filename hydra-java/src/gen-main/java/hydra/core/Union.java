package hydra.core;

/**
 * An instance of a union type; i.e. a string-indexed generalization of inl() or inr()
 */
public class Union<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.Union");
  
  public final hydra.core.Name typeName;
  
  public final hydra.core.Field<M> field;
  
  public Union (hydra.core.Name typeName, hydra.core.Field<M> field) {
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
  
  public Union withTypeName(hydra.core.Name typeName) {
    return new Union(typeName, field);
  }
  
  public Union withField(hydra.core.Field<M> field) {
    return new Union(typeName, field);
  }
}