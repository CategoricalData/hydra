package hydra.core;

public class Projection {
  public final Name typeName;
  
  public final FieldName field;
  
  public Projection (Name typeName, FieldName field) {
    this.typeName = typeName;
    this.field = field;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Projection)) {
      return false;
    }
    Projection o = (Projection) (other);
    return typeName.equals(o.typeName) && field.equals(o.field);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeName.hashCode() + 3 * field.hashCode();
  }
  
  public Projection withTypeName(Name typeName) {
    return new Projection(typeName, field);
  }
  
  public Projection withField(FieldName field) {
    return new Projection(typeName, field);
  }
}