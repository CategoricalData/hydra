package hydra.core;

/**
 * The name and type of a field
 */
public class FieldType<M> {
  public final FieldName name;
  
  public final Type<M> type;
  
  public FieldType (FieldName name, Type<M> type) {
    this.name = name;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldType)) {
      return false;
    }
    FieldType o = (FieldType) (other);
    return name.equals(o.name) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * type.hashCode();
  }
  
  public FieldType withName(FieldName name) {
    return new FieldType(name, type);
  }
  
  public FieldType withType(Type<M> type) {
    return new FieldType(name, type);
  }
}