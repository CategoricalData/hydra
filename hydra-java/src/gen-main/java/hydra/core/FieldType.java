package hydra.core;

/**
 * The name and type of a field
 */
public class FieldType<M> {
  public final hydra.core.FieldName name;
  
  public final hydra.core.Type<M> type;
  
  public FieldType (hydra.core.FieldName name, hydra.core.Type<M> type) {
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
  
  public FieldType withName(hydra.core.FieldName name) {
    return new FieldType(name, type);
  }
  
  public FieldType withType(hydra.core.Type<M> type) {
    return new FieldType(name, type);
  }
}