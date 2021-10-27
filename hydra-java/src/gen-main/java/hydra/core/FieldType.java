package hydra.core;

public class FieldType {
  public final hydra.core.FieldName name;
  
  public final hydra.core.Type type;
  
  /**
   * Constructs an immutable FieldType object
   */
  public FieldType(hydra.core.FieldName name, hydra.core.Type type) {
    this.name = name;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldType)) {
        return false;
    }
    FieldType o = (FieldType) other;
    return name.equals(o.name)
        && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode()
        + 3 * type.hashCode();
  }
  
  /**
   * Construct a new immutable FieldType object in which name is overridden
   */
  public FieldType withName(hydra.core.FieldName name) {
    return new FieldType(name, type);
  }
  
  /**
   * Construct a new immutable FieldType object in which type is overridden
   */
  public FieldType withType(hydra.core.Type type) {
    return new FieldType(name, type);
  }
}
