package hydra.core;

/**
 * A projection of a field from a record
 */
public class Projection {
  /**
   * The projected field
   */
  public final hydra.core.FieldName field;
  
  /**
   * The name of the record type which defines the field
   */
  public final hydra.core.Name context;
  
  /**
   * Constructs an immutable Projection object
   */
  public Projection(hydra.core.FieldName field, hydra.core.Name context) {
    this.field = field;
    this.context = context;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Projection)) {
        return false;
    }
    Projection o = (Projection) other;
    return field.equals(o.field)
        && context.equals(o.context);
  }
  
  @Override
  public int hashCode() {
    return 2 * field.hashCode()
        + 3 * context.hashCode();
  }
  
  /**
   * Construct a new immutable Projection object in which field is overridden
   */
  public Projection withField(hydra.core.FieldName field) {
    return new Projection(field, context);
  }
  
  /**
   * Construct a new immutable Projection object in which context is overridden
   */
  public Projection withContext(hydra.core.Name context) {
    return new Projection(field, context);
  }
}
