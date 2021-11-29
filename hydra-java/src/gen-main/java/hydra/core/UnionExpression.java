package hydra.core;

/**
 * A variant expression, or instance of a union type
 */
public class UnionExpression<A> {
  /**
   * The name of the type of this union expression
   */
  public final hydra.core.Name context;
  
  public final hydra.core.Field<A> field;
  
  /**
   * Constructs an immutable UnionExpression object
   */
  public UnionExpression(hydra.core.Name context, hydra.core.Field<A> field) {
    this.context = context;
    this.field = field;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnionExpression)) {
        return false;
    }
    UnionExpression o = (UnionExpression) other;
    return context.equals(o.context)
        && field.equals(o.field);
  }
  
  @Override
  public int hashCode() {
    return 2 * context.hashCode()
        + 3 * field.hashCode();
  }
  
  /**
   * Construct a new immutable UnionExpression object in which context is overridden
   */
  public UnionExpression withContext(hydra.core.Name context) {
    return new UnionExpression(context, field);
  }
  
  /**
   * Construct a new immutable UnionExpression object in which field is overridden
   */
  public UnionExpression withField(hydra.core.Field<A> field) {
    return new UnionExpression(context, field);
  }
}
