package hydra.core;

public class Term<A> {
  public final hydra.core.Expression<A> data;
  
  public final A meta;
  
  /**
   * Constructs an immutable Term object
   */
  public Term(hydra.core.Expression<A> data, A meta) {
    this.data = data;
    this.meta = meta;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Term)) {
        return false;
    }
    Term o = (Term) other;
    return data.equals(o.data)
        && meta.equals(o.meta);
  }
  
  @Override
  public int hashCode() {
    return 2 * data.hashCode()
        + 3 * meta.hashCode();
  }
  
  /**
   * Construct a new immutable Term object in which data is overridden
   */
  public Term withData(hydra.core.Expression<A> data) {
    return new Term(data, meta);
  }
  
  /**
   * Construct a new immutable Term object in which meta is overridden
   */
  public Term withMeta(A meta) {
    return new Term(data, meta);
  }
}
