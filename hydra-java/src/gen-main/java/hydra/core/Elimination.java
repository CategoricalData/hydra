package hydra.core;

/**
 * A corresponding elimination for an introduction term
 */
public abstract class Elimination<M> {
  private Elimination () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Element instance) ;
    
    R visit(Nominal instance) ;
    
    R visit(Optional instance) ;
    
    R visit(Record instance) ;
    
    R visit(Union instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Elimination instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Element instance) {
      return otherwise((instance));
    }
    
    default R visit(Nominal instance) {
      return otherwise((instance));
    }
    
    default R visit(Optional instance) {
      return otherwise((instance));
    }
    
    default R visit(Record instance) {
      return otherwise((instance));
    }
    
    default R visit(Union instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * Eliminates an element by mapping it to its data term. This is Hydra's delta function.
   */
  public static final class Element<M> extends hydra.core.Elimination<M> {
    /**
     * Eliminates an element by mapping it to its data term. This is Hydra's delta function.
     */
    public final java.lang.Void value;
    
    public Element (java.lang.Void value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Element)) {
        return false;
      }
      Element o = (Element) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Eliminates a nominal term by extracting the wrapped term
   */
  public static final class Nominal<M> extends hydra.core.Elimination<M> {
    /**
     * Eliminates a nominal term by extracting the wrapped term
     */
    public final hydra.core.Name value;
    
    public Nominal (hydra.core.Name value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Nominal)) {
        return false;
      }
      Nominal o = (Nominal) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Eliminates an optional term by matching over the two possible cases
   */
  public static final class Optional<M> extends hydra.core.Elimination<M> {
    /**
     * Eliminates an optional term by matching over the two possible cases
     */
    public final hydra.core.OptionalCases<M> value;
    
    public Optional (hydra.core.OptionalCases<M> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Optional)) {
        return false;
      }
      Optional o = (Optional) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Eliminates a record by projecting a given field
   */
  public static final class Record<M> extends hydra.core.Elimination<M> {
    /**
     * Eliminates a record by projecting a given field
     */
    public final hydra.core.Projection value;
    
    public Record (hydra.core.Projection value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Record)) {
        return false;
      }
      Record o = (Record) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Eliminates a union term by matching over the fields of the union. This is a case statement.
   */
  public static final class Union<M> extends hydra.core.Elimination<M> {
    /**
     * Eliminates a union term by matching over the fields of the union. This is a case statement.
     */
    public final hydra.core.CaseStatement<M> value;
    
    public Union (hydra.core.CaseStatement<M> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Union)) {
        return false;
      }
      Union o = (Union) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}