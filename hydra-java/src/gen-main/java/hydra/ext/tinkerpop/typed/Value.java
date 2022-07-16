package hydra.ext.tinkerpop.typed;

/**
 * A concrete value such as a number or string, a collection of other values, or an element reference
 */
public abstract class Value {
  private Value () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Literal instance) ;
    
    R visit(Collection instance) ;
    
    R visit(Element instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Value instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Literal instance) {
      return otherwise((instance));
    }
    
    default R visit(Collection instance) {
      return otherwise((instance));
    }
    
    default R visit(Element instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Literal extends Value {
    public final hydra.core.Literal value;
    
    public Literal (hydra.core.Literal value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Literal)) {
        return false;
      }
      Literal o = (Literal) (other);
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
  
  public static final class Collection extends Value {
    public final CollectionValue value;
    
    public Collection (CollectionValue value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Collection)) {
        return false;
      }
      Collection o = (Collection) (other);
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
  
  public static final class Element extends Value {
    public final Id value;
    
    public Element (Id value) {
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
}