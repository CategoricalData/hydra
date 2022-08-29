package hydra.ext.tinkerpop.v3;

/**
 * A property value
 */
public abstract class PropertyValue {
  private PropertyValue () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Literal instance) ;
    
    R visit(List instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PropertyValue instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Literal instance) {
      return otherwise((instance));
    }
    
    default R visit(List instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Literal extends hydra.ext.tinkerpop.v3.PropertyValue {
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
  
  public static final class List extends hydra.ext.tinkerpop.v3.PropertyValue {
    public final java.util.List<hydra.ext.tinkerpop.v3.PropertyValue> value;
    
    public List (java.util.List<hydra.ext.tinkerpop.v3.PropertyValue> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) (other);
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