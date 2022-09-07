package hydra.ext.tinkerpop.typed;

/**
 * The type of a value, such as a property value
 */
public abstract class Type {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/tinkerpop/typed.Type");
  
  private Type () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Literal instance) ;
    
    R visit(Collection instance) ;
    
    R visit(Element instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Type instance) {
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
  
  public static final class Literal extends hydra.ext.tinkerpop.typed.Type {
    public final hydra.core.LiteralType value;
    
    public Literal (hydra.core.LiteralType value) {
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
  
  public static final class Collection extends hydra.ext.tinkerpop.typed.Type {
    public final hydra.ext.tinkerpop.typed.CollectionType value;
    
    public Collection (hydra.ext.tinkerpop.typed.CollectionType value) {
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
  
  public static final class Element extends hydra.ext.tinkerpop.typed.Type {
    public final hydra.ext.tinkerpop.typed.IdType value;
    
    public Element (hydra.ext.tinkerpop.typed.IdType value) {
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