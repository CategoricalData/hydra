package hydra.ext.java.syntax;

public abstract class Type {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Type");
  
  private Type () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Primitive instance) ;
    
    R visit(Reference instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Type instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Primitive instance) {
      return otherwise((instance));
    }
    
    default R visit(Reference instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Primitive extends hydra.ext.java.syntax.Type {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Primitive");
    
    public final hydra.ext.java.syntax.PrimitiveTypeWithAnnotations value;
    
    public Primitive (hydra.ext.java.syntax.PrimitiveTypeWithAnnotations value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Primitive)) {
        return false;
      }
      Primitive o = (Primitive) (other);
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
  
  public static final class Reference extends hydra.ext.java.syntax.Type {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Reference");
    
    public final hydra.ext.java.syntax.ReferenceType value;
    
    public Reference (hydra.ext.java.syntax.ReferenceType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Reference)) {
        return false;
      }
      Reference o = (Reference) (other);
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