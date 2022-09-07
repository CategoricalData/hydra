package hydra.ext.java.syntax;

public abstract class TypeArgument {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.TypeArgument");
  
  private TypeArgument () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Reference instance) ;
    
    R visit(Wildcard instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TypeArgument instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Reference instance) {
      return otherwise((instance));
    }
    
    default R visit(Wildcard instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Reference extends hydra.ext.java.syntax.TypeArgument {
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
  
  public static final class Wildcard extends hydra.ext.java.syntax.TypeArgument {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Wildcard");
    
    public final hydra.ext.java.syntax.Wildcard value;
    
    public Wildcard (hydra.ext.java.syntax.Wildcard value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Wildcard)) {
        return false;
      }
      Wildcard o = (Wildcard) (other);
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