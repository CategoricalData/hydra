package hydra.ext.java.syntax;

public abstract class FieldAccess_Qualifier {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.FieldAccess.Qualifier");
  
  private FieldAccess_Qualifier () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Primary instance) ;
    
    R visit(Super instance) ;
    
    R visit(Typed instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(FieldAccess_Qualifier instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Primary instance) {
      return otherwise((instance));
    }
    
    default R visit(Super instance) {
      return otherwise((instance));
    }
    
    default R visit(Typed instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Primary extends hydra.ext.java.syntax.FieldAccess_Qualifier {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Primary");
    
    public final hydra.ext.java.syntax.Primary value;
    
    public Primary (hydra.ext.java.syntax.Primary value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Primary)) {
        return false;
      }
      Primary o = (Primary) (other);
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
  
  public static final class Super extends hydra.ext.java.syntax.FieldAccess_Qualifier {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Super");
    
    public Super () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Super)) {
        return false;
      }
      Super o = (Super) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Typed extends hydra.ext.java.syntax.FieldAccess_Qualifier {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Typed");
    
    public final hydra.ext.java.syntax.TypeName value;
    
    public Typed (hydra.ext.java.syntax.TypeName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Typed)) {
        return false;
      }
      Typed o = (Typed) (other);
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