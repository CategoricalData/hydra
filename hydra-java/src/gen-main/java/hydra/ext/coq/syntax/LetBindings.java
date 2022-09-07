package hydra.ext.coq.syntax;

public abstract class LetBindings {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.LetBindings");
  
  private LetBindings () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Named instance) ;
    
    R visit(Destructuring instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LetBindings instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Named instance) {
      return otherwise((instance));
    }
    
    default R visit(Destructuring instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Named extends hydra.ext.coq.syntax.LetBindings {
    public final hydra.ext.coq.syntax.LetNamed value;
    
    public Named (hydra.ext.coq.syntax.LetNamed value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Named)) {
        return false;
      }
      Named o = (Named) (other);
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
  
  public static final class Destructuring extends hydra.ext.coq.syntax.LetBindings {
    public final hydra.ext.coq.syntax.LetDestructuring value;
    
    public Destructuring (hydra.ext.coq.syntax.LetDestructuring value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Destructuring)) {
        return false;
      }
      Destructuring o = (Destructuring) (other);
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