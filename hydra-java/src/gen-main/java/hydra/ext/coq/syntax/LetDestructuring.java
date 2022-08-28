package hydra.ext.coq.syntax;

public abstract class LetDestructuring {
  private LetDestructuring () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Variant1 instance) ;
    
    R visit(Variant2 instance) ;
    
    R visit(Variant3 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LetDestructuring instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Variant1 instance) {
      return otherwise((instance));
    }
    
    default R visit(Variant2 instance) {
      return otherwise((instance));
    }
    
    default R visit(Variant3 instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Variant1 extends hydra.ext.coq.syntax.LetDestructuring {
    public final hydra.ext.coq.syntax.LetDestructuring_Variant1 value;
    
    public Variant1 (hydra.ext.coq.syntax.LetDestructuring_Variant1 value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variant1)) {
        return false;
      }
      Variant1 o = (Variant1) (other);
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
  
  public static final class Variant2 extends hydra.ext.coq.syntax.LetDestructuring {
    public final hydra.ext.coq.syntax.LetDestructuring_Variant2 value;
    
    public Variant2 (hydra.ext.coq.syntax.LetDestructuring_Variant2 value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variant2)) {
        return false;
      }
      Variant2 o = (Variant2) (other);
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
  
  public static final class Variant3 extends hydra.ext.coq.syntax.LetDestructuring {
    public final hydra.ext.coq.syntax.LetDestructuring_Variant3 value;
    
    public Variant3 (hydra.ext.coq.syntax.LetDestructuring_Variant3 value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variant3)) {
        return false;
      }
      Variant3 o = (Variant3) (other);
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