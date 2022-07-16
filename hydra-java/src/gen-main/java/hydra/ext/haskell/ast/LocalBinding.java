package hydra.ext.haskell.ast;

public abstract class LocalBinding {
  private LocalBinding () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Signature instance) ;
    
    R visit(Value instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LocalBinding instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Signature instance) {
      return otherwise((instance));
    }
    
    default R visit(Value instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Signature extends LocalBinding {
    public final TypeSignature value;
    
    public Signature (TypeSignature value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Signature)) {
        return false;
      }
      Signature o = (Signature) (other);
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
  
  public static final class Value extends LocalBinding {
    public final ValueBinding value;
    
    public Value (ValueBinding value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Value)) {
        return false;
      }
      Value o = (Value) (other);
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