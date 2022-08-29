package hydra.ext.scala.meta;

public abstract class Name {
  private Name () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Value instance) ;
    
    R visit(Anonymous instance) ;
    
    R visit(Indeterminate instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Name instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Value instance) {
      return otherwise((instance));
    }
    
    default R visit(Anonymous instance) {
      return otherwise((instance));
    }
    
    default R visit(Indeterminate instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Value extends hydra.ext.scala.meta.Name {
    public final String value;
    
    public Value (String value) {
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
  
  public static final class Anonymous extends hydra.ext.scala.meta.Name {
    public Anonymous () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Anonymous)) {
        return false;
      }
      Anonymous o = (Anonymous) (other);
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
  
  public static final class Indeterminate extends hydra.ext.scala.meta.Name {
    public final hydra.ext.scala.meta.PredefString value;
    
    public Indeterminate (hydra.ext.scala.meta.PredefString value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Indeterminate)) {
        return false;
      }
      Indeterminate o = (Indeterminate) (other);
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