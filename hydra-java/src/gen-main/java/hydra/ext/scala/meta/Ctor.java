package hydra.ext.scala.meta;

public abstract class Ctor {
  private Ctor () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Primary instance) ;
    
    R visit(Secondary instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Ctor instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Primary instance) {
      return otherwise((instance));
    }
    
    default R visit(Secondary instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Primary extends hydra.ext.scala.meta.Ctor {
    public final hydra.ext.scala.meta.Ctor_Primary value;
    
    public Primary (hydra.ext.scala.meta.Ctor_Primary value) {
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
  
  public static final class Secondary extends hydra.ext.scala.meta.Ctor {
    public final hydra.ext.scala.meta.Ctor_Secondary value;
    
    public Secondary (hydra.ext.scala.meta.Ctor_Secondary value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Secondary)) {
        return false;
      }
      Secondary o = (Secondary) (other);
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