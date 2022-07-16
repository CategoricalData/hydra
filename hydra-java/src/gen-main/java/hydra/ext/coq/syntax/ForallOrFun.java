package hydra.ext.coq.syntax;

public abstract class ForallOrFun {
  private ForallOrFun () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Forall instance) ;
    
    R visit(Fun instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ForallOrFun instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Forall instance) {
      return otherwise((instance));
    }
    
    default R visit(Fun instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Forall extends ForallOrFun {
    public final Forall value;
    
    public Forall (Forall value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Forall)) {
        return false;
      }
      Forall o = (Forall) (other);
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
  
  public static final class Fun extends ForallOrFun {
    public final Fun value;
    
    public Fun (Fun value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Fun)) {
        return false;
      }
      Fun o = (Fun) (other);
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