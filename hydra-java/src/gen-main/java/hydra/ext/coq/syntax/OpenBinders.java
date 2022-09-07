package hydra.ext.coq.syntax;

public abstract class OpenBinders {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.OpenBinders");
  
  private OpenBinders () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Type instance) ;
    
    R visit(Binders instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(OpenBinders instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Type instance) {
      return otherwise((instance));
    }
    
    default R visit(Binders instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Type extends hydra.ext.coq.syntax.OpenBinders {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Type");
    
    public final hydra.ext.coq.syntax.TypeBinders value;
    
    public Type (hydra.ext.coq.syntax.TypeBinders value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Type)) {
        return false;
      }
      Type o = (Type) (other);
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
  
  public static final class Binders extends hydra.ext.coq.syntax.OpenBinders {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Binders");
    
    public final java.util.List<hydra.ext.coq.syntax.Binder> value;
    
    public Binders (java.util.List<hydra.ext.coq.syntax.Binder> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Binders)) {
        return false;
      }
      Binders o = (Binders) (other);
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