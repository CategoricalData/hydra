package hydra.ext.coq.syntax;

public abstract class Application {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Application");
  
  private Application () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Normal instance) ;
    
    R visit(Annotated instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Application instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Normal instance) {
      return otherwise((instance));
    }
    
    default R visit(Annotated instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Normal extends hydra.ext.coq.syntax.Application {
    public final hydra.ext.coq.syntax.NormalApplication value;
    
    public Normal (hydra.ext.coq.syntax.NormalApplication value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Normal)) {
        return false;
      }
      Normal o = (Normal) (other);
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
  
  public static final class Annotated extends hydra.ext.coq.syntax.Application {
    public final hydra.ext.coq.syntax.AnnotatedApplication value;
    
    public Annotated (hydra.ext.coq.syntax.AnnotatedApplication value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Annotated)) {
        return false;
      }
      Annotated o = (Annotated) (other);
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