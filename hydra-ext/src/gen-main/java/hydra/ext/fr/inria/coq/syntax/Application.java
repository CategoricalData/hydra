// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public abstract class Application implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.Application");
  
  public static final hydra.core.Name FIELD_NAME_NORMAL = new hydra.core.Name("normal");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATED = new hydra.core.Name("annotated");
  
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
  
  public static final class Normal extends hydra.ext.fr.inria.coq.syntax.Application implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.NormalApplication value;
    
    public Normal (hydra.ext.fr.inria.coq.syntax.NormalApplication value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Annotated extends hydra.ext.fr.inria.coq.syntax.Application implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.AnnotatedApplication value;
    
    public Annotated (hydra.ext.fr.inria.coq.syntax.AnnotatedApplication value) {
      java.util.Objects.requireNonNull((value));
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