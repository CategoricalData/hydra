// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public abstract class Term10 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.Term10");
  
  public static final hydra.core.Name FIELD_NAME_APPLICATION = new hydra.core.Name("application");
  
  public static final hydra.core.Name FIELD_NAME_ONE_TERM = new hydra.core.Name("oneTerm");
  
  private Term10 () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Application instance) ;
    
    R visit(OneTerm instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Term10 instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Application instance) {
      return otherwise((instance));
    }
    
    default R visit(OneTerm instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Application extends hydra.ext.fr.inria.coq.syntax.Term10 implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.Application value;
    
    public Application (hydra.ext.fr.inria.coq.syntax.Application value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Application)) {
        return false;
      }
      Application o = (Application) (other);
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
  
  public static final class OneTerm extends hydra.ext.fr.inria.coq.syntax.Term10 implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.OneTerm value;
    
    public OneTerm (hydra.ext.fr.inria.coq.syntax.OneTerm value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OneTerm)) {
        return false;
      }
      OneTerm o = (OneTerm) (other);
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