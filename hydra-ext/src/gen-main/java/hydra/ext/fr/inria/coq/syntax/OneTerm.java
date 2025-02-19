// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public abstract class OneTerm implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.OneTerm");
  
  public static final hydra.core.Name FIELD_NAME_EXPLICIT = new hydra.core.Name("explicit");
  
  public static final hydra.core.Name FIELD_NAME_TERM1 = new hydra.core.Name("term1");
  
  private OneTerm () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Explicit instance) ;
    
    R visit(Term1 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(OneTerm instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Explicit instance) {
      return otherwise((instance));
    }
    
    default R visit(Term1 instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Explicit extends hydra.ext.fr.inria.coq.syntax.OneTerm implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.QualidAnnotated value;
    
    public Explicit (hydra.ext.fr.inria.coq.syntax.QualidAnnotated value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Explicit)) {
        return false;
      }
      Explicit o = (Explicit) (other);
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
  
  public static final class Term1 extends hydra.ext.fr.inria.coq.syntax.OneTerm implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.Term1 value;
    
    public Term1 (hydra.ext.fr.inria.coq.syntax.Term1 value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Term1)) {
        return false;
      }
      Term1 o = (Term1) (other);
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